# Load required libraries
library(tidyverse)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# Create a custom theme for all plots
custom_theme <- function() {
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
}

# Define color palettes for consistent use
strata_colors <- c("Urban Poor" = brewer.pal(3, "Set1")[1], "Urban Non-Poor" = brewer.pal(3, "Set1")[2], "Rural" = brewer.pal(3, "Set1")[3])
method_colors <- brewer.pal(3, "Pastel2")[1:2]
vaccine_colors <- brewer.pal(3, "Pastel2")[2:3]

# Source the data loading script to access all data objects
source("scripts/load_data.R")

# Create outputs directory if it doesn't exist
dir.create("outputs", showWarnings = FALSE)

#get age 1 and age 2 populations for 2024-2050
age1_2_pop <- pop_data %>% 
  filter(age %in% c(1, 2))

# Calculate baseline coverage rates (inverse of zero dose) for each group by country
baseline_coverage <- zero_dose_disparity %>%
  mutate(coverage_percent = 100 - zero_dose_percent) %>%
  select(country, group, proportion_of_children_percent, coverage_percent)

# Get most recent historical coverage data by country and vaccine
recent_coverage <- historical_coverage %>%
  group_by(country, vaccine = dose) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(country, vaccine, year, coverage)

# Get the beta regression model coefficients for projecting future coverage
model_coefs <- model_coefficients %>%
  rename(vaccine = dose)

# Function to interpolate urban rates for every year
interpolate_urban_rates <- function(urban_rates, start_year = 2020, end_year = 2050) {
  # Get unique countries
  countries <- unique(urban_rates$country)
  
  # Create a sequence of years
  years <- start_year:end_year
  
  # Create empty results dataframe
  interpolated_rates <- data.frame()
  
  # For each country, interpolate values for every year
  for (country_name in countries) {
    # Extract data for this country
    country_data <- urban_rates %>% 
      filter(country == country_name) %>%
      arrange(year)
    
    # If we have at least 2 data points, we can interpolate
    if (nrow(country_data) >= 2) {
      # Perform linear interpolation
      interp_fn <- approxfun(x = country_data$year, 
                            y = country_data$urban_rate, 
                            method = "linear", 
                            rule = 2)  # rule=2 uses closest value for extrapolation
      
      # Generate interpolated values for all years
      interpolated_values <- interp_fn(years)
      
      # Create dataframe for this country
      country_interp <- data.frame(
        country = country_name,
        year = years,
        urban_rate = interpolated_values
      )
      
      # Append to results
      interpolated_rates <- bind_rows(interpolated_rates, country_interp)
    } else {
      # If we don't have enough data for interpolation, just use what we have
      interpolated_rates <- bind_rows(interpolated_rates, country_data)
    }
  }
  
  return(interpolated_rates)
}

# Interpolate urban rates for all years
interpolated_urban_rates <- interpolate_urban_rates(urban_rates, 2020, 2050)

# Print a sample to verify
print("Sample of interpolated urban rates:")
print(interpolated_urban_rates %>% 
        filter(country == "Global") %>% 
        select(country, year, urban_rate) %>%
        arrange(year) %>%
        head(10))

# Function for beta regression (trend-based) projections of coverage by strata
project_coverage_by_strata <- function(start_year = 2024, end_year = 2050, method = "historical_progress") {
  
  # Get the urban rate projections (now using interpolated data)
  urban_proj <- interpolated_urban_rates %>%
    filter(year >= start_year, year <= end_year) %>%
    select(country, year, urban_rate)
  
  # For each country and year, calculate expected coverage by strata
  coverage_projections <- crossing(
    distinct(recent_coverage, country, vaccine),
    tibble(year = start_year:end_year)
  ) %>%
    left_join(urban_proj, by = c("country", "year"))
  
  # Get recent historical coverage to use as common starting point
  coverage_projections <- coverage_projections %>%
    left_join(recent_coverage, by = c("country", "vaccine")) %>%
    # Only keep rows where we have both urban rates and coverage
    filter(!is.na(urban_rate), !is.na(coverage)) %>%
    rename(recent_coverage_value = coverage, recent_year = year.y) %>%
    rename(year = year.x)
  
  # Pre-join with model_coefs to ensure both methods use the same set of data
  coverage_projections <- coverage_projections %>%
    left_join(model_coefs, by = c("country", "vaccine"))
  
  # Only continue with countries/vaccines that have model coefficients
  # This ensures both methods use the same dataset
  coverage_projections <- coverage_projections %>%
    filter(!is.na(intercept))
  
  # Calculate projected overall coverage based on method
  if (method == "historical_progress") {
    # Project future coverage using beta regression model
    coverage_projections <- coverage_projections %>%
      mutate(
        # Calculate a new intercept based on the most recent historical value
        # Using logit transformation: log(p/(1-p)) = intercept + slope*x
        # This ensures we start exactly at the recent coverage value
        new_intercept = log(recent_coverage_value / (1 - recent_coverage_value)),
        
        # Calculate projected coverage using only the slope from the model
        # and the new intercept based on recent coverage
        logit_coverage = new_intercept + slope * (year - recent_year),
        model_projected_coverage = pmin(0.99, pmax(0.01, exp(logit_coverage) / (1 + exp(logit_coverage)))),
        
        # Use model projections for all years
        projected_coverage = model_projected_coverage
      )
  } else if (method == "static") {
    # Use static projections (most recent value held constant)
    coverage_projections <- coverage_projections %>%
      # Use most recent value as constant projection
      mutate(projected_coverage = recent_coverage_value)
  }
  
  # Join with baseline coverage by strata for both methods
  coverage_projections <- coverage_projections %>%
    left_join(
      zero_dose_disparity %>% 
        pivot_wider(
          names_from = group, 
          values_from = c(proportion_of_children_percent, zero_dose_percent),
          names_glue = "{group}_{.value}"
        ),
      by = "country"
    )
  
  # Calculate projected coverage by strata - this applies to both static and beta_regression
  coverage_projections <- coverage_projections %>%
    mutate(
      # Assuming rural proportion decreases as urban increases
      rural_proportion = 100 - (urban_rate * 100),
      
      # Assuming the urban poor/non-poor split remains constant within urban areas
      `urban poor_adjusted` = `urban poor_proportion_of_children_percent` * urban_rate * 100 / 
                          (`urban poor_proportion_of_children_percent` + `urban non-poor_proportion_of_children_percent`),
      `urban non-poor_adjusted` = `urban non-poor_proportion_of_children_percent` * urban_rate * 100 / 
                             (`urban poor_proportion_of_children_percent` + `urban non-poor_proportion_of_children_percent`),
      
      # Overall coverage value - the core difference between methods
      avg_projected_coverage = projected_coverage * 100,
      
      # Apply disparities based on zero dose rates (higher zero dose = lower coverage)
      # This formula distributes the overall coverage across strata based on zero-dose disparities
      urban_poor_coverage = pmin(99, avg_projected_coverage * (1 - `urban poor_zero_dose_percent`) / 
                               (1 - (`urban poor_proportion_of_children_percent` * `urban poor_zero_dose_percent` + 
                                    `urban non-poor_proportion_of_children_percent` * `urban non-poor_zero_dose_percent` + 
                                    `rural_zero_dose_percent` * `rural_proportion_of_children_percent`))),
      
      urban_nonpoor_coverage = pmin(99, avg_projected_coverage * (1 - `urban non-poor_zero_dose_percent`) / 
                                  (1 - (`urban poor_proportion_of_children_percent` * `urban poor_zero_dose_percent` + 
                                       `urban non-poor_proportion_of_children_percent` * `urban non-poor_zero_dose_percent` + 
                                       `rural_zero_dose_percent` * `rural_proportion_of_children_percent`))),
      
      rural_coverage = pmin(99, avg_projected_coverage * (1 - `rural_zero_dose_percent`) / 
                          (1 - (`urban poor_proportion_of_children_percent` * `urban poor_zero_dose_percent` + 
                               `urban non-poor_proportion_of_children_percent` * `urban non-poor_zero_dose_percent` + 
                               `rural_zero_dose_percent` * `rural_proportion_of_children_percent`))),
      
      # Add method tag for comparison
      method = method
    )
  
  return(coverage_projections)
}

# Generate both sets of projections
coverage_projections_historical <- project_coverage_by_strata(start_year = 2023, method = "historical_progress")
coverage_projections_static <- project_coverage_by_strata(start_year = 2023, method = "static")

# Combine the projections for comparison
coverage_projections_combined <- bind_rows(
  coverage_projections_historical,
  coverage_projections_static
)

# Calculate absolute numbers by joining with population data
# DTP for age 1, measles for age 2
calculate_coverage_numbers <- function(projections) {
  projections %>%
    mutate(age = case_when(
      vaccine %in% c("DTP1", "DTP3") ~ 1,
      vaccine %in% c("measles1", "measles2") ~ 2,
      TRUE ~ NA_real_
    )) %>%
    filter(!is.na(age)) %>%
    left_join(age1_2_pop, by = c("country", "year", "age")) %>%
    filter(!is.na(population)) %>%
    mutate(
      urban_poor_count = population * (`urban poor_adjusted`/100) * (urban_poor_coverage/100),
      urban_nonpoor_count = population * (`urban non-poor_adjusted`/100) * (urban_nonpoor_coverage/100),
      rural_count = population * (rural_proportion/100) * (rural_coverage/100),
      total_vaccinated = urban_poor_count + urban_nonpoor_count + rural_count,
      overall_coverage = total_vaccinated / population * 100
    )
}

# Apply the calculation to both sets of projections
coverage_numbers_historical <- calculate_coverage_numbers(coverage_projections_historical)
coverage_numbers_static <- calculate_coverage_numbers(coverage_projections_static)
coverage_numbers_combined <- calculate_coverage_numbers(coverage_projections_combined)

# Print summary of rows per year to check if we have 2024 data
print("Checking data availability by year:")
cat("Historical progress model years: ")
print(sort(unique(coverage_numbers_historical$year)))
cat("Static model years: ")
print(sort(unique(coverage_numbers_static$year)))

# Check if we have 2024 data for both methods
print("VERIFICATION - 2024 Values Should Match Exactly:")
historical_2024 <- coverage_numbers_historical %>% 
  filter(country == "Global", year == 2024) %>% 
  select(country, year, vaccine, overall_coverage) %>%
  arrange(vaccine)

static_2024 <- coverage_numbers_static %>% 
  filter(country == "Global", year == 2024) %>% 
  select(country, year, vaccine, overall_coverage) %>%
  arrange(vaccine)

print(bind_rows(
  historical_2024 %>% mutate(method = "Historical Progress 2000-2019"),
  static_2024 %>% mutate(method = "Static")
) %>% arrange(vaccine, method))

# Create summary by country, year, and vaccine for both methods
summarize_coverage <- function(coverage_numbers) {
  coverage_numbers %>%
    group_by(country, year, vaccine, method) %>%
    summarize(
      population = sum(population),
      urban_poor_vaccinated = sum(urban_poor_count),
      urban_nonpoor_vaccinated = sum(urban_nonpoor_count),
      rural_vaccinated = sum(rural_count),
      total_vaccinated = sum(total_vaccinated),
      overall_coverage = total_vaccinated / population * 100,
      urban_poor_coverage = mean(urban_poor_coverage),
      urban_nonpoor_coverage = mean(urban_nonpoor_coverage),
      rural_coverage = mean(rural_coverage),
      .groups = "drop"
    )
}

coverage_summary_historical <- summarize_coverage(coverage_numbers_historical)
coverage_summary_static <- summarize_coverage(coverage_numbers_static)
coverage_summary_combined <- summarize_coverage(coverage_numbers_combined)

# Print a quick summary of the results for both methods
print("HISTORICAL PROJECTIONS - Global coverage:")
print(coverage_summary_historical %>% 
        filter(country == "Global", year %in% c(2025, 2030, 2040, 2050)) %>% 
        select(country, year, vaccine, overall_coverage))

print("STATIC PROJECTIONS - Global coverage:")
print(coverage_summary_static %>% 
        filter(country == "Global", year %in% c(2025, 2030, 2040, 2050)) %>% 
        select(country, year, vaccine, overall_coverage))

# Let's check if 2023 values are identical between methods
print("VERIFICATION - 2023 Values Should Match Exactly:")
historical_2023 <- coverage_summary_historical %>% 
  filter(country == "Global", year == 2023) %>% 
  select(country, year, vaccine, overall_coverage) %>%
  arrange(vaccine)

static_2023 <- coverage_summary_static %>% 
  filter(country == "Global", year == 2023) %>% 
  select(country, year, vaccine, overall_coverage) %>%
  arrange(vaccine)

print(bind_rows(
  historical_2023 %>% mutate(method = "Historical Progress 2000-2019"),
  static_2023 %>% mutate(method = "Static")
) %>% arrange(vaccine, method))

# Save results
write_csv(coverage_projections_historical, "outputs/coverage_projections_historical.csv")
write_csv(coverage_projections_static, "outputs/coverage_projections_static.csv")
write_csv(coverage_summary_combined, "outputs/coverage_summary_comparison.csv")

# Calculate zero-dose children by strata for both methods
calculate_zero_dose <- function(coverage_numbers) {
  coverage_numbers %>%
    group_by(country, year, vaccine, method) %>%
    summarize(
      population = sum(population),
      urban_poor_population = sum(population * (`urban poor_adjusted`/100)),
      urban_nonpoor_population = sum(population * (`urban non-poor_adjusted`/100)),
      rural_population = sum(population * (rural_proportion/100)),
      
      urban_poor_zero_dose = urban_poor_population * (1 - mean(urban_poor_coverage)/100),
      urban_nonpoor_zero_dose = urban_nonpoor_population * (1 - mean(urban_nonpoor_coverage)/100),
      rural_zero_dose = rural_population * (1 - mean(rural_coverage)/100),
      
      total_zero_dose = urban_poor_zero_dose + urban_nonpoor_zero_dose + rural_zero_dose,
      overall_zero_dose_percent = total_zero_dose / population * 100,
      .groups = "drop"
    )
}

zero_dose_summary_combined <- calculate_zero_dose(coverage_numbers_combined)

# Save zero-dose results
write_csv(zero_dose_summary_combined, "outputs/zero_dose_summary_comparison.csv")

# Visualize trends for a few selected countries
plot_countries <- c("Global", "Nigeria", "India", "Pakistan", "Ethiopia", "Indonesia", "Angola", "Colombia", "Afghanistan")

# Create method comparison plot - Overall coverage (Historical Progress vs. Static)
method_comparison <- coverage_summary_combined %>%
  filter(country %in% plot_countries, vaccine == "DTP1") %>%
  # Convert country to factor to ensure Global appears first
  mutate(country = factor(country, levels = plot_countries)) %>%
  select(country, year, method, overall_coverage)

# Create the method comparison plot
p_comparison <- ggplot(method_comparison, aes(x = year, y = overall_coverage, color = method)) +
  geom_line(linewidth = 1) +
  facet_wrap(~country) +
  labs(
    title = "Vaccination Coverage Projections: Historical Progress 2000-2019 vs. Static",
    subtitle = "DTP1 Overall Coverage (2024-2050)",
    x = "Year",
    y = "Coverage (%)",
    color = "Projection Method"
  ) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  scale_color_manual(values = method_colors,
                     labels = c("Historical Progress 2000-2019", "Static (Constant)")) +
  custom_theme()

# Save the method comparison plot
ggsave("outputs/method_comparison_plot.png", p_comparison, width = 12, height = 10, dpi = 600, bg = "white")
ggsave("outputs/method_comparison_plot.pdf", p_comparison, width = 12, height = 10, dpi = 600, bg = "white")

# Difference in zero-dose children between methods
zero_dose_diff <- zero_dose_summary_combined %>%
  filter(country %in% plot_countries, vaccine == "DTP1") %>%
  # Convert country to factor to ensure Global appears first
  mutate(country = factor(country, levels = plot_countries)) %>%
  select(country, year, method, total_zero_dose) %>%
  pivot_wider(names_from = method, values_from = total_zero_dose) %>%
  mutate(
    difference = static - historical_progress,
    percent_diff = (difference / static) * 100
  )

# Create the zero-dose difference plot
p_zero_diff <- ggplot(zero_dose_diff, aes(x = year, y = difference)) +
  geom_line(color = method_colors[1], linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~country, scales = "free_y") +
  labs(
    title = "Additional Zero-Dose Children Under Static Projections",
    subtitle = "Compared to Historical Progress 2000-2019 Projections (DTP1, 2024-2050)",
    x = "Year",
    y = "Difference in Zero-Dose Children"
  ) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  custom_theme()

# Save the zero-dose difference plot
ggsave("outputs/zero_dose_difference_plot.png", p_zero_diff, width = 12, height = 10, dpi = 600, bg = "white")
ggsave("outputs/zero_dose_difference_plot.pdf", p_zero_diff, width = 12, height = 10, dpi = 600, bg = "white")

# Calculate cumulative zero-dose differences by 2050
cumulative_diff <- zero_dose_diff %>%
  group_by(country) %>%
  summarize(
    cumulative_difference = sum(difference, na.rm = TRUE),
    avg_annual_difference = mean(difference, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Keep the factor ordering so Global remains first
  arrange(country)

# Print the cumulative differences
print("Cumulative additional zero-dose children by 2050 (Static vs. Historical Progress 2000-2019):")
print(cumulative_diff)

# Save the cumulative differences
write_csv(cumulative_diff, "outputs/cumulative_zero_dose_difference.csv")

# Create a cumulative plot creator and saving function that handles exactly 5 key plots
create_and_save_plots <- function(coverage_numbers_historical, coverage_numbers_static, 
                                coverage_summary_combined, zero_dose_summary_combined, 
                                zero_dose_diff, plot_countries) {
  
  # Set up a more controlled palette for consistent colors across plots using ColorBrewer
  method_colors <- c("historical_progress" = brewer.pal(3, "Blues")[3], "static" = brewer.pal(3, "Reds")[3])
  strata_colors <- c("Urban Poor" = brewer.pal(3, "Set1")[1], "Urban Non-Poor" = brewer.pal(3, "Set1")[2], "Rural" = brewer.pal(3, "Set1")[3])
  vaccine_colors <- c("DTP1" = brewer.pal(3, "Set2")[1], "measles1" = brewer.pal(3, "Set2")[2])
  
  # More streamlined theme for all plots
  plot_theme <- theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey90", fill = NA),
      axis.title = element_text(size = 11)
    )
  
  # ------------------------------------------------------------------
  # Figure 1: Baseline disparities (2025)
  # ------------------------------------------------------------------
  baseline_data <- coverage_numbers_historical %>%
    filter(year == 2025, vaccine == "DTP1") %>%
    filter(country %in% plot_countries) %>%
    select(country, urban_poor_coverage, urban_nonpoor_coverage) %>%
    pivot_longer(cols = c(urban_poor_coverage, urban_nonpoor_coverage),
                 names_to = "strata", 
                 values_to = "coverage") %>%
    mutate(
      strata = case_when(
        strata == "urban_poor_coverage" ~ "Urban Poor",
        strata == "urban_nonpoor_coverage" ~ "Urban Non-Poor"
      )
    )
  
  fig1 <- baseline_data %>% 
    # Ensure Global appears first by using factor with defined levels
    mutate(country = factor(country, levels = plot_countries)) %>%
    ggplot(aes(x = country, y = coverage, fill = strata)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = strata_colors[c("Urban Non-Poor", "Urban Poor")]) +
    labs(
      title = "DTP1 Vaccination Coverage by Urban Population Strata (2025)",
      x = NULL,
      y = "Coverage (%)",
      fill = "Urban Population Strata"
    ) +
    scale_y_continuous(limits = c(0, 100)) +
    plot_theme
  
  # Save Figure 1
  ggsave("outputs/fig1_baseline_disparities.png", fig1, width = 12, height = 10, dpi = 600, bg = "white")
  ggsave("outputs/fig1_baseline_disparities.pdf", fig1, width = 12, height = 10, dpi = 600, bg = "white")
  
  # ------------------------------------------------------------------
  # Figure 2: Method comparison (historical progress vs static)
  # ------------------------------------------------------------------
  method_comparison <- coverage_summary_combined %>%
    filter(country %in% plot_countries, vaccine == "DTP1") %>%
    # Ensure Global appears first
    mutate(country = factor(country, levels = plot_countries)) %>%
    select(country, year, method, overall_coverage)
  
  fig2 <- ggplot(method_comparison, aes(x = year, y = overall_coverage, color = method)) +
    geom_line(linewidth = 1) +
    facet_wrap(~country) +
    labs(
      title = "Vaccination Coverage Projections: Historical Progress 2000-2019 vs. Static",
      subtitle = "DTP1 Overall Coverage (2025-2050)",
      x = "Year",
      y = "Coverage (%)",
      color = "Projection Method"
    ) +
    scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
    scale_color_manual(values = method_colors,
                       labels = c("Historical Progress 2000-2019", "Static (Constant)")) +
    plot_theme
  
  # Save Figure 2
  ggsave("outputs/fig2_method_comparison.png", fig2, width = 12, height = 10, dpi = 600, bg = "white")
  ggsave("outputs/fig2_method_comparison.pdf", fig2, width = 12, height = 10, dpi = 600, bg = "white")
  
  # ------------------------------------------------------------------
  # Figure 3: Zero-dose difference plot
  # ------------------------------------------------------------------
  # Ensure zero_dose_diff has country as a factor with proper ordering
  zero_dose_diff <- zero_dose_diff %>%
    mutate(country = factor(country, levels = plot_countries))
    
  fig3 <- ggplot(zero_dose_diff, aes(x = year, y = difference)) +
    geom_line(color = method_colors[1], linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~country, scales = "free_y") +
    labs(
      title = "Additional Zero-Dose Children Under Static Projections",
      subtitle = "Compared to Historical Progress 2000-2019 Projections (DTP1, 2025-2050)",
      x = "Year",
      y = "Difference in Zero-Dose Children"
    ) +
    scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
    plot_theme
  
  # Save Figure 3
  ggsave("outputs/fig3_zero_dose_difference.png", fig3, width = 12, height = 10, dpi = 600, bg = "white")
  ggsave("outputs/fig3_zero_dose_difference.pdf", fig3, width = 12, height = 10, dpi = 600, bg = "white")
  
  # ------------------------------------------------------------------
  # Figures 4 & 5: DTP and Measles Global-Country Comparison Plots
  # ------------------------------------------------------------------
  create_global_country_plot <- function(data_historical, data_static, vaccine_name, countries) {
    # Ensure consistent country ordering
    countries_ordered <- c("Global", setdiff(countries, "Global"))
    
    # Combine the datasets with method labels
    global_data <- bind_rows(
      data_historical %>%
        filter(country == "Global", vaccine == vaccine_name, method == "historical_progress") %>%
        mutate(
          urban_poor_zerodose = population * (`urban poor_adjusted`/100) * (1 - urban_poor_coverage/100),
          urban_nonpoor_zerodose = population * (`urban non-poor_adjusted`/100) * (1 - urban_nonpoor_coverage/100),
          method_label = "Historical Progress 2000-2019"
        ),
      data_static %>%
        filter(country == "Global", vaccine == vaccine_name, method == "static") %>%
        mutate(
          urban_poor_zerodose = population * (`urban poor_adjusted`/100) * (1 - urban_poor_coverage/100),
          urban_nonpoor_zerodose = population * (`urban non-poor_adjusted`/100) * (1 - urban_nonpoor_coverage/100),
          method_label = "Static (Constant)"
        )
    )
    
    # Extract country-specific data (excluding Global)
    country_data <- bind_rows(
      data_historical %>%
        filter(country != "Global", country %in% countries, vaccine == vaccine_name, method == "historical_progress") %>%
        mutate(
          urban_poor_zerodose = population * (`urban poor_adjusted`/100) * (1 - urban_poor_coverage/100),
          urban_nonpoor_zerodose = population * (`urban non-poor_adjusted`/100) * (1 - urban_nonpoor_coverage/100),
          method_label = "Historical Progress 2000-2019"
        ),
      data_static %>%
        filter(country != "Global", country %in% countries, vaccine == vaccine_name, method == "static") %>%
        mutate(
          urban_poor_zerodose = population * (`urban poor_adjusted`/100) * (1 - urban_poor_coverage/100),
          urban_nonpoor_zerodose = population * (`urban non-poor_adjusted`/100) * (1 - urban_nonpoor_coverage/100),
          method_label = "Static (Constant)"
        )
    ) %>%
    # Ensure consistent country ordering for facets
    mutate(country = factor(country, levels = setdiff(countries_ordered, "Global")))
    
    # Global coverage plot (top left) - keep as line plot
    p_global_coverage <- ggplot(global_data, aes(x = year)) +
      geom_line(aes(y = urban_poor_coverage, color = "Urban Poor"), linewidth = 1) +
      geom_line(aes(y = urban_nonpoor_coverage, color = "Urban Non-Poor"), linewidth = 1) +
      facet_grid(method_label ~ .) +
      labs(
        title = paste0("Global ", ifelse(vaccine_name == "DTP1", "DTP", "Measles"), " Coverage"),
        subtitle = "Coverage in 12-23 months",
        x = NULL,
        y = "Coverage (%)",
        color = NULL
      ) +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(2025, 2045, by = 10), expand = c(0, 0)) +
      scale_color_manual(values = strata_colors) +
      plot_theme
    
    # Global zero-dose plot (bottom left) - change to stacked area plot
    p_global_zerodose <- ggplot(global_data, aes(x = year)) +
      geom_area(aes(y = urban_poor_zerodose + urban_nonpoor_zerodose, fill = "Urban Poor"), alpha = 0.8) +
      geom_area(aes(y = urban_nonpoor_zerodose, fill = "Urban Non-Poor"), alpha = 0.8) +
      facet_grid(method_label ~ .) +
      labs(
        title = paste0("Global ", ifelse(vaccine_name == "DTP1", "DTP", "Measles"), " Zero-dose Children"),
        subtitle = "Unvaccinated children in 12-23 months",
        x = "Year",
        y = "Zero-dose Children (thousands)",
        fill = NULL
      ) +
      scale_y_continuous(labels = function(x) x/1000, expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(2025, 2045, by = 10), expand = c(0, 0)) +
      scale_fill_manual(values = strata_colors) +
      plot_theme
    
    # Country coverage plot (top right) - keep as line plot
    p_country_coverage <- ggplot(country_data, aes(x = year)) +
      geom_line(aes(y = urban_poor_coverage, color = "Urban Poor"), linewidth = 1) +
      geom_line(aes(y = urban_nonpoor_coverage, color = "Urban Non-Poor"), linewidth = 1) +
      facet_grid(method_label ~ country) +
      labs(
        title = paste0(ifelse(vaccine_name == "DTP1", "DTP", "Measles"), " Coverage by Country"),
        subtitle = "Coverage in 12-23 months",
        x = NULL,
        y = "Coverage (%)",
        color = NULL
      ) +
      scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(2025, 2045, by = 10), expand = c(0, 0)) +
      scale_color_manual(values = strata_colors) +
      plot_theme
    
    # Country zero-dose plot (bottom right) - change to stacked area plot
    p_country_zerodose <- ggplot(country_data, aes(x = year)) +
      geom_area(aes(y = urban_poor_zerodose + urban_nonpoor_zerodose, fill = "Urban Poor"), alpha = 0.8) +
      geom_area(aes(y = urban_nonpoor_zerodose, fill = "Urban Non-Poor"), alpha = 0.8) +
      facet_grid(method_label ~ country) +
      labs(
        title = paste0(ifelse(vaccine_name == "DTP1", "DTP", "Measles"), " Zero-dose Children by Country"),
        subtitle = "Unvaccinated children in 12-23 months",
        x = "Year",
        y = "Zero-dose Children (thousands)",
        fill = NULL
      ) +
      scale_y_continuous(labels = function(x) x/1000, expand = c(0, 0)) +
      scale_x_continuous(breaks = seq(2025, 2045, by = 10), expand = c(0, 0)) +
      scale_fill_manual(values = strata_colors) +
      plot_theme
    
    # Combined layout with adjusted width ratio (global 1/3, country 2/3)
    p_combined <- (
      (p_global_coverage | p_country_coverage) + 
      plot_layout(widths = c(1, 2))
    ) / (
      (p_global_zerodose | p_country_zerodose) + 
      plot_layout(widths = c(1, 2))
    ) +
      plot_layout(guides = "collect") +
      plot_annotation(
        title = paste0(ifelse(vaccine_name == "DTP1", "DTP", "Measles"), " Coverage and Zero-dose Children"),
        subtitle = "Comparison of Historical Progress 2000-2019 vs. Static Projections (2025-2050)",
        theme = theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
          plot.subtitle = element_text(hjust = 0.5, size = 14)
        )
      ) &
      theme(legend.position = "bottom")
    
    return(p_combined)
  }
  
  # Create Figure 4: DTP1 Global-Country Comparison
  fig4 <- create_global_country_plot(
    data_historical = coverage_numbers_historical,
    data_static = coverage_numbers_static,
    vaccine_name = "DTP1",
    countries = setdiff(plot_countries, "Global")
  )
  
  # Save Figure 4
  ggsave("outputs/fig4_dtp1_global_country.png", fig4, width = 16, height = 12, dpi = 600, bg = "white")
  ggsave("outputs/fig4_dtp1_global_country.pdf", fig4, width = 16, height = 12, dpi = 600, bg = "white")
  
  # Create Figure 5: Measles Global-Country Comparison
  fig5 <- create_global_country_plot(
    data_historical = coverage_numbers_historical,
    data_static = coverage_numbers_static,
    vaccine_name = "measles1",
    countries = setdiff(plot_countries, "Global")
  )
  
  # Save Figure 5
  ggsave("outputs/fig5_measles_global_country.png", fig5, width = 16, height = 12, dpi = 600, bg = "white")
  ggsave("outputs/fig5_measles_global_country.pdf", fig5, width = 16, height = 12, dpi = 600, bg = "white")
  
  # Return all figures as a list for reference
  return(list(
    fig1 = fig1,
    fig2 = fig2,
    fig3 = fig3,
    fig4 = fig4,
    fig5 = fig5
  ))
}

# Call the consolidated plot creation function
plot_list <- create_and_save_plots(
  coverage_numbers_historical = coverage_numbers_historical,
  coverage_numbers_static = coverage_numbers_static,
  coverage_summary_combined = coverage_summary_combined,
  zero_dose_summary_combined = zero_dose_summary_combined,
  zero_dose_diff = zero_dose_diff,
  plot_countries = plot_countries
)

# Create supplementary plots directory if it doesn't exist
dir.create("outputs/supplementary", showWarnings = FALSE, recursive = TRUE)

# Save any additional plots that might be needed for supplementary material
save_supplementary_plots <- function(plot, filename, width, height, dpi = 600) {
  ggsave(paste0("outputs/supplementary/", filename, ".png"), plot, width = width, height = height, dpi = dpi, bg = "white")
  ggsave(paste0("outputs/supplementary/", filename, ".pdf"), plot, width = width, height = height, dpi = dpi, bg = "white")
}
