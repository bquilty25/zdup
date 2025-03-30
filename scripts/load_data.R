#!/usr/bin/env Rscript

# Script to load and process all input data

# Source logging utilities
source("scripts/logging_utils.R")

# Log message indicating data loading process is starting
log_message("Starting data loading process")

library(tidyverse)
library(readxl)
library(janitor)
library(data.table)

# Create necessary output directories
dir.create("results", showWarnings = FALSE)
dir.create("results/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("results/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("results/tables/yearly", showWarnings = FALSE, recursive = TRUE)
dir.create("results/plots", showWarnings = FALSE, recursive = TRUE)
dir.create("results/plots/raw_data", showWarnings = FALSE, recursive = TRUE)

# Create necessary output directories
dir.create("results/tables/annual_coverage_changes", showWarnings = FALSE, recursive = TRUE)
dir.create("results/data", showWarnings = FALSE, recursive = TRUE)

log_message("Loading input data...")

# Define all possible countries
countries <- c(
  "Global",
  "Nigeria",
  "India",
  "Democratic Republic of the Congo",
  "Pakistan",
  "Ethiopia",
  "Indonesia",
  "Philippines",
  "Angola",
  "Colombia",
  "Afghanistan"
)

# Function to get population projections for a country
get_population_projections <- function(country, start_year, end_year) {
  # Load population data from WPP2024 file
  pop_data <- read_xlsx("data/WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx", 
                     sheet = 2, skip = 16) %>%
    clean_names() %>%
    filter(
      year >= start_year,
      year <= end_year
    ) %>%
    select(year, country = region_subregion_country_or_area, x0:x100) %>%
    pivot_longer(cols = x0:x100, names_to = "age", values_to = "population") %>%
    mutate(
      age = parse_number(age),
      population = as.numeric(population) * 1000,
      country = ifelse(country == "World", "Global", country)
    ) %>%
    filter(country == !!country) %>%
    arrange(year, age)
  
  # Check if we have data
  if (nrow(pop_data) == 0) {
    stop(paste("No population data found for country:", country))
  }
  
  # Validate required columns
  required_cols <- c("year", "age", "population")
  missing_cols <- setdiff(required_cols, names(pop_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Log summary statistics
  log_message(paste("Population data loaded for", country))
  log_message(paste("Years:", min(pop_data$year), "to", max(pop_data$year)))
  log_message(paste("Total population range:", 
                   format(min(pop_data$population), big.mark = ","), "to",
                   format(max(pop_data$population), big.mark = ",")))
  
  return(pop_data)
}

# Load population data for each country
start_year <- 2024
end_year <- 2050

# Initialize empty list to store population data
pop_data_list <- list()

# Load data for each country
for(country in countries) {
  log_message(paste("Loading population data for", country))
  pop_data_list[[country]] <- get_population_projections(country, start_year, end_year)
}

# Combine all population data
pop_data <- bind_rows(pop_data_list)

# Add validation check
pop_summary <- pop_data %>%
  group_by(country, year) %>%
  summarise(
    total_pop = sum(population),
    missing_values = sum(is.na(population)),
    .groups = "drop"
  ) %>%
  arrange(country, year)

# Print summary
log_message("Population data summary:")
print(pop_summary)

# Read in WUENIC data
log_message("Loading vaccination coverage data...")
raw_dat_dtp1 <- read_xlsx("data/wuenic2023rev_web-update.xlsx", sheet = "DTP1")
raw_dat_dtp3 <- read_xlsx("data/wuenic2023rev_web-update.xlsx", sheet = "DTP3")
raw_dat_measles1 <- read_xlsx("data/wuenic2023rev_web-update.xlsx", sheet = "MCV1")
raw_dat_measles2 <- read_xlsx("data/wuenic2023rev_web-update.xlsx", sheet = "MCV2")

# Debug prints to check most recent values in raw data
log_message("=== DEBUG: Checking most recent coverage values in WUENIC data ===")
# Get list of column names to find latest year
dtp1_cols <- colnames(raw_dat_dtp1)
measles1_cols <- colnames(raw_dat_measles1)
# Find numeric columns and get the most recent year
year_cols_dtp1 <- dtp1_cols[grepl("^\\d{4}$", dtp1_cols)]
year_cols_measles1 <- measles1_cols[grepl("^\\d{4}$", measles1_cols)]
latest_year_dtp1 <- max(as.numeric(year_cols_dtp1))
latest_year_measles1 <- max(as.numeric(year_cols_measles1))

log_message(paste("Latest year in DTP1 data:", latest_year_dtp1))
log_message(paste("Latest year in measles1 data:", latest_year_measles1))

# Check coverage values for Global - fix to use column name as string
latest_year_col_dtp1 <- as.character(latest_year_dtp1)
latest_year_col_measles1 <- as.character(latest_year_measles1)

# Find the Global row in each dataset
global_row_dtp1 <- which(raw_dat_dtp1$country == "Global")
global_row_measles1 <- which(raw_dat_measles1$country == "Global")

if(length(global_row_dtp1) > 0 && latest_year_col_dtp1 %in% colnames(raw_dat_dtp1)) {
  global_dtp1 <- raw_dat_dtp1[global_row_dtp1, latest_year_col_dtp1]
  log_message(paste("Global DTP1 coverage for", latest_year_dtp1, ":", global_dtp1))
} else {
  log_message("Could not find Global DTP1 coverage data")
}

if(length(global_row_measles1) > 0 && latest_year_col_measles1 %in% colnames(raw_dat_measles1)) {
  global_measles1 <- raw_dat_measles1[global_row_measles1, latest_year_col_measles1]
  log_message(paste("Global measles1 coverage for", latest_year_measles1, ":", global_measles1))
} else {
  log_message("Could not find Global measles1 coverage data")
}

# Read in urbanization data
log_message("Loading urbanization data...")
urban_rates <- read_xlsx("data/WUP2018-F02-Proportion_Urban.xlsx", 
                        sheet = 1,
                        skip = 16) %>%
  clean_names() %>%
  filter(!is.na(region_subregion_country_or_area)) %>%
  rename(country = region_subregion_country_or_area) %>%
  mutate(
    country = case_when(
      country %in% c("WORLD", "World") ~ "Global",
      TRUE ~ country
    )
  ) %>%
  pivot_longer(cols = matches("^x\\d{4}$"),
               names_to = "year",
               values_to = "urban_rate") %>%
  mutate(
    year = as.numeric(gsub("x", "", year)),
    urban_rate = urban_rate / 100
  ) %>%
  filter(year >= 2015)

# Process historical vaccination coverage data - SINGLE SOURCE OF TRUTH
log_message("Processing historical vaccination coverage data...")
historical_coverage <- bind_rows(
  # DTP1 data
  raw_dat_dtp1 %>%
    pivot_longer(cols = matches("^\\d{4}$"), 
                names_to = "year", 
                values_to = "coverage") %>%
    mutate(dose = "DTP1"),
  # DTP3 data
  raw_dat_dtp3 %>%
    pivot_longer(cols = matches("^\\d{4}$"), 
                names_to = "year", 
                values_to = "coverage") %>%
    mutate(dose = "DTP3"),
  # Measles1 data
  raw_dat_measles1 %>%
    pivot_longer(cols = matches("^\\d{4}$"), 
                names_to = "year", 
                values_to = "coverage") %>%
    mutate(dose = "measles1"),
  # Measles2 data
  raw_dat_measles2 %>%
    pivot_longer(cols = matches("^\\d{4}$"), 
                names_to = "year", 
                values_to = "coverage") %>%
    mutate(dose = "measles2")
) %>%
  mutate(
    year = as.numeric(year),
    coverage = coverage/100
  )

# Calculate global averages with under-15 population weighting
log_message("Calculating global vaccination coverage averages...")
global_historical <- historical_coverage %>%
  filter(!is.na(coverage)) %>%
  left_join(pop_data %>% 
            filter(year == 2024, age < 15) %>%
            group_by(country) %>%
            summarise(total_pop = sum(population)),
            by = c("country")) %>%
  drop_na() %>%
  group_by(year, dose) %>%
  summarise(
    coverage = weighted.mean(coverage, total_pop, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(country = "Global")

# Add global averages to historical coverage
historical_coverage <- bind_rows(historical_coverage, global_historical)

# Create DTP coverage dataset from the single source of truth
log_message("Creating DTP coverage dataset...")
dtp_coverage <- historical_coverage %>%
  filter(dose %in% c("DTP1", "DTP3")) %>%
  select(country, year, dose, coverage) %>%
  pivot_wider(names_from = dose, values_from = coverage) %>%
  rename(dtp1_coverage = DTP1, dtp3_coverage = DTP3) %>%
  mutate(
    zero_dose_percent = 1 - dtp1_coverage,
    year = as.numeric(year)
  ) %>%
  filter(year >= 2015)

# Create Measles coverage dataset from the single source of truth
log_message("Creating Measles coverage dataset...")
measles_coverage <- historical_coverage %>%
  filter(dose %in% c("measles1", "measles2")) %>%
  select(country, year, dose, coverage) %>%
  pivot_wider(names_from = dose, values_from = coverage) %>%
  rename(measles1_coverage = measles1, measles2_coverage = measles2) %>%
  mutate(
    zero_dose_percent = 1 - measles1_coverage,
    year = as.numeric(year)
  ) %>%
  filter(year >= 2015)

# Process zero dose disparity data
log_message("Processing zero dose disparity data...")
zero_dose_disparity <- read_tsv("data/zero_dose_by_country_urban_rural.tsv") %>%
  janitor::clean_names() %>% 
  select(-p_value, -x95_percent_ci_lower_limit, -x95_percent_ci_upper_limit) %>% 
  mutate(across(where(is.numeric), ~ .x / 100))

# Calculate global averages for zero_dose_disparity
global_zero_dose <- zero_dose_disparity %>%
  left_join(pop_data %>% 
            filter(year == 2024, age < 15) %>%
            group_by(country) %>%
            summarise(total_pop = sum(population)),
            by = "country") %>% 
  drop_na() %>%
  group_by(group) %>%
  summarise(
    proportion_of_children_percent = weighted.mean(proportion_of_children_percent, total_pop, na.rm = TRUE),
    zero_dose_percent = weighted.mean(zero_dose_percent, total_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(country = "Global")

# Add global averages to zero_dose_disparity
zero_dose_disparity <- bind_rows(zero_dose_disparity, global_zero_dose)

log_message("Historical vaccination data processing complete!")

# Fit beta regression models to historical data
log_message("Fitting beta regression models to coverage data...")

# Function to fit beta regression model
fit_beta_reg <- function(data) {
  # Use coverage directly as it's already on 0-1 scale
  y <- data$coverage
  # Create predictor (years since 2023 instead of 2000)
  x <- data$year - 2023
  
  # Handle edge cases where all values are 0 or 1
  if(all(y == 0)) y <- y + 0.0001
  if(all(y == 1)) y <- y - 0.0001
  
  # Ensure y is between 0 and 1
  y <- pmin(pmax(y, 0.0001), 0.9999)
  
  # Fit beta regression
  model <- try(betareg::betareg(y ~ x))
  
  if(inherits(model, "try-error")) {
    # Return NA values if model fails
    return(list(
      predicted = rep(NA, length(x)),
      intercept = NA,
      slope = NA
    ))
  }
  
  # Get predictions (already on 0-1 scale)
  predicted <- predict(model, newdata = data.frame(x = x))
  
  # Get the most recent data point (should be 2023)
  most_recent_idx <- which.max(data$year)
  most_recent_year <- data$year[most_recent_idx]
  most_recent_coverage <- data$coverage[most_recent_idx]
  
  # Since we're using 2023 as reference, the intercept should already be for 2023
  # But we can double-check to ensure the model passes through the most recent point
  
  # Return model results
  list(
    predicted = predicted,
    intercept = coef(model)[1],
    slope = coef(model)[2]
  )
}

# Fit models using the clean historical data
coverage_model_fits <- historical_coverage %>%
  filter(
    year >= 2000,
    year <= 2019,
    country %in% countries,
    dose %in% c("DTP1", "DTP3", "measles1"),
    !is.na(coverage)
  ) %>%
  group_by(country, dose) %>%
  nest() %>%
  mutate(
    model = map(data, fit_beta_reg),
    coverage_predicted = map(model, ~.x$predicted),
    intercept = map_dbl(model, ~.x$intercept),
    slope = map_dbl(model, ~.x$slope)
  ) %>%
  unnest(c(data, coverage_predicted)) %>%
  ungroup()

# Extract model coefficients
log_message("Extracting model coefficients...")
model_coefficients <- coverage_model_fits %>%
  group_by(country, dose) %>%
  slice(1) %>%
  select(country, dose, intercept, slope) %>%
  arrange(country, dose)

# Print model coefficients for verification
log_message("\nModel coefficients:")
model_coefficients %>% print(n = Inf)

# Create plot of model fits
coverage_fit_plot <- coverage_model_fits %>%
  mutate(
    dose = factor(dose, 
      levels = c("DTP1", "DTP3", "measles1"),
      labels = c("DTP 1st dose", "DTP 3rd dose", "Measles 1st dose"))
  ) %>%
  ggplot(aes(x = year, color = dose)) +
  geom_line(aes(y = coverage, linetype = "Observed"), linewidth = 1) +
  geom_line(aes(y = coverage_predicted, linetype = "Fitted"), linewidth = 1) +
  {if(length(countries) > 1) 
    facet_wrap(~country)
   else 
    theme()  # No faceting needed for single country
  } +
  labs(
    title = "Vaccination Coverage: Observed vs Model Fits",
    subtitle = paste("Beta regression model fits for", paste(countries, collapse=", ")),
    x = "Year",
    y = "Coverage (%)",
    color = "Vaccine",
    linetype = "Data Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Set1") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

# Save the plot
ggsave("results/plots/raw_data/coverage_model_fits.pdf", coverage_fit_plot, width = 15, height = 10, bg = "white")
ggsave("results/plots/raw_data/coverage_model_fits.png", coverage_fit_plot, width = 15, height = 10, dpi = 300, bg = "white")

# Create and save annual coverage rate changes table
log_message("Creating annual coverage rate changes table...")
annual_coverage_table <- model_coefficients %>%
  select(country, dose, slope) %>%
  mutate(
    # Slope represents rate of change in logit-transformed coverage (0-1 scale)
    # Positive values = improving coverage, larger values = faster improvement
    slope = round(slope, 3)
  ) %>%
  arrange(country, dose)

# Save as CSV
write.csv(annual_coverage_table, 
          "results/tables/annual_coverage_changes/annual_coverage_changes.csv", 
          row.names = FALSE)

# Verification of vaccinate coverage rates to highlight any discrepancies
log_message("\nVerifying coverage rates - most recent year:")
verification_data <- historical_coverage %>%
  filter(
    year == max(year[year <= 2019]),
    dose %in% c("DTP1", "measles1"),
    country %in% c("Global", countries[1:5])  # Show Global and a few countries
  ) %>%
  select(country, year, dose, coverage) %>%
  pivot_wider(names_from = dose, values_from = coverage) %>%
  mutate(
    difference = DTP1 - measles1,
    difference_pct = 100 * difference
  ) %>%
  arrange(desc(abs(difference)))

print(verification_data)

# Final verification of 2023 values that will be used in the model
log_message("\n=== FINAL VERIFICATION: 2023 Values for Model ===")
latest_year_available <- max(dtp_coverage$year)
log_message(paste("Latest year available in processed data:", latest_year_available))

# Check the values that will be used as model inputs
model_input_values <- bind_rows(
  dtp_coverage %>% 
    filter(year == latest_year_available) %>%
    select(country, year, dtp1_coverage) %>%
    mutate(vaccine = "DTP1"),
  
  measles_coverage %>%
    filter(year == latest_year_available) %>%
    select(country, year, measles1_coverage) %>%
    rename(dtp1_coverage = measles1_coverage) %>%
    mutate(vaccine = "Measles1")
) %>%
  filter(country %in% c("Global", countries[1:5])) %>%
  arrange(country, vaccine)

print(model_input_values)

# Check if there are any large differences between DTP1 and Measles1
log_message("\nCountries with significant differences between DTP1 and Measles1 coverage:")
coverage_diffs <- model_input_values %>%
  pivot_wider(
    id_cols = c(country, year),
    names_from = vaccine,
    values_from = dtp1_coverage
  ) %>%
  mutate(
    difference = DTP1 - Measles1,
    difference_pct = 100 * difference
  ) %>%
  filter(abs(difference) > 0.05) %>%  # Show countries with >5% difference
  arrange(desc(abs(difference)))

print(coverage_diffs)

# Script to create plots of raw input data

# Create directory for raw data plots
dir.create("results/plots/raw_data", showWarnings = FALSE, recursive = TRUE)

# Function to fit beta regression model
fit_beta_reg <- function(data) {
  # Use coverage directly as it's already on 0-1 scale
  y <- data$coverage
  # Create predictor (years since 2023 instead of 2000)
  x <- data$year - 2023
  
  # Handle edge cases where all values are 0 or 1
  if(all(y == 0)) y <- y + 0.0001
  if(all(y == 1)) y <- y - 0.0001
  
  # Ensure y is between 0 and 1
  y <- pmin(pmax(y, 0.0001), 0.9999)
  
  # Fit beta regression
  model <- try(betareg::betareg(y ~ x))
  
  if(inherits(model, "try-error")) {
    # Return NA values if model fails
    return(list(
      predicted = rep(NA, length(x)),
      intercept = NA,
      slope = NA
    ))
  }
  
  # Get predictions (already on 0-1 scale)
  predicted <- predict(model, newdata = data.frame(x = x))
  
  # Get the most recent data point (should be 2023)
  most_recent_idx <- which.max(data$year)
  most_recent_year <- data$year[most_recent_idx]
  most_recent_coverage <- data$coverage[most_recent_idx]
  
  # Since we're using 2023 as reference, the intercept should already be for 2023
  # But we can double-check to ensure the model passes through the most recent point
  
  # Return model results
  list(
    predicted = predicted,
    intercept = coef(model)[1],
    slope = coef(model)[2]
  )
}

# Plot 1: Population pyramids for selected years
log_message("Creating population pyramids...")
pop_pyramids <- pop_data %>%
  filter(country %in% countries) %>%
  filter(year %in% c(2024, 2030, 2040, 2050)) %>%
  ggplot(aes(x = age, y = population/1e6)) +
  geom_line(linewidth = 1) +
  {if(length(countries) > 1) 
    facet_grid(country ~ year, scales = "free_y")
   else 
    facet_wrap(~year, scales = "free_y")
  } +
  labs(
    title = "Population Distribution by Age",
    subtitle = paste("Projected population structure for", paste(countries, collapse=", ")),
    x = "Age (years)",
    y = "Population (millions)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

ggsave("results/plots/raw_data/population_pyramids.pdf", pop_pyramids, width = 15, height = 10, bg = "white")
ggsave("results/plots/raw_data/population_pyramids.png", pop_pyramids, width = 15, height = 10, dpi = 300, bg = "white")

# Plot: Population trends by age group
log_message("Creating population trends by age group...")
pop_trends <- pop_data %>%
  filter(country %in% countries) %>%
  mutate(
    age_group = case_when(
      age < 5 ~ "0-4 years",
      age < 15 ~ "5-14 years",
      age < 25 ~ "15-24 years",
      age < 65 ~ "25-64 years",
      TRUE ~ "65+ years"
    ),
    age_group = factor(age_group, 
      levels = c("0-4 years", "5-14 years", "15-24 years", "25-64 years", "65+ years"))
  ) %>%
  group_by(country, year, age_group) %>%
  summarize(
    total_pop = sum(population)/1e6,
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = year, y = total_pop, color = age_group)) +
  geom_line(linewidth = 1) +
  {if(length(countries) > 1) 
    facet_wrap(~country, scales = "free_y")
   else 
    theme()  # No faceting needed for single country
  } +
  labs(
    title = "Population Trends by Age Group",
    subtitle = paste("Projected population size for", paste(countries, collapse=", ")),
    x = "Year",
    y = "Population (millions)",
    color = "Age Group"
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

ggsave("results/plots/raw_data/population_trends.pdf", pop_trends, width = 15, height = 10, bg = "white")
ggsave("results/plots/raw_data/population_trends.png", pop_trends, width = 15, height = 10, dpi = 300, bg = "white")

# Analysis of vaccination-relevant age cohorts
log_message("Analyzing population data for vaccination-relevant age cohorts...")

# Function to split yearly age data into more granular vaccine-relevant age groups
split_population_by_vaccine_age <- function(detailed_pop, vaccine_type) {
  # Calculate under-5 population for each country-year
  under5_pop <- detailed_pop %>%
    filter(age < 5) %>%
    group_by(country, year) %>%
    summarize(under5_pop = sum(population, na.rm = TRUE), .groups = "drop") %>%
    filter(under5_pop > 0)  # Only keep country-years with positive under-5 population
  
  # Define age configurations by vaccine type
  if (vaccine_type == "dtp") {
    age_configs <- tibble(
      min_age = c(0, 1, 2),
      max_age = c(1, 2, 5),  # Corrected to capture complete age ranges: 0-<1, 1-<2, 2-<5
      vaccine_age = c("0-11m", "12-23m", "2y+"),
      vaccine_cohort = c("0-11m", "12-23m", "2y+")
    )
  } else {
    age_configs <- tibble(
      min_age = c(0, 1, 2, 3),
      max_age = c(1, 2, 3, 5),  # Corrected to capture complete age ranges: 0-<1, 1-<2, 2-<3, 3-<5
      vaccine_age = c("0-12m", "12-23m", "24-35m", "36m+"),
      vaccine_cohort = c("0-12m", "12-23m", "24-35m", "36m+")
    )
  }
  
  # Calculate populations for each age group configuration
  result <- map_df(1:nrow(age_configs), function(i) {
    config <- age_configs[i, ]
    
    detailed_pop %>%
      filter(age >= config$min_age, age < config$max_age) %>%
      group_by(country, year) %>%
      summarize(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
      inner_join(under5_pop, by = c("country", "year")) %>%
      mutate(
        vaccine_age = config$vaccine_age,
        vaccine_cohort = config$vaccine_cohort
      ) %>%
      select(-under5_pop)
  })
  
  # Calculate proportions and validate
  result <- result %>%
    group_by(country, year) %>%
    mutate(
      total_pop = sum(population),
      proportion = population / total_pop
    ) %>%
    ungroup()
  
  # Ensure consistent ordering of age groups
  if (vaccine_type == "dtp") {
    result <- result %>%
      arrange(country, year, match(vaccine_age, c("0-11m", "12-23m", "2y+")))
  } else {
    result <- result %>%
      arrange(country, year, match(vaccine_age, c("0-12m", "12-23m", "24-35m", "36m+")))
  }
  
  # Validate proportions
  validation <- result %>%
    group_by(country, year) %>%
    summarize(
      total_proportion = sum(proportion),
      .groups = "drop"
    )
  
  if (any(abs(validation$total_proportion - 1) > 0.01)) {
    stop(paste0("Error: Age proportions do not sum to 1 for some country-years in ", 
                toupper(vaccine_type), " data"))
  }
  
  if (any(is.na(result$proportion)) || any(!is.finite(result$proportion))) {
    stop(paste0("Error: NA or non-finite proportions found in ", toupper(vaccine_type), " data"))
  }
  
  # Return result without the temporary column
  result %>% select(-total_pop)
}

# Apply the function to split population by vaccine-specific age groups
log_message("Splitting population data by vaccine-specific age groups...")
dtp_age_pops <- split_population_by_vaccine_age(pop_data, "dtp")
measles_age_pops <- split_population_by_vaccine_age(pop_data, "measles")

# Debug output for age populations
log_message("=== DEBUG: Vaccine Age Population Structure ===")
log_message("DTP age populations structure:")
log_message(paste("  - Class:", class(dtp_age_pops)))
log_message(paste("  - Dimensions:", nrow(dtp_age_pops), "rows x", ncol(dtp_age_pops), "columns"))
log_message(paste("  - Column names:", paste(colnames(dtp_age_pops), collapse=", ")))
log_message(paste("  - Unique vaccine_cohorts:", paste(unique(dtp_age_pops$vaccine_cohort), collapse=", ")))
log_message(paste("  - Unique vaccine_ages:", paste(unique(dtp_age_pops$vaccine_age), collapse=", ")))
log_message("  - Sample of values:")
print(head(dtp_age_pops, 3))

log_message("Measles age populations structure:")
log_message(paste("  - Class:", class(measles_age_pops)))
log_message(paste("  - Dimensions:", nrow(measles_age_pops), "rows x", ncol(measles_age_pops), "columns"))
log_message(paste("  - Column names:", paste(colnames(measles_age_pops), collapse=", ")))
log_message(paste("  - Unique vaccine_cohorts:", paste(unique(measles_age_pops$vaccine_cohort), collapse=", ")))
log_message(paste("  - Unique vaccine_ages:", paste(unique(measles_age_pops$vaccine_age), collapse=", ")))
log_message("  - Sample of values:")
print(head(measles_age_pops, 3))
log_message("=== END DEBUG: Vaccine Age Population Structure ===")

# Summarize population by vaccine group and country
dtp_age_summary <- dtp_age_pops %>%
  group_by(country, year, vaccine_cohort) %>%
  summarize(
    population = sum(population),
    .groups = 'drop'
  ) %>%
  arrange(country, year, vaccine_cohort)

# Debug output for summarized data
log_message("=== DEBUG: Summarized Age Population Structure ===")
log_message("DTP age summary structure:")
log_message(paste("  - Class:", class(dtp_age_summary)))
log_message(paste("  - Dimensions:", nrow(dtp_age_summary), "rows x", ncol(dtp_age_summary), "columns"))
log_message(paste("  - Column names:", paste(colnames(dtp_age_summary), collapse=", ")))
log_message("  - Sample of values:")
print(head(dtp_age_summary, 3))

measles_age_summary <- measles_age_pops %>%
  group_by(country, year, vaccine_cohort) %>%
  summarize(
    population = sum(population),
    .groups = 'drop'
  ) %>%
  arrange(country, year, vaccine_cohort)

log_message("Measles age summary structure:")
log_message(paste("  - Class:", class(measles_age_summary)))
log_message(paste("  - Dimensions:", nrow(measles_age_summary), "rows x", ncol(measles_age_summary), "columns"))
log_message(paste("  - Column names:", paste(colnames(measles_age_summary), collapse=", ")))
log_message("  - Sample of values:")
print(head(measles_age_summary, 3))
log_message("=== END DEBUG: Summarized Age Population Structure ===")

# Show summary for latest year
log_message("DTP-eligible population summary for 2024:")
dtp_age_summary %>%
  filter(year == 2024) %>%
  pivot_wider(
    names_from = vaccine_cohort,
    values_from = population
  ) %>%
  print(n = length(countries))

log_message("Measles-eligible population summary for 2024:")
measles_age_summary %>%
  filter(year == 2024) %>%
  pivot_wider(
    names_from = vaccine_cohort,
    values_from = population
  ) %>%
  print(n = length(countries))

# Plot vaccination cohort population trends
log_message("Creating vaccination cohort population trends plot...")

# DTP vaccinations by cohort population trends
dtp_pop_trends <- dtp_age_summary %>%
  filter(country %in% countries) %>%
  ggplot(aes(x = year, y = population/1e6, color = vaccine_cohort)) +
  geom_line(linewidth = 1) +
  {if(length(countries) > 1) 
    facet_wrap(~country, scales = "free_y")
   else 
    theme()  # No faceting needed for single country
  } +
  labs(
    title = "DTP Vaccination Cohort Population Trends",
    subtitle = paste("Projected population size for DTP-eligible age groups in", paste(countries, collapse=", ")),
    x = "Year",
    y = "Population (millions)",
    color = "Age Group"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

# Measles vaccinations by cohort population trends
measles_pop_trends <- measles_age_summary %>%
  filter(country %in% countries) %>%
  ggplot(aes(x = year, y = population/1e6, color = vaccine_cohort)) +
  geom_line(linewidth = 1) +
  {if(length(countries) > 1) 
    facet_wrap(~country, scales = "free_y")
   else 
    theme()  # No faceting needed for single country
  } +
  labs(
    title = "Measles Vaccination Cohort Population Trends",
    subtitle = paste("Projected population size for measles1-eligible age groups in", paste(countries, collapse=", ")),
    x = "Year",
    y = "Population (millions)",
    color = "Age Group"
  ) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

# Save the plots
ggsave("results/plots/raw_data/dtp_cohort_trends.pdf", dtp_pop_trends, width = 15, height = 10, bg = "white")
ggsave("results/plots/raw_data/dtp_cohort_trends.png", dtp_pop_trends, width = 15, height = 10, dpi = 300, bg = "white")
ggsave("results/plots/raw_data/measles_cohort_trends.pdf", measles_pop_trends, width = 15, height = 10, bg = "white")
ggsave("results/plots/raw_data/measles_cohort_trends.png", measles_pop_trends, width = 15, height = 10, dpi = 300, bg = "white")

# Save detailed vaccine age population data
saveRDS(dtp_age_pops, "results/data/dtp_age_populations.rds")
saveRDS(measles_age_pops, "results/data/measles_age_populations.rds")
saveRDS(dtp_age_summary, "results/data/dtp_age_summary.rds")
saveRDS(measles_age_summary, "results/data/measles_age_summary.rds")

# Stratum proportions plot

# Create global data for urban strata
global_urban_strata <- pop_data %>%
  filter(country == "Global",
         year %in% c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  # First calculate total population by country and year
  group_by(country, year) %>%
  summarise(total_pop = sum(population), .groups = 'drop') %>%
  # Then join with urban rates
  left_join(urban_rates, by = c("country", "year")) %>%
  # Calculate global urban poor/non-poor split with under-15 population weighting
  left_join(
    zero_dose_disparity %>%
      filter(group %in% c("urban poor", "urban non-poor")) %>%
      left_join(pop_data %>% 
                filter(year == 2024, age < 15) %>%
                group_by(country) %>%
                summarise(child_pop = sum(population)),
                by = "country") %>%
      group_by(group) %>%
      summarise(
        proportion = weighted.mean(proportion_of_children_percent, child_pop, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_wider(names_from = group, values_from = proportion) %>% 
      mutate(country = "Global"),
    by = "country"
  ) %>%
  mutate(
    # Keep original proportions without normalization
    urban_poor_prop = `urban poor`,
    urban_nonpoor_prop = `urban non-poor`,
    country = "Global"
  ) %>%
  pivot_longer(
    cols = c(urban_poor_prop, urban_nonpoor_prop),
    names_to = "stratum",
    values_to = "pop_prop"
  ) %>%
  mutate(
    urban_prop = pop_prop * urban_rate,
    stratum = factor(stratum,
      levels = c("urban_poor_prop", "urban_nonpoor_prop"),
      labels = c("Urban Poor", "Urban Non-poor"))
  )

# Create country-specific urban strata data
country_urban_strata <- zero_dose_disparity %>%
  filter(country %in% countries) %>%
  filter(group %in% c("urban poor", "urban non-poor")) %>%
  select(country, group, proportion_of_children_percent) %>%
  filter(!is.na(proportion_of_children_percent)) %>%  # Remove any NA values
  # Keep original proportions without normalization
  mutate(
    pop_prop = proportion_of_children_percent
  ) %>%
  # Create rows for each projection year
  crossing(year = c(2025, 2030, 2035, 2040, 2045, 2050)) %>%
  # Join with urban rates
  left_join(urban_rates, by = c("country", "year")) %>%
  mutate(
    urban_prop = pop_prop * urban_rate,
    stratum = case_when(
      group == "urban poor" ~ "Urban Poor",
      group == "urban non-poor" ~ "Urban Non-poor"
    ),
    stratum = factor(stratum, levels = c("Urban Poor", "Urban Non-poor"))
  ) %>%
  select(-group, -proportion_of_children_percent) %>%
  filter(!is.na(urban_prop))  # Remove any rows where we couldn't calculate urban proportion

# Combine country and global data
stratum_proportions_data <- bind_rows(country_urban_strata, global_urban_strata) %>% 
  select(-`urban poor`, -`urban non-poor`, -index, -note)

# Create the plot
stratum_proportions_plot <- stratum_proportions_data %>%
  ggplot(aes(x = year, y = urban_prop, color = stratum)) +
  geom_line(linewidth = 1) +
  {if(length(countries) > 1) 
    facet_wrap(~country)
   else 
    theme()  # No faceting needed for single country
  } +
  labs(
    title = "Urban Population Distribution Projections",
    subtitle = paste("Proportion of population for", paste(countries, collapse=", ")),
    x = "Year",
    y = "Proportion of Total Population",
    color = "Urban Stratum"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2025, 2050, by = 5)) +
  scale_color_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.minor = element_blank()
  )

# Save the plots
ggsave("results/plots/raw_data/stratum_proportions_plot.pdf", stratum_proportions_plot, width = 15, height = 10, bg = "white")
ggsave("results/plots/raw_data/stratum_proportions_plot.png", stratum_proportions_plot, width = 15, height = 10, dpi = 300, bg = "white")

#' Load and validate country data
#' @return A list containing validated data frames for population, coverage, and metadata
load_country_data <- function() {
  log_message("Loading country data...")
  
  # Read country data
  country_data <- read_csv("data/country_data.csv", show_col_types = FALSE) %>%
    select(country, year, population, urban_rate, urban_poor_prop,
           dtp_coverage, measles_coverage) %>%
    filter(!is.na(country))
  
  # Validate required columns
  required_cols <- c("country", "year", "population", "urban_rate", 
                    "urban_poor_prop", "dtp_coverage", "measles_coverage")
  missing_cols <- setdiff(required_cols, names(country_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in country_data.csv: ", 
         paste(missing_cols, collapse = ", "))
  }
  
  # Check for missing values
  na_check <- country_data %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    gather(column, na_count) %>%
    filter(na_count > 0)
  
  if (nrow(na_check) > 0) {
    warning("Missing values found in columns: \n",
            paste(paste(na_check$column, na_check$na_count, sep = ": "), 
                  collapse = "\n"))
  }
  
  # Validate numeric ranges
  country_data %>%
    verify(population >= 0, "Population must be non-negative")
  country_data %>%
    verify(between(urban_rate, 0, 1), "Urban rate must be between 0 and 1")
  country_data %>%
    verify(between(urban_poor_prop, 0, 1), 
           "Urban poor proportion must be between 0 and 1")
  country_data %>%
    verify(between(dtp_coverage, 0, 1), "DTP coverage must be between 0 and 1")
  country_data %>%
    verify(between(measles_coverage, 0, 1), 
           "Measles coverage must be between 0 and 1")
  
  # Process data
  population_data <- country_data %>%
    select(country, year, population, urban_rate, urban_poor_prop) %>%
    arrange(country, year)
  
  coverage_data <- country_data %>%
    select(country, year, dtp_coverage, measles_coverage) %>%
    arrange(country, year)
  
  # Create metadata
  metadata <- list(
    countries = unique(country_data$country),
    years = unique(country_data$year),
    start_year = min(country_data$year),
    end_year = max(country_data$year)
  )
  
  log_message("Data loading complete")
  
  return(list(
    population = population_data,
    coverage = coverage_data,
    metadata = metadata
  ))
}

#' Process age-specific data for a country
#' @param country Character string specifying the country
#' @return A list containing age-specific population proportions and vaccination data
process_age_data <- function(country) {
  log_message(paste("Processing age data for", country))
  
  # Define age groups (in years)
  age_groups <- c("0-1", "1-2", "2-3", "3-4", "4-5")
  
  # Load age-specific population proportions
  # This should be replaced with actual data when available
  age_props <- c(0.2, 0.2, 0.2, 0.2, 0.2)
  names(age_props) <- age_groups
  
  # Validate age proportions sum to 1
  if (!isTRUE(all.equal(sum(age_props), 1))) {
    stop("Age proportions must sum to 1 for country: ", country)
  }
  
  return(list(
    age_groups = age_groups,
    age_props = age_props
  ))
}

#' Get valid countries for analysis
#' @return Character vector of country names with complete data
get_valid_countries <- function() {
  data <- load_country_data()
  
  # Check data completeness for each country
  valid_countries <- data$population %>%
    group_by(country) %>%
    summarise(
      has_complete_data = !any(is.na(population)) && 
                         !any(is.na(urban_rate)) &&
                         !any(is.na(urban_poor_prop))
    ) %>%
    filter(has_complete_data) %>%
    pull(country)
  
  if (length(valid_countries) == 0) {
    stop("No countries have complete data for analysis")
  }
  
  log_message(paste("Found", length(valid_countries), "valid countries"))
  return(valid_countries)
}

#' Initialize results directory structure
create_results_dirs <- function() {
  # Create main directories
  dirs <- c(
    "results/raw",
    "results/plots/diagnostic",
    "results/plots/supplementary",
    "results/plots/combined",
    "results/plots/impact",
    "results/tables/summary",
    "results/tables/country",
    "results/tables/impact"
  )
  
  # Create directories if they don't exist
  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      log_message(paste("Created directory:", dir))
    }
  }
}

# Combine country and global data
all_urban_strata <- bind_rows(
  global_urban_strata,
  country_urban_strata
)

# Create final country data
country_data <- bind_rows(
  # Global data
  tibble(
    country = "Global",
    urban_poor_prop = unique(global_urban_strata$pop_prop[global_urban_strata$stratum == "Urban Poor"]),
    urban_nonpoor_prop = unique(global_urban_strata$pop_prop[global_urban_strata$stratum == "Urban Non-poor"])
  ),
  # Country-specific data
  country_urban_strata %>%
    filter(year == 2025) %>%  # Use 2025 as baseline year
    select(country, stratum, pop_prop) %>%
    mutate(stratum = tolower(as.character(stratum))) %>%
    pivot_wider(
      names_from = stratum,
      values_from = pop_prop,
      names_prefix = "urban_"
    ) %>%
    rename(
      urban_poor_prop = `urban_urban poor`,
      urban_nonpoor_prop = `urban_urban non-poor`
    )
)

# Save the country data
write.csv(country_data, "data/country_data.csv", row.names = FALSE)

log_message("Data processing complete!")

# Function to get population projections for a country
get_population_projections <- function(country, start_year, end_year) {
  # Load population data from WPP2024 file
  pop_data <- read.csv("data/population_projections.csv") %>%
    filter(
      country == !!country,
      year >= start_year,
      year <= end_year
    ) %>%
    arrange(year)
  
  # Check if we have data
  if (nrow(pop_data) == 0) {
    stop(paste("No population data found for country:", country))
  }
  
  # Validate required columns
  required_cols <- c("year", "age", "population")
  missing_cols <- setdiff(required_cols, names(pop_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Log summary statistics
  log_message(paste("Population data loaded for", country))
  log_message(paste("Years:", min(pop_data$year), "to", max(pop_data$year)))
  log_message(paste("Total population range:", 
                   format(min(pop_data$population), big.mark = ","), "to",
                   format(max(pop_data$population), big.mark = ",")))
  
  return(pop_data)
}

