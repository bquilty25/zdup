# ZDUP: Zero-Dose Urban/Rural Project

## Project Overview

This project analyzes vaccination coverage disparities between urban and rural populations, with a focus on zero-dose children (those who have not received any vaccinations). The analysis stratifies urban populations into poor and non-poor categories to reveal underlying inequities in vaccination coverage that are often masked in aggregated data.

The model projects vaccination coverage for diphtheria-tetanus-pertussis (DTP) and measles vaccines through 2050, comparing trend-based projections with static coverage scenarios. It identifies a "Red Queen" paradox in urban immunisation: substantial increases in coverage rates are necessary merely to prevent growth in the absolute number of unvaccinated children due to population expansion.

## Key Features

- Stratified analysis of urban populations (poor vs. non-poor)
- Trend-based and static coverage projections through 2050
- Analysis of DTP1, DTP3, and measles vaccination coverage
- Country-specific and global projections
- Visualization of zero-dose disparities across population strata

## Data Requirements

Some data files are not included in this repository due to size limitations. Please download the following files before running the analysis:

1. **WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx** (210MB)
   - Source: [UN Population Division - World Population Prospects 2024](https://population.un.org/wpp/Download/Standard/Population/)
   - Place in: `data/`

2. **WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx** (18MB)
   - Source: [UN Population Division - World Population Prospects 2024](https://population.un.org/wpp/Download/Standard/Population/)
   - Place in: `data/`

## Dependencies

This project requires R with the following packages:
- tidyverse
- ggplot2
- readxl
- janitor
- data.table
- patchwork
- RColorBrewer

Install the required packages with:

```r
install.packages(c("tidyverse", "ggplot2", "readxl", "janitor", "data.table", "patchwork", "RColorBrewer"))
```

## Setup and Usage

1. Clone this repository
2. Download the required data files (see Data Requirements)
3. Open the project in RStudio (or your preferred R environment)
4. Run the analysis scripts in the following order:

```r
# Load and process data
source("scripts/load_data.R")

# Run the projection model
source("scripts/mult_model.R")

# Generate the Quarto report (if using Quarto)
quarto::quarto_render("zero_dose_urban_projections_mult_analysis.qmd")
```

## Project Structure

- `data/`: Data files (including downloaded and intermediate data)
- `scripts/`: R scripts for data processing and analysis
  - `load_data.R`: Loads and processes input data
  - `mult_model.R`: Implements the projection model
  - `logging_utils.R`: Utility functions for logging
- `outputs/`: Generated figures, tables, and results
- `results/`: Intermediate results and additional outputs
- `zero_dose_urban_projections_mult_analysis.qmd`: Quarto document for the full analysis report

## Methodology

The model uses historical vaccination coverage data (2000-2019) from WHO/UNICEF Estimates of National Immunization Coverage (WUENIC) to project future coverage through 2050. Two projection methods are compared:

1. **Historical Progress**: Continues historical trends in coverage improvement
2. **Static Coverage**: Maintains coverage at 2023 levels

Population projections from UN World Population Prospects and urbanization rates from World Urbanization Prospects are used to calculate absolute numbers of zero-dose children.

## Citation

If you use this project in your research, please cite:

```
[Citation information will be provided upon publication]
```

## License

[License information]

## Contact

[Contact information] 