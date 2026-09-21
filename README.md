# Okun's Law Analysis for EAC Countries

This repository contains R scripts for:
- estimating and visualizing **Okun's Law** for East African Community (EAC) countries using data from the World Bank's World Development Indicators (WDI) dataset
- analyzing **school-to-work transition outcomes** for young graduates using a synthetic dataset that mimics graduate transition survey data
- building a **WDI panel dataset on gender and economic growth** for East Africa and estimating comparative regression models

## Overview

**Okun's Law** is an empirical relationship between unemployment and economic growth, stating that:

```
ΔUnemployment = α + β * (GDP Growth - Natural Growth Rate) + ε
```

Where:
- **ΔUnemployment**: Change in unemployment rate
- **GDP Growth**: Annual percentage change in GDP
- **β (Okun's Coefficient)**: Typically negative, indicating that higher GDP growth reduces unemployment
- **Natural Growth Rate**: Assumed at 3% for developing economies

## EAC Countries Analyzed

The analysis covers the following East African Community members:
- 🇧🇮 Burundi
- 🇰🇪 Kenya
- 🇷🇼 Rwanda
- 🇸🇸 South Sudan
- 🇹🇿 Tanzania
- 🇺🇬 Uganda

## Data Source

- **Dataset**: World Bank World Development Indicators (WDI)
- **Time Period**: 2000-2023
- **Indicators**:
  - `NY.GDP.MKTP.KD.ZG`: GDP growth (annual %)
  - `SP.URB.TOTL.IN.ZS`: Unemployment rate (% of labor force)

## Requirements

```r
install.packages(c("WDI", "tidyverse", "ggplot2", "gridExtra", "broom", "dplyr", "tidyr", "readr", "scales", "patchwork", "knitr", "kableExtra", "rmarkdown", "sandwich", "lmtest"))
```

### Required R Packages:
- **WDI**: Fetch data from World Bank
- **tidyverse**: Data manipulation and visualization
- **ggplot2**: Advanced plotting
- **gridExtra**: Arrange multiple plots
- **broom**: Extract regression results
- **readr**: Export analysis outputs
- **scales**: Format percentages and currency in charts
- **patchwork**: Combine multiple ggplot panels into a dashboard
- **knitr** and **kableExtra**: Produce formatted HTML tables
- **rmarkdown**: Render the analytical report
- **sandwich** and **lmtest**: Report heteroskedasticity-robust regression results

## Usage

1. Clone the repository:
```bash
git clone https://github.com/jndenzako/okun-law-eac-analysis.git
cd okun-law-eac-analysis
```

2. Open R or RStudio and run the Okun's Law analysis:
```r
source("okun_law_analysis.R")
```

3. Run the school-to-work transition analysis:
```r
source("school_to_work_transition_analysis.R")
```

4. Run the gender-growth WDI panel analysis:
```r
source("gender_growth_wdi_panel_analysis.R")
rmarkdown::render("gender_growth_wdi_panel_analysis.Rmd")
```

5. The scripts will:
   - Download WDI data for all EAC countries
   - Estimate Okun's Law regression for each country individually
   - Generate individual plots showing the relationship
   - Create a combined visualization
   - Produce a summary table with regression results
   - Generate a synthetic graduate transition survey for EAC countries
   - Estimate employment and earnings models for young graduates
   - Produce descriptive tables and a dashboard for school-to-work outcomes
   - Download a WDI-based gender-growth panel for East Africa
   - Produce formatted HTML tables, graphics, and regression summaries
   - Render a reproducible R Markdown report for the gender-growth analysis

## Output Files

- **`okun_law_eac_countries.png`**: Grid of Okun's Law plots for all countries
- **`okun_law_summary.csv`**: Summary table with regression coefficients, R², and p-values
- **`school_to_work_synthetic_dataset.csv`**: Synthetic microdata for young graduates
- **`school_to_work_country_summary.csv`**: Country-level transition indicators
- **`school_to_work_gender_summary.csv`**: Gender-disaggregated transition indicators
- **`school_to_work_key_indicators.csv`**: Headline school-to-work transition metrics
- **`school_to_work_employment_model.csv`**: Logistic regression results for employment odds
- **`school_to_work_earnings_model.csv`**: Linear regression results for graduate earnings
- **`school_to_work_transition_dashboard.png`**: Multi-panel visualization of transition outcomes
- **`gender_growth_wdi_panel_dataset.csv`**: Country-year WDI panel used for the gender-growth study
- **`gender_growth_wdi_panel_country_summary.csv`**: Coverage and indicator averages by country
- **`gender_growth_wdi_panel_descriptive_statistics.csv`**: Baseline sample descriptive statistics
- **`gender_growth_wdi_panel_regression_results.csv`**: Regression coefficients for pooled OLS and fixed-effects models
- **`gender_growth_wdi_panel_model_fit.csv`**: Model fit statistics for each specification
- **`gender_growth_wdi_panel_equations.csv`**: Regression equations used in the report
- **`gender_growth_wdi_panel_dashboard.png`**: Multi-panel visualization of gender inclusion and growth
- **`gender_growth_wdi_panel_analysis.html`**: Rendered R Markdown report

## Gender and Economic Growth Panel Analysis

The new WDI panel analysis studies how gender inclusion indicators move with GDP per capita growth in East Africa. It assembles a country-year panel from WDI series on female labor force participation, women in parliament, fertility, secondary enrollment, investment, inflation, and trade openness, then estimates pooled OLS and two-way fixed-effects models.

The analysis title is **"Women's Economic Inclusion and GDP per Capita Growth in East Africa: A WDI Panel Analysis, 2000-2011"**. The script attempts a direct WDI download first and falls back to a GitHub mirror of WDI extracts when the live API is unavailable, so the workflow remains reproducible in restricted environments.

## School-to-Work Transition Analysis

The synthetic school-to-work transition script mimics graduate tracer survey data for young people in EAC countries and examines:

- transition time from graduation to first job
- employment, unemployment, and inactivity outcomes
- formal employment and job-to-field matching
- earnings differentials across demographic and education characteristics
- the role of internships, digital skills, and job-search training

### Synthetic Variables Included

- Country
- Gender
- Residence
- Institution type
- Degree level
- Field of study
- Socioeconomic background
- Graduation year
- Age
- CGPA
- Internship participation
- Digital skills score
- Job-search training
- Months since graduation
- Employment status
- Transition months to first job
- Job relevance/match
- Formal employment status
- Contract type
- Monthly earnings in USD

## Expected Output

The console will display:
- Data summary statistics
- Regression coefficients for each country
- R-squared values and p-values
- Significance tests

Example output:
```
Country: Kenya 
Observations: 15 
Coefficient (Okun's Law Coefficient): -0.2534 
R-squared: 0.4521 
P-value: 0.0234
```

## Interpretation

- **Okun's Coefficient (β)**: 
  - Negative values indicate that higher growth reduces unemployment (expected)
  - Magnitude shows sensitivity (e.g., -0.25 means 1% extra growth reduces unemployment by 0.25%)
  
- **R-squared**: Shows how well GDP growth explains unemployment changes
  - Higher values (closer to 1) indicate a stronger relationship

- **P-value**: Tests statistical significance
  - p < 0.05: Result is statistically significant

## Key Findings

The script estimates Okun's Law parameters separately for each EAC country, allowing for country-specific analysis due to different economic structures and labor market characteristics.

## Notes

- The natural growth rate is assumed at 3% (typical for developing economies)
- Some countries may have incomplete data series, which is handled automatically
- Results should be interpreted in context of each country's economic conditions and data quality

## References

- Okun, A. M. (1962). Potential GNP: Its measurement and significance. *Proceedings of the Business and Economic Statistics Section*, American Statistical Association.
- World Bank. (2024). World Development Indicators. Retrieved from https://data.worldbank.org/

## License

This project is open source and available under the MIT License.

## Author

**jndenzako**

---

For questions or suggestions, feel free to open an issue or contribute improvements!