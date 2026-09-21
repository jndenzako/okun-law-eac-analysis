# Okun's Law Analysis for EAC Countries

This repository contains an R script to estimate and visualize **Okun's Law** for East African Community (EAC) countries using data from the World Bank's World Development Indicators (WDI) dataset.

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
install.packages(c("WDI", "tidyverse", "ggplot2", "gridExtra", "broom"))
```

### Required R Packages:
- **WDI**: Fetch data from World Bank
- **tidyverse**: Data manipulation and visualization
- **ggplot2**: Advanced plotting
- **gridExtra**: Arrange multiple plots
- **broom**: Extract regression results

## Usage

1. Clone the repository:
```bash
git clone https://github.com/jndenzako/okun-law-eac-analysis.git
cd okun-law-eac-analysis
```

2. Open R or RStudio and run:
```r
source("okun_law_analysis.R")
```

3. The script will:
   - Download WDI data for all EAC countries
   - Estimate Okun's Law regression for each country individually
   - Generate individual plots showing the relationship
   - Create a combined visualization
   - Produce a summary table with regression results

## Output Files

- **`okun_law_eac_countries.png`**: Grid of Okun's Law plots for all countries
- **`okun_law_summary.csv`**: Summary table with regression coefficients, R², and p-values

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