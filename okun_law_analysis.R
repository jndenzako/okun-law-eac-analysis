# Okun's Law Analysis for EAC Countries
# This script estimates and plots Okun's Law for East African Community countries
# Okun's Law: Change in Unemployment = α + β * (Output Growth - Natural Growth Rate)

# Install required packages (uncomment if needed)
# install.packages(c("WDI", "tidyverse", "ggplot2", "gridExtra", "broom"))

# Load libraries
library(WDI)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(broom)

# Define EAC countries and their World Bank country codes
eac_countries <- c(
  "BDI" = "Burundi",
  "KEN" = "Kenya",
  "RWA" = "Rwanda",
  "SSD" = "South Sudan",
  "TZA" = "Tanzania",
  "UGA" = "Uganda"
)

# WDI indicators for Okun's Law analysis
# NY.GDP.MKTP.KD.ZG: GDP growth (annual %)
# SP.URB.TOTL.IN.ZS: Unemployment rate (% of total labor force) - alternative indicators may be needed
indicators <- c(
  gdp_growth = "NY.GDP.MKTP.KD.ZG",      # GDP growth annual %
  unemployment = "SP.URB.TOTL.IN.ZS"     # Urban population % (proxy - adjust as needed)
)

# Fetch data from WDI for the period 2000-2023
cat("Fetching data from World Bank WDI database...\n")
wdi_data <- WDI(
  country = names(eac_countries),
  indicator = indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
)

# Clean and prepare data
df_clean <- wdi_data %>%
  select(country, iso2c, year, gdp_growth, unemployment) %>%
  rename(Country = country, ISO = iso2c, Year = year, GDP_Growth = gdp_growth, Unemployment = unemployment) %>%
  arrange(Country, Year) %>%
  # Calculate lagged values and changes
  group_by(Country) %>%
  mutate(
    Unemployment_Change = Unemployment - lag(Unemployment, 1),
    GDP_Growth_Lag = lag(GDP_Growth, 1)
  ) %>%
  ungroup() %>%
  # Remove rows with missing values
  filter(!is.na(GDP_Growth) & !is.na(Unemployment) & !is.na(Unemployment_Change)) %>%
  # Center GDP growth around natural growth rate (assume 3% for developing economies)
  mutate(GDP_Deviation = GDP_Growth - 3)

# Summary statistics
cat("\nSummary of Data:\n")
print(summary(df_clean))

# Create a list to store regression results
regression_results <- list()
plots <- list()

# Estimate Okun's Law for each EAC country individually
cat("\n=== Okun's Law Regression Results by Country ===\n\n")

for (country in unique(df_clean$Country)) {
  country_data <- df_clean %>%
    filter(Country == country) %>%
    drop_na()
  
  if (nrow(country_data) > 2) {
    # Estimate: ΔUnemployment = α + β * GDP_Deviation + ε
    model <- lm(Unemployment_Change ~ GDP_Deviation, data = country_data)
    
    # Store results
    regression_results[[country]] <- list(
      model = model,
      data = country_data,
      summary = glance(model)
    )
    
    # Print regression summary
    cat("Country:", country, "\n")
    cat("Observations:", nrow(country_data), "\n")
    cat("Coefficient (Okun's Law Coefficient):", round(coef(model)[2], 4), "\n")
    cat("R-squared:", round(summary(model)$r.squared, 4), "\n")
    cat("P-value:", round(summary(model)$coefficients[2, 4], 4), "\n")
    cat("---\n\n")
    
    # Create individual plots for each country
    p <- ggplot(country_data, aes(x = GDP_Deviation, y = Unemployment_Change)) +
      geom_point(size = 3, alpha = 0.6, color = "steelblue") +
      geom_smooth(method = "lm", se = TRUE, color = "red", fill = "red", alpha = 0.2) +
      labs(
        title = paste("Okun's Law:", country),
        x = "GDP Growth Deviation from 3% (Trend)",
        y = "Change in Unemployment Rate",
        subtitle = paste(
          "β =", round(coef(model)[2], 4),
          "| R² =", round(summary(model)$r.squared, 4)
        )
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10)
      )
    
    plots[[country]] <- p
  } else {
    cat("Country:", country, "- Insufficient data for regression\n\n")
  }
}

# Create combined plot for all countries
cat("\nGenerating visualization...\n")

# Arrange plots in a grid (3x2 for 6 countries)
if (length(plots) > 0) {
  combined_plot <- do.call(grid.arrange, c(plots, ncol = 2))
  
  # Save the plot
  ggsave(
    "okun_law_eac_countries.png",
    combined_plot,
    width = 14,
    height = 10,
    dpi = 300
  )
  print(combined_plot)
  cat("\nPlot saved as 'okun_law_eac_countries.png'\n")
}

# Create a summary table of all regression results
summary_table <- do.call(rbind, lapply(names(regression_results), function(country) {
  model <- regression_results[[country]]$model
  data.frame(
    Country = country,
    Observations = nrow(regression_results[[country]]$data),
    Okun_Coefficient = round(coef(model)[2], 4),
    Intercept = round(coef(model)[1], 4),
    R_squared = round(summary(model)$r.squared, 4),
    P_value = round(summary(model)$coefficients[2, 4], 4),
    Significant = ifelse(summary(model)$coefficients[2, 4] < 0.05, "Yes", "No")
  )
})) %>%
  arrange(desc(Okun_Coefficient))

cat("\n=== Summary Table ===\n")
print(summary_table)

# Save summary table
write.csv(summary_table, "okun_law_summary.csv", row.names = FALSE)
cat("\nSummary table saved as 'okun_law_summary.csv'\n")
