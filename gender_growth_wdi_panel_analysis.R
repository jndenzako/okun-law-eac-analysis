# Gender and Economic Growth Panel Analysis for East Africa
# This script downloads a WDI-based panel dataset and analyzes links between
# women's economic inclusion indicators and GDP per capita growth.

suppressPackageStartupMessages({
  library(WDI)
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(readr)
  library(purrr)
  library(stringr)
  library(forcats)
  library(broom)
  library(patchwork)
  library(knitr)
  library(kableExtra)
  library(sandwich)
  library(lmtest)
  library(scales)
})

analysis_title <- "Women's Economic Inclusion and GDP per Capita Growth in East Africa: A WDI Panel Analysis, 2000-2011"
output_prefix <- "gender_growth_wdi_panel"
analysis_years <- 2000:2013
regression_years <- 2000:2011
mirror_base_url <- "https://raw.githubusercontent.com/ronnywang/worldbank/master/WDI_bundle/parsed"

country_lookup <- c(
  BDI = "Burundi",
  KEN = "Kenya",
  RWA = "Rwanda",
  SSD = "South Sudan",
  TZA = "Tanzania",
  UGA = "Uganda"
)

analysis_countries <- c("BDI", "KEN", "RWA", "SSD", "TZA", "UGA")
regression_countries <- c("BDI", "KEN", "RWA", "TZA", "UGA")

indicator_lookup <- c(
  gdp_per_capita_growth = "NY.GDP.PCAP.KD.ZG",
  female_lfp = "SL.TLF.ACTI.FE.ZS",
  male_lfp = "SL.TLF.ACTI.MA.ZS",
  women_parliament = "SG.GEN.PARL.ZS",
  fertility_rate = "SP.DYN.TFRT.IN",
  female_secondary = "SE.SEC.ENRR.FE",
  male_secondary = "SE.SEC.ENRR.MA",
  gross_capital_formation = "NE.GDI.TOTL.ZS",
  inflation = "FP.CPI.TOTL.ZG",
  trade_open = "NE.TRD.GNFS.ZS"
)

indicator_labels <- c(
  gdp_per_capita_growth = "GDP per capita growth (%)",
  female_lfp = "Female labor force participation (%)",
  male_lfp = "Male labor force participation (%)",
  women_parliament = "Women in parliament (%)",
  fertility_rate = "Fertility rate (births per woman)",
  female_secondary = "Female secondary enrollment (% gross)",
  male_secondary = "Male secondary enrollment (% gross)",
  gross_capital_formation = "Gross capital formation (% GDP)",
  inflation = "Inflation, consumer prices (%)",
  trade_open = "Trade openness (% GDP)"
)

pretty_names <- c(
  gdp_per_capita_growth = "GDP per capita growth",
  female_lfp = "Female labor force participation",
  lfp_gender_gap = "Gender gap in labor force participation",
  women_parliament = "Women in parliament",
  girls_boys_secondary_ratio = "Girls-to-boys secondary enrollment ratio",
  fertility_rate = "Fertility rate",
  gross_capital_formation = "Gross capital formation",
  inflation = "Inflation",
  trade_open = "Trade openness"
)

country_palette <- c(
  Burundi = "#6A3D9A",
  Kenya = "#1B9E77",
  Rwanda = "#D95F02",
  `South Sudan` = "#7570B3",
  Tanzania = "#E7298A",
  Uganda = "#66A61E"
)

save_html_table <- function(data, file, caption = NULL, digits = 2, align = NULL) {
  if (is.null(align)) {
    align <- c("l", rep("r", ncol(data) - 1))
  }

  data %>%
    knitr::kable(
      format = "html",
      caption = caption,
      digits = digits,
      align = align,
      escape = FALSE
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      position = "left"
    ) %>%
    kableExtra::save_kable(file)
}

read_indicator_from_mirror <- function(indicator_code, column_name, countries, years) {
  indicator_url <- sprintf("%s/%s_WDI.csv", mirror_base_url, indicator_code)

  readr::read_csv(indicator_url, show_col_types = FALSE) %>%
    filter(`Country Code` %in% countries) %>%
    select(`Country Name`, `Country Code`, all_of(intersect(as.character(years), names(.)))) %>%
    pivot_longer(
      cols = -c(`Country Name`, `Country Code`),
      names_to = "Year",
      values_to = column_name
    ) %>%
    mutate(Year = as.integer(Year))
}

attempt_wdi_download <- function(countries, years, indicators) {
  message("Attempting direct WDI download...")

  tryCatch({
    suppressWarnings(
      WDI::WDI(
        country = countries,
        indicator = indicators,
        start = min(years),
        end = max(years),
        extra = TRUE
      )
    ) %>%
      select(country, iso3c, year, all_of(names(indicators))) %>%
      rename(Country = country, Country_Code = iso3c, Year = year)
  }, error = function(e) {
    message("Direct WDI download failed; using GitHub mirror of WDI extracts instead.")
    NULL
  })
}

download_panel_data <- function(countries = analysis_countries, years = analysis_years) {
  live_download <- attempt_wdi_download(countries, years, indicator_lookup)

  if (!is.null(live_download) && nrow(live_download) > 0) {
    panel_data <- live_download %>%
      mutate(data_source = "WDI API")
  } else {
    message("Downloading WDI indicator extracts from GitHub mirror...")

    panel_data <- purrr::imap(indicator_lookup, function(indicator_code, column_name) {
      read_indicator_from_mirror(indicator_code, column_name, countries, years)
    }) %>%
      purrr::reduce(full_join, by = c("Country Name", "Country Code", "Year")) %>%
      rename(Country = `Country Name`, Country_Code = `Country Code`) %>%
      mutate(data_source = "GitHub mirror of WDI extracts")
  }

  panel_data %>%
    mutate(
      Country = factor(Country, levels = unname(country_lookup)),
      lfp_gender_gap = female_lfp - male_lfp,
      girls_boys_secondary_ratio = female_secondary / male_secondary,
      sample_flag = case_when(
        Country_Code %in% regression_countries & Year %in% regression_years ~ "Core regression window",
        TRUE ~ "Descriptive only"
      )
    ) %>%
    arrange(Country, Year)
}

create_country_summary <- function(panel_data) {
  panel_data %>%
    group_by(Country, Country_Code) %>%
    summarise(
      Years_Available = n(),
      First_Year = min(Year, na.rm = TRUE),
      Last_Year = max(Year, na.rm = TRUE),
      GDP_Growth_Mean = mean(gdp_per_capita_growth, na.rm = TRUE),
      Female_LFP_Mean = mean(female_lfp, na.rm = TRUE),
      Women_Parliament_Mean = mean(women_parliament, na.rm = TRUE),
      Secondary_Ratio_Mean = mean(girls_boys_secondary_ratio, na.rm = TRUE),
      Missing_Rate_Pct = 100 * mean(
        is.na(gdp_per_capita_growth) |
          is.na(female_lfp) |
          is.na(women_parliament) |
          is.na(fertility_rate)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      across(c(GDP_Growth_Mean, Female_LFP_Mean, Women_Parliament_Mean, Secondary_Ratio_Mean, Missing_Rate_Pct), ~round(.x, 2))
    )
}

create_descriptive_statistics <- function(model_data) {
  model_data %>%
    select(
      gdp_per_capita_growth,
      female_lfp,
      women_parliament,
      fertility_rate,
      gross_capital_formation,
      inflation,
      trade_open,
      girls_boys_secondary_ratio
    ) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      Median = median(Value, na.rm = TRUE),
      SD = sd(Value, na.rm = TRUE),
      Min = min(Value, na.rm = TRUE),
      Max = max(Value, na.rm = TRUE),
      Observations = sum(!is.na(Value)),
      .groups = "drop"
    ) %>%
    mutate(
      Variable = recode(Variable, !!!pretty_names),
      across(c(Mean, Median, SD, Min, Max), ~round(.x, 2))
    )
}

build_model_samples <- function(panel_data) {
  baseline_sample <- panel_data %>%
    filter(Country_Code %in% regression_countries, Year %in% regression_years) %>%
    filter(
      if_all(
        c(
          gdp_per_capita_growth,
          female_lfp,
          women_parliament,
          fertility_rate,
          gross_capital_formation,
          inflation,
          trade_open
        ),
        ~ !is.na(.x)
      )
    )

  education_sample <- baseline_sample %>%
    filter(!is.na(girls_boys_secondary_ratio))

  list(baseline_sample = baseline_sample, education_sample = education_sample)
}

fit_models <- function(baseline_sample, education_sample) {
  pooled_model <- lm(
    gdp_per_capita_growth ~ female_lfp + women_parliament + fertility_rate +
      gross_capital_formation + inflation + trade_open,
    data = baseline_sample
  )

  fixed_effects_model <- lm(
    gdp_per_capita_growth ~ female_lfp + women_parliament + fertility_rate +
      gross_capital_formation + inflation + trade_open + factor(Country) + factor(Year),
    data = baseline_sample
  )

  education_model <- lm(
    gdp_per_capita_growth ~ female_lfp + women_parliament + girls_boys_secondary_ratio +
      fertility_rate + gross_capital_formation + inflation + trade_open,
    data = education_sample
  )

  list(
    pooled_model = pooled_model,
    fixed_effects_model = fixed_effects_model,
    education_model = education_model
  )
}

extract_model_results <- function(model, model_name, robust = FALSE) {
  if (robust) {
    robust_test <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
    model_results <- broom::tidy(robust_test)
    names(model_results)[names(model_results) == "std.error"] <- "robust_se"
  } else {
    model_results <- broom::tidy(model)
    names(model_results)[names(model_results) == "std.error"] <- "standard_se"
  }

  model_results %>%
    mutate(
      Model = model_name,
      Significance = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      )
    )
}

create_regression_table <- function(models) {
  bind_rows(
    extract_model_results(models$pooled_model, "Pooled OLS", robust = FALSE),
    extract_model_results(models$fixed_effects_model, "Two-way FE (HC1)", robust = TRUE),
    extract_model_results(models$education_model, "Education-augmented OLS", robust = FALSE)
  ) %>%
    filter(
      !str_detect(term, "factor\\(Country\\)|factor\\(Year\\)")
    ) %>%
    mutate(
      Term = recode(
        term,
        `(Intercept)` = "Intercept",
        female_lfp = "Female labor force participation",
        women_parliament = "Women in parliament",
        fertility_rate = "Fertility rate",
        gross_capital_formation = "Gross capital formation",
        inflation = "Inflation",
        trade_open = "Trade openness",
        girls_boys_secondary_ratio = "Girls-to-boys secondary enrollment ratio"
      ),
      Estimate = round(estimate, 3),
      SE = round(coalesce(robust_se, standard_se), 3),
      Statistic = round(statistic, 3),
      P_Value = round(p.value, 4)
    ) %>%
    select(Model, Term, Estimate, SE, Statistic, P_Value, Significance)
}

create_model_glance <- function(models, baseline_sample, education_sample) {
  bind_rows(
    broom::glance(models$pooled_model) %>% mutate(Model = "Pooled OLS", Sample = "Baseline sample", Observations = nrow(baseline_sample)),
    broom::glance(models$fixed_effects_model) %>% mutate(Model = "Two-way FE (HC1)", Sample = "Baseline sample", Observations = nrow(baseline_sample)),
    broom::glance(models$education_model) %>% mutate(Model = "Education-augmented OLS", Sample = "Education sample", Observations = nrow(education_sample))
  ) %>%
    transmute(
      Model,
      Sample,
      Observations,
      R_Squared = round(r.squared, 3),
      Adj_R_Squared = round(adj.r.squared, 3),
      AIC = round(AIC, 1),
      BIC = round(BIC, 1)
    )
}

create_equation_table <- function() {
  tibble(
    Model = c("Pooled OLS", "Two-way fixed effects", "Education-augmented OLS"),
    Equation = c(
      "g_it = α + β1 FemaleLFP_it + β2 WomenParliament_it + β3 Fertility_it + β4 Capital_it + β5 Inflation_it + β6 Trade_it + ε_it",
      "g_it = α + β1 FemaleLFP_it + β2 WomenParliament_it + β3 Fertility_it + β4 Capital_it + β5 Inflation_it + β6 Trade_it + μ_i + λ_t + ε_it",
      "g_it = α + β1 FemaleLFP_it + β2 WomenParliament_it + β3 SecondaryRatio_it + β4 Fertility_it + β5 Capital_it + β6 Inflation_it + β7 Trade_it + ε_it"
    )
  )
}

create_plots <- function(panel_data, baseline_sample, models) {
  female_lfp_plot <- panel_data %>%
    filter(Country_Code %in% regression_countries) %>%
    ggplot(aes(x = Year, y = female_lfp, color = Country)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 1.8, na.rm = TRUE) +
    scale_color_manual(values = country_palette[unique(as.character(panel_data$Country))]) +
    labs(
      title = "Female labor force participation rose across most countries",
      x = NULL,
      y = "Female labor force participation (%)",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  parliament_plot <- panel_data %>%
    filter(Country_Code %in% regression_countries) %>%
    ggplot(aes(x = Year, y = women_parliament, color = Country)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 1.8, na.rm = TRUE) +
    scale_color_manual(values = country_palette[unique(as.character(panel_data$Country))]) +
    labs(
      title = "Women's parliamentary representation improved unevenly",
      x = NULL,
      y = "Women in parliament (%)",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  scatter_plot <- baseline_sample %>%
    ggplot(aes(x = female_lfp, y = gdp_per_capita_growth, color = Country)) +
    geom_point(size = 2.2, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
    scale_color_manual(values = country_palette[unique(as.character(panel_data$Country))]) +
    labs(
      title = "Higher female labor participation coincides with faster growth",
      x = "Female labor force participation (%)",
      y = "GDP per capita growth (%)",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  coefficient_plot_data <- broom::tidy(models$fixed_effects_model, conf.int = TRUE) %>%
    filter(term %in% c(
      "female_lfp",
      "women_parliament",
      "fertility_rate",
      "gross_capital_formation",
      "inflation",
      "trade_open"
    )) %>%
    mutate(
      term = fct_relevel(
        recode(
          term,
          female_lfp = "Female labor force participation",
          women_parliament = "Women in parliament",
          fertility_rate = "Fertility rate",
          gross_capital_formation = "Gross capital formation",
          inflation = "Inflation",
          trade_open = "Trade openness"
        ),
        "Female labor force participation",
        "Women in parliament",
        "Fertility rate",
        "Gross capital formation",
        "Inflation",
        "Trade openness"
      )
    )

  coefficient_plot <- ggplot(coefficient_plot_data, aes(x = estimate, y = term)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray55") +
    geom_point(color = "#2C7FB8", size = 2.8) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.18, color = "#2C7FB8") +
    labs(
      title = "Two-way FE estimates",
      x = "Coefficient estimate",
      y = NULL
    ) +
    theme_minimal(base_size = 11)

  dashboard_plot <- (female_lfp_plot + parliament_plot) / (scatter_plot + coefficient_plot) +
    plot_annotation(
      title = analysis_title,
      subtitle = "Core regression sample: Burundi, Kenya, Rwanda, Tanzania, and Uganda (2000-2011)."
    )

  list(
    female_lfp_plot = female_lfp_plot,
    parliament_plot = parliament_plot,
    scatter_plot = scatter_plot,
    coefficient_plot = coefficient_plot,
    dashboard_plot = dashboard_plot
  )
}

run_gender_growth_wdi_panel_analysis <- function() {
  cat("\n", analysis_title, "\n", strrep("=", nchar(analysis_title)), "\n\n", sep = "")

  panel_data <- download_panel_data()
  model_samples <- build_model_samples(panel_data)
  models <- fit_models(model_samples$baseline_sample, model_samples$education_sample)
  country_summary <- create_country_summary(panel_data)
  descriptive_statistics <- create_descriptive_statistics(model_samples$baseline_sample)
  regression_table <- create_regression_table(models)
  model_glance <- create_model_glance(models, model_samples$baseline_sample, model_samples$education_sample)
  equation_table <- create_equation_table()
  plots <- create_plots(panel_data, model_samples$baseline_sample, models)

  write_csv(panel_data, paste0(output_prefix, "_dataset.csv"))
  write_csv(country_summary, paste0(output_prefix, "_country_summary.csv"))
  write_csv(descriptive_statistics, paste0(output_prefix, "_descriptive_statistics.csv"))
  write_csv(regression_table, paste0(output_prefix, "_regression_results.csv"))
  write_csv(model_glance, paste0(output_prefix, "_model_fit.csv"))
  write_csv(equation_table, paste0(output_prefix, "_equations.csv"))

  save_html_table(
    country_summary,
    paste0(output_prefix, "_country_summary.html"),
    caption = "Country-level coverage and average indicators"
  )
  save_html_table(
    descriptive_statistics,
    paste0(output_prefix, "_descriptive_statistics.html"),
    caption = "Descriptive statistics for the baseline regression sample"
  )
  save_html_table(
    regression_table,
    paste0(output_prefix, "_regression_table.html"),
    caption = "Regression estimates for growth and gender inclusion indicators"
  )
  save_html_table(
    model_glance,
    paste0(output_prefix, "_model_fit.html"),
    caption = "Model fit summary"
  )
  save_html_table(
    equation_table,
    paste0(output_prefix, "_model_equations.html"),
    caption = "Regression equations used in the analysis",
    digits = 0,
    align = c("l", "l")
  )

  ggsave(
    filename = paste0(output_prefix, "_dashboard.png"),
    plot = plots$dashboard_plot,
    width = 15,
    height = 10,
    dpi = 320
  )
  ggsave(
    filename = paste0(output_prefix, "_female_lfp_trend.png"),
    plot = plots$female_lfp_plot,
    width = 9,
    height = 5.5,
    dpi = 320
  )
  ggsave(
    filename = paste0(output_prefix, "_women_parliament_trend.png"),
    plot = plots$parliament_plot,
    width = 9,
    height = 5.5,
    dpi = 320
  )
  ggsave(
    filename = paste0(output_prefix, "_growth_scatter.png"),
    plot = plots$scatter_plot,
    width = 8,
    height = 5.5,
    dpi = 320
  )

  cat("Data source used:", unique(panel_data$data_source), "\n")
  cat("Panel observations downloaded:", nrow(panel_data), "\n")
  cat("Baseline regression observations:", nrow(model_samples$baseline_sample), "\n")
  cat("Education-augmented observations:", nrow(model_samples$education_sample), "\n\n")

  cat("Saved files:\n")
  cat("-", paste0(output_prefix, "_dataset.csv"), "\n")
  cat("-", paste0(output_prefix, "_country_summary.csv"), "\n")
  cat("-", paste0(output_prefix, "_descriptive_statistics.csv"), "\n")
  cat("-", paste0(output_prefix, "_regression_results.csv"), "\n")
  cat("-", paste0(output_prefix, "_model_fit.csv"), "\n")
  cat("-", paste0(output_prefix, "_equations.csv"), "\n")
  cat("-", paste0(output_prefix, "_dashboard.png"), "\n")
  cat("-", paste0(output_prefix, "_country_summary.html"), "\n")
  cat("-", paste0(output_prefix, "_descriptive_statistics.html"), "\n")
  cat("-", paste0(output_prefix, "_regression_table.html"), "\n")
  cat("-", paste0(output_prefix, "_model_fit.html"), "\n")
  cat("-", paste0(output_prefix, "_model_equations.html"), "\n\n")

  print(model_glance)

  invisible(list(
    title = analysis_title,
    panel_data = panel_data,
    baseline_sample = model_samples$baseline_sample,
    education_sample = model_samples$education_sample,
    country_summary = country_summary,
    descriptive_statistics = descriptive_statistics,
    regression_table = regression_table,
    model_glance = model_glance,
    equation_table = equation_table,
    models = models,
    plots = plots
  ))
}

analysis_results <- run_gender_growth_wdi_panel_analysis()
