# Synthetic School-to-Work Transition Analysis for Young Graduates
# This script generates a synthetic school-to-work transition survey dataset
# and performs descriptive, inferential, and visual analysis for EAC countries.

# Install required packages if needed
# install.packages(c("dplyr", "ggplot2", "tidyr", "gridExtra", "broom", "readr", "scales"))

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
  library(broom)
  library(readr)
  library(scales)
})

set.seed(1234)

output_prefix <- "school_to_work"
survey_year <- 2026

eac_countries <- c("Burundi", "Kenya", "Rwanda", "South Sudan", "Tanzania", "Uganda")

generate_synthetic_stwt_data <- function(n = 1800, survey_year = 2026) {
  country_probs <- c(0.10, 0.20, 0.14, 0.08, 0.24, 0.24)
  gender_levels <- c("Female", "Male")
  residence_levels <- c("Rural", "Urban")
  institution_levels <- c("Public", "Private")
  degree_levels <- c("Certificate", "Diploma", "Bachelor", "Postgraduate")
  field_levels <- c(
    "Education", "Business", "Engineering", "ICT",
    "Health", "Social Sciences", "Agriculture"
  )
  socioeconomic_levels <- c("Low", "Lower-Middle", "Middle", "Upper-Middle", "High")

  synthetic_data <- tibble(
    Graduate_ID = seq_len(n),
    Country = sample(eac_countries, n, replace = TRUE, prob = country_probs),
    Gender = sample(gender_levels, n, replace = TRUE, prob = c(0.52, 0.48)),
    Residence = sample(residence_levels, n, replace = TRUE, prob = c(0.58, 0.42)),
    Institution_Type = sample(institution_levels, n, replace = TRUE, prob = c(0.74, 0.26)),
    Degree_Level = sample(degree_levels, n, replace = TRUE, prob = c(0.12, 0.23, 0.55, 0.10)),
    Field_of_Study = sample(field_levels, n, replace = TRUE, prob = c(0.15, 0.18, 0.12, 0.11, 0.10, 0.20, 0.14)),
    Socioeconomic_Background = sample(socioeconomic_levels, n, replace = TRUE, prob = c(0.22, 0.25, 0.24, 0.18, 0.11)),
    Graduation_Year = sample(2020:2025, n, replace = TRUE, prob = c(0.14, 0.16, 0.18, 0.18, 0.18, 0.16)),
    Age = sample(20:30, n, replace = TRUE, prob = c(0.05, 0.08, 0.12, 0.15, 0.16, 0.14, 0.10, 0.08, 0.06, 0.04, 0.02)),
    CGPA = round(pmin(pmax(rnorm(n, mean = 3.2, sd = 0.45), 2.0), 4.0), 2),
    Internship = rbinom(n, 1, 0.56),
    Digital_Skills_Score = round(pmin(pmax(rnorm(n, mean = 61, sd = 16), 20), 100)),
    Job_Search_Training = rbinom(n, 1, 0.44)
  ) %>%
    mutate(
      Months_Since_Graduation = pmax((survey_year - Graduation_Year) * 12 - sample(0:11, n(), replace = TRUE), 3L)
    )

  country_transition_effect <- c(
    Burundi = 1.5, Kenya = -1.3, Rwanda = -0.8,
    `South Sudan` = 3.1, Tanzania = -0.3, Uganda = 0.2
  )
  field_transition_effect <- c(
    Education = 1.2, Business = -0.4, Engineering = -1.4,
    ICT = -1.9, Health = -1.1, `Social Sciences` = 0.7, Agriculture = 0.6
  )
  degree_transition_effect <- c(Certificate = 1.3, Diploma = 0.6, Bachelor = -0.4, Postgraduate = -1.0)
  socioeconomic_transition_effect <- c(Low = 1.5, `Lower-Middle` = 0.6, Middle = 0, `Upper-Middle` = -0.8, High = -1.2)

  latent_transition <- 11 +
    unname(country_transition_effect[synthetic_data$Country]) +
    unname(field_transition_effect[synthetic_data$Field_of_Study]) +
    unname(degree_transition_effect[synthetic_data$Degree_Level]) +
    unname(socioeconomic_transition_effect[synthetic_data$Socioeconomic_Background]) +
    ifelse(synthetic_data$Gender == "Female", 0.5, 0) +
    ifelse(synthetic_data$Residence == "Rural", 1.2, -0.2) -
    2.1 * synthetic_data$Internship -
    0.8 * synthetic_data$Job_Search_Training -
    0.05 * (synthetic_data$Digital_Skills_Score - 60) -
    1.1 * (synthetic_data$CGPA - 3.0) +
    rnorm(n, mean = 0, sd = 2.4)

  transition_months <- round(pmin(pmax(latent_transition, 1), 30))
  employed_flag <- transition_months <= synthetic_data$Months_Since_Graduation

  inactive_probability <- plogis(
    -2.2 +
      0.8 * (synthetic_data$Degree_Level == "Certificate") +
      0.5 * (synthetic_data$Socioeconomic_Background == "High") -
      0.6 * synthetic_data$Job_Search_Training
  )
  inactive_flag <- (!employed_flag) & (runif(n) < inactive_probability)

  synthetic_data <- synthetic_data %>%
    mutate(
      Employment_Status = case_when(
        employed_flag ~ "Employed",
        inactive_flag ~ "Inactive",
        TRUE ~ "Unemployed"
      ),
      Transition_Months = ifelse(Employment_Status == "Employed", transition_months, NA_real_),
      Current_Search_Duration = ifelse(
        Employment_Status == "Employed",
        Transition_Months,
        Months_Since_Graduation
      ),
      First_Job_Relevance = ifelse(
        Employment_Status == "Employed",
        rbinom(
          n(),
          1,
          plogis(
            -0.4 +
              0.8 * (Field_of_Study %in% c("Engineering", "ICT", "Health")) +
              0.4 * Internship +
              0.02 * (Digital_Skills_Score - 60) +
              0.3 * (Degree_Level %in% c("Bachelor", "Postgraduate"))
          )
        ),
        NA_integer_
      ),
      Formal_Employment = ifelse(
        Employment_Status == "Employed",
        rbinom(
          n(),
          1,
          plogis(
            -0.3 +
              0.5 * (Residence == "Urban") +
              0.5 * (Institution_Type == "Private") +
              0.6 * (Degree_Level %in% c("Bachelor", "Postgraduate")) +
              0.3 * Internship
          )
        ),
        NA_integer_
      ),
      Contract_Type = case_when(
        Employment_Status != "Employed" ~ NA_character_,
        Formal_Employment == 1 & runif(n()) < 0.48 ~ "Permanent",
        Formal_Employment == 1 ~ "Temporary",
        runif(n()) < 0.35 ~ "Self-Employed",
        TRUE ~ "Casual"
      )
    )

  country_income_effect <- c(
    Burundi = 0.05, Kenya = 0.28, Rwanda = 0.18,
    `South Sudan` = 0.12, Tanzania = 0.22, Uganda = 0.16
  )
  field_income_effect <- c(
    Education = -0.08, Business = 0.05, Engineering = 0.24,
    ICT = 0.28, Health = 0.20, `Social Sciences` = -0.02, Agriculture = -0.05
  )

  employed_rows <- synthetic_data$Employment_Status == "Employed"
  log_earnings <- 5.35 +
    unname(country_income_effect[synthetic_data$Country[employed_rows]]) +
    unname(field_income_effect[synthetic_data$Field_of_Study[employed_rows]]) +
    0.17 * (synthetic_data$Formal_Employment[employed_rows] == 1) +
    0.08 * (synthetic_data$Gender[employed_rows] == "Male") +
    0.16 * (synthetic_data$Residence[employed_rows] == "Urban") +
    0.035 * (synthetic_data$Digital_Skills_Score[employed_rows] / 10) +
    0.11 * (synthetic_data$CGPA[employed_rows] - 3) -
    0.025 * synthetic_data$Transition_Months[employed_rows] +
    rnorm(sum(employed_rows), mean = 0, sd = 0.28)

  synthetic_data$Monthly_Earnings_USD <- NA_real_
  synthetic_data$Monthly_Earnings_USD[employed_rows] <- round(exp(log_earnings), 2)

  synthetic_data %>%
    mutate(
      Internship = ifelse(Internship == 1, "Yes", "No"),
      Job_Search_Training = ifelse(Job_Search_Training == 1, "Yes", "No"),
      First_Job_Relevance = ifelse(First_Job_Relevance == 1, "Matched", ifelse(Employment_Status == "Employed", "Not Matched", NA)),
      Formal_Employment = ifelse(Formal_Employment == 1, "Yes", ifelse(Employment_Status == "Employed", "No", NA))
    )
}

create_country_summary <- function(data) {
  data %>%
    group_by(Country) %>%
    summarise(
      Sample_Size = n(),
      Employment_Rate = round(mean(Employment_Status == "Employed") * 100, 1),
      Unemployment_Rate = round(mean(Employment_Status == "Unemployed") * 100, 1),
      Inactivity_Rate = round(mean(Employment_Status == "Inactive") * 100, 1),
      Median_Transition_Months = round(median(Transition_Months, na.rm = TRUE), 1),
      Mean_Search_Duration = round(mean(Current_Search_Duration), 1),
      Formal_Employment_Rate = round(mean(Formal_Employment == "Yes", na.rm = TRUE) * 100, 1),
      Job_Match_Rate = round(mean(First_Job_Relevance == "Matched", na.rm = TRUE) * 100, 1),
      Average_Earnings_USD = round(mean(Monthly_Earnings_USD, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(Employment_Rate))
}

create_gender_summary <- function(data) {
  data %>%
    group_by(Gender) %>%
    summarise(
      Sample_Size = n(),
      Employment_Rate = round(mean(Employment_Status == "Employed") * 100, 1),
      Median_Transition_Months = round(median(Transition_Months, na.rm = TRUE), 1),
      Formal_Employment_Rate = round(mean(Formal_Employment == "Yes", na.rm = TRUE) * 100, 1),
      Average_Earnings_USD = round(mean(Monthly_Earnings_USD, na.rm = TRUE), 2),
      .groups = "drop"
    )
}

fit_models <- function(data) {
  model_data <- data %>%
    mutate(
      Employed_Flag = ifelse(Employment_Status == "Employed", 1, 0),
      Internship = factor(Internship),
      Job_Search_Training = factor(Job_Search_Training),
      Residence = factor(Residence),
      Gender = factor(Gender),
      Institution_Type = factor(Institution_Type),
      Degree_Level = factor(Degree_Level),
      Field_of_Study = factor(Field_of_Study),
      Country = factor(Country),
      Socioeconomic_Background = factor(Socioeconomic_Background)
    )

  employment_model <- glm(
    Employed_Flag ~ Country + Gender + Residence + Institution_Type +
      Degree_Level + Field_of_Study + Socioeconomic_Background +
      CGPA + Digital_Skills_Score + Internship + Job_Search_Training,
    family = binomial(link = "logit"),
    data = model_data
  )

  earnings_model <- lm(
    log(Monthly_Earnings_USD) ~ Country + Gender + Residence + Institution_Type +
      Degree_Level + Field_of_Study + CGPA + Digital_Skills_Score +
      Transition_Months + Formal_Employment,
    data = data %>% filter(Employment_Status == "Employed")
  )

  list(employment_model = employment_model, earnings_model = earnings_model)
}

create_plots <- function(data, country_summary) {
  status_plot_data <- data %>%
    count(Country, Employment_Status) %>%
    group_by(Country) %>%
    mutate(Share = n / sum(n)) %>%
    ungroup()

  transition_plot <- ggplot(
    data %>% filter(Employment_Status == "Employed"),
    aes(x = reorder(Country, Transition_Months, median), y = Transition_Months, fill = Country)
  ) +
    geom_boxplot(alpha = 0.75, show.legend = FALSE) +
    coord_flip() +
    labs(
      title = "Time to first job by country",
      x = NULL,
      y = "Months to first job"
    ) +
    theme_minimal()

  employment_plot <- ggplot(status_plot_data, aes(x = Country, y = Share, fill = Employment_Status)) +
    geom_col() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("Employed" = "#1b9e77", "Inactive" = "#7570b3", "Unemployed" = "#d95f02")) +
    labs(
      title = "Employment outcomes by country",
      x = NULL,
      y = "Share of graduates",
      fill = "Status"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  training_plot <- ggplot(
    data %>%
      mutate(Internship = factor(Internship, levels = c("No", "Yes"))) %>%
      group_by(Internship, Gender) %>%
      summarise(Employment_Rate = mean(Employment_Status == "Employed"), .groups = "drop"),
    aes(x = Internship, y = Employment_Rate, fill = Gender)
  ) +
    geom_col(position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Employment rate by internship participation",
      x = "Internship participation",
      y = "Employment rate",
      fill = "Gender"
    ) +
    theme_minimal()

  earnings_plot <- ggplot(
    data %>% filter(Employment_Status == "Employed"),
    aes(x = Transition_Months, y = Monthly_Earnings_USD, color = Formal_Employment)
  ) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    scale_y_continuous(labels = dollar_format(prefix = "$")) +
    labs(
      title = "Earnings and speed of transition",
      x = "Months to first job",
      y = "Monthly earnings (USD)",
      color = "Formal job"
    ) +
    theme_minimal()

  dashboard <- grid.arrange(transition_plot, employment_plot, training_plot, earnings_plot, ncol = 2)

  list(
    transition_plot = transition_plot,
    employment_plot = employment_plot,
    training_plot = training_plot,
    earnings_plot = earnings_plot,
    dashboard = dashboard
  )
}

analysis_data <- generate_synthetic_stwt_data(n = 1800, survey_year = survey_year)
country_summary <- create_country_summary(analysis_data)
gender_summary <- create_gender_summary(analysis_data)
models <- fit_models(analysis_data)
plots <- create_plots(analysis_data, country_summary)

employment_model_results <- tidy(models$employment_model, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

earnings_model_results <- tidy(models$earnings_model, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

key_indicators <- tibble(
  Indicator = c(
    "Sample size",
    "Employment rate (%)",
    "Unemployment rate (%)",
    "Inactivity rate (%)",
    "Median transition months",
    "Average monthly earnings among employed (USD)",
    "Formal employment rate among employed (%)",
    "Field-job match rate among employed (%)"
  ),
  Value = c(
    nrow(analysis_data),
    round(mean(analysis_data$Employment_Status == "Employed") * 100, 1),
    round(mean(analysis_data$Employment_Status == "Unemployed") * 100, 1),
    round(mean(analysis_data$Employment_Status == "Inactive") * 100, 1),
    round(median(analysis_data$Transition_Months, na.rm = TRUE), 1),
    round(mean(analysis_data$Monthly_Earnings_USD, na.rm = TRUE), 2),
    round(mean(analysis_data$Formal_Employment == "Yes", na.rm = TRUE) * 100, 1),
    round(mean(analysis_data$First_Job_Relevance == "Matched", na.rm = TRUE) * 100, 1)
  )
)

cat("=== Synthetic School-to-Work Transition Analysis ===\n\n")
cat("Synthetic data generated for", nrow(analysis_data), "young graduates across", length(eac_countries), "EAC countries.\n\n")

cat("Key indicators:\n")
print(key_indicators)

cat("\nCountry summary:\n")
print(country_summary)

cat("\nGender summary:\n")
print(gender_summary)

cat("\nEmployment model odds ratios (selected terms):\n")
print(head(employment_model_results, 12))

cat("\nEarnings model coefficients (selected terms):\n")
print(head(earnings_model_results, 12))

write_csv(analysis_data, paste0(output_prefix, "_synthetic_dataset.csv"))
write_csv(country_summary, paste0(output_prefix, "_country_summary.csv"))
write_csv(gender_summary, paste0(output_prefix, "_gender_summary.csv"))
write_csv(key_indicators, paste0(output_prefix, "_key_indicators.csv"))
write_csv(employment_model_results, paste0(output_prefix, "_employment_model.csv"))
write_csv(earnings_model_results, paste0(output_prefix, "_earnings_model.csv"))

ggsave(
  filename = paste0(output_prefix, "_transition_dashboard.png"),
  plot = plots$dashboard,
  width = 14,
  height = 10,
  dpi = 300
)

cat("\nOutput files created:\n")
cat("-", paste0(output_prefix, "_synthetic_dataset.csv"), "\n")
cat("-", paste0(output_prefix, "_country_summary.csv"), "\n")
cat("-", paste0(output_prefix, "_gender_summary.csv"), "\n")
cat("-", paste0(output_prefix, "_key_indicators.csv"), "\n")
cat("-", paste0(output_prefix, "_employment_model.csv"), "\n")
cat("-", paste0(output_prefix, "_earnings_model.csv"), "\n")
cat("-", paste0(output_prefix, "_transition_dashboard.png"), "\n")
