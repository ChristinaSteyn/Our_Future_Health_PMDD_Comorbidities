# =============================================================================
# 01_Baseline_summaries.r
# Baseline characteristics, missingness, and complete vs incomplete case comparison.
# Requires master_df.rds produced by 00_build_master_df.r
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(ggplot2)
  library(stringr)
  library(epiR)
  library(data.table)
})

# ── Load master dataframe ──────────────────────────────────────────────────────
df <- readRDS("master_df.rds")
cat("Loaded master_df.rds. Rows:", nrow(df), "\n")

sum(is.na(df$age))
hist(df$age)
levels(as.factor(df$multiple_deprivation))
sum(is.na(df$multiple_deprivation))

# ── Variable lists ─────────────────────────────────────────────────────────────
num_vars <- c(
  "age", "bmi", "multimorbidity_score", "composite_sleep_score",
  "composite_activity_score", "menstrual_cycle_length", "sum_phq9_gad7"
)

factor_vars <- c(
  "ethnicity", "income", "smoke_tobacco_ever", "alcohol_use",
  "social_interaction", "parental_history_mental_illness",
  "pat_hist_mental", "mat_hist_mental", "higher_ed", "physical_activity",
  "contraception_use", "contr_method", "reg_contr_method", "reg_menses",
  "vitamin_d", "vitamin_b", "calcium", "psychotropic_med",
  "geographic_region", "multiple_deprivation"
)

all_vars <- c(num_vars, factor_vars)

# ── Ensure correct types ───────────────────────────────────────────────────────
df_filtered <- df %>%
  select(population, all_of(all_vars)) %>%
  mutate(across(all_of(num_vars),    as.numeric)) %>%
  mutate(across(all_of(factor_vars), as.factor))

# ── Sample sizes per population ────────────────────────────────────────────────
sample_sizes <- df_filtered %>%
  group_by(population) %>%
  summarize(n = n(), .groups = "drop")

create_pop_colname <- function(pop, n) paste0(pop, " (n=", n, ")")

# ── Numeric summary: mean (SD) ─────────────────────────────────────────────────
num_summary <- df_filtered %>%
  group_by(population) %>%
  summarize(across(all_of(num_vars),
                   ~paste0(round(mean(.x, na.rm = TRUE), 2),
                           " (", round(sd(.x, na.rm = TRUE), 2), ")"),
                   .names = "{col}"),
            .groups = "drop") %>%
  left_join(sample_sizes, by = "population") %>%
  mutate(population = create_pop_colname(population, n)) %>%
  select(-n) %>%
  pivot_longer(-population, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = population, values_from = value)

# ── Factor summary: n (%) ─────────────────────────────────────────────────────
factor_summary <- df_filtered %>%
  select(population, all_of(factor_vars)) %>%
  pivot_longer(-population, names_to = "variable", values_to = "value") %>%
  mutate(value_char = ifelse(is.na(as.character(value)), "ZZZ_NA_ZZZ", as.character(value))) %>%
  group_by(population, variable, value_char) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(population, variable) %>%
  mutate(percent  = 100 * count / sum(count),
         combined = paste0(count, " (", round(percent, 1), "%)")) %>%
  ungroup() %>%
  mutate(value = ifelse(value_char == "ZZZ_NA_ZZZ", NA_character_, value_char)) %>%
  select(population, variable, value, combined) %>%
  left_join(sample_sizes, by = "population") %>%
  mutate(population = create_pop_colname(population, n)) %>%
  select(-n) %>%
  pivot_wider(names_from = population, values_from = combined) %>%
  arrange(variable, is.na(value), value)

# ── Combine and write ──────────────────────────────────────────────────────────
combined_summary <- bind_rows(
  num_summary    %>% mutate(variable_type = "numeric", value = NA_character_, .before = 1),
  factor_summary %>% mutate(variable_type = "factor",  .before = 1)
) %>%
  arrange(variable_type, variable, is.na(value), value)

print(combined_summary)
write.csv(combined_summary, "Summary_variables_per_population.csv")
system("dx upload Summary_variables_per_population.csv --destination /Outputs/Baseline/", intern = TRUE)

# ── Missingness summary ────────────────────────────────────────────────────────
missing_summary <- df_filtered %>%
  mutate(across(all_of(all_vars), as.character)) %>%
  pivot_longer(-population, names_to = "variable", values_to = "value") %>%
  group_by(population, variable) %>%
  summarize(
    n_non_missing = sum(!is.na(value) & value != ""),
    n_missing     = sum(is.na(value) | value == ""),
    pct_missing   = 100 * n_missing / (n_missing + n_non_missing),
    .groups = "drop"
  )

print(missing_summary)
write.csv(missing_summary, "Missingness_summary.csv")
system("dx upload Missingness_summary.csv --destination /Outputs/Baseline/", intern = TRUE)
invisible(gc())

# ── Complete vs incomplete case comparison ────────────────────────────────────
comparison_vars <- c(
  "pid", "population", "age", "ethnicity", "multiple_deprivation", "higher_ed",
  "bmi", "smoke_tobacco_ever", "alcohol_use", "social_interaction",
  "composite_sleep_score", "physical_activity", "contr_method", "psychotropic_med"
)

create_summary_table <- function(data, vars = comparison_vars) {
  d <- data %>% select(any_of(vars))

  nv <- intersect(c("age", "bmi", "composite_sleep_score"), names(d))
  fv <- intersect(c("ethnicity", "smoke_tobacco_ever", "alcohol_use", "physical_activity",
                    "social_interaction", "higher_ed", "contr_method", "psychotropic_med",
                    "multiple_deprivation"), names(d))

  if (length(nv) > 0) d <- d %>% mutate(across(all_of(nv), as.numeric))
  if (length(fv) > 0) d <- d %>% mutate(across(all_of(fv), as.factor))

  total_n <- nrow(d)

  num_sum <- if (length(nv) > 0)
    d %>% summarize(across(all_of(nv),
                           ~paste0(round(mean(.x, na.rm = TRUE), 2),
                                   " (", round(sd(.x, na.rm = TRUE), 2), ")"))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "Summary")

  fac_sum <- if (length(fv) > 0)
    d %>% select(all_of(fv)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(variable, value) %>%
    summarize(count = n(), .groups = "drop") %>%
    group_by(variable) %>%
    mutate(percent = 100 * count / sum(count),
           Summary = paste0(count, " (", round(percent, 1), "%)")) %>%
    ungroup() %>% select(variable, value, Summary) %>% arrange(variable, value)

  result <- bind_rows(
    if (!is.null(num_sum)) num_sum %>% mutate(variable_type = "numeric", value = NA_character_),
    if (!is.null(fac_sum)) fac_sum %>% mutate(variable_type = "factor",  value = as.character(value))
  )
  result$N <- total_n
  result
}

cf_complete_incomplete <- function(df, population_name) {
  subset <- df %>%
    filter(population == population_name) %>%
    select(any_of(comparison_vars))

  complete   <- subset %>% na.omit()
  incomplete <- subset[!subset$pid %in% complete$pid, ]

  cat("\n---", population_name, "---\n")
  cat("Complete cases:", nrow(complete), "/", nrow(subset),
      sprintf("(%.1f%%)\n", 100 * nrow(complete) / nrow(subset)))

  bind_rows(
    create_summary_table(complete)   %>% mutate(Population = population_name, Status = "Complete",   .before = 1),
    create_summary_table(incomplete) %>% mutate(Population = population_name, Status = "Incomplete", .before = 1)
  )
}

combined_cf <- bind_rows(
  cf_complete_incomplete(df, "PMDD"),
  cf_complete_incomplete(df, "MDD"),
  cf_complete_incomplete(df, "CTRL")
)

write.csv(combined_cf, "Complete_incomplete_summary.csv", row.names = FALSE)
system("dx upload Complete_incomplete_summary.csv --destination /Outputs/Baseline/", intern = TRUE)
