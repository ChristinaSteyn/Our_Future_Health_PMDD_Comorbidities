# =============================================================================
# 04_Regression_PMDD_v_CTRL.r
# Poisson regression: PMDD vs CTRL (reference) across all comorbidity categories.
# Requires master_df.rds and regression_helpers.r
# =============================================================================

source("utils_regression_helpers.r")

# ── Load and prepare data ──────────────────────────────────────────────────────
df <- readRDS("master_df.rds")
cat("Loaded master_df.rds. Rows:", nrow(df), "\n")
hist(df$age)

COMPARISON <- "PMDD_v_CTRL"
DEST       <- paste0("/Outputs/Regression_comorbidities/", COMPARISON, "/")

# Scale continuous confounders
df$age                  <- as.vector(scale(as.numeric(df$age)))
df$bmi                  <- as.vector(scale(as.numeric(df$bmi)))
df$composite_sleep_score <- as.vector(scale(as.numeric(df$composite_sleep_score)))

# Filter to PMDD vs CTRL, set CTRL as reference
sub <- df %>%
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Completeness check
test <- na.omit(sub[, c("population", "age", "multiple_deprivation", "ethnicity",
                         "higher_ed", "smoke_tobacco_ever", "alcohol_use",
                         "social_interaction", "bmi", "composite_sleep_score",
                         "physical_activity", "contr_method")])
cat(sprintf("Complete cases: %d / %d (%.1f%%)\n", nrow(test), nrow(sub), 100 * nrow(test) / nrow(sub)))
print(table(sub$population)); print(table(test$population))

# ── Model formulas ─────────────────────────────────────────────────────────────
# Note: psychotropic_med excluded from model3 vs CTRL (controls have no MDD diagnosis)
model_formulas <- list(
  model1 = "population",
  model2 = "population + age + I(age^2) + ethnicity",
  model3 = paste("population + age + I(age^2) + ethnicity + multiple_deprivation +",
                 "higher_ed + smoke_tobacco_ever + alcohol_use +",
                 "social_interaction + bmi + I(bmi^2) + physical_activity +",
                 "composite_sleep_score + I(composite_sleep_score^2) +",
                 "contr_method")
)

# ── Broad category comorbidities ───────────────────────────────────────────────
obj <- sub
for (i in seq_along(BROAD_LIST)) {
  obj <- obj %>%
    mutate(!!sym(BROAD_VARS[i]) := if_else(str_detect(diag_2_m, fixed(BROAD_LIST[i])), 1, 0))
}

output <- analyze_poisson_model(obj, "Broad.csv", model_formulas, BROAD_VARS, ref_group = "CTRL")
dx_upload("Broad.csv",                        DEST)
dx_upload("Broad_separation_diagnostics.csv", DEST)
rm(output); invisible(gc())

# ── Complications in pregnancy (ever pregnant only) ────────────────────────────
obj <- sub %>% filter(children_birthed_num_1_1 > 0) %>%
  mutate(Complications_in_pregnancy = if_else(
    str_detect(diag_2_m, fixed("Complications or difficulties in pregnancy or childbirth")), 1, 0))

output <- analyze_poisson_model(obj, "Complications_pregnancy.csv", model_formulas,
                                "Complications_in_pregnancy", ref_group = "CTRL")
dx_upload("Complications_pregnancy.csv",                        DEST)
dx_upload("Complications_pregnancy_separation_diagnostics.csv", DEST)
rm(output); invisible(gc())

# ── Subcategory comorbidities ──────────────────────────────────────────────────
for (spec in SUBCATEGORY_SPECS) {
  obj <- binarise_subcategory(sub, spec$source_col, spec$list, spec$vars)
  output <- analyze_poisson_model(obj, spec$file, model_formulas, spec$vars, ref_group = "CTRL")
  dx_upload(spec$file, DEST)
  dx_upload(gsub("\\.csv$", "_separation_diagnostics.csv", spec$file), DEST)
  rm(output); invisible(gc())
}

# ── Pregnancy-related depression subtypes (ever pregnant only) ─────────────────
obj <- sub %>% filter(children_birthed_num_1_1 > 0)
obj <- binarise_subcategory(obj, "diag_psych_depr_1_m",
                            c("Perinatal depression", "Postnatal depression"),
                            c("Perinatal_depression",  "Postnatal_depression"))
output <- analyze_poisson_model(obj, "Depression_pregnancy_subtypes.csv", model_formulas,
                                c("Perinatal_depression", "Postnatal_depression"), ref_group = "CTRL")
dx_upload("Depression_pregnancy_subtypes.csv",                        DEST)
dx_upload("Depression_pregnancy_subtypes_separation_diagnostics.csv", DEST)
rm(output); invisible(gc())

# ── Multimorbidity binary ──────────────────────────────────────────────────────
obj <- sub  # multimorbidity_binary already in master_df
output <- analyze_poisson_model(obj, "Multimorbidity.csv", model_formulas,
                                "multimorbidity_binary", ref_group = "CTRL")
dx_upload("Multimorbidity.csv",                        DEST)
dx_upload("Multimorbidity_separation_diagnostics.csv", DEST)
rm(output); invisible(gc())
