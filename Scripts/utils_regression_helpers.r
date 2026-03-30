# =============================================================================
# regression_helpers.r
# Comorbidity definitions and the analyze_poisson_model() function.
# Source this file at the top of 03_ and 04_ regression scripts.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(lmtest)
  library(sandwich)
  library(zoo)
})

# ── Comorbidity definitions ────────────────────────────────────────────────────

BROAD_LIST <- c(
  "Autoimmune disorder", "Blood disorders (Anaemia)", "Cancer",
  "Digestive system or liver problems",
  "Endocrine, nutritional and metabolic disorders (e.g. diabetes, thyroid disorder, vitamin deficiencies)",
  "Eye or visual problems", "Fractures, breaks, or joint problems",
  "Heart or circulatory disease (e.g. high blood pressure or stroke)",
  "Kidney or urinary system disorders", "Lung or respiratory problems",
  "Mental health conditions (e.g. depression, bipolar disorder)",
  "Neurodevelopmental conditions (e.g. Autism spectrum disorder, ADHD)",
  "Neurological disorders (things that affect that brain or nervous system. E.g., Epilepsy)",
  "Reproductive system problems"
)
BROAD_VARS <- c(
  "Autoimmune_disorder", "Blood_disorders", "Cancer",
  "Digestive_system_liver_problems", "Endocrine_nutritional_metabolic_disorders",
  "Eye_visual_problems", "Fractures_breaks_joint_problems", "Heart_circulatory_disease",
  "Kidney_urinary_system_disorders", "Lung_respiratory_problems",
  "Mental_health_conditions", "Neurodevelopmental_conditions",
  "Neurological_disorders", "Reproductive_system_problems"
)

SUBCATEGORY_SPECS <- list(
  list(
    source_col = "diag_endocr_1_m",
    list   = c("Type 1 diabetes", "Type 2 diabetes", "Overactive thyroid",
               "Underactive thyroid", "Cushing syndrome", "Lactose intolerance",
               "Vitamin A deficiency", "Thiamine deficiency", "Vitamin D deficiency"),
    vars   = c("Type_1_diabetes", "Type_2_diabetes", "Overactive_thyroid",
               "Underactive_thyroid", "Cushing_syndrome", "Lactose_intolerance",
               "Vitamin_A_deficiency", "Thiamine_deficiency", "Vitamin_D_deficiency"),
    file   = "Endocrine.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_psych_1_m",
    list   = c("Anxiety", "Bipolar disorder", "Body dysmorphia", "Depression",
               "Post Traumatic Stress Disorder", "Obsessive Compulsive Disorder",
               "Eating disorder", "Schizophrenia", "Personality disorder"),
    vars   = c("Anxiety", "Bipolar_disorder", "Body_dysmorphia", "Depression",
               "Post_Traumatic_Stress_Disorder", "Obsessive_Compulsive_Disorder",
               "Eating_disorder", "Schizophrenia", "Personality_disorder"),
    file   = "Psychiatric.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_neuro_dev_1_m",
    list   = c("Autism spectrum disorder", "Developmental learning disorders",
               "Attention deficit hyperactivity disorder (ADHD)",
               "Disorder of intellectual development"),
    vars   = c("Autism_spectrum_disorder", "Developmental_learning_disorders",
               "ADHD", "Disorder_of_intellectual_development"),
    file   = "Neurodev.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_repro_1_m",
    list   = c("Endometriosis", "Polycystic Ovary Syndrome (PCOS)",
               "Fibrocystic Breast, or another Benign Breast Disease (such as proliferative Benign Breast Disease or LCIS)",
               "Ductal Carcinoma in situ"),
    vars   = c("Endometriosis", "PCOS", "Benign_breast_disease", "Ductal_Carcinoma_in_situ"),
    file   = "Reproductive.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_auto_1_m",
    list   = c("Rheumatoid arthritis", "Lupus", "Multiple Sclerosis (MS)",
               "Graves' disease", "Guillain-Barre syndrome", "Psoriasis"),
    vars   = c("Rheumatoid_arthritis", "Lupus", "Multiple_Sclerosis",
               "Graves_disease", "Guillain_Barre_syndrome", "Psoriasis"),
    file   = "Autoimmune.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_resp_1_m",
    list   = c("Chronic Obstructive pulmonary disease, COPD (including emphysema and chronic bronchitis)",
               "Asthma", "Hay Fever"),
    vars   = c("COPD", "Asthma", "Hay_Fever"),
    file   = "Respiratory.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_cvd_1_m",
    list   = c("B-12 Deficiency (Pernicious Anaemia)", "High Cholesterol",
               "Heart Attack (Myocardial Infarction)", "High Blood Pressure (Hypertension)"),
    vars   = c("B12_Deficiency", "High_Cholesterol", "Myocardial_Infarction", "Hypertension"),
    file   = "CVD.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_gastro_1_m",
    list   = c("Gastro-oesophageal Acid Reflux (GORD)", "Irritable bowel syndrome",
               "Inflammatory Bowel Disease", "Crohn's Disease", "Coeliac Disease",
               "Fatty liver disease", "Hepatitis"),
    vars   = c("GORD", "Irritable_bowel_syndrome", "Inflammatory_Bowel_Disease",
               "Crohns_Disease", "Coeliac_Disease", "Fatty_liver_disease", "Hepatitis"),
    file   = "Digestive.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_neuro_1_m",
    list   = c("Migraine with aura", "Migraine without aura", "Epilepsy"),
    vars   = c("Migraine_with_aura", "Migraine_without_aura", "Epilepsy"),
    file   = "Neurological.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  ),
  list(
    source_col = "diag_psych_depr_1_m",
    list   = c("Major Depressive Disorder"),
    vars   = c("Major_Depressive_Disorder"),
    file   = "Depression_subtypes.csv",
    dest   = "/Outputs/Regression_comorbidities/{COMPARISON}/"
  )
)

# ── Poisson regression function ────────────────────────────────────────────────
# model_formulas: named list with model1, model2, model3 RHS strings
# comorbidity_vars: character vector of binary outcome column names
# ref_group: string like "CTRL" or "MDD" — used to label extraction column

analyze_poisson_model <- function(data, filename, model_formulas, comorbidity_vars, ref_group) {

  pmdd_col <- "populationPMDD"

  safe_extract <- function(tbl, row, col) {
    if (row %in% rownames(tbl) && col %in% colnames(tbl)) as.numeric(tbl[row, col]) else 0
  }

  population_summary_list <- list(model1 = data.frame(), model2 = data.frame(), model3 = data.frame())
  separation_diagnostics  <- data.frame()

  for (comorb in comorbidity_vars) {

    formula_model3 <- paste(comorb, "~", model_formulas[["model3"]])
    vars_in_model  <- all.vars(as.formula(formula_model3))
    df_clean       <- data %>% dplyr::select(all_of(vars_in_model)) %>% na.omit()

    if (nrow(df_clean) == 0) { warning(paste("No complete cases for", comorb)); next }

    sep_table <- table(df_clean[[comorb]], df_clean$population)
    print(paste0(comorb, ":")); print(sep_table)

    sep_check <- data.frame(
      Comorbidity         = comorb,
      Outcome_0_ref       = safe_extract(sep_table, "0", ref_group),
      Outcome_1_ref       = safe_extract(sep_table, "1", ref_group),
      Outcome_0_PMDD      = safe_extract(sep_table, "0", "PMDD"),
      Outcome_1_PMDD      = safe_extract(sep_table, "1", "PMDD"),
      Total_N             = nrow(df_clean),
      Outcome_Prevalence  = mean(df_clean[[comorb]]),
      stringsAsFactors    = FALSE
    )
    names(sep_check)[2:3] <- paste0(c("Outcome_0_", "Outcome_1_"), ref_group)
    separation_diagnostics <- rbind(separation_diagnostics, sep_check)

    for (model_name in names(model_formulas)) {
      print(paste0(comorb, ": ", model_name))
      formula_str  <- paste(comorb, "~", model_formulas[[model_name]])
      convergence_issue <- "Not fitted"

      tryCatch({
        model       <- glm(as.formula(formula_str), data = df_clean, family = poisson(link = "log"))
        coefs       <- coef(model)
        fitted_vals <- fitted(model)
        coef_table  <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
        robust_ses  <- coef_table[, "Std. Error"]
        p_values    <- coef_table[, "Pr(>|z|)"]

        convergence_issue <- case_when(
          !model$converged            ~ "Did not converge",
          any(fitted_vals < 1e-10)   ~ "Zero fitted values",
          !all(is.finite(coefs))     ~ "Infinite coefficients",
          !all(is.finite(robust_ses)) ~ "Infinite standard errors",
          TRUE                        ~ "Stable"
        )

        ci_lower  <- coefs - 1.96 * robust_ses
        ci_upper  <- coefs + 1.96 * robust_ses
        pop_table <- table(model.frame(model)$population)

        results <- data.frame(
          Term            = names(coefs),
          RR              = round(exp(coefs), 3),
          CI_Lower        = round(exp(ci_lower), 3),
          CI_Upper        = round(exp(ci_upper), 3),
          P_Value         = signif(p_values, 3),
          N               = nobs(model),
          pct_retained    = nobs(model) / nrow(data),
          N_ref           = as.numeric(pop_table[ref_group]),
          N_PMDD          = as.numeric(pop_table["PMDD"]),
          Model_Stability = convergence_issue,
          stringsAsFactors = FALSE
        )
        names(results)[names(results) == "N_ref"] <- paste0("N_", ref_group)

        if (pmdd_col %in% results$Term) {
          pop_result <- results[results$Term == pmdd_col, ] %>%
            mutate(Comorbidity = comorb) %>% select(Comorbidity, everything())
          population_summary_list[[model_name]] <- rbind(population_summary_list[[model_name]], pop_result)
        }

      }, error = function(e) {
        warning(paste("Model fitting failed for", comorb, model_name, ":", e$message))
        failed <- data.frame(
          Comorbidity = comorb, Term = pmdd_col,
          RR = NA, CI_Lower = NA, CI_Upper = NA, P_Value = NA,
          N = nrow(df_clean), pct_retained = nrow(df_clean) / nrow(data),
          N_ref = NA, N_PMDD = NA,
          Model_Stability = paste("Error:", e$message),
          stringsAsFactors = FALSE
        )
        names(failed)[names(failed) == "N_ref"] <- paste0("N_", ref_group)
        population_summary_list[[model_name]] <- rbind(population_summary_list[[model_name]], failed)
      })
    }
  }

  combine_results <- bind_rows(population_summary_list, .id = "model_name")
  write.csv(combine_results, filename, row.names = FALSE)

  sep_file <- gsub("\\.csv$", "_separation_diagnostics.csv", filename)
  write.csv(separation_diagnostics, sep_file, row.names = FALSE)

  list(results = combine_results, separation_diagnostics = separation_diagnostics)
}

# ── Helper: binarise from a source column ─────────────────────────────────────
binarise_subcategory <- function(df, source_col, comorbidity_list, comorbidity_vars) {
  for (i in seq_along(comorbidity_list)) {
    pattern <- fixed(comorbidity_list[i])
    new_col <- comorbidity_vars[i]
    df <- df %>%
      mutate(!!sym(new_col) := case_when(
        !!sym(source_col) %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
        str_detect(!!sym(source_col), pattern)                          ~ 1,
        TRUE                                                            ~ 0
      ))
  }
  df
}

# ── Upload helper ──────────────────────────────────────────────────────────────
dx_upload <- function(file, dest) system(paste("dx upload", file, "--destination", dest), intern = TRUE)
