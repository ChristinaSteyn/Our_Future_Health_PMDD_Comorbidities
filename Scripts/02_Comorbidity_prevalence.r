# =============================================================================
# 02_Comorbidity_prevalence.r
# Estimates prevalence of each comorbidity per group (PMDD / MDD / CTRL).
# Requires master_df.rds produced by 00_build_master_df.r
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(stringr)
  library(rlang)
})

# ── Load master dataframe ──────────────────────────────────────────────────────
df <- readRDS("master_df.rds")
cat("Loaded master_df.rds. Rows:", nrow(df), "\n")
hist(df$age)

# ── Helper: prevalence, SE, 95% CI ────────────────────────────────────────────
prevalence_se_ci <- function(population_df, category, subcategory) {
  cases       <- sum(grepl(subcategory, population_df[[category]], fixed = TRUE))
  exclusion_vals <- c("Prefer not to answer", "Do not know")
  pop_tot <- population_df %>%
    filter(!(diag_2_m %in% c("", exclusion_vals) | (!!sym(category) %in% exclusion_vals))) %>%
    nrow()

  prev  <- cases / pop_tot
  se    <- sqrt((prev * (1 - prev)) / pop_tot)
  ci    <- if (cases > 0 & pop_tot > 0) prop.test(cases, pop_tot, conf.level = 0.95)$conf.int else c(NA, NA)

  data.frame(cases = cases, N = pop_tot, prevalence = prev,
             SE = se, CI_95_L = ci[1], CI_95_U = ci[2])
}

# ── Helper: format results wide ────────────────────────────────────────────────
format_df <- function(results) {
  results %>%
    mutate(
      prevalence = round(prevalence * 100, 2),
      ci_95      = paste0(round(CI_95_L * 100, 2), ", ", round(CI_95_U * 100, 2))
    ) %>%
    select(label, population, prevalence, ci_95, N) %>%
    pivot_wider(
      names_from  = population,
      values_from = c(prevalence, ci_95, N),
      names_glue  = "{population}_{.value}"
    ) %>%
    select(label,
           PMDD_prevalence, PMDD_ci_95, PMDD_N,
           MDD_prevalence,  MDD_ci_95,  MDD_N,
           CTRL_prevalence, CTRL_ci_95, CTRL_N)
}

# ── Helper: run prevalence across populations and comorbidities ───────────────
run_prevalence <- function(df_obj, category_col, comorbidity_list, comorbidity_label) {
  df_obj %>%
    split(.$population) %>%
    map_dfr(~{
      pop <- .x
      map2_dfr(comorbidity_list, comorbidity_label, ~{
        res            <- prevalence_se_ci(pop, category_col, .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label       <- .y
        res
      })
    }) %>%
    select(population, subcategory, label, everything())
}

upload <- function(file, dest) system(paste0("dx upload ", file, " --destination ", dest), intern = TRUE)

# ── Broad category comorbidities ───────────────────────────────────────────────
broad_list <- c(
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
broad_labels <- c(
  "Autoimmune_disorder", "Blood_disorders", "Cancer", "Digestive_system_liver_problems",
  "Endocrine_nutritional_metabolic_disorders", "Eye_visual_problems",
  "Fractures_breaks_joint_problems", "Heart_circulatory_disease",
  "Kidney_urinary_system_disorders", "Lung_respiratory_problems", "Mental_health_conditions",
  "Neurodevelopmental_conditions", "Neurological_disorders", "Reproductive_system_problems"
)

results <- run_prevalence(df %>% select(pid, diag_2_m, population), "diag_2_m", broad_list, broad_labels)
results <- format_df(results)
write.csv(results, "Broad_category_comorbidities.csv")
upload("Broad_category_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Complications in pregnancy (ever pregnant only) ───────────────────────────
obj <- df %>% select(pid, diag_2_m, population, children_birthed_num_1_1) %>%
  filter(children_birthed_num_1_1 > 0)

results <- run_prevalence(obj, "diag_2_m",
  "Complications or difficulties in pregnancy or childbirth",
  "Complications_in_pregnancy")
results <- format_df(results)
write.csv(results, "Complications_pregnancy_prevalence.csv")
upload("Complications_pregnancy_prevalence.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Endocrine comorbidities ────────────────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_endocr_1_m, diag_2_m, population),
  "diag_endocr_1_m",
  c("Type 1 diabetes", "Type 2 diabetes", "Overactive thyroid", "Underactive thyroid",
    "Cushing syndrome", "Lactose intolerance", "Vitamin A deficiency",
    "Thiamine deficiency", "Vitamin D deficiency"),
  c("Type_1_diabetes", "Type_2_diabetes", "Overactive_thyroid", "Underactive_thyroid",
    "Cushing_syndrome", "Lactose_intolerance", "Vitamin_A_deficiency",
    "Thiamine_deficiency", "Vitamin_D_deficiency")
)
results <- format_df(results)
write.csv(results, "Endocrine_metabolic_comorbidities.csv")
upload("Endocrine_metabolic_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Mental health comorbidities ────────────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_psych_1_m, diag_2_m, population),
  "diag_psych_1_m",
  c("Anxiety", "Bipolar disorder", "Body dysmorphia", "Depression",
    "Post Traumatic Stress Disorder", "Obsessive Compulsive Disorder",
    "Eating disorder", "Schizophrenia", "Personality disorder"),
  c("Anxiety", "Bipolar_disorder", "Body_dysmorphia", "Depression",
    "Post_Traumatic_Stress_Disorder", "Obsessive_Compulsive_Disorder",
    "Eating_disorder", "Schizophrenia", "Personality_disorder")
)
results <- format_df(results)
write.csv(results, "Mental_health_comorbidities.csv")
upload("Mental_health_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Neurodevelopmental comorbidities ──────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_neuro_dev_1_m, diag_2_m, population),
  "diag_neuro_dev_1_m",
  c("Autism spectrum disorder", "Developmental learning disorders",
    "Attention deficit hyperactivity disorder (ADHD)", "Disorder of intellectual development"),
  c("Autism_spectrum_disorder", "Developmental_learning_disorders",
    "ADHD", "Disorder_of_intellectual_development")
)
results <- format_df(results)
write.csv(results, "Neurodevelopmental_comorbidities.csv")
upload("Neurodevelopmental_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Reproductive comorbidities ─────────────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_repro_1_m, diag_2_m, population),
  "diag_repro_1_m",
  c("Endometriosis", "Polycystic Ovary Syndrome (PCOS)",
    "Fibrocystic Breast, or another Benign Breast Disease (such as proliferative Benign Breast Disease or LCIS)",
    "Ductal Carcinoma in situ"),
  c("Endometriosis", "PCOS", "Benign_breast_disease", "Ductal_Carcinoma_in_situ")
)
results <- format_df(results)
write.csv(results, "Reproductive_comorbidities.csv")
upload("Reproductive_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Autoimmune comorbidities ───────────────────────────────────────────────────
autoimmune_list   <- c("Rheumatoid arthritis", "Lupus", "Multiple Sclerosis (MS)",
                       "Graves' disease", "Guillain-Barre syndrome", "Psoriasis")
autoimmune_labels <- c("Rheumatoid_arthritis", "Lupus", "Multiple_Sclerosis",
                       "Graves_disease", "Guillain_Barre_syndrome", "Psoriasis")

results <- run_prevalence(df %>% select(pid, diag_auto_1_m, diag_2_m, population),
                          "diag_auto_1_m", autoimmune_list, autoimmune_labels)
results <- format_df(results)
write.csv(results, "Autoimmune_comorbidities.csv")
upload("Autoimmune_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")

# Exploratory: include "None of the above" and "Other (not listed)"
results <- run_prevalence(df %>% select(pid, diag_auto_1_m, diag_2_m, population),
                          "diag_auto_1_m",
                          c(autoimmune_list, "None of the above", "Other (not listed)"),
                          c(autoimmune_labels, "None_of_above", "Other"))
results <- format_df(results)
write.csv(results, "Autoimmune_comorbidities_with_other.csv")
upload("Autoimmune_comorbidities_with_other.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Kidney / urinary comorbidities ─────────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_urol_1_m, diag_2_m, population),
  "diag_urol_1_m",
  c("Chronic kidney disease (or chronic kidney failure)", "Kidney stones",
    "None of the above", "Other (not listed)"),
  c("Chronic_kidney_disease", "Kidney_stones", "None_of_the_above", "Other")
)
results <- format_df(results)
write.csv(results, "Kidney_or_urinary_cormorbidities.csv")
upload("Kidney_or_urinary_cormorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Cardiovascular comorbidities ───────────────────────────────────────────────
results <- run_prevalence(
  df[!is.na(df$population), ] %>% select(pid, diag_cvd_1_m, diag_2_m, population),
  "diag_cvd_1_m",
  c("B-12 Deficiency (Pernicious Anaemia)", "High Cholesterol",
    "Heart Attack (Myocardial Infarction)", "High Blood Pressure (Hypertension)"),
  c("B12_Deficiency", "High_Cholesterol", "Myocardial_Infarction", "Hypertension")
)
results <- format_df(results)
write.csv(results, "Cardiovascular_comorbidities.csv")
upload("Cardiovascular_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Respiratory comorbidities ──────────────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_resp_1_m, diag_2_m, population),
  "diag_resp_1_m",
  c("Chronic Obstructive pulmonary disease, COPD (including emphysema and chronic bronchitis)",
    "Asthma", "Hay Fever"),
  c("COPD", "Asthma", "Hay_Fever")
)
results <- format_df(results)
write.csv(results, "Respiratory_comorbidities.csv")
upload("Respiratory_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Digestive / liver comorbidities ───────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_gastro_1_m, diag_2_m, population),
  "diag_gastro_1_m",
  c("Gastro-oesophageal Acid Reflux (GORD)", "Irritable bowel syndrome",
    "Inflammatory Bowel Disease", "Crohn's Disease", "Coeliac Disease",
    "Fatty liver disease", "Hepatitis"),
  c("GORD", "Irritable_bowel_syndrome", "Inflammatory_Bowel_Disease",
    "Crohns_Disease", "Coeliac_Disease", "Fatty_liver_disease", "Hepatitis")
)
results <- format_df(results)
write.csv(results, "Digestive_liver_comorbidities.csv")
upload("Digestive_liver_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Neurological comorbidities ─────────────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_neuro_1_m, diag_2_m, population),
  "diag_neuro_1_m",
  c("Migraine with aura", "Migraine without aura", "Epilepsy"),
  c("Migraine_with_aura", "Migraine_without_aura", "Epilepsy")
)
results <- format_df(results)
write.csv(results, "Neuro_comorbidities.csv")
upload("Neuro_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Depression subtypes ────────────────────────────────────────────────────────
results <- run_prevalence(
  df %>% select(pid, diag_psych_depr_1_m, diag_2_m, population),
  "diag_psych_depr_1_m",
  c("Major Depressive Disorder", "Other (not listed)"),
  c("Major_Depressive_Disorder", "Other_not_listed")
)
results <- format_df(results)
write.csv(results, "Depression_subtypes_comorbidities.csv")
upload("Depression_subtypes_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Pregnancy-related depression subtypes (ever pregnant only) ─────────────────
obj <- df %>%
  select(pid, diag_psych_depr_1_m, diag_2_m, population, children_birthed_num_1_1) %>%
  filter(children_birthed_num_1_1 > 0)

results <- run_prevalence(obj, "diag_psych_depr_1_m",
  c("Perinatal depression", "Postnatal depression"),
  c("Perinatal_depression", "Postnatal_depression"))
results <- format_df(results)
write.csv(results, "Depression_pregnancy_subtypes_comorbidities.csv")
upload("Depression_pregnancy_subtypes_comorbidities.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Mean multimorbidity score ──────────────────────────────────────────────────
obj <- df %>% select(pid, diag_2_m, population)
excluded_options <- c(
  "Mental health conditions (e.g.depression, bipolar disorder)",
  "Complications or difficulties in pregnancy or childbirth",
  "", "Other not listed", "None of the above", "Do not know", "Prefer not to answer"
)
obj$choices_list    <- strsplit(obj$diag_2_m, "\\|")
obj$filtered_choices <- lapply(obj$choices_list, function(x) setdiff(trimws(x), excluded_options))
obj$multimorbidity_score <- sapply(obj$filtered_choices, length)
obj <- obj[!sapply(obj$choices_list, function(x) any(trimws(x) %in% excluded_options)), ]
obj <- obj[!is.na(obj$multimorbidity_score), ]

morbidity_stats <- obj %>%
  group_by(population) %>%
  summarise(mean_morbidity = mean(multimorbidity_score),
            sd_morbidity   = sd(multimorbidity_score),
            n              = n(),
            standard_error = sd_morbidity / sqrt(n)) %>%
  mutate(ci_results = map2(mean_morbidity, standard_error, ~{
    ci <- .x + c(-1, 1) * qnorm(0.975) * .y
    list(lower = ci[1], upper = ci[2])
  })) %>%
  unnest_wider(ci_results)

write.csv(morbidity_stats, "Mean_multimorbidity_score.csv")
upload("Mean_multimorbidity_score.csv", "/Outputs/Comorbidity_prevalence/")
invisible(gc())

# ── Multimorbidity score boxplot ───────────────────────────────────────────────
obj <- df %>% select(pid, diag_2_m, population)
obj$choices_list <- strsplit(obj$diag_2_m, "\\|")
obj$multimorbidity_score <- sapply(obj$choices_list, function(x) {
  x <- trimws(x)
  if ("None of the above" %in% x)    return(0)
  if ("Do not know" %in% x)          return(NA_integer_)
  if ("Prefer not to answer" %in% x) return(NA_integer_)
  mental_health           <- grepl("Mental health conditions", x, ignore.case = TRUE)
  pregnancy_complications <- grepl("Complications or difficulties in pregnancy", x, ignore.case = TRUE)
  other_not_listed        <- x %in% "Other not listed"
  if (any(other_not_listed) & any(mental_health | pregnancy_complications)) return(NA_integer_)
  if (all(mental_health | pregnancy_complications) & any(mental_health | pregnancy_complications)) return(0)
  if (all(other_not_listed)) return(NA_integer_)
  valid <- x[!mental_health & !pregnancy_complications & !other_not_listed]
  if (length(valid) == 0) NA_integer_ else length(valid)
})

obj <- obj[!is.na(obj$multimorbidity_score), ]
obj$population[obj$population == "CTRL"] <- "Control"
obj$Group <- factor(obj$population, levels = c("PMDD", "MDD", "Control"))

group_colors <- c("PMDD" = "#f1a226", "MDD" = "#800074", "Control" = "#298c8c")
summary_df   <- obj %>%
  group_by(Group) %>%
  summarise(mean_score = mean(multimorbidity_score, na.rm = TRUE),
            se         = sd(multimorbidity_score, na.rm = TRUE) / sqrt(n()),
            .groups    = "drop")

p <- ggplot(obj, aes(x = Group, y = multimorbidity_score, fill = Group)) +
  geom_boxplot(alpha = 0.6, width = 0.5, color = "black", outlier.shape = NA) +
  geom_point(data = summary_df, aes(x = Group, y = mean_score),
             color = "black", size = 4, inherit.aes = FALSE) +
  geom_errorbar(data = summary_df,
                aes(x = Group, ymin = mean_score - se, ymax = mean_score + se),
                width = 0.1, color = "black", linewidth = 0.4, inherit.aes = FALSE) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(limits = c(0, 11), breaks = 0:11, expand = c(0, 0)) +
  labs(title = "Distribution of Multimorbidity Scores with Group Means", x = "Group", y = "Score") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold")) +
  geom_segment(aes(x = 1, xend = 2, y = 9,   yend = 9),   color = "black") +
  annotate("text", x = 1.5, y = 9.2, label = "***", size = 5) +
  geom_segment(aes(x = 1, xend = 3, y = 9.5, yend = 9.5), color = "black") +
  annotate("text", x = 2,   y = 9.8, label = "***", size = 5) +
  geom_segment(aes(x = 2, xend = 3, y = 8.5, yend = 8.5), color = "black") +
  annotate("text", x = 2.5, y = 8.8, label = "***", size = 5)

png("Distribution_of_Multimorbidity_Scores_Boxplot.png")
print(p)
dev.off()
upload("Distribution_of_Multimorbidity_Scores_Boxplot.png", "/Outputs/Comorbidity_prevalence/")
