#
## Code availability for the manuscript titled: _'Physical and mental health comorbidities of premenstrual dysphoric disorder: Analysis of 4,475 participants in Our Future Health'_


### Scripts folder comprises four files with R scripts used for the data analysis in the manuscript. 

#

Here's a README-style summary you can drop into your GitHub repo:

---

## Scripts

### `00_build_master_df.r`
Downloads all raw OFH data files from the DNAnexus platform and constructs the master analytical dataset. This script should be run once before any downstream analysis. It merges questionnaire, participant, geography, LSOA, and deprivation index files; filters to female participants; assigns population labels (PMDD, MDD, CTRL); and engineers all derived variables including age, BMI, ethnicity, smoking status, alcohol use, social interaction, parental history of mental illness, income, education, contraceptive method (ever and current), psychotropic medication use, multimorbidity score, composite sleep score, composite activity score, physical activity category, menstrual cycle length, supplement use, and PHQ-9/GAD-7 total scores. The final dataset is saved as `master_df.rds`.

---

### `01_Baseline_summaries.r`
Loads `master_df.rds` and produces baseline descriptive statistics for the PMDD, MDD, and CTRL groups. Outputs a summary table of numeric variables (mean, SD) and categorical variables (n, %) per group, a missingness summary across all analytical variables, and a comparison of complete versus incomplete cases for each population to assess potential selection bias from listwise deletion.

---

### `02_Comorbidity_prevalence.r`
Loads `master_df.rds` and estimates the prevalence of comorbidities in each group (PMDD, MDD, CTRL) with 95% confidence intervals. Covers broad diagnostic categories as well as subcategories within endocrine, mental health, neurodevelopmental, reproductive, autoimmune, cardiovascular, respiratory, digestive, and neurological conditions. Pregnancy-related analyses are restricted to women who have ever been pregnant. Also produces mean multimorbidity scores and a boxplot of the multimorbidity score distribution across groups.

---

### `03_Regression_PMDD_v_MDD.r`
Loads `master_df.rds` and runs Poisson regression models comparing PMDD to MDD (reference group) for each comorbidity category. Sources `utils_regression_helpers.r` for shared comorbidity definitions and the modelling function. Three models are fitted per comorbidity: unadjusted (model 1), adjusted for age and ethnicity (model 2), and fully adjusted including deprivation, education, lifestyle, sleep, BMI, contraceptive method, and psychotropic medication use (model 3). Results are reported as prevalence ratios with robust 95% confidence intervals (HC0 sandwich estimator). Separation diagnostics are saved alongside main results.

---

### `04_Regression_PMDD_v_CTRL.r`
Identical in structure to `03_Regression_PMDD_v_MDD.r` but compares PMDD to controls (CTRL) as the reference group. The fully adjusted model (model 3) excludes psychotropic medication use, as this covariate is not appropriate for a healthy control comparison. Sources `utils_regression_helpers.r` for shared comorbidity definitions and the modelling function.

---

### `utils_regression_helpers.r`
Utility file sourced by `03_Regression_PMDD_v_MDD.r` and `04_Regression_PMDD_v_CTRL.r`. Not intended to be run directly. Contains all shared comorbidity list and variable name definitions (`BROAD_LIST`, `BROAD_VARS`, `SUBCATEGORY_SPECS`), the `analyze_poisson_model()` function which fits and extracts results from Poisson models with robust standard errors, the `binarise_subcategory()` helper for creating binary comorbidity indicators from multi-response fields, and the `dx_upload()` wrapper for uploading outputs to DNAnexus.


#
