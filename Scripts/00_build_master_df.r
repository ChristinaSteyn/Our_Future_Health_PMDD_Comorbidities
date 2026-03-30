# =============================================================================
# 00_build_master_df.r
# Downloads raw OFH data, engineers all derived variables, and saves a clean
# master data.frame as master_df.rds for use by all downstream scripts.
# =============================================================================

# ── Libraries ─────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(ggplot2)
  library(stringr)
  library(epiR)
  library(data.table)
  library(rlang)
})

# ── Download raw files ─────────────────────────────────────────────────────────
system("dx download OFH_exported_files/OFH_questionnaire.csv",         intern = TRUE)
system("dx download OFH_exported_files/OFH_participant.csv",           intern = TRUE)
system("dx download OFH_exported_files/OFH_participant_geographies.csv", intern = TRUE)
system("dx download OFH_exported_files/lsoa.csv",                      intern = TRUE)
system("dx download Multiple_deprivation_score_data/Deprivation_index.csv", intern = TRUE)
system("dx download OFH_exported_files/PMDD.csv",                      intern = TRUE)
system("dx download OFH_exported_files/CTRL.csv",                      intern = TRUE)
system("dx download OFH_exported_files/MDD.csv",                       intern = TRUE)

# ── Read raw files ─────────────────────────────────────────────────────────────
df          <- read.table("OFH_questionnaire.csv",            sep = ",", header = TRUE, quote = "\"")
participant <- read.table("OFH_participant.csv",              sep = ",", header = TRUE, quote = "\"")
geographies <- read.table("OFH_participant_geographies.csv",  sep = ",", header = TRUE, quote = "\"")
LSOA        <- read.table("lsoa.csv",                         sep = ",", header = TRUE, quote = "\"")
index       <- read.table("Deprivation_index.csv",            sep = ",", header = TRUE, quote = "\"")

fem_PMDD_ID <- read.table("PMDD.csv", sep = ",", header = TRUE, quote = "\"")
fem_CTRL_ID <- read.table("CTRL.csv", sep = ",", header = TRUE, quote = "\"")
fem_MDD_ID  <- read.table("MDD.csv",  sep = ",", header = TRUE, quote = "\"")

nrow(df); nrow(participant); nrow(geographies); nrow(LSOA); nrow(index)

# ── Fix missing pid mismatch ───────────────────────────────────────────────────
missing_pids <- setdiff(head(df$pid, nrow(df)), head(participant$pid, nrow(df)))
participant  <- participant[!participant$pid %in% missing_pids, ]
df           <- df[!df$pid %in% missing_pids, ]

# ── Merge geographies and deprivation index ────────────────────────────────────
df    <- full_join(df, geographies, by = "pid")
df    <- full_join(df, LSOA,        by = "pid")
index <- index[, c(2, 6)]
colnames(index) <- c("lsoa_at_reg", "multiple_deprivation_index")
df    <- full_join(df, index, by = "lsoa_at_reg")

# ── Select variables of interest ───────────────────────────────────────────────
vars_to_keep <- c(
  "pid", "submission_date", "demog_weight_1_1", "demog_height_1_1",
  "smoke_tobacco_type_1_m", "alcohol_curr_1_1", "lifestyle_social_visits_1_1",
  "father_diag_a_2_m", "mother_diag_a_2_m", "housing_income_1_1",
  "edu_qual_1_m", "gyn_contracept_implant_1_1", "gyn_contracept_methods_1_m",
  "medicat_1_m", "medicat_repro_1_m", "medicat_repro_contracept_1_m",
  "diag_2_m", "diag_cvd_1_m", "children_birthed_num_1_1",
  "diag_endocr_1_m", "diag_psych_1_m", "diag_neuro_dev_1_m",
  "diag_repro_1_m", "diag_auto_1_m", "diag_urol_1_m", "diag_resp_1_m",
  "diag_gastro_1_m", "diag_neuro_1_m", "diag_psych_depr_1_m",
  "sleep_hrs_1_1", "sleep_trouble_1_1", "sleep_chronotype_1_1",
  "sleep_snoring_1_1", "sleep_daytime_1_1",
  "activity_mod_days_2_1", "activity_mod_mins_2_1",
  "activity_vig_days_2_1", "activity_vig_mins_2_1", "activity_type_1_m",
  "gyn_menstr_cycle_days_1_1", "gyn_menstr_cycle_days_2_1",
  "gyn_menopause_1_1", "gyn_menopause_2_1",
  "phq9_item1_interest_1_1", "phq9_item2_down_1_1", "phq9_item3_sleep_1_1",
  "phq9_item4_energy_1_1", "phq9_item5_appetite_1_1", "phq9_item6_bad_1_1",
  "phq9_item7_concentr_1_1", "phq9_item8_movement_1_1", "phq9_item9_harm_1_1",
  "gad7_item1_anx_1_1", "gad7_item2_worry_control_1_1",
  "gad7_item3_worry_amount_1_1", "gad7_item4_relax_1_1",
  "gad7_item5_restless_1_1", "gad7_item6_annoyed_1_1", "gad7_item7_afraid_1_1",
  "medicat_suppl_1_m", "medicat_psych_1_m",
  "region_at_reg", "multiple_deprivation_index"
)

df <- df %>% select(all_of(vars_to_keep))
invisible(gc())

# ── Filter to females and label populations ────────────────────────────────────
OFH_female <- participant[
  participant$demog_sex_1_1 %in% "Female" | participant$demog_sex_2_1 %in% "Female", ]

df_female <- df[df$pid %in% OFH_female$pid, ]
df_female <- df_female[!df_female$diag_2_m %in% c("", "Do not know", "Prefer not to answer"), ]
fT <- nrow(df_female)

# Filter controls to only those who answered diag_2_m
fem_CTRL_ID <- fem_CTRL_ID[fem_CTRL_ID$pid %in% df_female$pid, ]

# Assign population labels
df$population <- ifelse(df$pid %in% fem_PMDD_ID$pid, "PMDD",
                 ifelse(df$pid %in% fem_CTRL_ID$pid,  "CTRL",
                 ifelse(df$pid %in% fem_MDD_ID$pid,   "MDD", NA)))
df <- df[!is.na(df$population), ]

participant$population <- ifelse(participant$pid %in% fem_PMDD_ID$pid, "PMDD",
                          ifelse(participant$pid %in% fem_CTRL_ID$pid,  "CTRL",
                          ifelse(participant$pid %in% fem_MDD_ID$pid,   "MDD", NA)))
participant <- participant[!is.na(participant$population), ]
participant <- participant[match(df$pid, participant$pid), ]

nrow(df); nrow(participant)
invisible(gc())

# ── Age ────────────────────────────────────────────────────────────────────────
participant$birth_date <- as.Date(
  paste0(participant$birth_year, "-", participant$birth_month, "-01"))
df$submission_date <- as.Date(df$submission_date)
df$age <- time_length(interval(participant$birth_date, df$submission_date), "years")
invisible(gc())

# ── BMI ────────────────────────────────────────────────────────────────────────
df$bmi <- df$demog_weight_1_1 / (df$demog_height_1_1 / 100)^2
invisible(gc())

# ── Ethnicity ──────────────────────────────────────────────────────────────────
df$demog_ethnicity_1_1 <- participant$demog_ethnicity_1_1
df <- df %>%
  mutate(
    ethnicity = case_when(
      demog_ethnicity_1_1 %in% c(
        "White – English / Welsh / Scottish / Northern Irish / British",
        "White – Irish", "White – Gypsy or Irish Traveller", "White – Polish",
        "Any other white background"
      ) ~ "White",
      demog_ethnicity_1_1 %in% c(
        "Mixed – White and Black Caribbean", "Mixed – White and Black African",
        "Mixed – White and Asian", "Any other mixed multiple ethnic background"
      ) ~ "Mixed",
      demog_ethnicity_1_1 %in% c(
        "Asian or Asian British – Indian", "Asian or Asian British – Pakistani",
        "Asian or Asian British – Bangladeshi", "Chinese",
        "Any other Asian/Asian British background"
      ) ~ "Asian",
      demog_ethnicity_1_1 %in% c(
        "Black or Black British – African", "Black or Black British – Caribbean",
        "Any other Black / African / Caribbean background"
      ) ~ "Black",
      demog_ethnicity_1_1 %in% c("Arab")  ~ "Arab",
      demog_ethnicity_1_1 %in% c("Other") ~ "Other",
      TRUE ~ NA_character_
    )
  )
invisible(gc())

# ── Smoking (ever smoked) ──────────────────────────────────────────────────────
df <- df %>%
  mutate(
    smoke_tobacco_ever = case_when(
      smoke_tobacco_type_1_m %in% c(
        "I have not used any of these tobacco products",
        "Chewing tobacco, snus, snuff, or dip (e.g. Skruf, Tulsi, Sikandar, conwood, Al Capone Powder)"
      ) ~ 0,
      smoke_tobacco_type_1_m %in% c("", "Prefer not to answer") ~ NA_real_,
      TRUE ~ 1
    )
  )
invisible(gc())

# ── Alcohol use ────────────────────────────────────────────────────────────────
df <- df %>%
  mutate(alcohol_use = case_when(
    alcohol_curr_1_1 %in% c("Daily or almost daily") ~ "Daily_or_almost_daily",
    alcohol_curr_1_1 %in% c("Three or four times a week", "Once or twice a week") ~ "One_to_four_times_a_week",
    alcohol_curr_1_1 %in% c("One to three times a month", "Special occasions only", "Never") ~ "Less_than_once_a_week",
    alcohol_curr_1_1 == "Prefer not to answer" ~ NA_character_,
    TRUE ~ NA_character_
  ))
invisible(gc())

# ── Social interaction ─────────────────────────────────────────────────────────
df <- df %>%
  mutate(social_interaction = case_when(
    lifestyle_social_visits_1_1 == "Almost daily" ~ "Almost_daily",
    lifestyle_social_visits_1_1 %in% c("2-4 times a week", "About once a week") ~ "One_to_four_times_a_week",
    lifestyle_social_visits_1_1 %in% c("About once a month", "Once every few month", "Never or almost never") ~ "Less_than_once_a_week",
    lifestyle_social_visits_1_1 %in% c("No friends/family outside household", "Do not know", "Prefer not to answer") ~ NA_character_,
    TRUE ~ NA_character_
  ))

# ── Parental history of mental illness ────────────────────────────────────────
# Rules: either parent = 1 → 1; both = 0 → 0; any missing → NA
remove_hist_mental <- "Do not know|Prefer not to answer"
df <- df %>%
  mutate(
    pat_hist_mental = case_when(
      grepl("Mental health conditions", father_diag_a_2_m, fixed = TRUE) ~ 1L,
      grepl(remove_hist_mental, father_diag_a_2_m) ~ NA_integer_,
      father_diag_a_2_m %in% "" ~ NA_integer_,
      TRUE ~ 0L
    ),
    mat_hist_mental = case_when(
      grepl("Mental health conditions", mother_diag_a_2_m, fixed = TRUE) ~ 1L,
      grepl(remove_hist_mental, mother_diag_a_2_m) ~ NA_integer_,
      mother_diag_a_2_m %in% "" ~ NA_integer_,
      TRUE ~ 0L
    ),
    parental_history_mental_illness = case_when(
      pat_hist_mental == 1L | mat_hist_mental == 1L ~ 1L,
      pat_hist_mental == 0L & mat_hist_mental == 0L ~ 0L,
      is.na(pat_hist_mental) & is.na(mat_hist_mental) ~ NA_integer_,
      (pat_hist_mental == 0L & is.na(mat_hist_mental)) |
        (is.na(pat_hist_mental) & mat_hist_mental == 0L) ~ NA_integer_,
      TRUE ~ NA_integer_
    )
  )
invisible(gc())

# ── Annual household income ────────────────────────────────────────────────────
df <- df %>%
  mutate(income = case_when(
    housing_income_1_1 == "Less than £18,000"      ~ "Less_than_£18,000",
    housing_income_1_1 == "£18,000 to £30,999"     ~ "£18,000_to_£51,999",
    housing_income_1_1 == "£31,000 to £51,999"     ~ "£18,000_to_£51,999",
    housing_income_1_1 == "£52,000 to £100,000"    ~ "Greater_than_£51,999",
    housing_income_1_1 == "Greater than £100,000"  ~ "Greater_than_£51,999",
    TRUE ~ NA_character_
  ))
invisible(gc())

# ── Higher education ───────────────────────────────────────────────────────────
higher_ed_pattern <- "College or University degree|Other professional qualifications eg\\: nursing, teaching"
df$higher_ed <- ifelse(
  df$edu_qual_1_m == "Prefer not to answer", NA,
  grepl(higher_ed_pattern, df$edu_qual_1_m, fixed = FALSE)
)
invisible(gc())

# ── Contraception use (ever) ───────────────────────────────────────────────────
df <- df %>%
  mutate(
    contraception_use = case_when(
      gyn_contracept_implant_1_1 == "Yes" ~ "Yes",
      gyn_contracept_implant_1_1 == "No"  ~ "No",
      TRUE ~ NA_character_
    )
  )
invisible(gc())

# ── Geographic region ──────────────────────────────────────────────────────────
df <- df %>%
  mutate(
    geographic_region = case_when(
      is.na(region_at_reg) | region_at_reg == "" ~ NA_character_,
      TRUE ~ as.character(region_at_reg)
    )
  )

# ── Multiple deprivation index (quintiles) ────────────────────────────────────
df <- df %>%
  mutate(
    multiple_deprivation_index = as.numeric(na_if(as.character(multiple_deprivation_index), "")),
    multiple_deprivation = factor(
      ntile(multiple_deprivation_index, 5),
      levels = 1:5, labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
    )
  )

# ── Contraceptive method (ever used) ──────────────────────────────────────────
include_CHC_patterns <- "Combined Pill|Patch|Vaginal ring"
include_POC_patterns <- "Injection|IUS \\(Hormonal coil\\)|Progesterone only pill \\(mini pill\\)"
df$hormonal_cont_never <- df$gyn_contracept_implant_1_1 == "No"
df <- df %>%
  mutate(
    CHC_only    = grepl(include_CHC_patterns, gyn_contracept_methods_1_m) & !grepl(include_POC_patterns, gyn_contracept_methods_1_m),
    POC_only    = grepl(include_POC_patterns, gyn_contracept_methods_1_m) & !grepl(include_CHC_patterns, gyn_contracept_methods_1_m),
    CHC_and_POC = grepl(include_CHC_patterns, gyn_contracept_methods_1_m) &  grepl(include_POC_patterns, gyn_contracept_methods_1_m),
    contr_method = case_when(
      hormonal_cont_never ~ "Never",
      CHC_only            ~ "CHC",
      POC_only            ~ "POC",
      CHC_and_POC         ~ "CHC_&_POC",
      TRUE                ~ NA_character_
    )
  )
invisible(gc())

# ── Regular contraceptive method (current use) ────────────────────────────────
CHC_patterns <- "Combined pill|Patch|Vaginal ring"
POC_patterns <- "Injection|Implant|IUS \\(Hormonal coil\\)|Progesterone only pill \\(mini pill\\)"

df <- df %>%
  mutate(
    No_regular_contr_use = case_when(
      is.na(medicat_1_m) | medicat_1_m %in% c("", "Do not know", "Prefer not to answer") ~ NA,
      grepl("Contraceptive medication, coil, implant or patch", medicat_repro_1_m) ~ FALSE,
      TRUE ~ TRUE
    )
  )

df$reg_CHC_only   <- grepl(CHC_patterns, df$medicat_repro_contracept_1_m) & !grepl(POC_patterns, df$medicat_repro_contracept_1_m)
df$reg_POC_only   <- grepl(POC_patterns, df$medicat_repro_contracept_1_m) & !grepl(CHC_patterns, df$medicat_repro_contracept_1_m)
df$reg_CHC_and_POC <- grepl(CHC_patterns, df$medicat_repro_contracept_1_m) &  grepl(POC_patterns, df$medicat_repro_contracept_1_m)

df <- df %>%
  mutate(
    reg_contr_method = case_when(
      No_regular_contr_use == TRUE  ~ "None",
      reg_CHC_and_POC == TRUE       ~ "CHC_&_POC",
      reg_CHC_only == TRUE          ~ "CHC",
      reg_POC_only == TRUE          ~ "POC",
      TRUE                          ~ NA_character_
    )
  )
df$reg_contr_method <- factor(df$reg_contr_method, levels = c("CHC", "POC", "CHC_&_POC", "None"))
df$reg_contr_method <- relevel(df$reg_contr_method, ref = "None")
invisible(gc())

# ── Psychotropic medication use ────────────────────────────────────────────────
df <- df %>%
  mutate(
    psychotropic_med = case_when(
      grepl("Mental health conditions or insomnia \\(e\\.g\\. depression, bipolar disorder\\)",
            medicat_1_m, fixed = FALSE) ~ "Yes",
      medicat_1_m %in% c("Do not know", "Prefer not to answer", "") | is.na(medicat_1_m) ~ NA_character_,
      TRUE ~ "No"
    )
  )
invisible(gc())

# ── Multimorbidity score ───────────────────────────────────────────────────────
# Counts valid diagnoses, excluding: mental health, pregnancy complications,
# "Other not listed", "Do not know", "Prefer not to answer".
# "None of the above" → 0; only-excluded selections → 0; ambiguous → NA.
df$choices_list <- strsplit(df$diag_2_m, "\\|")

df$multimorbidity_score <- sapply(df$choices_list, function(x) {
  x <- trimws(x)
  if ("None of the above" %in% x) return(0)
  if ("Do not know" %in% x)       return(NA_integer_)
  if ("Prefer not to answer" %in% x) return(NA_integer_)

  mental_health          <- grepl("Mental health conditions", x, ignore.case = TRUE)
  pregnancy_complications <- grepl("Complications or difficulties in pregnancy", x, ignore.case = TRUE)
  other_not_listed        <- x %in% c("Other not listed")

  if (any(other_not_listed) & any(mental_health | pregnancy_complications)) return(NA_integer_)
  if (all(mental_health | pregnancy_complications) & any(mental_health | pregnancy_complications)) return(0)
  if (all(other_not_listed)) return(NA_integer_)

  valid <- x[!mental_health & !pregnancy_complications & !other_not_listed]
  if (length(valid) == 0) NA_integer_ else length(valid)
})
invisible(gc())

# ── Composite sleep score ──────────────────────────────────────────────────────
sleep_Qs <- c("sleep_hrs_1_1", "sleep_trouble_1_1", "sleep_chronotype_1_1",
              "sleep_snoring_1_1", "sleep_daytime_1_1")
obj <- df[, c(sleep_Qs, "population")]

sleep_A <- ifelse(obj$sleep_hrs_1_1 %in% c(7, 8), 1, 0)
sleep_B <- ifelse(obj$sleep_trouble_1_1 == "Never/rarely", 1, 0)
sleep_C <- ifelse(obj$sleep_chronotype_1_1 %in% c("Definitely a 'morning' person", "More a 'morning' than 'evening' person"), 1, 0)
sleep_D <- ifelse(obj$sleep_snoring_1_1 == "No", 1, 0)
sleep_E <- ifelse(obj$sleep_daytime_1_1 == "Never/rarely", 1, 0)

df$composite_sleep_score <- rowSums(data.frame(sleep_A, sleep_B, sleep_C, sleep_D, sleep_E), na.rm = TRUE)
all_missing <- rowSums(df[, sleep_Qs] == "" | is.na(df[, sleep_Qs])) == length(sleep_Qs)
df$composite_sleep_score[all_missing] <- NA
invisible(gc())

# ── Composite activity score ───────────────────────────────────────────────────
df$activity_mod_mins_per_week <- as.numeric(df$activity_mod_days_2_1) * as.numeric(df$activity_mod_mins_2_1)
df$activity_vig_mins_per_week <- as.numeric(df$activity_vig_days_2_1) * as.numeric(df$activity_vig_mins_2_1)

has_mod_data <- !is.na(df$activity_mod_mins_per_week)
has_vig_data <- !is.na(df$activity_vig_mins_per_week)

A_mod <- ifelse(has_mod_data & df$activity_mod_mins_per_week >= 150, 1, 0)
B_vig <- ifelse(has_vig_data & df$activity_vig_mins_per_week >= 75,  1, 0)
combined_mins <- ifelse(has_mod_data & has_vig_data,
                        df$activity_mod_mins_per_week + df$activity_vig_mins_per_week,
                 ifelse(has_mod_data, df$activity_mod_mins_per_week,
                 ifelse(has_vig_data, df$activity_vig_mins_per_week, NA)))
C_combined <- ifelse(!is.na(combined_mins) & combined_mins >= 110, 1, 0)

df$composite_activity_score <- case_when(
  has_mod_data & has_vig_data  ~ ifelse(A_mod == 0 & B_vig == 0, C_combined, A_mod + B_vig),
  has_mod_data & !has_vig_data ~ pmax(A_mod, C_combined),
  !has_mod_data & has_vig_data ~ pmax(B_vig, C_combined),
  TRUE ~ NA_real_
)
invisible(gc())

# ── Physical activity category ─────────────────────────────────────────────────
df <- df %>%
  mutate(
    physical_activity = case_when(
      str_detect(activity_type_1_m, "Prefer not to answer") ~ NA_character_,
      str_detect(activity_type_1_m, "None of the above")    ~ "No_physical_activity",
      str_detect(activity_type_1_m, "Strenuous sports")     ~ "High_level_activity",
      str_detect(activity_type_1_m, "Light DIY|Walking for pleasure|Other exercises|Heavy DIY") ~ "Low_or_Medium_level_activity",
      TRUE ~ NA_character_
    )
  )

# ── Menstrual cycle length ─────────────────────────────────────────────────────
df <- df %>%
  mutate(
    menstrual_cycle_length = case_when(
      gyn_menstr_cycle_days_1_1 != "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_1_1)),
      gyn_menstr_cycle_days_1_1 == "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_2_1))
    ),
    menstrual_cycle_length_premenopause = case_when(
      gyn_menopause_1_1 == "Yes" | gyn_menopause_2_1 == "Yes" ~ NA_real_,
      gyn_menstr_cycle_days_1_1 != "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_1_1)),
      gyn_menstr_cycle_days_1_1 == "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_2_1))
    ),
    cycle_length = case_when(
      gyn_menstr_cycle_days_1_1 != "" ~ gyn_menstr_cycle_days_1_1,
      gyn_menstr_cycle_days_1_1 == "" ~ gyn_menstr_cycle_days_2_1
    ),
    reg_menses = case_when(
      cycle_length == "Irregular cycle"                              ~ 1,
      cycle_length %in% c("Do not know", "Prefer not to answer", "") ~ NA_real_,
      is.na(cycle_length)                                            ~ NA_real_,
      TRUE                                                           ~ 0
    ),
    cycle_length_premenopause = case_when(
      gyn_menopause_1_1 == "Yes" | gyn_menopause_2_1 == "Yes" ~ NA_character_,
      gyn_menstr_cycle_days_1_1 != "" ~ gyn_menstr_cycle_days_1_1,
      gyn_menstr_cycle_days_1_1 == "" ~ gyn_menstr_cycle_days_2_1
    ),
    reg_menses_premenopause = case_when(
      gyn_menopause_1_1 == "Yes" | gyn_menopause_2_1 == "Yes"         ~ NA_real_,
      cycle_length_premenopause == "Irregular cycle"                   ~ 1,
      cycle_length_premenopause %in% c("Do not know", "Prefer not to answer", "") ~ NA_real_,
      is.na(cycle_length_premenopause)                                 ~ NA_real_,
      TRUE                                                             ~ 0
    )
  )
invisible(gc())

# ── Supplement use ─────────────────────────────────────────────────────────────
na_values <- c("Do not know", "Prefer not to answer", "")
df$vitamin_d <- ifelse(df$medicat_suppl_1_m %in% na_values, NA,
               ifelse(grepl("Vitamin D", df$medicat_suppl_1_m, ignore.case = TRUE), 1, 0))
df$vitamin_b <- ifelse(df$medicat_suppl_1_m %in% na_values, NA,
               ifelse(grepl("Vitamin B", df$medicat_suppl_1_m, ignore.case = TRUE), 1, 0))
df$calcium   <- ifelse(df$medicat_suppl_1_m %in% na_values, NA,
               ifelse(grepl("Calcium",   df$medicat_suppl_1_m, ignore.case = TRUE), 1, 0))

# ── PHQ-9 and GAD-7 total scores ───────────────────────────────────────────────
phq_items <- c("phq9_item1_interest_1_1", "phq9_item2_down_1_1", "phq9_item3_sleep_1_1",
               "phq9_item4_energy_1_1",   "phq9_item5_appetite_1_1", "phq9_item6_bad_1_1",
               "phq9_item7_concentr_1_1", "phq9_item8_movement_1_1", "phq9_item9_harm_1_1")
gad_items  <- c("gad7_item1_anx_1_1", "gad7_item2_worry_control_1_1", "gad7_item3_worry_amount_1_1",
               "gad7_item4_relax_1_1", "gad7_item5_restless_1_1", "gad7_item6_annoyed_1_1",
               "gad7_item7_afraid_1_1")

setDT(df)
recode_lookup <- c("Not at all" = 0, "Several days" = 1,
                   "More than half the days" = 2, "Nearly every day" = 3,
                   "Do not know" = NA, "Prefer not to answer" = NA)

for (col in phq_items) df[, (col) := recode_lookup[as.character(get(col))]]
df[, total_phq9 := rowSums(.SD, na.rm = FALSE), .SDcols = phq_items]

for (col in gad_items) df[, (col) := recode_lookup[as.character(get(col))]]
df[, total_gad7 := rowSums(.SD, na.rm = FALSE), .SDcols = gad_items]

df$sum_phq9_gad7 <- ifelse(
  is.na(df$total_phq9) | is.na(df$total_gad7), NA,
  df$total_phq9 + df$total_gad7
)

setDF(df)
invisible(gc())

# ── Multimorbidity binary ──────────────────────────────────────────────────────
df$multimorbidity_binary <- ifelse(
  is.na(df$multimorbidity_score), NA_integer_,
  ifelse(df$multimorbidity_score > 1, 1L, 0L)
)

# ── Save master dataframe ──────────────────────────────────────────────────────
saveRDS(df, "master_df.rds")
cat("master_df.rds saved. Rows:", nrow(df), "\n")
system("dx upload master_df.rds --destination /Outputs/", intern = TRUE)
