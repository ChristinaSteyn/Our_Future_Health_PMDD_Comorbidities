#Estimate the prevalence of each comorbidity per group
#Import OFH data
system("dx download OFH_exported_files/OFH_questionnaire.csv", intern=TRUE)

system("dx download OFH_exported_files/OFH_participant.csv", intern=TRUE)

system("dx download OFH_exported_files/OFH_participant_geographies.csv", intern=TRUE)

system("dx download OFH_exported_files/lsoa.csv", intern=TRUE)

system("dx download Multiple_deprivation_score_data/Deprivation_index.csv", intern=TRUE)

# Setup and Data Loading ----
# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(ggplot2)
  library(stringr)
  library(epiR)
  library(data.table)
  library(lubridate)
  library(rlang)

})

#Read questionnaire into R
df <- read.table("OFH_questionnaire.csv", sep=",", header=TRUE, quote = "\"")

#Read participant data into R
participant <- read.table("OFH_participant.csv", sep=",", header=TRUE, quote = "\"")

#Read participant geographies into R
geographies <- read.table("OFH_participant_geographies.csv", sep=",", header=TRUE, quote = "\"")

#Read participant LSOA data into R
LSOA <- read.table("lsoa.csv", sep=",", header=TRUE, quote = "\"")

#Read in multiple deprivation score file
index <- read.table("Deprivation_index.csv", sep=",", header=TRUE, quote = "\"")

nrow(df)
nrow(participant)
nrow(geographies)
nrow(LSOA)
nrow(index)

#There is a missing pid in df so remove this id from participant
#For some reason this didn't work so needed to use head function
missing_pids <- setdiff(df$pid, participant$pid)

missing_pids <- setdiff(head(df$pid, nrow(df)), head(participant$pid, nrow(df)))
participant <- participant[!participant$pid %in% missing_pids, ]
df <- df[!df$pid %in% missing_pids, ]

nrow(df)
nrow(participant)

head(df)
head(geographies)

#merge df and geographies
df <- full_join(df, geographies, by = "pid")
df <- full_join(df, LSOA, by = "pid")

head(df)
nrow(df)

#Map LSOA data to multiple deprivation index scores
head(index)
index <- index[ , c(2, 6)]
colnames(index) <- c("lsoa_at_reg", "multiple_deprivation_index")
df <- full_join(df, index, by = "lsoa_at_reg")
head(df)

#Create Master data frame
#Filter df to only include variables of interest to reduce memory

#Variables:
#-pid
#-population
#-submission_date
#-demog_weight_1_1
#-demog_height_1_1
#-smoke_tobacco_type_1_m
#-alcohol_curr_1_1
#-lifestyle_social_visits_1_1
#-father_diag_a_2_m
#-mother_diag_a_2_m
#-housing_income_1_1
#-region_at_reg
#-edu_qual_1_m
#-gyn_contracept_implant_1_1
#-gyn_contracept_methods_1_m
#-medicat_repro_1_m
#-medicat_repro_contracept_1_m
#-medicat_psych_1_m
#-diag_2_m
#-sleep_hrs_1_1
#-sleep_trouble_1_1
#-sleep_chronotype_1_1
#-sleep_snoring_1_1
#-sleep_daytime_1_1
#-activity_mod_days_2_1
#-activity_mod_mins_2_1
#-activity_vig_days_2_1
#-activity_vig_mins_2_1
#-gyn_menstr_cycle_days_1_1 
#-gyn_menstr_cycle_days_2_1 
#-gyn_menopause_1_1
#-gyn_menopause_2_1 
#-phq-9 items
#gad-7 items

vars_to_keep <- c(
  "pid", "submission_date", "demog_weight_1_1", "demog_height_1_1",
  "smoke_tobacco_type_1_m", "alcohol_curr_1_1", "lifestyle_social_visits_1_1",
  "father_diag_a_2_m", "mother_diag_a_2_m", "housing_income_1_1",
  "edu_qual_1_m", "gyn_contracept_implant_1_1", "gyn_contracept_methods_1_m", "medicat_1_m",
     "medicat_repro_1_m", "medicat_repro_contracept_1_m",
  "diag_2_m", "diag_cvd_1_m", "children_birthed_num_1_1", "diag_endocr_1_m", "diag_psych_1_m", "diag_neuro_dev_1_m", "diag_repro_1_m", "diag_auto_1_m",
    "diag_urol_1_m", "diag_resp_1_m", "diag_gastro_1_m", "diag_neuro_1_m", "diag_psych_depr_1_m", "sleep_hrs_1_1", "sleep_trouble_1_1", "sleep_chronotype_1_1",
  "sleep_snoring_1_1", "sleep_daytime_1_1", "activity_mod_days_2_1",
  "activity_mod_mins_2_1", "activity_vig_days_2_1", "activity_vig_mins_2_1", "activity_type_1_m",
  "gyn_menstr_cycle_days_1_1", "gyn_menstr_cycle_days_2_1",
  "gyn_menopause_1_1", "gyn_menopause_2_1", "phq9_item1_interest_1_1", "phq9_item2_down_1_1", "phq9_item3_sleep_1_1", "phq9_item4_energy_1_1", 
               "phq9_item5_appetite_1_1", "phq9_item6_bad_1_1", "phq9_item7_concentr_1_1", "phq9_item8_movement_1_1",
               "phq9_item9_harm_1_1", "gad7_item1_anx_1_1", "gad7_item2_worry_control_1_1", "gad7_item3_worry_amount_1_1", 
"gad7_item4_relax_1_1", "gad7_item5_restless_1_1", "gad7_item6_annoyed_1_1", "gad7_item7_afraid_1_1", "medicat_suppl_1_m", "region_at_reg", "multiple_deprivation_index",
  "medicat_1_m", "medicat_psych_1_m")


df <- df %>%
  select(all_of(vars_to_keep))

invisible(gc())


#Filter OFH participant file to only include females 
OFH_female <- participant[participant$demog_sex_1_1 %in% "Female" | participant$demog_sex_2_1 %in% "Female", ]
df_female <- df[df$pid %in% OFH_female$pid, ]
#Exclude individuals who didn't answer the DIAG_2_M questionnaire
#Don't exclude "Other not listed" because these individuals form part of the total cohort (may or may not have psychiatric illness)
df_female <- df_female[!df_female$diag_2_m %in% c("", "Do not know", "Prefer not to answer"), ]
#Total is the total number of females who answered the DIAG_2_M questionnaire
fT <- nrow(df_female)
invisible(gc())

#Import PMDD patient IDs (obtained from Cohort Browser - converted dataset using Table Exporter in tools)
system("dx download OFH_exported_files/PMDD.csv", intern=TRUE)
#Import Female control IDs
system("dx download OFH_exported_files/CTRL.csv", intern=TRUE)
#Import Female MDD IDs
system("dx download OFH_exported_files/MDD.csv", intern=TRUE)

invisible(gc())

#Import PMDD patient IDs into R
fem_PMDD_ID <- read.table("PMDD.csv", sep=",", header=TRUE, quote = "\"")
#Import Female control IDs into R
fem_CTRL_ID <- read.table("CTRL.csv", sep=",", header=TRUE, quote = "\"")
#Filter female control group to remove individuals who didn't answer the diag_2_m questionnaire
#i.e select individuals who are within the total female group 
fem_CTRL_ID <- fem_CTRL_ID[fem_CTRL_ID$pid %in% df_female$pid, ]
#Import Female MDD IDs into R
fem_MDD_ID <- read.table("MDD.csv", sep=",", header=TRUE, quote = "\"")


invisible(gc())

#Add population label
df$population <- ifelse(df$pid %in% fem_PMDD_ID$pid, "PMDD",
                        ifelse(df$pid %in% fem_CTRL_ID$pid, "CTRL",
                               ifelse(df$pid %in% fem_MDD_ID$pid, "MDD", NA)))

#Filter out NAs
df <- df[!is.na(df$population), ]

#Filter NAs for participant and ensure order of pids matches that in df
participant$population <- ifelse(participant$pid %in% fem_PMDD_ID$pid, "PMDD",
                        ifelse(participant$pid %in% fem_CTRL_ID$pid, "CTRL",
                               ifelse(participant$pid %in% fem_MDD_ID$pid, "MDD", NA)))

participant <- participant[!is.na(participant$population), ]
participant <- participant[match(df$pid, participant$pid), ]

nrow(df)
nrow(participant)
head(df$pid)
head(participant$pid)
tail(df$pid)
tail(participant$pid)


invisible(gc())
colnames(df)

invisible(gc())
#Age 

# Convert birth date into a proper Date object (assuming the first of the month)
participant$birth_date <- as.Date(paste0(participant$birth_year, "-", participant$birth_month, "-01"))
# Ensure submission_date is in Date format
df$submission_date <- as.Date(df$submission_date)
# Calculate age using lubridate's interval function for efficiency
df$age <- time_length(interval(participant$birth_date, df$submission_date), "years")

invisible(gc())


#BMI
df$bmi <- df$demog_weight_1_1/(df$demog_height_1_1/100)^2
invisible(gc())

#Ethnicity
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
      
      demog_ethnicity_1_1 %in% c("Arab")   ~ "Arab",
      demog_ethnicity_1_1 %in% c("Other")  ~ "Other",
      
      TRUE ~ NA_character_
    )
  )
invisible(gc())

#Smoking status (ever smoked)
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

#Alcohol use
df <- df %>%
  mutate(alcohol_use = case_when(
    alcohol_curr_1_1 %in% c("Daily or almost daily") ~ "Daily_or_almost_daily",
    alcohol_curr_1_1 %in% c("Three or four times a week", "Once or twice a week") ~ "One_to_four_times_a_week",
    alcohol_curr_1_1 %in% c("One to three times a month", "Special occasions only", "Never") ~ "Less_than_once_a_week",
    alcohol_curr_1_1 == "Prefer not to answer" ~ NA_character_,
    TRUE ~ NA_character_
  ))
invisible(gc())

#Social interaction
df <- df %>%
  mutate(social_interaction = case_when(
    lifestyle_social_visits_1_1 == "Almost daily" ~ "Almost_daily",
    lifestyle_social_visits_1_1 %in% c("2-4 times a week", "About once a week") ~ "One_to_four_times_a_week",
    lifestyle_social_visits_1_1 %in% c("About once a month", "Once every few month", "Never or almost never") ~ "Less_than_once_a_week",
    lifestyle_social_visits_1_1 %in% c("No friends/family outside household", "Do not know", "Prefer not to answer") ~ NA_character_,
    TRUE ~ NA_character_  # For any unexpected values
  ))

#Parental history mental illness
#Rules:

#Either parent = 1 → result = 1
#Both parents = 0 → result = 0
#Both parents missing → result = NA
#One parent = 0, other missing → result = NA

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
      # If either parent has 1, set to 1
      pat_hist_mental == 1L | mat_hist_mental == 1L ~ 1L,
      # If both parents have 0, set to 0
      pat_hist_mental == 0L & mat_hist_mental == 0L ~ 0L,
      # If both parents are missing, set to NA
      is.na(pat_hist_mental) & is.na(mat_hist_mental) ~ NA_integer_,
      # If one parent is 0 and the other is missing, set to NA
      (pat_hist_mental == 0L & is.na(mat_hist_mental)) |
      (is.na(pat_hist_mental) & mat_hist_mental == 0L) ~ NA_integer_,
      # This should catch any remaining cases
      TRUE ~ NA_integer_
    )
  )

invisible(gc())

#Annual Household income
df <- df %>%
  mutate(income = case_when(
    housing_income_1_1 == "Less than £18,000" ~ "Less_than_£18,000",
    housing_income_1_1 == "£18,000 to £30,999" ~ "£18,000_to_£51,999",
    housing_income_1_1 == "£31,000 to £51,999" ~ "£18,000_to_£51,999",
    housing_income_1_1 == "£52,000 to £100,000" ~ "Greater_than_£51,999",
    housing_income_1_1 == "Greater than £100,000" ~ "Greater_than_£51,999",
    TRUE ~ NA_character_  # Ensuring NA is returned for any unexpected values
  ))

invisible(gc())

#Higher education
# Identify those who have a college or university degree using grepl
higher_ed <- "College or University degree|Other professional qualifications eg\\: nursing, teaching"

df$higher_ed <- ifelse(df$edu_qual_1_m == "Prefer not to answer", 
                       NA, 
                       grepl(higher_ed, df$edu_qual_1_m, fixed = FALSE))
invisible(gc())

#Contraception use
df <- df %>%
  mutate(
    contraception_use = case_when(
      gyn_contracept_implant_1_1 == "Yes" ~ "Yes",
      gyn_contracept_implant_1_1 == "No"  ~ "No",
      TRUE ~ NA_character_
    )
  )

invisible(gc())

#Geography
df <- df %>%
  mutate(
    geographic_region = case_when(
      is.na(region_at_reg) ~ NA_character_,
      region_at_reg == "" ~ NA_character_,
      TRUE ~ as.character(region_at_reg)
    )
  )

#Multiple deprivation index
df <- df %>%
  mutate(
    multiple_deprivation = case_when(
      is.na(multiple_deprivation_index) ~ NA_real_,
      multiple_deprivation_index == "" ~ NA_real_,
      TRUE ~ as.numeric(multiple_deprivation_index)
    )
  )

#Contraceptive method
# Define individuals who have never used hormonal contraception
df$hormonal_cont_never <- df$gyn_contracept_implant_1_1 == "No"

# Define patterns
include_CHC_patterns <- "Combined Pill|Patch|Vaginal ring"
include_POC_patterns <- "Injection|IUS \\(Hormonal coil\\)|Progesterone only pill \\(mini pill\\)"  # escape parentheses
# Create Combined Hormonal Contraception (CHC) only / POC only / Both
df <- df %>%
  mutate(
    CHC_only = grepl(include_CHC_patterns, gyn_contracept_methods_1_m) & !grepl(include_POC_patterns, gyn_contracept_methods_1_m),
    POC_only = grepl(include_POC_patterns, gyn_contracept_methods_1_m) & !grepl(include_CHC_patterns, gyn_contracept_methods_1_m),
    CHC_and_POC = grepl(include_CHC_patterns, gyn_contracept_methods_1_m) & grepl(include_POC_patterns, gyn_contracept_methods_1_m),
    contr_method = case_when(
      hormonal_cont_never ~ "Never",
      CHC_only ~ "CHC",
      POC_only ~ "POC",
      CHC_and_POC ~ "CHC_&_POC",
      TRUE ~ NA_character_
    )
  )

invisible(gc())

#Regular contraceptive method
#Identify individuals who do not regularly use hormonal contraception
df <- df %>%
  mutate(
    No_regular_contr_use = case_when(
      # First check medicat_1_m
      is.na(medicat_1_m) | medicat_1_m == "" | medicat_1_m == "Do not know" | medicat_1_m == "Prefer not to answer" ~ NA,
      
      # Then check medicat_repro_1_m  
    grepl("Contraceptive medication, coil, implant or patch", medicat_repro_1_m) ~ FALSE,
      
      # Default for all other cases
      TRUE ~ TRUE
    )
  )

# Define inclusion and exclusion patterns for regular CHC only use
CHC_patterns <- "Combined pill|Patch|Vaginal ring"  # Must contain any of these
POC_patterns <- "Injection|Implant|IUS \\(Hormonal coil\\)|Progesterone only pill \\(mini pill\\)"  # Must NOT contain any of these

# Apply filtering
df$reg_CHC_only <- grepl(CHC_patterns, df$medicat_repro_contracept_1_m) & !grepl(POC_patterns, df$medicat_repro_contracept_1_m)

# Apply filtering
df$reg_POC_only <- grepl(POC_patterns, df$medicat_repro_contracept_1_m) & !grepl(CHC_patterns, df$medicat_repro_contracept_1_m)

# Define inclusion and exclusion patterns for CHC and POC use
df$reg_CHC_and_POC <- grepl(CHC_patterns, df$medicat_repro_contracept_1_m) & grepl(POC_patterns, df$medicat_repro_contracept_1_m)

# Create new column which defines CHC only, POC only, CHC and POC, None and gives NA to all else
df <- df %>%
  mutate(
    reg_contr_method = case_when(
      No_regular_contr_use == TRUE ~ "None",
      reg_CHC_and_POC == TRUE ~ "CHC_&_POC",
      reg_CHC_only == TRUE ~ "CHC",
      reg_POC_only == TRUE ~ "POC", 
      TRUE ~ NA_character_
    )
  )

df$reg_contr_method <- as.factor(df$reg_contr_method)   
df$reg_contr_method <- factor(df$reg_contr_method, levels = c("CHC", "POC", "CHC_&_POC", "None"))
df$reg_contr_method <- relevel(df$reg_contr_method, ref = "None")

invisible(gc())


#Psychotropic medication use 

df <- df %>%
  mutate(
    psychotropic_med = case_when(
      grepl("Mental health conditions or insomnia \\(e\\.g\\. depression, bipolar disorder\\)", 
            medicat_1_m, fixed = FALSE) ~ "Yes",
      medicat_1_m %in% c("Do not know", "Prefer not to answer", "") ~ NA_character_,
      is.na(medicat_1_m) ~ NA_character_,
      TRUE ~ "No"
    )
  )

invisible(gc())


#Multimorbidity score
# Select necessary columns

#People who choose None of the above are given zero
#People who only chose Mental health or pregnancy complications alone are given zero
#People who chose other not listed alone or other not listed with mental health and/or pregancy complications are given NA
#People who chose Do not know or Prefer not to answer are given NA
#The number of multimorbidities are counted for everyone else excluding Mental health/Pregnancy complications/Other not listed/Prefer not to answer/Do not know

# Split multiple diagnoses into lists 
df$choices_list <- strsplit(df$diag_2_m, "\\|")

df$multimorbidity_score <- sapply(df$choices_list, function(x) {
  x <- trimws(x)  # clean whitespace
  
  # Handle exclusive selections first
  if ("None of the above" %in% x) {
    return(0)
  }
  
  if ("Do not know" %in% x) {
    return(NA_integer_)
  }
  
  if ("Prefer not to answer" %in% x) {
    return(NA_integer_)
  }
  
  # Define exclusion categories
  mental_health <- grepl("Mental health conditions", x, ignore.case = TRUE)
  pregnancy_complications <- grepl("Complications or difficulties in pregnancy", x, ignore.case = TRUE)
  other_not_listed <- x %in% c("Other not listed")
    
  # If "Other not listed" is combined with mental health and/or pregnancy complications, give NA
  if (any(other_not_listed) & any(mental_health | pregnancy_complications)) {
    return(NA_integer_)
  }
  
  # If they ONLY selected mental health and/or pregnancy complications 
  # (and possibly blanks), give them 0
  if (all(mental_health | pregnancy_complications) & 
      any(mental_health | pregnancy_complications)) {
    return(0)
  }
  
  # If they ONLY selected "Other not listed" or blanks, give them NA
  if (all(other_not_listed)) {
    return(NA_integer_)
  }
  
  # Otherwise, filter out excluded items and count valid diagnoses
  valid <- x[!mental_health &
            !pregnancy_complications &
            !other_not_listed]
    
  if (length(valid) == 0) {
    return(NA_integer_)
  } else {
    return(length(valid))
  }
})

invisible(gc())

#Composite sleep score
sleep_Qs <- c(
  "sleep_hrs_1_1", "sleep_trouble_1_1", 
  "sleep_chronotype_1_1", "sleep_snoring_1_1", 
  "sleep_daytime_1_1"
)

obj <- df[, c(sleep_Qs, "population")]

# Give points for healthy sleep
sleep_A <- ifelse(obj$sleep_hrs_1_1 %in% c(7, 8), 1, 0)
sleep_B <- ifelse(obj$sleep_trouble_1_1 == "Never/rarely", 1, 0)
sleep_C <- ifelse(obj$sleep_chronotype_1_1 %in% c("Definitely a 'morning' person", "More a 'morning' than 'evening' person"), 1, 0)
sleep_D <- ifelse(obj$sleep_snoring_1_1 == "No", 1, 0)
sleep_E <- ifelse(obj$sleep_daytime_1_1 == "Never/rarely", 1, 0)

# Combine into a data frame and calculate total score
composite_sleep <- data.frame(sleep_A, sleep_B, sleep_C, sleep_D, sleep_E)

df$composite_sleep_score <- rowSums(composite_sleep, na.rm = TRUE)

# Identify rows where ALL sleep questions were unanswered ("", NA)
all_missing <- rowSums(df[, sleep_Qs] == "" | is.na(df[, sleep_Qs])) == length(sleep_Qs)

# Assign NA sleep_score to those individuals
df$composite_sleep_score[all_missing] <- NA
invisible(gc())

#Composite activty level score
df$activity_mod_mins_per_week <- as.numeric(df$activity_mod_days_2_1) * as.numeric(df$activity_mod_mins_2_1)
df$activity_vig_mins_per_week <- as.numeric(df$activity_vig_days_2_1) * as.numeric(df$activity_vig_mins_2_1)

# Create indicators for data availability
has_mod_data <- !is.na(df$activity_mod_mins_per_week)
has_vig_data <- !is.na(df$activity_vig_mins_per_week)

# Calculate individual components (treating missing as 0 for available data)
A_mod <- ifelse(has_mod_data & df$activity_mod_mins_per_week >= 150, 1, 0)
B_vig <- ifelse(has_vig_data & df$activity_vig_mins_per_week >= 75, 1, 0)

# For combined score, use available data only
combined_mins <- ifelse(has_mod_data & has_vig_data, 
                       df$activity_mod_mins_per_week + df$activity_vig_mins_per_week,
                       ifelse(has_mod_data, df$activity_mod_mins_per_week,
                             ifelse(has_vig_data, df$activity_vig_mins_per_week, NA)))

C_combined <- ifelse(!is.na(combined_mins) & combined_mins >= 110, 1, 0)

# Create composite score with flexible logic
df$composite_activity_score <- case_when(
  # If we have both moderate and vigorous data
  has_mod_data & has_vig_data ~ ifelse(A_mod == 0 & B_vig == 0, C_combined, A_mod + B_vig),
  
  # If we only have moderate data
  has_mod_data & !has_vig_data ~ pmax(A_mod, C_combined),
  
  # If we only have vigorous data  
  !has_mod_data & has_vig_data ~ pmax(B_vig, C_combined),
  
  # If we have no activity data
  TRUE ~ NA_real_
)

invisible(gc())

#Activity levels (retain more data)
# Create categorical activity level variable
df <- df %>%
  mutate(
    physical_activity = case_when(
      # Prefer not to answer overrides everything
      str_detect(activity_type_1_m, "Prefer not to answer") ~ NA_character_,

      # None of the above overrides all except Prefer not to answer
      str_detect(activity_type_1_m, "None of the above") ~ "No_physical_activity",

      # Strenuous sports overrides all except the above
      str_detect(activity_type_1_m, "Strenuous sports") ~ "High_level_activity",

      # All other categories fall into Low/Medium
      str_detect(
        activity_type_1_m,
        "Light DIY|Walking for pleasure|Other exercises|Heavy DIY"
      ) ~ "Low_or_Medium_level_activity",

      # If nothing matches, return NA
      TRUE ~ NA_character_
    )
  )

      
# Menstrual cycle length (numeric; use Q1 if available, otherwise Q2; non-numeric -> NA)
df <- df %>%
  mutate(
    menstrual_cycle_length = case_when(
      gyn_menstr_cycle_days_1_1 != "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_1_1)),
      gyn_menstr_cycle_days_1_1 == "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_2_1))
    )
  )

# Menstrual cycle length (premenopausal only, numeric)
df <- df %>%
  mutate(
    menstrual_cycle_length_premenopause = case_when(
      gyn_menopause_1_1 == "Yes" | gyn_menopause_2_1 == "Yes" ~ NA_real_,
      gyn_menstr_cycle_days_1_1 != "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_1_1)),
      gyn_menstr_cycle_days_1_1 == "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_2_1))
    )
  )

# Cycle length (raw responses, keep text like "Irregular cycle")
df <- df %>%
  mutate(
    cycle_length = case_when(
      gyn_menstr_cycle_days_1_1 != "" ~ gyn_menstr_cycle_days_1_1,
      gyn_menstr_cycle_days_1_1 == "" ~ gyn_menstr_cycle_days_2_1
    ),
    
    # Indicator for irregular menses (0/1/NA)
    reg_menses = case_when(
      cycle_length == "Irregular cycle" ~ 1,
      cycle_length %in% c("Do not know", "Prefer not to answer", "") ~ NA_real_,
      is.na(cycle_length) ~ NA_real_,
      TRUE ~ 0
    )
  )

# Cycle length (premenopausal women only; raw responses)
df <- df %>%
  mutate(
    cycle_length_premenopause = case_when(
      gyn_menopause_1_1 == "Yes" | gyn_menopause_2_1 == "Yes" ~ NA_character_,
      gyn_menstr_cycle_days_1_1 != "" ~ gyn_menstr_cycle_days_1_1,
      gyn_menstr_cycle_days_1_1 == "" ~ gyn_menstr_cycle_days_2_1
    ),
    
    # Indicator for irregular menses (premenopausal only, 0/1/NA)
    reg_menses_premenopause = case_when(
      gyn_menopause_1_1 == "Yes" | gyn_menopause_2_1 == "Yes" ~ NA_real_,
      cycle_length_premenopause == "Irregular cycle" ~ 1,
      cycle_length_premenopause %in% c("Do not know", "Prefer not to answer", "") ~ NA_real_,
      is.na(cycle_length_premenopause) ~ NA_real_,
      TRUE ~ 0
    )
  )

invisible(gc())

#Supplement use
# Create binary categorical variables for vitamin D, vitamin B, and calcium
# Based on medicat_suppl_1_m column

# Define the values that should be coded as NA
na_values <- c("Do not know", "Prefer not to answer", "")

# Create vitamin D binary variable
df$vitamin_d <- ifelse(df$medicat_suppl_1_m %in% na_values, NA,
                       ifelse(grepl("Vitamin D", df$medicat_suppl_1_m, ignore.case = TRUE), 1, 0))

# Create vitamin B binary variable  
df$vitamin_b <- ifelse(df$medicat_suppl_1_m %in% na_values, NA,
                       ifelse(grepl("Vitamin B", df$medicat_suppl_1_m, ignore.case = TRUE), 1, 0))

# Create calcium binary variable
df$calcium <- ifelse(df$medicat_suppl_1_m %in% na_values, NA,
                     ifelse(grepl("Calcium", df$medicat_suppl_1_m, ignore.case = TRUE), 1, 0))


#Sum PHQ9 - GAD7 Score
phq_items <- c("phq9_item1_interest_1_1", "phq9_item2_down_1_1", "phq9_item3_sleep_1_1", "phq9_item4_energy_1_1", 
               "phq9_item5_appetite_1_1", "phq9_item6_bad_1_1", "phq9_item7_concentr_1_1", "phq9_item8_movement_1_1",
               "phq9_item9_harm_1_1")

gad_items <- c("gad7_item1_anx_1_1", "gad7_item2_worry_control_1_1", "gad7_item3_worry_amount_1_1", 
"gad7_item4_relax_1_1", "gad7_item5_restless_1_1", "gad7_item6_annoyed_1_1", "gad7_item7_afraid_1_1")


# Convert your data.frame to a data.table (this doesn't copy the object)
setDT(df)

# Define your recode map
recode_lookup <- c(
  "Not at all" = 0,
  "Several days" = 1,
  "More than half the days" = 2,
  "Nearly every day" = 3,
  "Do not know" = NA,
  "Prefer not to answer" = NA
)

# Recode in-place using data.table's efficient update by reference
for (col in phq_items) {
  df[ , (col) := recode_lookup[as.character(get(col))]]
}

# Calculate total score (in-place, still memory-safe)
df[ , total_phq9 := rowSums(.SD, na.rm = FALSE), .SDcols = phq_items]

# Recode in-place using data.table's efficient update by reference
for (col in gad_items) {
  df[ , (col) := recode_lookup[as.character(get(col))]]
}

# Calculate total score (in-place, still memory-safe)
df[ , total_gad7 := rowSums(.SD, na.rm = FALSE), .SDcols = gad_items]

df$sum_phq9_gad7 <- ifelse(
  is.na(df$total_phq9) | is.na(df$total_gad7) | df$total_phq9 == "" | df$total_gad7 == "",
  NA,
  df$total_phq9 + df$total_gad7
)

invisible(gc())
#Do not filter population by age 
hist(df$age)

# Define function to calculate prevalence, SE, CI

prevalence_se_ci <- function(population_df, category, subcategory) {
  cases <- sum(grepl(subcategory, population_df[[category]], fixed = TRUE))
  
  # Total population:
#Remove individuals who didn't answer DIAG_2_M or answered DIAG_2_M with "Do not know" or "Prefer not to answer"
  #Remove individuals in each category e.g diag_endocr_1_m and diag_auto_1_m who answered 
    #"Prefer not not answer" or "Do not know" but not those
#who left these subcategory questionnaires blank because they should be counted as "No" not removed
exclusion_vals <- c("Prefer not to answer", "Do not know")

pop_tot <- population_df %>%
  filter(
    !(
      diag_2_m %in% c("", exclusion_vals) |
      (!!sym(category) %in% exclusion_vals)
    )
  ) %>%
  nrow()

    
  prev <- cases / pop_tot
  se <- sqrt((prev * (1 - prev)) / pop_tot)
  
  if (cases > 0 & pop_tot > 0) {
    ci <- prop.test(cases, pop_tot, conf.level = 0.95)$conf.int
    ci_95L <- ci[1]
    ci_95U <- ci[2]
  } else {
    ci_95L <- NA
    ci_95U <- NA
  }
  
  return(data.frame(
    cases = cases,
    N = pop_tot,
    prevalence = prev,
    SE = se,
    CI_95_L = ci_95L,
    CI_95_U = ci_95U
  ))
}

#Format dataframe

library(dplyr)
library(tidyr)

format_df <- function(results) {
 results <- results %>%
  mutate(
    prevalence = round(prevalence * 100, 2),
    ci_95 = paste0(round(CI_95_L * 100, 2), ", ", round(CI_95_U * 100, 2))
  ) %>%
  select(label, population, prevalence, ci_95, N) %>%
  pivot_wider(
    names_from = population,
    values_from = c(prevalence, ci_95, N),
    names_glue = "{population}_{.value}"
  ) %>%
  # reorder so each prevalence is followed by its CI
  select(
    label,
   PMDD_prevalence, PMDD_ci_95, PMDD_N,
      MDD_prevalence, MDD_ci_95, MDD_N,
      CTRL_prevalence, CTRL_ci_95, CTRL_N
  )
return(results)
}

# ----

# Broad category comorbidities 
comorbidity_list <- c(
  "Autoimmune disorder", "Blood disorders (Anaemia)", "Cancer", "Digestive system or liver problems",
  "Endocrine, nutritional and metabolic disorders (e.g. diabetes, thyroid disorder, vitamin deficiencies)", 
  "Eye or visual problems", "Fractures, breaks, or joint problems", "Heart or circulatory disease (e.g. high blood pressure or stroke)", 
  "Kidney or urinary system disorders", "Lung or respiratory problems", "Mental health conditions (e.g. depression, bipolar disorder)", 
  "Neurodevelopmental conditions (e.g. Autism spectrum disorder, ADHD)",  
  "Neurological disorders (things that affect that brain or nervous system. E.g., Epilepsy)", "Reproductive system problems"
)

comorbidity_label <- c(
  "Autoimmune_disorder", "Blood_disorders", "Cancer",
  "Digestive_system_liver_problems",
  "Endocrine_nutritional_metabolic_disorders", 
  "Eye_visual_problems", "Fractures_breaks_joint_problems", "Heart_circulatory_disease", 
  "Kidney_urinary_system_disorders", "Lung_respiratory_problems", "Mental_health_conditions", 
  "Neurodevelopmental_conditions",  "Neurological_disorders", "Reproductive_system_problems"
)

# ----

# Set up object with relevant columns
obj <- df %>%
  select(pid, diag_2_m, population)

# Apply function across populations and comorbidities
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map_dfr(comorbidity_list, function(subcat) {
      res <- prevalence_se_ci(pop, "diag_2_m", subcat)
      res$population <- unique(pop$population)
      res$subcategory <- subcat
      return(res)
    })
  })

# Map category names into clean labels
results <- results %>%
  mutate(
    label = case_when(
      subcategory == "Autoimmune disorder" ~ "Autoimmune_disorder",
      subcategory == "Blood disorders (Anaemia)" ~ "Blood_disorders",
      subcategory == "Cancer" ~ "Cancer",
      subcategory == "Digestive system or liver problems" ~ "Digestive_system_liver_problems",
      subcategory == "Endocrine, nutritional and metabolic disorders (e.g. diabetes, thyroid disorder, vitamin deficiencies)" ~ "Endocrine_nutritional_metabolic_disorders",
      subcategory == "Eye or visual problems" ~ "Eye_visual_problems",
      subcategory == "Fractures, breaks, or joint problems" ~ "Fractures_breaks_joint_problems",
      subcategory == "Heart or circulatory disease (e.g. high blood pressure or stroke)" ~ "Heart_circulatory_disease",
      subcategory == "Kidney or urinary system disorders" ~ "Kidney_urinary_system_disorders",
      subcategory == "Lung or respiratory problems" ~ "Lung_respiratory_problems",
      subcategory == "Mental health conditions (e.g. depression, bipolar disorder)" ~ "Mental_health_conditions",
      subcategory == "Neurodevelopmental conditions (e.g. Autism spectrum disorder, ADHD)" ~ "Neurodevelopmental_conditions",
      subcategory == "Neurological disorders (things that affect that brain or nervous system. E.g., Epilepsy)" ~ "Neurological_disorders",
      subcategory == "Reproductive system problems" ~ "Reproductive_system_problems",
      TRUE ~ NA_character_
    )
  )

results <- format_df(results)
write.csv(results, "Broad_category_comorbidities.csv")
system(paste0("dx upload Broad_category_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)
invisible(gc())

#Complications or difficulties in pregnancy or childbirth
rm(obj)

#Subset population to only include women who have ever been pregnant
# Set up object with relevant columns
obj <- df %>%
  select(pid, diag_2_m, population, children_birthed_num_1_1) 


obj <- obj[obj$children_birthed_num_1_1 > 0, ]

# Broad category comorbidities 
comorbidity_list <- c(
  "Complications or difficulties in pregnancy or childbirth"
)

comorbidity_label <- c(
  "Complications_in_pregnancy"
)

# ----

# Apply function across populations and comorbidities
comp_preg_summary <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map_dfr(comorbidity_list, function(subcat) {
      res <- prevalence_se_ci(pop, "diag_2_m", subcat)
      res$population <- unique(pop$population)
      res$subcategory <- subcat
      return(res)
    })
  })

comp_preg_summary$label <- comorbidity_label
comp_preg_summary <- format_df(comp_preg_summary)

write.csv(comp_preg_summary, "Complications_pregnancy_prevalence.csv")
system("dx upload Complications_pregnancy_prevalence.csv --destination /Outputs/Comorbidity_prevalence/", intern=TRUE)

#Note kernel kept crashing until I ran this without running the previous two cells
# Endocrine, nutritional and metabolic disorders

rm(obj)
rm(results)

comorbidity_list <- c(
  "Type 1 diabetes", "Type 2 diabetes",
  "Overactive thyroid", "Underactive thyroid", "Cushing syndrome", 
  "Lactose intolerance", "Vitamin A deficiency", "Thiamine deficiency",
  "Vitamin D deficiency"
)

comorbidity_label <- c(
  "Type_1_diabetes", "Type_2_diabetes",
  "Overactive_thyroid", "Underactive_thyroid", "Cushing_syndrome",
  "Lactose_intolerance", "Vitamin_A_deficiency", "Thiamine_deficiency",
  "Vitamin_D_deficiency"
)

# Select relevant columns
obj <- df[ , c("pid", "diag_endocr_1_m", "diag_2_m", "population")]

library(dplyr)
library(purrr)


# Apply function to each population and subcategory
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_endocr_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())  

results <- format_df(results)

write.csv(results, "Endocrine_metabolic_comorbidities.csv")
system(paste0("dx upload Endocrine_metabolic_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

# Mental Health conditions

comorbidity_list <- c(
  "Anxiety", "Bipolar disorder", "Body dysmorphia",
  "Depression", "Post Traumatic Stress Disorder", 
  "Obsessive Compulsive Disorder", "Eating disorder", 
  "Schizophrenia", "Personality disorder"
)

comorbidity_label <- c(
  "Anxiety", "Bipolar_disorder", "Body_dysmorphia",
  "Depression", "Post_Traumatic_Stress_Disorder", 
  "Obsessive_Compulsive_Disorder", "Eating_disorder", 
  "Schizophrenia", "Personality_disorder"
)

# Select relevant columns
obj <- df[, c("pid", "diag_psych_1_m", "diag_2_m", "population")]

# Apply function to each population and subcategory
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_psych_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())  

results <- format_df(results)

write.csv(results, "Mental_health_comorbidities.csv")
system(paste0("dx upload Mental_health_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)


# Neurodevelopmental conditions
comorbidity_list <- c(
  "Autism spectrum disorder",
  "Developmental learning disorders",
  "Attention deficit hyperactivity disorder (ADHD)",
  "Disorder of intellectual development"
)

comorbidity_label <- c(
  "Autism_spectrum_disorder",
  "Developmental_learning_disorders",
  "ADHD",
  "Disorder_of_intellectual_development"
)

# Select relevant columns
obj <- df[, c("pid", "diag_neuro_dev_1_m", "diag_2_m", "population")]

# Apply function to each population and subcategory
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_neuro_dev_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())  

results <- format_df(results)

write.csv(results, "Neurodevelopmental_comorbidities.csv")
system(paste0("dx upload Neurodevelopmental_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

invisible(gc())

#Reproductive

# Reproductive system conditions
comorbidity_list <- c(
  "Endometriosis", 
  "Polycystic Ovary Syndrome (PCOS)",
  "Fibrocystic Breast, or another Benign Breast Disease (such as proliferative Benign Breast Disease or LCIS)",
  "Ductal Carcinoma in situ"
)

comorbidity_label <- c(
  "Endometriosis", 
  "PCOS",
  "Benign_breast_disease",
  "Ductal_Carcinoma_in_situ"
)

# Select relevant columns
obj <- df[, c("pid", "diag_repro_1_m", "diag_2_m", "population")]

# Apply function to each population and subcategory
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_repro_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything()) 

results <- format_df(results)

write.csv(results, "Reproductive_comorbidities.csv")
system(paste0("dx upload Reproductive_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

invisible(gc())

# Autoimmune conditions
comorbidity_list <- c(
  "Rheumatoid arthritis", 
  "Lupus", 
  "Multiple Sclerosis (MS)", 
  "Graves’ disease", 
  "Guillain-Barre syndrome", 
  "Psoriasis"
)

comorbidity_label <- c(
  "Rheumatoid_arthritis", 
  "Lupus", 
  "Multiple_Sclerosis", 
  "Graves_disease", 
  "Guillain_Barre_syndrome", 
  "Psoriasis"
)

# Select relevant columns
obj <- df[, c("pid", "diag_auto_1_m", "diag_2_m", "population")]


# Apply function to each population and subcategory
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_auto_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())
results <- format_df(results)

write.csv(results, "Autoimmune_comorbidities.csv")
system(paste0("dx upload Autoimmune_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)


# Autoimmune conditions exploratory analysis (include Other and None of above)
comorbidity_list <- c(
  "Rheumatoid arthritis", 
  "Lupus", 
  "Multiple Sclerosis (MS)", 
  "Graves’ disease", 
  "Guillain-Barre syndrome", 
  "Psoriasis", "None of the above", "Other (not listed)"
)

comorbidity_label <- c(
  "Rheumatoid_arthritis", 
  "Lupus", 
  "Multiple_Sclerosis", 
  "Graves_disease", 
  "Guillain_Barre_syndrome", 
  "Psoriasis", "None_of_above", "Other"
)

# Select relevant columns
obj <- df[, c("pid", "diag_auto_1_m", "diag_2_m", "population")]


# Apply function to each population and subcategory
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_auto_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Autoimmune_comorbidities_with_other.csv")
system(paste0("dx upload Autoimmune_comorbidities_with_other.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

invisible(gc())

# exploratory analysis
#Kidney and urinary disorders  (include Other and None of above)
comorbidity_list <- c(
  "Chronic kidney disease (or chronic kidney failure)", 
  "Kidney stones", 
  "None of the above", "Other (not listed)"
)

comorbidity_label <- c(
  "Chronic_kidney_disease", 
  "Kidney_stones", 
  "None_of_the_above", "Other"
)

# Select relevant columns
obj <- df[, c("pid", "diag_urol_1_m", "diag_2_m", "population")]

# Apply function to each population and subcategory
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_urol_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Kidney_or_urinary_cormorbidities.csv")
system(paste0("dx upload Kidney_or_urinary_cormorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

# Cardiovascular conditions
comorbidity_list <- c(
  "B-12 Deficiency (Pernicious Anaemia)", 
  "High Cholesterol", 
  "Heart Attack (Myocardial Infarction)",
  "High Blood Pressure (Hypertension)"
)

comorbidity_label <- c(
  "B12_Deficiency", 
  "High_Cholesterol", 
  "Myocardial_Infarction",
  "Hypertension"
)

# Select relevant columns, exclude missing populations
obj <- df[!is.na(df$population), c("pid", "diag_cvd_1_m", "diag_2_m", "population")]

# Apply function per population and per comorbidity
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_cvd_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Cardiovascular_comorbidities.csv")
system(paste0("dx upload Cardiovascular_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

# Lung and respiratory conditions
comorbidity_list <- c(
  "Chronic Obstructive pulmonary disease, COPD (including emphysema and chronic bronchitis)", 
  "Asthma", 
  "Hay Fever"
)

comorbidity_label <- c(
  "COPD", 
  "Asthma", 
  "Hay_Fever"
)

# Select relevant columns
obj <- df[ , c("pid", "diag_resp_1_m", "diag_2_m", "population")]

# Apply function per population and comorbidity
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_resp_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Respiratory_comorbidities.csv")
system(paste0("dx upload Respiratory_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

invisible(gc())

# Digestive system or liver problems
comorbidity_list <- c(
  "Gastro-oesophageal Acid Reflux (GORD)", 
  "Irritable bowel syndrome", 
  "Inflammatory Bowel Disease", 
  "Crohn’s Disease", 
  "Coeliac Disease", 
  "Fatty liver disease", 
  "Hepatitis"
)

comorbidity_label <- c(
  "GORD", 
  "Irritable_bowel_syndrome", 
  "Inflammatory_Bowel_Disease", 
  "Crohns_Disease", 
  "Coeliac_Disease", 
  "Fatty_liver_disease", 
  "Hepatitis"
)

# Select relevant columns
obj <- df[ , c("pid", "diag_gastro_1_m", "diag_2_m", "population")]

# Apply function per population and comorbidity
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_gastro_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Digestive_liver_comorbidities.csv")
system(paste0("dx upload Digestive_liver_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

# Neurological
comorbidity_list <- c(
  "Migraine with aura", 
  "Migraine without aura", 
  "Epilepsy"
)

comorbidity_label <- c(
  "Migraine_with_aura", 
  "Migraine_without_aura", 
  "Epilepsy"
)

# Select relevant columns
obj <- df[, c("pid", "diag_neuro_1_m", "diag_2_m", "population")]

# Apply function per population and comorbidity
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_neuro_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Neuro_comorbidities.csv")
system(paste0("dx upload Neuro_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

# Depression subtypes
comorbidity_list <- c(
  "Major Depressive Disorder", 
    "Other (not listed)"
)

comorbidity_label <-  c(
  "Major_Depressive_Disorder",  
    "Other_not_listed"
)


# Select relevant columns
obj <- df[, c("pid", "diag_psych_depr_1_m", "diag_2_m", "population")]

# Apply function per population and comorbidity
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_psych_depr_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Depression_subtypes_comorbidities.csv")
system(paste0("dx upload Depression_subtypes_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

#Pregnancy-related Depression subtypes
comorbidity_list <- c(
  "Perinatal depression", 
  "Postnatal depression"
)

comorbidity_label <-  c(
  "Perinatal_depression", 
  "Postnatal_depression"
)


# Select relevant columns
obj <- df[, c("pid", "diag_psych_depr_1_m", "diag_2_m", "population", "children_birthed_num_1_1")]


obj <- obj[obj$children_birthed_num_1_1 > 0, ]

# Apply function per population and comorbidity
results <- obj %>%
  split(.$population) %>%
  map_dfr(~{
    pop <- .x
    map2_dfr(
      comorbidity_list,
      comorbidity_label,
      ~{
        res <- prevalence_se_ci(pop, "diag_psych_depr_1_m", .x)
        res$population <- unique(pop$population)
        res$subcategory <- .x
        res$label <- .y
        return(res)
      }
    )
  }) %>%
  select(population, subcategory, label, everything())

results <- format_df(results)

write.csv(results, "Depression_pregnancy_subtypes_comorbidities.csv")
system(paste0("dx upload Depression_pregnancy_subtypes_comorbidities.csv --destination /Outputs/Comorbidity_prevalence/"), intern=TRUE)

#Multimorbidity score for 13 broad category comorbidities (excluding mental illness and complications with preganncy)
rm(obj)
library(dplyr)
library(tidyr)
library(purrr)
library(epiR)

# Select necessary columns
obj <- df[, c("pid", "diag_2_m", "population")]

# List of options to exclude
excluded_options <- c(
  "Mental health conditions (e.g.depression, bipolar disorder)",
  "Complications or difficulties in pregnancy or childbirth",
  "", "Other not listed", "None of the above", 
  "Do not know", "Prefer not to answer"
)

# Split multiple diagnoses (no spaces after pipes here assumed)
obj$choices_list <- strsplit(obj$diag_2_m, "\\|")

# Remove excluded options
obj$filtered_choices <- lapply(obj$choices_list, function(x) setdiff(trimws(x), excluded_options))

# Count the number of remaining conditions
obj$multimorbidity_score <- sapply(obj$filtered_choices, length)

# Remove individuals who selected any excluded option
obj <- obj[!sapply(obj$choices_list, function(x) any(trimws(x) %in% excluded_options)), ]

# Remove rows with missing multimorbidity score
obj <- obj[!is.na(obj$multimorbidity_score), ]

# Summarize by population
morbidity_stats <- obj %>%
  group_by(population) %>%
  summarise(
    mean_morbidity = mean(multimorbidity_score),
    sd_morbidity = sd(multimorbidity_score),
    n = n(),
    standard_error = sd_morbidity / sqrt(n)
  ) %>%
  mutate(
    ci_results = map2(mean_morbidity, standard_error, ~{
      est <- .x
      se <- .y
      ci <- est + c(-1, 1) * qnorm(0.975) * se
      list(lower = ci[1], upper = ci[2])
    })
  ) %>%
  unnest_wider(ci_results)

write.csv(morbidity_stats, "Mean_multimorbidity_score.csv")
system("dx upload Mean_multimorbidity_score.csv --destination /Outputs/Comorbidity_prevalence/", intern=TRUE)

                   #Violin plot multimorbidity score for 13 broad category comorbidities (excluding mental illness and complications with preganncy)
invisible(gc())

rm(obj)

# Select necessary columns
obj <- df[, c("pid", "diag_2_m", "population")]

#Multimorbidity score
# Select necessary columns

#People who choose None of the above are given zero
#People who only chose Mental health or pregnancy complications alone are given zero
#People who chose other not listed alone or other not listed with mental health and/or pregancy complications are given NA
#People who chose Do not know or Prefer not to answer are given NA
#The number of multimorbidities are counted for everyone else excluding Mental health/Pregnancy complications/Other not listed/Prefer not to answer/Do not know

# Split multiple diagnoses into lists 
obj$choices_list <- strsplit(obj$diag_2_m, "\\|")

obj$multimorbidity_score <- sapply(obj$choices_list, function(x) {
  x <- trimws(x)  # clean whitespace
  
  # Handle exclusive selections first
  if ("None of the above" %in% x) {
    return(0)
  }
  
  if ("Do not know" %in% x) {
    return(NA_integer_)
  }
  
  if ("Prefer not to answer" %in% x) {
    return(NA_integer_)
  }
  
  # Define exclusion categories
  mental_health <- grepl("Mental health conditions", x, ignore.case = TRUE)
  pregnancy_complications <- grepl("Complications or difficulties in pregnancy", x, ignore.case = TRUE)
  other_not_listed <- x %in% c("Other not listed")
    
  # If "Other not listed" is combined with mental health and/or pregnancy complications, give NA
  if (any(other_not_listed) & any(mental_health | pregnancy_complications)) {
    return(NA_integer_)
  }
  
  # If they ONLY selected mental health and/or pregnancy complications 
  # (and possibly blanks), give them 0
  if (all(mental_health | pregnancy_complications) & 
      any(mental_health | pregnancy_complications)) {
    return(0)
  }
  
  # If they ONLY selected "Other not listed" or blanks, give them NA
  if (all(other_not_listed)) {
    return(NA_integer_)
  }
  
  # Otherwise, filter out excluded items and count valid diagnoses
  valid <- x[!mental_health &
            !pregnancy_complications &
            !other_not_listed]
    
  if (length(valid) == 0) {
    return(NA_integer_)
  } else {
    return(length(valid))
  }
})

#Sanity check
by(obj$multimorbidity_score, obj$population, summary)
sum(is.na(obj$multimorbidity_score))

# Remove rows with missing multimorbidity score
obj <- obj[!is.na(obj$multimorbidity_score), ]

# Rename population column to Group
obj$population <- as.character(obj$population)
# Then do the replacement
obj$population[obj$population == "CTRL"] <- "Control"
# Then convert back to factor with desired levels
obj$population <- factor(obj$population, levels = c("PMDD", "MDD", "Control"))
colnames(obj)[colnames(obj) == "population"] <- "Group"
obj$Group <- factor(obj$Group, levels = c("PMDD", "MDD", "Control"))

# Custom colors
group_colors <- c("PMDD" = "#f1a226", "MDD" = "#800074", "Control" = "#298c8c")


# Calculate mean and SE for each group
summary_df <- obj %>%
  group_by(Group) %>%
  summarise(
    mean_score = mean(multimorbidity_score, na.rm = TRUE),
    se = sd(multimorbidity_score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p <- ggplot(obj, aes(x = Group, y = multimorbidity_score, fill = Group)) +
  geom_boxplot(
    alpha = 0.6,
    width = 0.5,
    color = "black", 
    outlier.shape = NA
  ) +
  # Add mean points
  geom_point(
    data = summary_df,
    aes(x = Group, y = mean_score),
    color = "black",
    size = 4,
      inherit.aes = FALSE
  ) +
  # Add error bars for mean
  geom_errorbar(
    data = summary_df,
    aes(x = Group, ymin = mean_score - se, ymax = mean_score + se),
    width = 0.1,
    color = "black",
    linewidth = 0.4,   # updated from size
  inherit.aes = FALSE) +
  scale_fill_manual(values = group_colors) +
  scale_y_continuous(limits = c(0, 11), breaks = 0:11, expand = c(0, 0)) +
  labs(
    title = "Distribution of Multimorbidity Scores with Group Means",
    x = "Group",
    y = "Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  )

# Add manual significance bars
p <- p +
  # PMDD vs MDD
geom_segment(aes(x = 1, xend = 2, y = 9, yend = 9), color = "black") + 
annotate("text", x = 1.5, y = 9.2, label = "***", size = 5) + 
# PMDD vs CTRL 
geom_segment(aes(x = 1, xend = 3, y = 9.5, yend = 9.5), color = "black") + 
annotate("text", x = 2, y = 9.8, label = "***", size = 5) + 
# MDD vs CTRL 
geom_segment(aes(x = 2, xend = 3, y = 8.5, yend = 8.5), color = "black") + 
annotate("text", x = 2.5, y = 8.8, label = "***", size = 5)

# Save high-resolution PNG
png("Distribution_of_Multimorbidity_Scores_Boxplot.png")
print(p)
dev.off()

                   
system("dx upload Distribution_of_Multimorbidity_Scores_Boxplot.png --destination /Outputs/Comorbidity_prevalence/", intern=TRUE)
           