#Script to run sequential logistic regression models comparing the prevalence of each medical condition of interest in PMDD vs MDD

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
  library(stats)        
})

#Import OFH data
system("dx download OFH_exported_files/OFH_questionnaire.csv", intern=TRUE)

system("dx download OFH_exported_files/OFH_participant_data.csv", intern=TRUE)

#Read questionnaire into R
df <- read.table("OFH_questionnaire.csv", sep=",", header=TRUE, quote = "\"")

#Read participant data into R
participant <- read.table("OFH_participant_data.csv", sep=",", header=TRUE, quote = "\"")

class(df)
class(participant)

nrow(df)
nrow(participant)

#There is a missing pid in df so remove this id from participant
#For some reason this didn't work so needed to use head function
missing_pids <- setdiff(df$pid, participant$pid)

missing_pids <- setdiff(head(df$pid, nrow(df)), head(participant$pid, nrow(df)))
participant <- participant[!participant$pid %in% missing_pids, ]
nrow(df)
nrow(participant)

#Filter OFH participant file to only include females 
OFH_female <- participant[participant$demog_sex_1_1 %in% "Female" | participant$demog_sex_2_1 %in% "Female", ]
df_female <- df[df$pid %in% OFH_female$pid, ]
#Exclude individuals who didn't answer the DIAG_2_M questionnaire
#Don't exclude "Other not listed" because these individuals form part of the total cohort (may or may not have psychiatric illness)
df_female <- df_female[!df_female$diag_2_m %in% c("", "Do not know", "Prefer not to answer"), ]
#Total is the total number of females who answered the DIAG_2_M questionnaire
fT <- nrow(df_female)


#Import PMDD patient IDs (obtained from Cohort Browser - converted dataset using Table Exporter in tools)
system("dx download OFH_exported_files/PMDD.csv", intern=TRUE)
#Import Female control IDs
system("dx download OFH_exported_files/CTRL.csv", intern=TRUE)
#Import Female MDD IDs
system("dx download OFH_exported_files/MDD.csv", intern=TRUE)

#Import PMDD patient IDs into R
fem_PMDD_ID <- read.table("PMDD.csv", sep=",", header=TRUE, quote = "\"")
#Import Female control IDs into R
fem_CTRL_ID <- read.table("CTRL.csv", sep=",", header=TRUE, quote = "\"")
#Filter female control group to remove individuals who didn't answer the diag_2_m questionnaire
#i.e select individuals who are within the total female group 
fem_CTRL_ID <- fem_CTRL_ID[fem_CTRL_ID$pid %in% df_female$pid, ]
#Import Female MDD IDs into R
fem_MDD_ID <- read.table("MDD.csv", sep=",", header=TRUE, quote = "\"")

#Add population label
df$population <- ifelse(df$pid %in% fem_PMDD_ID$pid, "PMDD",
                        ifelse(df$pid %in% fem_CTRL_ID$pid, "CTRL",
                               ifelse(df$pid %in% fem_MDD_ID$pid, "MDD", NA)))

#Filter out NAs
df <- df[!is.na(df$population), ]

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
#-edu_qual_1_m
#-gyn_contracept_implant_1_1
#-gyn_contracept_methods_1_m
#-medicat_repro_1_m
#-medicat_repro_contracept_1_m
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
#-gad-7 items
#-children_birthed_num_1_1
#-diag_endocr_1_m
#-diag_psych_1_m
#-diag_neuro_dev_1_m
#-diag_repro_1_m
#-diag_auto_1_m
#-diag_urol_1_m
#diag_cvd_1_m
#-diag_resp_1_m
#-diag_gastro_1_m
#-diag_neuro_1_m

vars_to_keep <- c(
  "pid", "population", "submission_date", "demog_weight_1_1", "demog_height_1_1",
  "smoke_tobacco_type_1_m", "alcohol_curr_1_1", "lifestyle_social_visits_1_1",
  "father_diag_a_2_m", "mother_diag_a_2_m", "housing_income_1_1",
  "edu_qual_1_m", "gyn_contracept_implant_1_1", "gyn_contracept_methods_1_m",
     "medicat_repro_1_m", "medicat_repro_contracept_1_m",
  "diag_2_m", "sleep_hrs_1_1", "sleep_trouble_1_1", "sleep_chronotype_1_1",
  "sleep_snoring_1_1", "sleep_daytime_1_1", "activity_mod_days_2_1",
  "activity_mod_mins_2_1", "activity_vig_days_2_1", "activity_vig_mins_2_1", "activity_type_1_m",
  "gyn_menstr_cycle_days_1_1", "gyn_menstr_cycle_days_2_1",
  "gyn_menopause_1_1", "gyn_menopause_2_1", "phq9_item1_interest_1_1", "phq9_item2_down_1_1", "phq9_item3_sleep_1_1", "phq9_item4_energy_1_1", 
               "phq9_item5_appetite_1_1", "phq9_item6_bad_1_1", "phq9_item7_concentr_1_1", "phq9_item8_movement_1_1",
               "phq9_item9_harm_1_1", "gad7_item1_anx_1_1", "gad7_item2_worry_control_1_1", "gad7_item3_worry_amount_1_1", 
"gad7_item4_relax_1_1", "gad7_item5_restless_1_1", "gad7_item6_annoyed_1_1", "gad7_item7_afraid_1_1",
"children_birthed_num_1_1", "diag_endocr_1_m", "diag_psych_1_m", "diag_neuro_dev_1_m", "diag_repro_1_m", "diag_auto_1_m", "diag_urol_1_m", 
"diag_cvd_1_m", "diag_resp_1_m", "diag_gastro_1_m", "diag_neuro_1_m")


df <- df %>%
  select(all_of(vars_to_keep))

invisible(gc())
#Prepare data

# Convert the outcome variable into a factor
df$population <- factor(df$population, levels = c("CTRL", "PMDD", "MDD"))
df$population <- relevel(factor(df$population), ref = "PMDD")


#Specify the class and level for each variable

invisible(gc())

#Age 
# Convert birth date into a proper Date object (assuming the first of the month)
participant$birth_date <- as.Date(paste0(participant$birth_year, "-", participant$birth_month, "-01"))
# Ensure submission_date is in Date format
df$submission_date <- as.Date(df$submission_date)
# Calculate age using lubridate's interval function for efficiency
df$age <- time_length(interval(participant$birth_date, df$submission_date), "years")
df$age <- as.vector(scale(as.numeric(df$age)))

invisible(gc())

#BMI
df$bmi <- df$demog_weight_1_1/(df$demog_height_1_1/100)^2
df$bmi <- as.vector(scale(as.numeric(df$bmi)))

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
      ) ~ "Non-White",
      
      demog_ethnicity_1_1 %in% c(
        "Asian or Asian British – Indian", "Asian or Asian British – Pakistani",
        "Asian or Asian British – Bangladeshi", "Chinese",
        "Any other Asian/Asian British background"
      ) ~ "Non-White",
      
      demog_ethnicity_1_1 %in% c(
        "Black or Black British – African", "Black or Black British – Caribbean",
        "Any other Black / African / Caribbean background"
      ) ~ "Non-White",
      
      demog_ethnicity_1_1 %in% c("Arab")   ~ "Non-White",
      demog_ethnicity_1_1 %in% c("Other")  ~ "Non-White",
      
      TRUE ~ NA_character_
    )
  )
df$ethnicity <- as.factor(df$ethnicity)
df$ethnicity <- relevel(df$ethnicity, ref = "White")

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

df$smoke_tobacco_ever <- factor(df$smoke_tobacco_ever, levels = c(0, 1)) 

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

df$alcohol_use <- factor(df$alcohol_use, 
levels = c("Less_than_once_a_week", "One_to_four_times_a_week", "Daily_or_almost_daily"))

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

df$social_interaction <- factor(df$social_interaction, 
levels = c("Less_than_once_a_week", "One_to_four_times_a_week", "Almost_daily"))

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

df$parental_history_mental_illness <- factor(df$parental_history_mental_illness, levels = c(0, 1))

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

df$income <- factor(df$income, levels = c("Less_than_£18,000", "£18,000_to_£51,999", "Greater_than_£51,999"))
df$income <- relevel(df$income, ref= "Less_than_£18,000")

#Higher education
# Identify those who have a college or university degree using grepl
higher_ed <- "College or University degree|Other professional qualifications eg\\: nursing, teaching"

df$higher_ed <- as.numeric(ifelse(df$edu_qual_1_m == "Prefer not to answer", 
                       NA, 
                       grepl(higher_ed, df$edu_qual_1_m, fixed = FALSE)))

df$higher_ed <- factor(df$higher_ed, levels = c(0, 1))

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
df$contraception_use <- factor(df$contraception_use, levels = c("No", "Yes")) 

invisible(gc())

#Contraceptive method
# Define individuals who have never used hormonal contraception
df$hormonal_cont_never <- df$gyn_contracept_implant_1_1 == "No"

# Define patterns
include_CHC_patterns <- "Combined Pill|Patch|Vaginal ring"
include_POC_patterns <- "Injection|IUS \\(Hormonal coil\\)|Progesterone only pill \\(mini pill\\)"  # escape parentheses

# Create CHC only / POC only / Both
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

df$contr_method <- as.factor(df$contr_method)
df$contr_method <- factor(df$contr_method, levels = c("CHC", "POC", "CHC_&_POC", "Never"))

df$contr_method <- relevel(df$contr_method, ref = "Never")

invisible(gc())


#Regular contraceptive method
#Identify individuals who do not regularly use hormonal contraception
df$No_regular_contr_use <- !grepl("Contraceptive medication, coil, implant or patch", df$medicat_repro_1_m)

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
df$reg_contr_method <- ifelse(df$No_regular_contr_use, "None",
                       ifelse(df$reg_CHC_only, "reg_CHC", 
                       ifelse(df$reg_POC_only, "reg_POC", 
                       ifelse(df$reg_CHC_and_POC, "reg_CHC_&_POC", NA))))

df$reg_contr_method <- as.factor(df$reg_contr_method)   
df$reg_contr_method <- factor(df$reg_contr_method, levels = c("reg_CHC", "reg_POC", "reg_CHC_&_POC", "None"))
df$reg_contr_method <- relevel(df$reg_contr_method, ref = "None")


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

df$multimorbidity_score <- as.vector(scale(df$multimorbidity_score))                           
invisible(gc())

#Composite sleep score
sleep_Qs <- c(
  "sleep_hrs_1_1", "sleep_trouble_1_1", 
  "sleep_chronotype_1_1", "sleep_snoring_1_1", 
  "sleep_daytime_1_1"
)

sleep_data <- df[, c(sleep_Qs, "population")]  # <-- Use different name

# Give points for healthy sleep
sleep_A <- ifelse(sleep_data$sleep_hrs_1_1 %in% c(7, 8), 1, 0)
sleep_B <- ifelse(sleep_data$sleep_trouble_1_1 == "Never/rarely", 1, 0)
sleep_C <- ifelse(sleep_data$sleep_chronotype_1_1 %in% c("Definitely a 'morning' person", "More a 'morning' than 'evening' person"), 1, 0)
sleep_D <- ifelse(sleep_data$sleep_snoring_1_1 == "No", 1, 0)
sleep_E <- ifelse(sleep_data$sleep_daytime_1_1 == "Never/rarely", 1, 0)


# Combine into a data frame and calculate total score
composite_sleep <- data.frame(sleep_A, sleep_B, sleep_C, sleep_D, sleep_E)

df$composite_sleep_score <- rowSums(composite_sleep)

# Identify rows where ALL sleep questions were unanswered ("", NA)
all_missing <- rowSums(df[, sleep_Qs] == "" | is.na(df[, sleep_Qs])) == length(sleep_Qs)

# Assign NA sleep_score to those individuals
df$composite_sleep_score[all_missing] <- NA
df$composite_sleep_score <- as.vector(scale(as.numeric(df$composite_sleep_score)))

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

df$composite_activity_score <- as.vector(scale(df$composite_activity_score))

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

df$physical_activity <- as.factor(df$physical_activity)   
df$physical_activity <- factor(df$physical_activity, levels= c("No_physical_activity", "Low_or_Medium_level_activity", "High_level_activity"))
df$physical_activity <- relevel(df$physical_activity, ref= "No_physical_activity")

# Menstrual cycle length (numeric; use Q1 if available, otherwise Q2; non-numeric -> NA)
df <- df %>%
  mutate(
    menstrual_cycle_length = case_when(
      gyn_menstr_cycle_days_1_1 != "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_1_1)),
      gyn_menstr_cycle_days_1_1 == "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_2_1))
    )
  )

df$menstrual_cycle_length <- as.vector(scale(df$menstrual_cycle_length))

# Menstrual cycle length (premenopausal only, numeric)
df <- df %>%
  mutate(
    menstrual_cycle_length_premenopause = case_when(
      gyn_menopause_1_1 == "Yes" | gyn_menopause_2_1 == "Yes" ~ NA_real_,
      gyn_menstr_cycle_days_1_1 != "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_1_1)),
      gyn_menstr_cycle_days_1_1 == "" ~ suppressWarnings(as.numeric(gyn_menstr_cycle_days_2_1))
    )
  )

df$menstrual_cycle_length_premenopause <- as.vector(scale(df$menstrual_cycle_length_premenopause))

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

df$reg_menses <- factor(df$reg_menses, levels = c(0, 1)) 


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
df$reg_menses_premenopause <- factor(df$reg_menses_premenopause, levels = c(0, 1)) 

invisible(gc())

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

df$sum_phq9_gad7 <- as.vector(scale(df$sum_phq9_gad7))

sub<- df %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

test <- na.omit(sub[ ,c("population", "age", "income", "ethnicity","higher_ed", "smoke_tobacco_ever", 
 "alcohol_use", "social_interaction", "bmi", "composite_sleep_score", 
"parental_history_mental_illness", "physical_activity", "contr_method")])

nrow(test)/nrow(sub)
table(sub$population)
table(test$population)
nrow(test)

invisible(gc())

#PMDD vs MDD
# Fit binomial glm regression model to examine association between psychiatric status (PMDD v MDD) and each comorbidity
#Model 1: comorbidity ~ population
#Model 2: comorbidity ~ population + age + ethnicity
#Model 3: comorbidity ~ population + age + ethnicity + higher_ed + smoke_tobacco_ever + 
#alcohol_use + social_interaction + bmi + composite_sleep_score 
#Model 4: comorbidity ~ population + age + ethnicity + income + higher_ed + smoke_tobacco_ever + 
#alcohol_use + social_interaction + bmi + physical_activity + composite_sleep_score + 
#contr_method + parental_history_mental_illness

# Define model formulas with quadratic terms
model_formulas <- list(
  model1 = "population",
  model2 = "population + age + I(age^2) + ethnicity",
  model3 = paste("population + age + I(age^2) + ethnicity +",
                "higher_ed + smoke_tobacco_ever + alcohol_use +",
                 "social_interaction + composite_sleep_score + I(composite_sleep_score^2)"),
  model4 = paste("population + age + I(age^2) + ethnicity + income +",
                "higher_ed + smoke_tobacco_ever + alcohol_use +",
                 "social_interaction + bmi + I(bmi^2) + physical_activity + composite_sleep_score + I(composite_sleep_score^2) +",
                 "parental_history_mental_illness +",
                 "contr_method")
)

# Define analysis function
analyze_logistic_model <- function(data, output_prefix = "model_output") {

  # Store extracted populationPMDD results
  population_summary_list <- list(
    model1 = data.frame(),
    model2 = data.frame(),
    model3 = data.frame(),
    model4 = data.frame()

  )

  #All three models run on complete data from model 3
  # Main model loop
  for (comorb in comorbidity_vars) {
    for (model_name in names(model_formulas)) {
        print(paste0(comorb, ": ", model_name))
      formula_rhs <- model_formulas[[model_name]]
      formula_str <- paste(comorb, "~", formula_rhs)
      formula_model4 <- paste(comorb, "~", model_formulas[["model4"]])#This ensures model 1 and 2 are run on same sample as model 4
      vars_in_model <- all.vars(as.formula(formula_model4))
    #vars_in_model <- all.vars(as.formula(formula_str)) #Uncomment this for maximising sample size and running 
    #model 1, 2, and 3 on complete data for each model separately i.e different sample sizes for each model

      df_clean <- data %>%
      dplyr::select(all_of(vars_in_model)) %>%
      na.omit()

      
      # Print number of complete and incomplete cases
      #print(paste0("Number of complete cases for ", comorb, " ", model_name, ": ", nrow(df_clean)))
      #print(paste0("Number of incomplete cases for ", comorb, " ", model_name, ": ", nrow(data) - nrow(df_clean)))
      
      # Check if we have enough data to fit the model
      if (nrow(df_clean) == 0) {
        warning(paste("No complete cases for", comorb, model_name))
        next
      }
      
      tryCatch({
        model <- glm(as.formula(formula_str), data = df_clean, family = binomial)
        #print(paste0("Observations for ", comorb, " (", model_name, "): ", nobs(model)))
        #print(table(model.frame(model)$population))
        
        coefs <- summary(model)$coefficients
        
        # Get confidence intervals with error handling
        ci <- tryCatch({
          suppressMessages(confint(model))
        }, error = function(e) {
          # If confint fails, use profile likelihood method or Wald CIs
          confint.default(model)
        })
          
        # Get sample sizes by population group
        pop_table <- table(model.frame(model)$population)
        n_mdd <- as.numeric(pop_table["MDD"])
        n_pmdd <- as.numeric(pop_table["PMDD"])  
        
        results <- data.frame(
          Term = rownames(coefs),
          OR = round(exp(coefs[, "Estimate"]), 3),
          CI_Lower = round(exp(ci)[,1], 3),
          CI_Upper = round(exp(ci)[,2], 3),
          P_Value = signif(coefs[, "Pr(>|z|)"], 3),
          N = nobs(model),
          pct_retained = nobs(model)/nrow(data),
          N_MDD = n_mdd,
          N_PMDD = n_pmdd,
          stringsAsFactors = FALSE
        )
        
        # Extract only 'populationPMDD' results
        if ("populationPMDD" %in% results$Term) {
          pop_result <- results[results$Term %in% c("populationPMDD"), ] 
          # Add Comorbidity as first column
          pop_result <- pop_result %>%
            mutate(Comorbidity = comorb) %>%
            select(Comorbidity, everything())
            population_summary_list[[model_name]] <- rbind(population_summary_list[[model_name]], pop_result)
        }
        
      }, error = function(e) {
        warning(paste("Model fitting failed for", comorb, model_name, ":", e$message))
      })
    }
  }
  
  # Save all models together
  combine_results <- bind_rows(population_summary_list, .id = "model_name")
  
  # Write to CSV file
  csv_path <- "All_models_population_summary.csv"
  write.csv(combine_results, csv_path, row.names = FALSE)
  
  # Upload the CSV file (commented out for portability - uncomment if using DNAnexus)
  # system(paste("dx upload", csv_path, "--destination", "/PMDD_outputs/Regression_comorbidities/Broad/"), intern = TRUE)
  
  return(combine_results)
}

#----------------------------------------------------------------------
# Prepare dataset: filter relevant population and set reference level

#Binarise each broad category diagnosis

invisible(gc())

# Broad category comorbidities 
comorbidity_list <- c(
  "Autoimmune disorder", "Blood disorders (Anaemia)", "Cancer", "Digestive system or liver problems",
  "Endocrine, nutritional and metabolic disorders (e.g. diabetes, thyroid disorder, vitamin deficiencies)", 
  "Eye or visual problems", "Fractures, breaks, or joint problems", "Heart or circulatory disease (e.g. high blood pressure or stroke)", 
  "Kidney or urinary system disorders", "Lung or respiratory problems", "Mental health conditions (e.g. depression, bipolar disorder)", 
  "Neurodevelopmental conditions (e.g. Autism spectrum disorder, ADHD)",  
  "Neurological disorders (things that affect that brain or nervous system. E.g., Epilepsy)", "Reproductive system problems"
)

# Define comorbidities to loop over
comorbidity_vars <- c(
  "Autoimmune_disorder", "Blood_disorders", "Cancer",
  "Digestive_system_liver_problems", "Endocrine_nutritional_metabolic_disorders",
  "Eye_visual_problems", "Fractures_breaks_joint_problems", "Heart_circulatory_disease",
  "Kidney_urinary_system_disorders", "Lung_respiratory_problems", 
  "Mental_health_conditions",  
  "Neurodevelopmental_conditions", "Neurological_disorders", "Reproductive_system_problems"
)


for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
    
  df <- df %>%
    mutate(!!sym(new_col) := if_else(str_detect(diag_2_m, pattern), 1, 0))
}


#Check class of diagnoses (just check one example)
class(df$Heart_circulatory_disease)         

invisible(gc())

obj <- df %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Broad/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------
invisible(gc())

#Complications in pregnancy 
#repeat in subgroup of women who have ever been pregnant

# Set up object with relevant columns

#Binarise diagnosis

library(dplyr)
library(stringr)

# Broad category comorbidities 
comorbidity_list <- c(
  "Complications or difficulties in pregnancy or childbirth"
)

comorbidity_vars <- c(
  "Complications_in_pregnancy"
)

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  obj <- df %>%
    mutate(!!sym(new_col) := if_else(str_detect(diag_2_m, pattern), 1, 0))
}

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

obj <- obj[obj$children_birthed_num_1_1 > 0, ]

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Complications_in_pregnancy/"), intern = TRUE)

rm(output)


#----------------------------------------------------------------------------------
invisible(gc())

# Endocrine comorbidities 

#Binarise subcategory diagnoses and perform regression

comorbidity_list <- c(
  "Type 1 diabetes", "Type 2 diabetes",
  "Overactive thyroid", "Underactive thyroid", "Cushing syndrome", 
  "Lactose intolerance", "Vitamin A deficiency", "Thiamine deficiency",
  "Vitamin D deficiency"
)

comorbidity_vars <- c(
  "Type_1_diabetes", "Type_2_diabetes",
  "Overactive_thyroid", "Underactive_thyroid", "Cushing_syndrome",
  "Lactose_intolerance", "Vitamin_A_deficiency", "Thiamine_deficiency",
  "Vitamin_D_deficiency"
)


obj <- df  # Start with your original dataframe

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  # Update obj by adding the new column
  obj <- obj %>%
    mutate(!!sym(new_col) := case_when(
      diag_endocr_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
      str_detect(diag_endocr_1_m, pattern) ~ 1,
      TRUE ~ 0
    ))
}

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Endocrine/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

# Psychiatric comorbidities 
comorbidity_list <- c(
  "Anxiety", "Bipolar disorder", "Body dysmorphia",
  "Depression", "Post Traumatic Stress Disorder", 
  "Obsessive Compulsive Disorder", "Eating disorder", 
  "Schizophrenia", "Personality disorder"
)

comorbidity_vars <- c(
  "Anxiety", "Bipolar_disorder", "Body_dysmorphia",
  "Depression", "Post_Traumatic_Stress_Disorder", 
  "Obsessive_Compulsive_Disorder", "Eating_disorder", 
  "Schizophrenia", "Personality_disorder"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_psych_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_psych_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}          

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Psychiatric/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

#Neurodevelopmental
comorbidity_list <- c(
  "Autism spectrum disorder",
  "Developmental learning disorders",
  "Attention deficit hyperactivity disorder (ADHD)",
  "Disorder of intellectual development"
)

comorbidity_vars <- c(
  "Autism_spectrum_disorder",
  "Developmental_learning_disorders",
  "ADHD",
  "Disorder_of_intellectual_development"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_neuro_dev_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_neuro_dev_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}                     

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Neurodevelopmental/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

# Reproductive system conditions
comorbidity_list <- c(
  "Endometriosis", 
  "Polycystic Ovary Syndrome (PCOS)",
  "Fibrocystic Breast, or another Benign Breast Disease (such as proliferative Benign Breast Disease or LCIS)",
  "Ductal Carcinoma in situ"
)

comorbidity_vars <- c(
  "Endometriosis", 
  "PCOS",
  "Benign_breast_disease",
  "Ductal_Carcinoma_in_situ"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_repro_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_repro_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}                     


obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Reproductive/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

#Autoimmune
comorbidity_list <- c(
  "Rheumatoid arthritis", 
  "Lupus", 
  "Multiple Sclerosis (MS)", 
  "Graves’ disease", 
  "Guillain-Barre syndrome", 
  "Psoriasis"
)

comorbidity_vars <- c(
  "Rheumatoid_arthritis", 
  "Lupus", 
  "Multiple_Sclerosis", 
  "Graves_disease", 
  "Guillain_Barre_syndrome", 
  "Psoriasis"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_auto_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_auto_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}            

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Autoimmune/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

#Respiratory
comorbidity_list <- c(
  "Chronic Obstructive pulmonary disease, COPD (including emphysema and chronic bronchitis)", 
  "Asthma", 
  "Hay Fever"
)

comorbidity_vars <- c(
  "COPD", 
  "Asthma", 
  "Hay_Fever"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_resp_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_resp_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}            

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Respiratory/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

#CVD
comorbidity_list <- c(
  "B-12 Deficiency (Pernicious Anaemia)", 
  "High Cholesterol", 
  "Heart Attack (Myocardial Infarction)",
  "High Blood Pressure (Hypertension)"
)

comorbidity_vars <- c(
  "B12_Deficiency", 
  "High_Cholesterol", 
  "Myocardial_Infarction",
  "Hypertension"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_cvd_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_cvd_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}                      

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/CVD/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

#Digestive, nutritional, metabolic

comorbidity_list <- c(
  "Gastro-oesophageal Acid Reflux (GORD)", 
  "Irritable bowel syndrome", 
  "Inflammatory Bowel Disease", 
  "Crohn’s Disease", 
  "Coeliac Disease", 
  "Fatty liver disease", 
  "Hepatitis"
)

comorbidity_vars <- c(
  "GORD", 
  "Irritable_bowel_syndrome", 
  "Inflammatory_Bowel_Disease", 
  "Crohns_Disease", 
  "Coeliac_Disease", 
  "Fatty_liver_disease", 
  "Hepatitis"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_gastro_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_gastro_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}                   

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Digestive/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

invisible(gc())

#Binarise subcategory diagnoses and perform regression

# Neurological
comorbidity_list <- c(
  "Migraine with aura", 
  "Migraine without aura", 
  "Epilepsy"
)

comorbidity_vars <- c(
  "Migraine_with_aura", 
  "Migraine_without_aura", 
  "Epilepsy"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_neuro_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_neuro_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}                         

obj <- obj %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Neurological/"), intern = TRUE)

rm(output)

#----------------------------------------------------------------------------------

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

df$multimorbidity_binary <- ifelse(
  is.na(df$multimorbidity_score), 
  NA_integer_, 
  ifelse(df$multimorbidity_score > 1, 1L, 0L)
)

comorbidity_list <- c(
  "multimorbidity_binary"
)

comorbidity_vars <- c(
  "multimorbidity_binary"
)
                              
obj <- df %>%
  filter(population %in% c("MDD", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("MDD", "PMDD")), ref = "MDD"))

# Analyze and get results
output <- analyze_logistic_model(data = obj)

system(paste("dx upload All_models_population_summary.csv", "--destination", "/PMDD_outputs/Regression_comorbidities/Multimorbidity/"), intern = TRUE)

rm(output)
                              