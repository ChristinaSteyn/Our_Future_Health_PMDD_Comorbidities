#PMDD regression comorbidities
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
  library(stats)    
  library(lmtest)
  library(Sandwich)
  library(zoo)
	
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
  # Convert to character, remove blanks, convert back to numeric
    multiple_deprivation_index = as.character(multiple_deprivation_index),
    multiple_deprivation_index = na_if(multiple_deprivation_index, ""),
    multiple_deprivation_index = as.numeric(multiple_deprivation_index),
    # Create quintiles only for non-NA values
    multiple_deprivation = ntile(multiple_deprivation_index, 5),
    
    # Convert to labeled factor (NA values will remain NA)
    multiple_deprivation = factor(
      multiple_deprivation,
      levels = 1:5,
      labels = c("Q1", "Q2", "Q3", "Q4", "Q5")
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

#No filtering by age
sum(is.na(df$age))
hist(df$age)
#Scale numeric variables

df$age <- as.vector(scale(as.numeric(df$age)))
df$bmi <- as.vector(scale(as.numeric(df$bmi)))
df$composite_sleep_score <- as.vector(scale(as.numeric(df$composite_sleep_score)))

sub<- df %>%
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

test <- na.omit(sub[ ,c("population", "age", "multiple_deprivation", "ethnicity","higher_ed", "smoke_tobacco_ever", 
 "alcohol_use", "social_interaction", "bmi", "composite_sleep_score", "physical_activity", "contr_method", "psychotropic_med")])

nrow(test)/nrow(sub)
table(sub$population)
table(test$population)
nrow(test)


# Define model formulas (keep your existing formulas)
model_formulas <- list(
  model1 = "population",
  model2 = "population + age + I(age^2) + ethnicity",
  model3 = paste("population + age + I(age^2) + ethnicity + multiple_deprivation +",
                "higher_ed + smoke_tobacco_ever + alcohol_use +",
                "social_interaction + bmi + I(bmi^2) + physical_activity + composite_sleep_score + I(composite_sleep_score^2) +",
                "contr_method")
)

# Define analysis function
analyze_poisson_model <- function(data, filename, output_prefix = "model_output") {
  # Store extracted populationPMDD results
  population_summary_list <- list(
    model1 = data.frame(),
    model2 = data.frame(),
    model3 = data.frame()
  )
  
  # Store separation diagnostics
  separation_diagnostics <- data.frame()
  
  # Safe extraction function for table values
  safe_extract <- function(tbl, row, col) {
    if (row %in% rownames(tbl) && col %in% colnames(tbl)) {
      return(as.numeric(tbl[row, col]))
    } else {
      return(0)
    }
  }
  
  # Main model loop
  for (comorb in comorbidity_vars) {
    
    # Get clean data once per comorbidity (all models use model3 variables)
    formula_model3 <- paste(comorb, "~", model_formulas[["model3"]])
    vars_in_model <- all.vars(as.formula(formula_model3))
    df_clean <- data %>%
      dplyr::select(all_of(vars_in_model)) %>%
      na.omit()
    
    # Check if we have enough data
    if (nrow(df_clean) == 0) {
      warning(paste("No complete cases for", comorb))
      next
    }
    
    # Calculate separation diagnostics ONCE per comorbidity
    sep_table <- table(df_clean[[comorb]], df_clean$population)
    print(paste0(comorb, ":"))
    print(sep_table)
    
    sep_check <- data.frame(
      Comorbidity = comorb,
      Outcome_0_CTRL = safe_extract(sep_table, "0", "CTRL"),
      Outcome_1_CTRL = safe_extract(sep_table, "1", "CTRL"),
      Outcome_0_PMDD = safe_extract(sep_table, "0", "PMDD"),
      Outcome_1_PMDD = safe_extract(sep_table, "1", "PMDD"),
      Total_N = nrow(df_clean),
      Outcome_Prevalence = mean(df_clean[[comorb]]),
      stringsAsFactors = FALSE
    )
    separation_diagnostics <- rbind(separation_diagnostics, sep_check)
    
    # Loop through models
    for (model_name in names(model_formulas)) {
      print(paste0(comorb, ": ", model_name))
      formula_rhs <- model_formulas[[model_name]]
      formula_str <- paste(comorb, "~", formula_rhs)
      
      # Initialize convergence flag
      model_stable <- FALSE
      convergence_issue <- "Not fitted"
      
      tryCatch({
        # Fit Poisson model
        model <- glm(as.formula(formula_str), data = df_clean, family = poisson(link = "log"))
        
        # Get coefficients and fitted values
        coefs <- coef(model)
        fitted_vals <- fitted(model)
        
        # Get robust standard errors with HC0
        coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC0"))
        robust_ses <- coef_table[, "Std. Error"]
        z_values <- coef_table[, "z value"]
        p_values <- coef_table[, "Pr(>|z|)"]
        
        # Comprehensive convergence check
        formal_converged <- model$converged
        has_zero_fitted <- any(fitted_vals < 1e-10)
        coefs_finite <- all(is.finite(coefs))
        ses_finite <- all(is.finite(robust_ses))
        
        # Determine model stability
        if (!formal_converged) {
          convergence_issue <- "Did not converge"
          model_stable <- FALSE
        } else if (has_zero_fitted) {
          convergence_issue <- "Zero fitted values"
          model_stable <- FALSE
        } else if (!coefs_finite) {
          convergence_issue <- "Infinite coefficients"
          model_stable <- FALSE
        } else if (!ses_finite) {
          convergence_issue <- "Infinite standard errors"
          model_stable <- FALSE
        } else {
          convergence_issue <- "Stable"
          model_stable <- TRUE
        }
        
        # Calculate robust confidence intervals (95%)
        ci_lower <- coefs - 1.96 * robust_ses
        ci_upper <- coefs + 1.96 * robust_ses
          
        # Get sample sizes by population group
        pop_table <- table(model.frame(model)$population)
        n_ctrl <- as.numeric(pop_table["CTRL"])
        n_pmdd <- as.numeric(pop_table["PMDD"])  
        
        results <- data.frame(
          Term = names(coefs),
          RR = round(exp(coefs), 3),  # Risk Ratio (Prevalence Ratio)
          CI_Lower = round(exp(ci_lower), 3),
          CI_Upper = round(exp(ci_upper), 3),
          P_Value = signif(p_values, 3),
          N = nobs(model),
          pct_retained = nobs(model)/nrow(data),
          N_CTRL = n_ctrl,
          N_PMDD = n_pmdd,
          Model_Stability = convergence_issue,
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
        
        # Add a row for failed models
        failed_result <- data.frame(
          Comorbidity = comorb,
          Term = "populationPMDD",
          RR = NA,
          CI_Lower = NA,
          CI_Upper = NA,
          P_Value = NA,
          N = nrow(df_clean),
          pct_retained = nrow(df_clean)/nrow(data),
          N_CTRL = NA,
          N_PMDD = NA,
          Model_Stability = paste("Error:", e$message),
          stringsAsFactors = FALSE
        )
        population_summary_list[[model_name]] <- rbind(population_summary_list[[model_name]], failed_result)
      })
    }
  }
  
  # Save all models together
  combine_results <- bind_rows(population_summary_list, .id = "model_name")
  
  # Write main results to CSV file
  csv_path <- filename
  write.csv(combine_results, csv_path, row.names = FALSE)
  
  # Write separation diagnostics to CSV
  sep_csv_path <- gsub("\\.csv$", "_separation_diagnostics.csv", filename)
  write.csv(separation_diagnostics, sep_csv_path, row.names = FALSE)
  
  # Upload the CSV files (commented out for portability)
  # system(paste("dx upload", csv_path, "--destination", "/PMDD_outputs/Regression_comorbidities/Broad/"), intern = TRUE)
  # system(paste("dx upload", sep_csv_path, "--destination", "/PMDD_outputs/Regression_comorbidities/Broad/"), intern = TRUE)
  
  return(list(results = combine_results, separation_diagnostics = separation_diagnostics))
}
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Broad.csv")

system(paste("dx upload Broad.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Broad_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  obj <- obj %>%
    mutate(!!sym(new_col) := if_else(str_detect(diag_2_m, pattern), 1, 0))
}

obj <- obj %>%
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))


obj <- obj[obj$children_birthed_num_1_1 > 0, ]

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename= "Complications_pregnancy.csv")

system(paste("dx upload Complications_pregnancy.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Complications_pregnancy_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))


# Analyze and get results
output <- analyze_poisson_model(data = obj, filename= "Endocrine.csv")

system(paste("dx upload Endocrine.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Endocrine_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename= "Psychiatric.csv")

system(paste("dx upload Psychiatric.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Psychiatric_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Neurodev.csv")

system(paste("dx upload Neurodev.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Neurodev_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename= "Reproductive.csv")

system(paste("dx upload Reproductive.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Reproductive_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename= "Autoimmune.csv")

system(paste("dx upload Autoimmune.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Autoimmune_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Respiratory.csv")

system(paste("dx upload Respiratory.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Respiratory_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "CVD.csv")

system(paste("dx upload CVD.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload CVD_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Digestive.csv")

system(paste("dx upload Digestive.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Digestive_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Neurological.csv")

system(paste("dx upload Neurological.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Neurological_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
                                     invisible(gc())

#Binarise subcategory diagnoses and perform regression

# Depression subtypes
comorbidity_list <- c(
  "Major Depressive Disorder"
)

comorbidity_vars <- c(
  "Major_Depressive_Disorder"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_neuro_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_psych_depr_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}                         

obj <- obj %>%
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Depression_subtypes.csv")

system(paste("dx upload Depression_subtypes.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Depression_subtypes_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
                                     invisible(gc())

#Binarise subcategory diagnoses and perform regression

# Depression pregnancy subtypes
comorbidity_list <- c(
  "Perinatal depression", 
  "Postnatal depression"
)

comorbidity_vars <- c(
  "Perinatal_depression", 
  "Postnatal_depression"
)

obj <- df

for (i in seq_along(comorbidity_list)) {
  pattern <- fixed(comorbidity_list[i])  # exact match, case-sensitive
  new_col <- comorbidity_vars[i]
  
  #Give NA to those who answered do not know or prefer not to answer    
obj <- obj %>%
  mutate(!!sym(new_col) := case_when(
    diag_neuro_1_m %in% c("Do not know", "Prefer not to answer") ~ NA_real_,
    str_detect(diag_psych_depr_1_m, pattern) ~ 1,
    TRUE ~ 0
  ))
}                         

obj <- obj %>%
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

obj <- obj[obj$children_birthed_num_1_1 > 0, ]


# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Depression_pregnancy_subtypes.csv")

system(paste("dx upload Depression_pregnancy_subtypes.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Depression_pregnancy_subtypes_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                                     
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
  filter(population %in% c("CTRL", "PMDD")) %>%
  mutate(population = relevel(factor(population, levels = c("CTRL", "PMDD")), ref = "CTRL"))

# Analyze and get results
output <- analyze_poisson_model(data = obj, filename = "Multimorbidity.csv")

system(paste("dx upload Multimorbidity.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)
system(paste("dx upload Multimorbidity_separation_diagnostics.csv", "--destination", "/Outputs/Regression_comorbidities/PMDD_v_CTRL/"), intern = TRUE)

rm(output)
                              
