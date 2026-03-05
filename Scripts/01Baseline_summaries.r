#Baseline characteristics, missingess, and comparison of complete vs incomplete cases

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

sum(is.na(df$multiple_deprivation_index))

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
  "diag_2_m", "sleep_hrs_1_1", "sleep_trouble_1_1", "sleep_chronotype_1_1",
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
    multiple_deprivation = case_when(
      is.na(multiple_deprivation_index) ~ NA_real_,
      multiple_deprivation_index == "" ~ NA_real_,
      TRUE ~ as.numeric(multiple_deprivation_index)
    )
  )

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


#Do not filter population by age 
sum(is.na(df$age))
hist(df$age)

levels(as.factor(df$multiple_deprivation))
sum(is.na(df$multiple_deprivation))

invisible(gc())

# List of variables to keep
vars_to_keep <- c("population",
  "age", "bmi", "ethnicity", "income", "smoke_tobacco_ever", "alcohol_use",
   "social_interaction", "parental_history_mental_illness", "pat_hist_mental", "mat_hist_mental", "higher_ed",
  "contraception_use", "contr_method", "reg_contr_method",
  "multimorbidity_score", "composite_sleep_score", "composite_activity_score", "physical_activity",
  "menstrual_cycle_length", "reg_menses", "sum_phq9_gad7", 
"vitamin_d", "vitamin_b", "calcium", "psychotropic_med", "geographic_region", "multiple_deprivation"
)

# Filter dataframe to only keep those variables
df_filtered <- df %>%
  select(all_of(vars_to_keep))

# Explicitly convert variables
df_filtered <- df_filtered %>%
  mutate(
    # Numeric variables
    age = as.numeric(age),
    bmi = as.numeric(bmi),
    multimorbidity_score = as.numeric(multimorbidity_score),
    composite_sleep_score = as.numeric(composite_sleep_score),
    composite_activity_score = as.numeric(composite_activity_score),
    menstrual_cycle_length = as.numeric(menstrual_cycle_length),
    reg_menses = as.numeric(reg_menses),
      sum_phq9_gad7 = as.numeric(sum_phq9_gad7),
    # Factor variables
    income = factor(income),
    ethnicity = factor(ethnicity),
    smoke_tobacco_ever = factor(smoke_tobacco_ever),
      alcohol_use = factor(alcohol_use),
      social_interaction = factor(social_interaction),
    parental_history_mental_illness = factor(parental_history_mental_illness),
     pat_hist_mental = factor(pat_hist_mental), 
      mat_hist_mental = factor(mat_hist_mental),
    higher_ed = factor(higher_ed),
      physical_activity = factor(physical_activity),
    contraception_use = factor(contraception_use),
    contr_method = factor(contr_method), 
      reg_contr_method = factor(reg_contr_method), 
    vitamin_d = factor(vitamin_d), 
    vitamin_b = factor(vitamin_b), 
    calcium = factor(calcium),
  psychotropic_med = factor(psychotropic_med),
  geographic_region = factor(geographic_region),
    multiple_deprivation = factor(multiple_deprivation)
)


# Specify numeric and factor variables
num_vars <- c(
  "age", "bmi", "multimorbidity_score", "composite_sleep_score",
  "composite_activity_score", "menstrual_cycle_length",
   "sum_phq9_gad7"
)

factor_vars <- c(
  "ethnicity", "income", "smoke_tobacco_ever", "alcohol_use",
    "social_interaction", "parental_history_mental_illness",
    "pat_hist_mental", "mat_hist_mental",
  "higher_ed", "physical_activity", "contraception_use", "contr_method", "reg_contr_method", 
  "reg_menses", "vitamin_d", "vitamin_b", "calcium", "psychotropic_med", "geographic_region", "multiple_deprivation")

# Convert variables to correct types
df_filtered <- df_filtered %>%
  mutate(across(all_of(num_vars), as.numeric)) %>%
  mutate(across(all_of(factor_vars), as.factor))

# Get sample sizes for each population
sample_sizes <- df_filtered %>%
  group_by(population) %>%
  summarize(n = n(), .groups = "drop")

# Create column names with sample sizes
create_pop_colname <- function(pop, n) {
  paste0(pop, " (n=", n, ")")
}

# Summarize numeric variables with mean (sd) format
num_summary <- df_filtered %>%
  group_by(population) %>%
  summarize(across(all_of(num_vars),
                   ~paste0(round(mean(.x, na.rm = TRUE), 2), 
                          " (", round(sd(.x, na.rm = TRUE), 2), ")"),
                   .names = "{col}"),
            .groups = "drop") %>%
  # Add sample sizes to population names
  left_join(sample_sizes, by = "population") %>%
  mutate(population = create_pop_colname(population, n)) %>%
  select(-n) %>%
  # Transpose to have variables as rows and populations as columns
  pivot_longer(-population, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = population, values_from = value)

# Summarize factor variables with count (percent%) format
factor_summary <- df_filtered %>%
  select(population, all_of(factor_vars)) %>%
  pivot_longer(-population, names_to = "variable", values_to = "value") %>%
  # Convert to character to handle NAs properly, then back to factor with NAs last
  mutate(value_char = as.character(value)) %>%
  mutate(value_char = ifelse(is.na(value_char), "ZZZ_NA_ZZZ", value_char)) %>%  # Temporary placeholder for NAs
  group_by(population, variable, value_char) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(population, variable) %>%
  mutate(percent = 100 * count / sum(count),
         combined = paste0(count, " (", round(percent, 1), "%)")) %>%
  ungroup() %>%
  # Convert back the NA placeholder and create proper ordering
  mutate(value = ifelse(value_char == "ZZZ_NA_ZZZ", NA_character_, value_char)) %>%
  select(population, variable, value, combined) %>%
  # Add sample sizes to population names
  left_join(sample_sizes, by = "population") %>%
  mutate(population = create_pop_colname(population, n)) %>%
  select(-n) %>%
  # Keep variable and value as separate columns
  select(variable, value, population, combined) %>%
  # Transpose to have variable levels as rows and populations as columns
  pivot_wider(names_from = population, values_from = combined) %>%
  # Arrange to ensure NAs appear last within each variable group
  arrange(variable, is.na(value), value)

# Combine numeric and factor summaries into a single table
# Add a type column to distinguish between numeric and factor variables
num_summary_labeled <- num_summary %>%
  mutate(variable_type = "numeric", 
         value = NA_character_,
         .before = 1) %>%
  relocate(variable_type, variable, value)

factor_summary_labeled <- factor_summary %>%
  mutate(variable_type = "factor", .before = 1) %>%
  relocate(variable_type, variable, value)

# Combine the tables
combined_summary <- bind_rows(num_summary_labeled, factor_summary_labeled) %>%
  arrange(variable_type, variable, is.na(value), value)

# Display the final combined table
print("Combined Summary Table:")
combined_summary

write.csv(combined_summary, "Summary_variables_per_population.csv")
system(paste("dx upload Summary_variables_per_population.csv", "--destination", "/Outputs/Baseline/"), intern = TRUE)

all_vars <- c(
  "age","bmi","ethnicity", "income", "smoke_tobacco_ever", "alcohol_use",
    "social_interaction", "parental_history_mental_illness",
    "pat_hist_mental", "mat_hist_mental",
  "higher_ed","contraception_use","contr_method", "reg_contr_method",
  "multimorbidity_score","composite_sleep_score","composite_activity_score", "physical_activity",
  "menstrual_cycle_length",
  "reg_menses", "sum_phq9_gad7", "vitamin_d", "vitamin_b", "calcium", "psychotropic_med", "geographic_region", "multiple_deprivation"
)

missing_summary <- df_filtered %>%
  select(population, all_of(all_vars)) %>%
  # Convert all variables to character so pivot_longer works
  mutate(across(all_of(all_vars), as.character)) %>%
  pivot_longer(-population, names_to = "variable", values_to = "value") %>%
  group_by(population, variable) %>%
  summarize(
    n_non_missing = sum(!is.na(value) & value != ""),
    n_missing = sum(is.na(value) | value == ""),
    pct_missing = 100 * n_missing / (n_missing + n_non_missing),
    .groups = "drop"
  )

missing_summary
write.csv(missing_summary, "Missingness_summary.csv")
system(paste("dx upload Missingness_summary.csv", "--destination", "/Outputs/Baseline/"), intern = TRUE)

invisible(gc())
#Compare complete and incomplete cases: Summarise the basic characteristics 

create_summary_table <- function(data, vars_to_keep = NULL) {
  
  # Default variables if none provided
  if (is.null(vars_to_keep)) {
    vars_to_keep <- c("age", "ethnicity", "multiple_deprivation", "higher_ed", "bmi", "smoke_tobacco_ever", "alcohol_use",
       "social_interaction", "composite_sleep_score", "physical_activity",
      "contr_method", "psychotropic_med")
  }
  
  # Filter dataframe to only keep those variables
  df_filtered <- data %>%
    select(all_of(vars_to_keep))
  
  # Define numeric and factor variables based on the selected variables
  num_vars <- c()
  factor_vars <- c()
  
  # Check which variables exist in the filtered data and categorize them
  if ("age" %in% names(df_filtered)) num_vars <- c(num_vars, "age")
  if ("bmi" %in% names(df_filtered)) num_vars <- c(num_vars, "bmi")
  if ("composite_sleep_score" %in% names(df_filtered)) num_vars <- c(num_vars, "composite_sleep_score")
  if ("ethnicity" %in% names(df_filtered)) factor_vars <- c(factor_vars, "ethnicity")
  if ("smoke_tobacco_ever" %in% names(df_filtered)) factor_vars <- c(factor_vars, "smoke_tobacco_ever")
  if ("alcohol_use" %in% names(df_filtered)) factor_vars <- c(factor_vars, "alcohol_use")
  if ("physical_activity" %in% names(df_filtered)) factor_vars <- c(factor_vars, "physical_activity")
  if ("social_interaction" %in% names(df_filtered)) factor_vars <- c(factor_vars, "social_interaction")
  if ("higher_ed" %in% names(df_filtered)) factor_vars <- c(factor_vars, "higher_ed")
  if ("contr_method" %in% names(df_filtered)) factor_vars <- c(factor_vars, "contr_method")
  if ("psychotropic_med" %in% names(df_filtered)) factor_vars <- c(factor_vars, "psychotropic_med")
  if ("multiple_deprivation" %in% names(df_filtered)) factor_vars <- c(factor_vars, "multiple_deprivation")

  
  # Convert variables to correct types
  if (length(num_vars) > 0) {
    df_filtered <- df_filtered %>%
      mutate(across(all_of(num_vars), as.numeric))
  }
  
  if (length(factor_vars) > 0) {
    df_filtered <- df_filtered %>%
      mutate(across(all_of(factor_vars), as.factor))
  }
  
  # Get total sample size
  total_n <- nrow(df_filtered)
  
  # Initialize empty summaries
  num_summary <- NULL
  factor_summary <- NULL
  
  # Summarize numeric variables with mean (sd) format (only if numeric variables exist)
  if (length(num_vars) > 0) {
    num_summary <- df_filtered %>%
      summarize(across(all_of(num_vars),
                       ~paste0(round(mean(.x, na.rm = TRUE), 2), 
                              " (", round(sd(.x, na.rm = TRUE), 2), ")"),
                       .names = "{col}")) %>%
      # Transpose to have variables as rows
      pivot_longer(everything(), names_to = "variable", values_to = paste0("Summary"))
  }
  
  # Summarize factor variables with count (percent%) format (only if factor variables exist)
  if (length(factor_vars) > 0) {
    factor_summary <- df_filtered %>%
      select(all_of(factor_vars)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      # Remove NAs for each variable before calculating percentages
      filter(!is.na(value)) %>%
      group_by(variable, value) %>%
      summarize(count = n(), .groups = "drop") %>%
      group_by(variable) %>%
      mutate(percent = 100 * count / sum(count),
             !!paste0("Summary") := paste0(count, " (", round(percent, 1), "%)")) %>%
      ungroup() %>%
      select(variable, value, !!paste0("Summary")) %>%
      arrange(variable, value)
  }
  
  # Combine numeric and factor summaries into a single table
  combined_summary <- NULL
  
  if (!is.null(num_summary) && !is.null(factor_summary)) {
    # Both numeric and factor variables exist
    num_summary_labeled <- num_summary %>%
      mutate(variable_type = "numeric", 
             value = NA_character_,
             .before = 1) %>%
      relocate(variable_type, variable, value)
    
    factor_summary_labeled <- factor_summary %>%
      mutate(variable_type = "factor",
             value = as.character(value),
             .before = 1) %>%
      relocate(variable_type, variable, value)
    
    combined_summary <- bind_rows(num_summary_labeled, factor_summary_labeled) %>%
      arrange(variable_type, variable, is.na(value), value)
      
  } else if (!is.null(num_summary)) {
    # Only numeric variables exist
    combined_summary <- num_summary %>%
      mutate(variable_type = "numeric", 
             value = NA_character_,
             .before = 1) %>%
      relocate(variable_type, variable, value)
      
  } else if (!is.null(factor_summary)) {
    # Only factor variables exist
    combined_summary <- factor_summary %>%
      mutate(variable_type = "factor",
             value = as.character(value),
             .before = 1) %>%
      relocate(variable_type, variable, value)
  }
  combined_summary$N <-  total_n
  return(combined_summary)
}


vars_to_keep <- c("pid", "population", "age", "ethnicity", "multiple_deprivation", "higher_ed", "bmi", "smoke_tobacco_ever", "alcohol_use",
       "social_interaction", "composite_sleep_score", "physical_activity",
      "contr_method", "psychotropic_med")

cf_complete_incomplete <- function(df, population_name) {
 
  #Filter population
  subset <- df %>%
    filter(population == population_name) %>%
    select(pid,
      population, age, multiple_deprivation, higher_ed, ethnicity, composite_sleep_score,
      physical_activity, smoke_tobacco_ever, alcohol_use, social_interaction, bmi,
      contr_method, psychotropic_med) 
  
  # Calculate how many cases would be complete if we removed all NAs (for comparison)
  complete_cases_count <- subset %>% na.omit() %>% nrow()
  total_cases <- nrow(subset)
  cat("Complete cases if using na.omit():", complete_cases_count, "\n")
  cat("Total cases:", total_cases, "\n")
  cat("Percentage complete if using na.omit():", round(complete_cases_count/total_cases * 100, 2), "%\n")
  
  # Compare complete vs incomplete cases:
  complete <- subset %>% na.omit()
  diff <- subset$pid[!subset$pid %in% complete$pid]
  incomplete <- subset[subset$pid %in% diff, ]
  
  vars_to_keep <- c("pid", "population", "age", "ethnicity", 
    "multiple_deprivation", "higher_ed", "bmi", "smoke_tobacco_ever", "alcohol_use",
    "social_interaction", "composite_sleep_score", "physical_activity",
    "contr_method", "psychotropic_med")
  
  summary_complete <- create_summary_table(complete, vars_to_keep = vars_to_keep)
  summary_incomplete <- create_summary_table(incomplete, vars_to_keep = vars_to_keep)
  
  # Add identifiers for population and completeness status
  summary_complete <- summary_complete %>%
    mutate(
      Population = population_name,
      Status = "Complete",
      .before = 1
    )
  
  summary_incomplete <- summary_incomplete %>%
    mutate(
      Population = population_name,
      Status = "Incomplete",
      .before = 1
    )
  
  # Return both summaries as a list
  return(list(complete = summary_complete, incomplete = summary_incomplete))
}

# Run for all populations and collect results
results_pmdd <- cf_complete_incomplete(df, "PMDD")
results_mdd <- cf_complete_incomplete(df, "MDD")
results_ctrl <- cf_complete_incomplete(df, "CTRL")

# Combine all results into a single dataframe
combined_summary <- bind_rows(
  results_pmdd$complete,
  results_pmdd$incomplete,
  results_mdd$complete,
  results_mdd$incomplete,
  results_ctrl$complete,
  results_ctrl$incomplete
)

# Write combined file
combined_file <- "Complete_incomplete_summary.csv"
write.csv(combined_summary, combined_file, row.names = FALSE)

# Upload combined file
system(paste("dx upload", shQuote(combined_file), "--destination /Outputs/Baseline/"), intern = TRUE)

