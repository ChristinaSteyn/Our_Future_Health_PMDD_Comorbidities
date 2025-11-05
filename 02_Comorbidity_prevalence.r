#Comorbidities of PMDD - script to:
#1. Determine the prevalence of various medical conditions in females with PMDD, MDD, 
#and Controls with no hisotry of psychiatric illness
#2. Calculate and plot the distributions for multimorbidity scores in each population (PMDD, MDD, Control)

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

rm(fem_PMDD_ID)
rm(fem_CTRL_ID)
rm(fem_MDD_ID)

gc()

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
system(paste0("dx upload Broad_category_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)


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
system("dx upload Complications_pregnancy_prevalence.csv --destination /PMDD_outputs/Comorbidities/", intern=TRUE)

          
#Note kernel kept crashing until I ran this without running the previous two cells
# Endocrine, nutritional and metabolic disorders
invisible(gc())

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
system(paste0("dx upload Endocrine_metabolic_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)

invisible(gc())

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
system(paste0("dx upload Mental_health_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)

invisible(gc())

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
system(paste0("dx upload Neurodevelopmental_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)
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
system(paste0("dx upload Reproductive_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)
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
system(paste0("dx upload Autoimmune_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)


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
system(paste0("dx upload Autoimmune_comorbidities_with_other.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)
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
system(paste0("dx upload Kidney_or_urinary_cormorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)
invisible(gc())

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
system(paste0("dx upload Cardiovascular_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)

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
system(paste0("dx upload Respiratory_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)
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
system(paste0("dx upload Digestive_liver_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)
invisible(gc())

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
system(paste0("dx upload Neuro_comorbidities.csv --destination /PMDD_outputs/Comorbidities/"), intern=TRUE)
invisible(gc())

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
system("dx upload Mean_multimorbidity_score.csv --destination /PMDD_outputs/Comorbidities/", intern=TRUE)
invisible(gc())

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

                   
system("dx upload Distribution_of_Multimorbidity_Scores_Boxplot.png --destination /PMDD_outputs/Comorbidities/", intern=TRUE)
           