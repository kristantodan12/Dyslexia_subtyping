library(readxl)
library(dplyr)

#######################################
###Reading the data from excel files###
#######################################
# Define the file path
file_path <- "Data/DD Subtyping_DAF_v.3.xlsx"

# Read the description data
description <- read_excel(file_path, sheet = "Description (Clean_For APP)")

# Read the article data
article_data <- read_excel(file_path, sheet = "RealDF (Clean_For APP) 2")

# Convert Year column to integer
article_data <- article_data %>%
    mutate(Year = as.integer(Year))

# Make DOI column clickable
article_data <- article_data %>%
    mutate(DOI = paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'))

### Adding columns to combine all values that have multiple columns
article_data <- article_data %>%
    rowwise() %>%
    mutate(
        Construct_All = paste(na.omit(c_across(matches("^Para.*Construct$"))[!c_across(matches("^Para.*Construct$")) %in% c("Not applicable")]), collapse = "; "),
        Measure_All = paste(na.omit(c_across(matches("^Para.*Measure$"))[!c_across(matches("^Para.*Measure$")) %in% c("Not applicable")]), collapse = "; "),
        Var_All = paste(na.omit(c_across(matches("^Para.*Var$"))[!c_across(matches("^Para.*Var$")) %in% c("Not applicable")]), collapse = "; "),
        Sample_ForValidation_All = Sample_ForValidation_All <- paste(na.omit(c_across(starts_with("Sample_ForValidation_"))[!c_across(starts_with("Sample_ForValidation_")) %in% c("Not applicable")]), collapse = "; "),
        Standardised_All = paste(na.omit(c_across(matches("^Para.*Standardised$"))[!c_across(matches("^Para.*Standardised$")) %in% c("Not applicable")]), collapse = "; "),
        Indicator_All = paste(na.omit(c_across(matches("^Para.*Indicator$"))[!c_across(matches("^Para.*Indicator$")) %in% c("Not applicable")]), collapse = "; "),
        OutlierRemoval_All = paste(na.omit(c_across(matches("^Para.*OutlierRemoval$"))[!c_across(matches("^Para.*OutlierRemoval$")) %in% c("Not applicable")]), collapse = "; "),
        MissingDataTreat_All = paste(na.omit(c_across(matches("^Para.*MissingDataTreat$"))[!c_across(matches("^Para.*MissingDataTreat$")) %in% c("Not applicable")]), collapse = "; "),
        Validation_Method_All = Sample_ForValidation_All <- paste(na.omit(c_across(starts_with("Validation_Method_"))[!c_across(starts_with("Validation_Method_")) %in% c("Not applicable")]), collapse = "; "),
        Validation_SameSample_All = Sample_ForValidation_All <- paste(na.omit(c_across(starts_with("Validation_SameSample_"))[!c_across(starts_with("Validation_SameSample_")) %in% c("Not applicable")]), collapse = "; "),
        Robustness_All = Sample_ForValidation_All <- paste(na.omit(c_across(starts_with("Robustness_"))[!c_across(starts_with("Robustness_")) %in% c("Not applicable")]), collapse = "; "),
        UnifiedLabel_All = paste(na.omit(c_across(matches("^Subtype.*UniLabel$"))[!c_across(matches("^Subtype.*UniLabel$")) %in% c("Not applicable")]), collapse = "; "),
        Prevalence_All = paste(na.omit(sprintf("%.2f", as.numeric(c_across(matches("^Subtype.*Prevalence$"))[!c_across(matches("^Subtype.*Prevalence$")) %in% c("Not applicable")]))), collapse = "; ")
    ) %>%
    ungroup() %>%
    mutate(
        Construct_All = ifelse(Construct_All == "", "Not applicable", Construct_All),
        Measure_All = ifelse(Measure_All == "", "Not applicable", Measure_All),
        Var_All = ifelse(Var_All == "", "Not applicable", Var_All),
        Sample_ForValidation_All = ifelse(Sample_ForValidation_All == "", "Not applicable", Sample_ForValidation_All),
        Standardised_All = ifelse(Standardised_All == "", "Not applicable", Standardised_All),
        Indicator_All = ifelse(Indicator_All == "", "Not applicable", Indicator_All),
        OutlierRemoval_All = ifelse(OutlierRemoval_All == "", "Not applicable", OutlierRemoval_All),
        MissingDataTreat_All = ifelse(MissingDataTreat_All == "", "Not applicable", MissingDataTreat_All),
        Validation_Method_All = ifelse(Validation_Method_All == "", "Not applicable", Validation_Method_All),
        Validation_SameSample_All = ifelse(Validation_SameSample_All == "", "Not applicable", Validation_SameSample_All),
        Robustness_All = ifelse(Robustness_All == "", "Not applicable", Robustness_All),
        UnifiedLabel_All = ifelse(UnifiedLabel_All == "", "Not applicable", UnifiedLabel_All),
        Prevalence_All = ifelse(Prevalence_All == "", "Not applicable", Prevalence_All)
    )






#######################################
###The Multiverse###
#######################################

### Extracting important variables and the unique values
unique_theories <- unique(article_data$TheoryName)
unique_languages <- unique(article_data$Subj_Lang)
unique_rep_pop <- unique(article_data$Rep_Pop)
unique_dys_severity <- unique(article_data$Dys_Severity)
unique_comorbidity <- unique(article_data$Comorbidity)
unique_total_n <- unique(article_data$Total_N)
unique_deficit_cutoff_inSD <- unique(article_data$Deficit_Cutoff_inSD)
unique_normal_cutoff_inSD <- unique(article_data$Normal_Cutoff_inSD)
unique_description_cutoff <- unique(article_data$Description_CutOff_Criteria)
unique_software_preprocess <- unique(article_data$Software_PreProcess)
unique_sample_forclustering <- unique(article_data$Sample_ForClustering)
unique_results_validation <- unique(article_data$Results_Validated)
unique_construct_all <- unique(article_data$Construct_All)
unique_standardised_all <- unique(article_data$Standardised_All)
unique_indicator_all <- unique(article_data$Indicator_All)
unique_outlier_removal_all <- unique(article_data$OutlierRemoval_All)
unique_missing_data_treat_all <- unique(article_data$MissingDataTreat_All)
unique_unified_label_all <- unique(article_data$UnifiedLabel_All)
unique_prevalence_all <- unique(article_data$Prevalence_All)

unique_construct <- unique(unlist(article_data %>% select(matches("^Para.*Construct$"))))
unique_measure <- unique(unlist(article_data %>% select(matches("^Para.*Measure$"))))
unique_var <- unique(unlist(article_data %>% select(matches("^Para.*Var$"))))
unique_standardised <- unique(unlist(article_data %>% select(matches("^Para.*Standardised$"))))
unique_indicator <- unique(unlist(article_data %>% select(matches("^Para.*Indicator$"))))
unique_outlier_removal <- unique(unlist(article_data %>% select(matches("^Para.*OutlierRemoval$"))))
unique_missing_data_treat <- unique(unlist(article_data %>% select(matches("^Para.*MissingDataTreat$"))))
unique_subtypes <- unique(unlist(article_data %>% select(matches("^Subtype.*UniLabel$"))))
unique_description <- unique(unlist(article_data %>% select(matches("^Subtype.*Description$"))))
unique_prevalence <- unique(unlist(article_data %>% select(matches("^Subtype.*Prevalence$"))))
unique_subtype_ori <- unique(unlist(article_data %>% select(matches("^Subtype[1-8]$"))))
unique_validation_method <- unique(unlist(article_data %>% select(matches("^Validation_Method_[1-2]$"))))
unique_validation_samesample <- unique(unlist(article_data %>% select(matches("^Validation_SameSample_[1-2]$"))))
unique_robustness <- unique(unlist(article_data %>% select(matches("^Robustness_[1-8]$"))))





#######################################
###Additional Analyses###
#######################################

### Theory vs Subtype ###
# Create a matrix to store counts
count_matrix_theory <- matrix(0, nrow = length(unique_theories), ncol = length(unique_subtypes),
                       dimnames = list(unique_theories, unique_subtypes))

# Populate the matrix with counts
for (theory in unique_theories) {
  for (subtype in unique_subtypes) {
    count_matrix_theory[theory, subtype] <- sum(article_data$TheoryName == theory & 
                                         apply(article_data %>% select(matches("^Subtype.*UniLabel$")), 1, function(x) subtype %in% x))
  }
}

# Convert matrix to data frame for ggplot2
count_df_theory <- as.data.frame(as.table(count_matrix_theory))
colnames(count_df_theory) <- c("TheoryName", "Subtype_UniLabel", "Count")

# Convert Subtype_UniLabel to a factor with levels ordered alphabetically
count_df_theory$Subtype_UniLabel <- factor(count_df_theory$Subtype_UniLabel, levels = sort(unique(count_df_theory$Subtype_UniLabel)))

# Reorder TheoryName and Subtype_UniLabel alphabetically
count_df_theory <- count_df_theory[order(count_df_theory$TheoryName, count_df_theory$Subtype_UniLabel), ]

### Language vs Subtype ###
# Unique language
unique_languages <- unique(article_data$Subj_Lang)

# Create a matrix to store counts
count_matrix_language <- matrix(0, nrow = length(unique_languages), ncol = length(unique_subtypes),
                       dimnames = list(unique_languages, unique_subtypes))

# Populate the matrix with counts
for (language in unique_languages) {
  for (subtype in unique_subtypes) {
    count_matrix_language[language, subtype] <- sum(article_data$Subj_Lang == language & 
                                         apply(article_data %>% select(matches("^Subtype.*UniLabel$")), 1, function(x) subtype %in% x))
  }
}

# Convert matrix to data frame for ggplot2
count_df_language <- as.data.frame(as.table(count_matrix_language))
colnames(count_df_language) <- c("Subj_Lang", "Subtype_UniLabel", "Count")

# Convert Subtype_UniLabel to a factor with levels ordered alphabetically
count_df_language$Subtype_UniLabel <- factor(count_df_language$Subtype_UniLabel, levels = sort(unique(count_df_language$Subtype_UniLabel)))

# Reorder Subj_Lang and Subtype_UniLabel alphabetically
count_df_language <- count_df_language[order(count_df_language$Subj_Lang, count_df_language$Subtype_UniLabel), ]

### Subtype and Prevalence ### 


