##############################################################
## SUBTYPE STATS: DATA PREPROCESSING & DESCRIPTIVE ANALYSIS ##
##############################################################

## SET WD ##

## LOAD LIBRARY ##
library(dplyr)

## READ-IN DATA & PREVIEW ##
df_raw <- read.csv("Subtype Stats_v.1_For R.csv", header = TRUE)
head(df_raw)
colnames(df_raw)
nrow(df_raw)

## CONVERT SUBTYPE PREVALENCE INTO DECIMAL NUMBERS ##
df_raw$Subtype_Prevalence <- as.numeric(gsub("%", "", df_raw$Subtype_Prevalence)) / 100

## SPECIFY VARIABLE TYPE ##
df_raw$Item_No <- as.factor(df_raw$Item_No)
df_raw$Entry_No <- as.factor(df_raw$Entry_No)
df_raw$TheoryName <- as.factor(df_raw$TheoryName)
df_raw$Subtypes_N <- as.numeric(df_raw$Subtypes_N)
df_raw$Subtypes_No. <- as.factor(df_raw$Subtypes_No.)
df_raw$Subtype_Label <- as.factor(df_raw$Subtype_Label)
df_raw$Subtype_UniLabel <- as.factor(df_raw$Subtype_UniLabel)
df_raw$Subtype_Prevalence <- as.numeric(df_raw$Subtype_Prevalence)

## REMOVE ROWS WITH SUBTYPE LABEL "Not applicable"
df <- df_raw %>%
  filter(!grepl("Not applicable", Subtype_UniLabel))
nrow(df) # Inspect how many were dropped

# class(df_raw$Subtype_Prevalence) # For inspection

## BASIC DESCRIPTIVE STATS ##

# No. of subtyping results (n = 66)
length(unique(df$Entry_No))

# No. of publications were the results from (n = 55)
length(unique(df$Item_No)) 

# No. of unique subtyping theories (n = 20)
length(unique(df$TheoryName)) 

# No. of unique subtypes (n = 81)
length(unique(df$Subtype_UniLabel)) 

## SUBTYPES & PREVALENCE ##
freq_subtype <- as.data.frame(table(df$Subtype_UniLabel))
freq_subtype_sorted <- freq_subtype[order(-freq_subtype$Freq), ]
head(freq_subtype_sorted, 10) # Inspect

# Custom function to calculate mean, standard deviation, and range
summary_stats <- function(x) {
  count_value <- length(x)
  mean_value <- mean(x)
  sd_value <- sd(x)
  max_value <- max(x)
  min_value <- min(x)
  return(c(mean = mean_value, sd = sd_value, max = max_value, min = min_value, count = count_value))
}

# Average prevalence per unique subtype, despite the theory models
subtype_prev_avg <- as.data.frame(aggregate(Subtype_Prevalence ~ Subtype_UniLabel, data = df, FUN = summary_stats))
subtype_prev_avg <- subtype_prev_avg[order(-subtype_prev_avg$Subtype_Prevalence), ] # Sort by largest values
subtype_prev_avg

# Average prevalence per unique subtype, grouped by the subtyping theory
subtype_prev_avg_t <- as.data.frame(aggregate(Subtype_Prevalence ~ TheoryName + Subtype_UniLabel, data = df, FUN = summary_stats))
subtype_prev_avg_t <- subtype_prev_avg_t[order(subtype_prev_avg_t$TheoryName, -(subtype_prev_avg_t$Subtype_Prevalence)), ] # Sort by theory names
subtype_prev_avg_t

# Average prevalence per unique subtype, grouped by the subtype, then theory
subtype_prev_avg_s <- as.data.frame(aggregate(Subtype_Prevalence ~ Subtype_UniLabel + TheoryName, data = df, FUN = summary_stats))
subtype_prev_avg_s <- subtype_prev_avg_s[order(subtype_prev_avg_s$Subtype_UniLabel, -(subtype_prev_avg_s$Subtype_Prevalence)), ] # Sort by theory names
subtype_prev_avg_s
