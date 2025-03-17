# Clean eviction data and prepare for analysis

# Setup ----
library(tidyverse)
#remotes::install_github("virginiaequitycenter/ECtools")
library(ECtools)

# Download the data using the process outlined in https://github.com/virginiaequitycenter/va-evictions
# Once the data is downloaded, copy the cases_residential_only.txt file to the /data folder in this repository
# Dates: 1/1/2018 - 12/24/2024

# Read eviction data
evictions <- read.delim("data/cases_residential_only.txt", sep = ",") %>%
  mutate(fips = as.character(formatC(fips, width = 3, format = "d", flag = "0")))

# Read service areas
legal_aid_service_areas <- read_csv("data/legal_aid_service_areas.csv") %>%
  select(-geometry)

# Join evictions with service areas
evictions <- evictions %>%
  left_join(legal_aid_service_areas, by = join_by(fips == fips))

# Deal with missing service areas (Richmond & Newport News)
evictions <- evictions %>%
  mutate(legal_aid_service_area = case_when(
    county == "Richmond City General District Court" ~ "Central Virginia Legal Aid Society",
    county == "Newport News-Civil General District Court" ~ "Legal Aid Society of Eastern Virginia",
    TRUE ~ legal_aid_service_area
  ))

# Identify if the defendant had an attorney present ----
evictions$defendant_attorney <- na_if(evictions$defendant_attorney, "")

evictions <- evictions %>%
  mutate(
    d_attorney_present = case_when(
      defendant_attorney == "NONE" | defendant_attorney == "SELF-REPRESENTED" | is.na(defendant_attorney) ~ FALSE,
      TRUE ~ TRUE),
    # Calculate principal amount
    principal_amount = parse_number(principal_amount))

# Identify non-residential plaintiffs ----
# Need to move to original script
evictions$plaintiff_non_residential <- identify_non_residential(evictions$plaintiff_name)

# Random spot checks:
# s1 <- sample_n(evictions, 20) %>%
#   select(plaintiff_name, clean_party_name, plaintiff_non_residential)
# s2 <- sample_n(evictions, 20) %>%
#   select(plaintiff_name, clean_party_name, plaintiff_non_residential)
# s3 <- sample_n(evictions, 20) %>%
#   select(plaintiff_name, clean_party_name, plaintiff_non_residential)

# Explore dispositions: ----
# Cases that went to judgment step:
# disposition           n
# <chr>             <int>
# 1 Default Judgment 297848
# 2 Judgment         120954
# 3 Other            372965
# 4 NA                13003

evictions <- evictions %>%
  mutate(to_judgment = case_when(
    grepl("Judgment", disposition) ~ TRUE,
    TRUE ~ FALSE
  ))

# Types of judgments:
# judgment                                n
# <chr>                                 <int>
# 1 ""                                  15386     NA
# 2 "Case Dismissed"                   268080.    Dismissed
# 3 "Case Dismissed with prejudice"      2282.    Dismissed
# 4 "Case Dismissed without prejudice"   6521.    Dismissed
# 5 "Defendant"                           979.    Defendant
# 6 "Non-suit"                          84406.    NonSuit
# 7 "Not Found/Unserved"                 5069.    Unserved
# 8 "Other"                              6367.    Other
# 9 "Plaintiff"                        415576.    Plaintiff
# 10 "Transfer/Change of Venue"             98.   Other
# 11 "null with prejudice"                   1.   Dismissed
# 12 "null without prejudice"                5.   Dismissed 
 

# Temp solution:
evictions <- evictions %>%
  mutate(
    default = case_when(
      grepl("Default", disposition) ~ TRUE,
      TRUE ~ FALSE),
    judgment_clean = case_when(
      grepl("Dismissed|null", judgment) ~ "Case Dismissed",
      grepl("Non-suit", judgment) ~ "Non-suit",
      grepl("Unserved", judgment) ~ "Unserved",
      grepl("Defendant", judgment) ~ "Defendant",
      grepl("Plaintiff", judgment) ~ "Plaintiff",
      grepl("Other|Transfer", judgment) ~ "Other",
      TRUE ~ "None"))

# Summarize by geography ----
summarize_evictions <- function(df, grouper) {
  
  #Total cases filed 
  total_filed <- df %>% 
    group_by({{ grouper }}) %>%
    count() %>%
    rename(total_filed = n)
  assign("total_filed", total_filed, .GlobalEnv)
  
  #Total cases that went to judgment 
  total_judgment <- df %>%
    filter(to_judgment == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(total_judgment = n)
  assign("total_judgment", total_judgment, .GlobalEnv)
    
  #Total default cases per group
  total_default <- df %>% 
    filter(default == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(total_default = n)
  assign("total_default", total_default, .GlobalEnv)
  
  #Total plaintiff won cases per group
  total_plaintiff_won <- df %>% 
    filter(judgment_clean == "Plaintiff") %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(total_plaintiff_won = n)
  assign("total_plaintiff_won", total_plaintiff_won, .GlobalEnv)
  
  #Total cases with immediate posession per group 
  total_immediate <- df %>%
    filter(possession == "Immediate") %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(total_immediate = n)
  assign("total_immediate", total_immediate, .GlobalEnv)
  
  #Median principal amount per group 
  median_principal <- df %>%
    group_by({{ grouper }}) %>%
    summarise(median_principal = median(principal_amount, na.rm = TRUE))
  assign("median_principal", median_principal, .GlobalEnv)
  
  #Number of cases where the defendant had an attorney 
  n_d_attorney <- df %>%
    filter(d_attorney_present == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_d_attorney = n)
  assign("n_d_attorney", n_d_attorney, .GlobalEnv)
  
  #Number of cases with non-person evictors 
  cases_plaintiff_business <- df %>%
    filter(plaintiff_non_residential == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(cases_plaintiff_business = n)
  assign("cases_plaintiff_business", cases_plaintiff_business, .GlobalEnv)
  
  # Join by geography:
  evictions_summary <- total_filed %>%
    left_join(total_default) %>%
    left_join(total_judgment) %>%
    left_join(total_plaintiff_won) %>%
    left_join(total_immediate) %>%
    left_join(median_principal) %>%
    left_join(n_d_attorney) %>%
    left_join(cases_plaintiff_business)
  assign("evictions_summary", evictions_summary, .GlobalEnv)
  
  # Replace NA values with 0
  evictions_summary <- evictions_summary %>%
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )
  assign("evictions_summary", evictions_summary, .GlobalEnv)
}

# Zip code ----
summarize_evictions(evictions, defendant_zip)

write_csv(evictions_summary, "data/evictions_zip.csv")

rm(total_filed, total_judgment, total_default, total_plaintiff_won, total_immediate, 
   median_principal, n_d_attorney, cases_plaintiff_business)

# County ----
summarize_evictions(evictions, fips)

write_csv(evictions_summary, "data/evictions_county.csv")

rm(total_filed, total_judgment, total_default, total_plaintiff_won, total_immediate, 
   median_principal, n_d_attorney, cases_plaintiff_business)

# Legal Aid Service Area ----
summarize_evictions(evictions, legal_aid_service_area)

write_csv(evictions_summary, "data/evictions_servicearea.csv")
