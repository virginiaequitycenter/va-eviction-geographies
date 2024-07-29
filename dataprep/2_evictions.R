# Clean eviction data and prepare for analysis

# Setup ----
library(tidyverse)
#remotes::install_github("virginiaequitycenter/ECtools")
library(ECtools)

# Read eviction data
evictions <- read.delim("data/cases_residential_only.txt", sep = ",")

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

# Explore judgments: ----
# judgment                                n
# <chr>                               <int>
#   1 ""                                   5773
# 2 "Case Dismissed"                   227577
# 3 "Case Dismissed with prejudice"       801
# 4 "Case Dismissed without prejudice"   2408
# 5 "Defendant"                           848
# 6 "Non-suit"                          69405
# 7 "Not Found/Unserved"                 4505
# 8 "Other"                              5948
# 9 "Plaintiff"                        349681
# 10 "Transfer/Change of Venue"             89
# 11 "null without prejudice"                3

# Questions:
# What should we do when there is no judgement? 
# What about when the judgement is other? 
# Do we care if a case was dismissed w/ or w/o prejudice? 

# Temp solution:
evictions <- evictions %>%
  mutate(
    default = case_when(
      grepl("Default", latest_hearing_result) ~ TRUE,
      TRUE ~ FALSE),
    judgment_clean = case_when(
      grepl("Case|Non-suit|Unserved|null", judgment) ~ "Case Dismissed",
      grepl("Defendant", judgment) ~ "Defendant",
      grepl("Plaintiff", judgment) ~ "Plaintiff",
      grepl("Other|Transfer", judgment) ~ "Other",
      TRUE ~ "None"))

# Summarize by geography ----
summarize_evictions <- function(df, grouper) {
  
  #Total cases filed per group
  total_filed <- df %>% 
    group_by({{ grouper }}) %>%
    count() %>%
    rename(total_filed = n)
  assign("total_filed", total_filed, .GlobalEnv)
  
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
    left_join(total_plaintiff_won) %>%
    left_join(total_immediate) %>%
    left_join(median_principal) %>%
    left_join(n_d_attorney) %>%
    left_join(cases_plaintiff_business)
  assign("evictions_summary", evictions_summary, .GlobalEnv)
}

# Zip code ----
summarize_evictions(evictions, defendant_zip)

write_csv(evictions_summary, "data/evictions_zip.csv")

rm(total_filed, total_default, total_plaintiff_won, total_immediate, median_principal, n_d_attorney, cases_plaintiff_business)

# County ----
summarize_evictions(evictions, locality)

write_csv(evictions_summary, "data/evictions_county.csv")

rm(total_filed, total_default, total_plaintiff_won, total_immediate, median_principal, n_d_attorney, cases_plaintiff_business)

# Legal Aid Service Area ----
summarize_evictions(evictions, legal_aid_service_area)

write_csv(evictions_summary, "data/evictions_servicearea.csv")

