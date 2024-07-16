
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

# Summarize by zip ----
  
# Derive some variables: 
total_filed <- evictions %>% 
  group_by(defendant_zip) %>%
  count() %>%
  rename(total_filed = n)

total_default <- evictions %>% 
  filter(default == TRUE) %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(total_default = n)

total_plaintiff_won <- evictions %>% 
  filter(judgment_clean == "Plaintiff") %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(total_plaintiff_won = n)

total_immediate <- evictions %>%
  filter(possession == "Immediate") %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(total_immediate = n)

median_principal <- evictions %>%
  group_by(defendant_zip) %>%
  summarise(median_principal = median(principal_amount, na.rm = TRUE))

# Number of cases where the defendant had an attorney 
n_d_attorney <- evictions %>%
  filter(d_attorney_present == TRUE) %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(n_d_attorney = n)

# Number of cases with non-person evictors 
cases_plaintiff_business <- evictions %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(cases_plaintiff_business = n)

# Question: how should we handle NA values? Convert to zero?

# Join by zip geography:
evictions_zip <- total_filed %>%
  left_join(total_default) %>%
  left_join(total_plaintiff_won) %>%
  left_join(total_immediate) %>%
  left_join(median_principal) %>%
  left_join(n_d_attorney) %>%
  left_join(cases_plaintiff_business)

write_csv(evictions_zip, "data/evictions_zip.csv")

# Summarize by county ----

# Derive some variables: 
total_filed_c <- evictions %>% 
  group_by(locality) %>%
  count() %>%
  rename(total_filed_c = n)

total_default_c <- evictions %>% 
  filter(default == TRUE) %>%
  group_by(locality) %>%
  count() %>%
  rename(total_default_c = n)

total_plaintiff_won_c <- evictions %>% 
  filter(judgment_clean == "Plaintiff") %>%
  group_by(locality) %>%
  count() %>%
  rename(total_plaintiff_won_c = n)

total_immediate_c <- evictions %>%
  filter(possession == "Immediate") %>%
  group_by(locality) %>%
  count() %>%
  rename(total_immediate_c = n)

median_principal_c <- evictions %>%
  group_by(locality) %>%
  summarise(median_principal_c = median(principal_amount, na.rm = TRUE))

# Number of cases where the defendant had an attorney 
n_d_attorney_c <- evictions %>%
  filter(d_attorney_present == TRUE) %>%
  group_by(locality) %>%
  count() %>%
  rename(n_d_attorney_c = n)

# Number of cases with non-person evictors 
cases_plaintiff_business_c <- evictions %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(locality) %>%
  count() %>%
  rename(cases_plaintiff_business_c = n)

# Question: how should we handle NA values? Convert to zero?

# Join by county geography:
evictions_county <- total_filed_c %>%
  left_join(total_default_c) %>%
  left_join(total_plaintiff_won_c) %>%
  left_join(total_immediate_c) %>%
  left_join(median_principal_c) %>%
  left_join(n_d_attorney_c) %>%
  left_join(cases_plaintiff_business_c)

write_csv(evictions_county, "data/evictions_county.csv")

# Summarize by legal aid service area ----

# Derive some variables: 
total_filed_sa <- evictions %>% 
  group_by(legal_aid_service_area) %>%
  count() %>%
  rename(total_filed_sa = n)

total_default_sa <- evictions %>% 
  filter(default == TRUE) %>%
  group_by(legal_aid_service_area) %>%
  count() %>%
  rename(total_default_sa = n)

total_plaintiff_won_sa <- evictions %>% 
  filter(judgment_clean == "Plaintiff") %>%
  group_by(legal_aid_service_area) %>%
  count() %>%
  rename(total_plaintiff_won_sa = n)

total_immediate_sa <- evictions %>%
  filter(possession == "Immediate") %>%
  group_by(legal_aid_service_area) %>%
  count() %>%
  rename(total_immediate_sa = n)

median_principal_sa <- evictions %>%
  group_by(legal_aid_service_area) %>%
  summarise(median_principal_sa = median(principal_amount, na.rm = TRUE))

# Number of cases where the defendant had an attorney 
n_d_attorney_sa <- evictions %>%
  filter(d_attorney_present == TRUE) %>%
  group_by(legal_aid_service_area) %>%
  count() %>%
  rename(n_d_attorney_sa = n)

# Number of cases with non-person evictors 
cases_plaintiff_business_sa <- evictions %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(legal_aid_service_area) %>%
  count() %>%
  rename(cases_plaintiff_business_sa = n)

# Question: how should we handle NA values? Convert to zero?

# Join by legal aid service area geography:
evictions_servicearea <- total_filed_sa %>%
  left_join(total_default_sa) %>%
  left_join(total_plaintiff_won_sa) %>%
  left_join(total_immediate_sa) %>%
  left_join(median_principal_sa) %>%
  left_join(n_d_attorney_sa) %>%
  left_join(cases_plaintiff_business_sa)

write_csv(evictions_servicearea, "data/evictions_servicearea.csv")

