# Clean eviction data and prepare for analysis

# Setup ----
library(sf)
library(tidyverse)
#remotes::install_github("virginiaequitycenter/ECtools")
library(ECtools)

# Download the data using the process outlined in https://github.com/virginiaequitycenter/va-evictions
# Once the data is downloaded, copy the cases_residential_only.txt file to the /data folder in this repository
# Dates: 1/1/2018 - 12/24/2024

# Read eviction data
evictions <- read.delim("data/cases_residential_only.txt", sep = ",") %>%
  mutate(fips = as.character(formatC(fips, width = 3, format = "d", flag = "0"))) %>%
  rename(court = county) #774693 obs

# Read service areas
service_areas <- read_csv("data/service_areas.csv") 

# Join evictions with service areas
evictions <- evictions %>%
  left_join(service_areas, by = join_by(fips == fips))

# Explore missing data ----
# test <- evictions %>%
#   select(c2dp_case_id, court, fips, locality, legal_aid_service_area)
# missing <- test %>% filter_all(any_vars(is.na(.)))
# missing %>% select(court) %>% unique()

# Issues:
# - 703 is not a valid fips --> needs to be 700 (Newport News City)
# - 763 is not a valid fips --> needs to be 760 (Richmond City)
# - Richmond City General District Court is in Richmond City (locality) --> Central Virginia Legal Aid Society 
# - Newport News-Civil General District Court is in Newport News City --> Legal Aid Society of Eastern Virginia

# Deal with Richmond and Newport News issues
evictions <- evictions %>%
  mutate(
    fips = case_when(
      fips == "703" ~ "700",
      fips == "763" ~ "760",
      TRUE ~ fips),
    locality = case_when(
      court == "Richmond City General District Court" ~ "Richmond City",
      court == "Newport News-Civil General District Court" ~ "Newport News City",
      TRUE ~ locality),
    legal_aid_service_area = case_when(
      court == "Richmond City General District Court" ~ "Central Virginia Legal Aid Society",
      court == "Newport News-Civil General District Court" ~ "Legal Aid Society of Eastern Virginia",
      TRUE ~ legal_aid_service_area)
    )

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
# (Note this takes ~5 minutes)
evictions$plaintiff_non_residential <- identify_non_residential(evictions$plaintiff_name)

# Random spot checks:
# s1 <- sample_n(evictions, 20) %>%
#   select(plaintiff_name, clean_party_name, plaintiff_non_residential)
# s2 <- sample_n(evictions, 20) %>%
#   select(plaintiff_name, clean_party_name, plaintiff_non_residential)
# s3 <- sample_n(evictions, 20) %>%
#   select(plaintiff_name, clean_party_name, plaintiff_non_residential)

# Explore outcomes ----
# Cases that went to judgment step:
count(evictions, disposition)

evictions <- evictions %>%
  mutate(to_judgment = case_when(
    grepl("Judgment", disposition) ~ TRUE,
    TRUE ~ FALSE))

# Types of judgments, especially Defaults: 
count(evictions, judgment)

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

# Save ----
# Keep only variables used in app 
evictions <- evictions %>%
  select(case_number, plaintiff_name, defendant_name, case_type, principal_amount, 
         serial_filing, plaintiff_non_residential, d_attorney_present, disposition, 
         judgment = judgment_clean, to_judgment, possession, default, filed_date, 
         filed_year, defendant_zip, court, fips, locality, legal_aid_service_area)

write_csv(evictions, "data/evictions_clean.csv")

# Segment by year ----
evictions <- read_csv("data/evictions_clean.csv") #774693 obs

# 2018-2019 (pre-COVID)
evictions1819 <- evictions %>%
  filter(filed_year <= 2019 & filed_year >= 2018) #319031 obs

# 2020-2021 (COVID)
evictions2021 <- evictions %>%
  filter(filed_year <= 2021 & filed_year >= 2020) #109057 obs

# 2022-2023 (post-COVID)
evictions2223 <- evictions %>%
  filter(filed_year <= 2023 & filed_year >= 2022) # 238961 obs

# Summarize eviction info by geography and save----
source("dataprep/summarize_evictions.R")

# *County----
esum_county_1819 <- summarize_evictions(evictions1819, fips) #124 obs
write_csv(esum_county_1819, "data/app_data/esum_county_1819.csv")

esum_county_2021 <- summarize_evictions(evictions2021, fips) #124 obs 
write_csv(esum_county_2021, "data/app_data/esum_county_2021.csv")

esum_county_2223 <- summarize_evictions(evictions2223, fips) #124 obs 
write_csv(esum_county_2223, "data/app_data/esum_county_2223.csv")

# *Zip---- 
esum_zip_1819 <- summarize_evictions(evictions1819, defendant_zip) #1003 obs
write_csv(esum_zip_1819, "data/app_data/esum_zip_1819.csv")

esum_zip_2021 <- summarize_evictions(evictions2021, defendant_zip) #847 obs
write_csv(esum_zip_2021, "data/app_data/esum_zip_2021.csv")

esum_zip_2223 <- summarize_evictions(evictions2223, defendant_zip) #924 obs
write_csv(esum_zip_2223, "data/app_data/esum_zip_2223.csv")

# *Legal Aid Service Area----
esum_lasa_1819 <- summarize_evictions(evictions1819, legal_aid_service_area) #8 obs
write_csv(esum_lasa_1819, "data/app_data/esum_lasa_1819.csv")

esum_lasa_2021 <- summarize_evictions(evictions2021, legal_aid_service_area) #8 obs
write_csv(esum_lasa_2021, "data/app_data/esum_lasa_2021.csv")

esum_lasa_2223 <- summarize_evictions(evictions2223, legal_aid_service_area) #8 obs
write_csv(esum_lasa_2223, "data/app_data/esum_lasa_2223.csv")

# Explore timing (eventually?) ----

# Day of month -- too much variation, maybe for another time 
# evictions24 <- evictions24 %>%
#   mutate(day = mday(filed_date))
# 
# evictions24 %>%
#   ggplot(aes(x = day)) +
#   geom_histogram(binwidth = 1) +
#   facet_wrap(~plaintiff_non_residential, scales = "free_y")
