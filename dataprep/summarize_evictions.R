summarize_evictions <- function(df, grouper) {
  
  #Total cases filed 
  total_filed <- df %>% 
    group_by({{ grouper }}) %>%
    count() %>%
    rename(total_filed = n)
  assign("total_filed", total_filed, .GlobalEnv)
  
  #Number of cases that went to judgment 
  n_judgment <- df %>%
    filter(to_judgment == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_judgment = n)
  assign("n_judgment", n_judgment, .GlobalEnv)
  
  #Number of cases that went to default
  n_default <- df %>% 
    filter(default == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_default = n)
  assign("n_default", n_default, .GlobalEnv)
  
  #Total plaintiff-won cases 
  n_plaintiff_won <- df %>% 
    filter(judgment == "Plaintiff") %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_plaintiff_won = n)
  assign("n_plaintiff_won", n_plaintiff_won, .GlobalEnv)
  
  #Total cases with immediate possession 
  n_immediate <- df %>%
    filter(possession == "Immediate") %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_immediate = n)
  assign("n_immediate", n_immediate, .GlobalEnv)
  
  #Median principal amount
  median_principal <- df %>%
    group_by({{ grouper }}) %>%
    summarise(median_principal = round(median(principal_amount, na.rm = TRUE), digits = 2))
  assign("median_principal", median_principal, .GlobalEnv)
  
  #Number of cases where the defendant had an attorney 
  n_d_attorney <- df %>%
    filter(d_attorney_present == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_d_attorney = n)
  assign("n_d_attorney", n_d_attorney, .GlobalEnv)
  
  #Number of serial filings 
  n_serial <- df %>%
    filter(serial_filing == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_serial = n)
  assign("n_serial", n_serial, .GlobalEnv)
  
  #Number of cases filed by non-person evictors/plaintiffs  
  n_cases_filed_by_businesses <- df %>%
    filter(plaintiff_non_residential == TRUE) %>%
    group_by({{ grouper }}) %>%
    count() %>%
    rename(n_cases_filed_by_businesses = n)
  assign("n_cases_filed_by_businesses", n_cases_filed_by_businesses, .GlobalEnv)
  
  #Total plaintiffs/evictors 
  total_plaintiffs <- df %>%
    group_by({{ grouper }}) %>%
    summarize(total_plaintiffs = n_distinct(plaintiff_name, na.rm = TRUE))
  assign("total_plaintiffs", total_plaintiffs, .GlobalEnv)
  
  #Total non-person plaintiffs/evictors
  n_business_plaintiffs <- df %>%
    filter(plaintiff_non_residential == TRUE) %>%
    group_by({{ grouper }}) %>%
    summarize(n_business_plaintiffs = n_distinct(plaintiff_name, na.rm = TRUE))
  assign("n_business_plaintiffs", n_business_plaintiffs, .GlobalEnv)
  
  #Number of plaintiffs that engage in serial filing behavior 
  n_plaintiffs_serial <- df %>%
    filter(serial_filing == TRUE) %>%
    group_by({{ grouper }}) %>%
    summarize(n_plaintiffs_serial = n_distinct(plaintiff_name, na.rm = TRUE))
  assign("n_plaintiffs_serial", n_plaintiffs_serial, .GlobalEnv)
  
  #Join by geography
  evictions_summary <- total_filed %>%
    left_join(n_judgment) %>%
    left_join(n_default) %>%
    left_join(n_plaintiff_won) %>%
    left_join(n_immediate) %>%
    left_join(median_principal) %>%
    left_join(n_d_attorney) %>%
    left_join(n_serial) %>%
    left_join(n_cases_filed_by_businesses) %>%
    left_join(total_plaintiffs) %>%
    left_join(n_business_plaintiffs) %>%
    left_join(n_plaintiffs_serial)
  assign("evictions_summary", evictions_summary, .GlobalEnv)
  
  #Replace NA values with 0
  evictions_summary <- evictions_summary %>%
    mutate(
      across(everything(), ~replace_na(.x, 0)))
  assign("evictions_summary", evictions_summary, .GlobalEnv)
  
  #Calculate relevant proportions
  evictions_summary <- evictions_summary %>%
    mutate(
      # Percent of filings that were serial
      pct_serial = ((n_serial / total_filed) * 100),
      # Percent of plaintiffs that operate as a business
      pct_business_plaintiffs = ((n_business_plaintiffs / total_plaintiffs) * 100),
      # Percent of cases that were filed by business landlords
      pct_cases_business = ((n_cases_filed_by_businesses / total_filed) * 100),
      # Percent of cases that went to judgment
      pct_judgment = ((n_judgment / total_filed) * 100),
      # Proportion of plaintiffs that engage in serial filing behavior
      prop_plaintiff_serial = n_plaintiffs_serial / total_plaintiffs,
      # Percent default 
      pct_default = ((n_default / total_filed) * 100))
  assign("evictions_summary", evictions_summary, .GlobalEnv)

}
