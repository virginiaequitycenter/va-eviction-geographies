# Explore serial filings

# Libraries ----
library(lme4)
library(lmerTest)
library(tidyverse)

# New data ----
evictions24 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year == "2024")

collapsed24 <- read_csv("modeling/data/collapsed24.csv")

# Without community characteristics variables ----

# Predicting whether or not it's a serial filing based on whether the landlord is a business and 
# with a random effect of whether the landlord is a business within the court region 
# The random effect asks if different courts behave differently wrt the plaintiff as a business landlord 
m_rcf <- glmer(serial_filing ~ plaintiff_non_residential + (1 + plaintiff_non_residential | court),
               data = evictions24, family = binomial)

summary(m_rcf)
# There's more spread (larger sd) in random effects for business landlords nested w/in courts compared to
# just court randomness 

# See both random effects for each court:
rcf <- data.frame(ranef(m_rcf))

ggplot(rcf, aes(x = condval, y = grp, color = term)) +
  geom_point()

ggplot(rcf, aes(x = condval)) +
  geom_histogram() +
  facet_wrap(~term)


# Add rent burden ----
# (and other variables, eventually)

burden <- collapsed24 %>%
  select(NAME, court, fips, total_burdened, pct_burdened)

evictions24_small <- evictions24 %>%
    select(case_number, court, fips, plaintiff_non_residential, serial_filing)

joined24 <- full_join(burden, evictions24_small) %>%
    mutate(scale_burden = c(scale(pct_burdened)))

# Deal with combined court jurisdiction areas ----
ok <- joined24 %>%
  filter((!NAME %in% c("fairfax city and county", "harrisonburg and rockingham county", 
                       "lexington and rockbridge county", "newport news county and city", 
                       "norton and wise county", "williamsburg and james city county")),
           (!court %in% c("Fairfax County General District Court", "Harrisonburg/Rockingham General District Court", 
                          "Lexington/Rockbridge General District Court","Newport News-Civil General District Court",
                          "Wise/Norton General District Court", "Williamsburg/James City County General District Court")))

fill_courts <- function(df, NAME, court){
  df %>%
    filter(NAME %in% {{NAME}} |
             court %in% {{court}}) %>%
    fill(NAME, total_burdened, pct_burdened, scale_burden, .direction = "down") %>%
    drop_na()
}

fairfax <- fill_courts(joined24, "fairfax city and county", "Fairfax County General District Court")
hburg <- fill_courts(joined24, "harrisonburg and rockingham county", "Harrisonburg/Rockingham General District Court")
lexington <- fill_courts(joined24, "lexington and rockbridge county", "Lexington/Rockbridge General District Court")
newport <- fill_courts(joined24, "newport news county and city", "Newport News-Civil General District Court")

norton <- fill_courts(joined24, "norton and wise county", "Wise/Norton General District Court") 
wburg <- fill_courts(joined24, "williamsburg and james city county", "Williamsburg/James City County General District Court")

mod_dat24 <- rbind(fairfax, hburg, lexington, newport, norton, wburg, ok)
  
# Model with rent burden
m_rcf_interaction <- glmer(serial_filing ~ plaintiff_non_residential*scale_burden + 
                             (1 + plaintiff_non_residential | court),
                           data = mod_dat24, family = binomial)
# Whether or not a filing is serial based on the interaction between the landlord being a business and the levels of rent burden in 
# a court district with a random effect of whether the landlord is a business within the court region. 

summary(m_rcf_interaction)
# Random effects are the same as the previous model 
# Fixed effects are if it's a business landlord and rent burden
# Rent burden and (not interacting with) business landlords are independently significant positive predictors of a filing being serial
# There's no significant interaction bw business landlords and rent burden 

# See random effects for every court 
rcf_burden <- data.frame(ranef(m_rcf_interaction))

ggplot(rcf_burden, aes(x = condval, y = grp, color = term)) +
  geom_point()

ggplot(rcf_burden, aes(x = condval)) +
  geom_histogram() +
  facet_wrap(~term)

# Model with more covariates
# # I think it's likely that density will diminish model convergence
# #   and soak up the interesting variance -- TBD 