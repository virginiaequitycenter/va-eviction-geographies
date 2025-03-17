# Modeling VA eviction and census data 

library(corrplot)
library(Hmisc)
library(lme4)
library(sf)
library(tidycensus)
library(tidyverse)

# Questions ----

# 1. What neighborhood racial and socioeconomic compositions influence rent exploitation? 
# 2. What neighborhood racial and socioeconomic compositions influence eviction rates? 
# 3. What is the relationship between eviction rates and rent exploitation? 

# Data ----
# Eviction and census data:
county_rent <- readRDS("data/county_rent.RDS")

# Segregation Indices 
# To maintain parity across tools, this data is pulled & calculated using the same mechanisms as the Equity Atlas:
# https://github.com/virginiaequitycenter/equity-dashboard/blob/main/datacode/addl_county_data.R

# Get tract-level info:
seg_tract <- get_acs(geography = "tract", 
                     state = "VA",
                     year = 2023,
                     table = "B03002", 
                     output = "wide", 
                     cache_table = T) # 2022 5-year ACS

seg_tract <- seg_tract %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         asian = B03002_006E,
         indig = B03002_005E,
         other = B03002_007E + B03002_008E,
         multi = B03002_009E,
         hisp = B03002_012E, 
         total = B03002_001E,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,11)) %>% 
  select(white, black, indig, asian, other, multi, hisp, total, county, tract)

# Get county-level info:
seg_county <- get_acs(geography = "county", 
                      year = 2023, 
                      state = "VA",
                      table = "B03002", 
                      geometry = F, 
                      output = "wide", 
                      cache_table = T)

seg_county <- seg_county %>%
  mutate(cowhite = B03002_003E,
         coblack = B03002_004E,
         coasian = B03002_006E,
         coindig = B03002_005E,
         coother = B03002_007E + B03002_008E,
         comulti = B03002_009E,
         cohisp = B03002_012E, 
         cototal = B03002_001E,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5)) %>% 
  select(cowhite, coblack, coindig, coasian, coother, comulti, cohisp, cototal, county)

# Join county totals to tract data to serve as denominator 
seg_tract <- left_join(seg_tract, seg_county, by = c("county"))

# *Dissimilarity ----
# This is a measure of evenness, or what of one group that would have to move to another area, in order to equalize the population distribution
# Black/White:
dissim_wb <- seg_tract %>%
  mutate(d.wb = abs(white/cowhite - black/coblack)) %>%
  group_by(county) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

# Hispanic/White:
dissim_wh <- seg_tract %>%
  mutate(d.wh = abs(white/cowhite - hisp/cohisp)) %>%
  group_by(county) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T))

dissim <- left_join(dissim_wb, dissim_wh, by = c("county"))

# *Separation Index ----
# This is another measure of evenness, defined as the condition when all census tracts in a given area 
# “have the same relative number of minority and majority members as the city as a whole” (Massey and Denton 1988:284)
# It is more robust than the dissimilarity index because it takes into account multiple variable dissimilarity. There are a few 
# different ways to calculate this measure, however, explored below. 

# ** Proportion ----
# From the study: "The difference in average neighborhood proportion White between the two groups is the separation index"
# Black/White:
s_proportion_wb <- seg_tract %>%
  mutate(sep.wb = white/(white + black)) %>%
  group_by(county) %>%
  summarise(sep_wb = mean(sep.wb, na.rm = T))

# Hispanic/White
s_proportion_wh <- seg_tract %>%
  mutate(sep.wh = white/(white + hisp)) %>%
  group_by(county) %>%
  summarise(sep_wh = mean(sep.wh, na.rm = T))

# ** Variance Ratio ----
# From James and Taeuber: https://www.jstor.org/stable/270845
# S is equivalent to the variance ratio index (V) 
# where t_i is the # of people of the reference race in the smaller area,
# p_i is the proportion of people of the reference race in the smaller area, 
# T is the # of people of the reference race in the larger area, 
# P is the proportion of people of the reference race in the larger area

# S = \sum t_i (p_i - P)^2 / TP(1-P)

#Black/White:
# t_i = black
# p_i = prop_black = black / (white + black)
# T = coblack
# P = prop_coblack = coblack / (cowhite + coblack)

s_var_bw <- seg_tract %>%
  mutate(prop_b = black/(white + black),
         prop_cob = coblack/(cowhite + coblack)) %>%
  group_by(county) %>%
  summarise(var_bw = sum())
  



-------

# Join segregation variables of interest with county eviction and exploitation data 
county_rent <- county_rent %>%
  left_join(dissim_wb, by = join_by(county_fips == county)) %>%
  left_join(dissim_wh, by = join_by(county_fips == county)) %>%
  left_join(sep_wb, by = join_by(county_fips == county)) %>%
  left_join(sep_wh, by = join_by(county_fips == county))
  

# # Interaction -- not used
# # This is a measure of exposure, and it indicates the degree of potential contact between minority and majority members in neighborhoods
# # Black/White
# inter_bw <- seg_tract %>%
#   mutate(int.bw=(black/coblack * white/total))%>%
#   group_by(county)%>%
#   summarise(inter_bw= sum(int.bw, na.rm=T))
# 
# # Hispanic/White
# inter_hw <- seg_tract %>%
#   mutate(int.hw=(hisp/cohisp * white/total))%>%
#   group_by(county)%>%
#   summarise(inter_hw= sum(int.hw, na.rm=T))
# 
# # Isolation -- not used
# # This is another measure of exposure, and it indicates the likeliness that one group is isolated, or only surrounded by other members of the same group
# 
# # Black
# isol_b <- seg_tract %>%
#   mutate(isob=(black/coblack * black/total) )%>%
#   group_by(county) %>%
#   summarise(iso_b = sum(isob, na.rm=T))
# 
# # Hispanic
# isol_h <- seg_tract %>%
#   mutate(isoh=(hisp/cohisp * hisp/total)) %>%
#   group_by(county) %>%
#   summarise(iso_h = sum(isoh, na.rm=T))

# Correlations ----
county_pcts <- county_rent %>%
  select(percent_white, percent_black, percent_asian, percent_aian, percent_nhpi,
         percent_other, percent_two, percent_hispanic, percent_burdened,
         exploit, filed_unit, judgment_rate, percent_plaintiff_business) %>%
  mutate(filed_unit = filed_unit * 100,
         judgment_rate = judgment_rate * 100) %>%
  as.data.frame() %>%
  select(-geometry)

county_cor <- cor(county_pcts, use = "complete.obs", method = "spearman") # spearman is more robust 

corrplot(county_cor)

# Modeling ----

# Main questions:
#1.) What community characteristics (such as race, ethnicity, or socioeconomics) influence evictions? 
#2.) What landlord characteristics (such as individuals or businesses) influence evictions?
#3.) Are businesses more likely to participate in serial eviction filings? 
#4.) Maybe: are eviction rates positively correlated with levels of rent burden and/or exploitation?

# Study data:
# ACS 5 year: med rent, med tax, exploitation, race/eth, med hh income, pov rate, % rental units, med age of unit
# -Zip codes nested w/in counties
# -Zip codes w/greater than or equal to 5% renter pop - only 3 in VA, lowest is only 8%. Some counties only have 300 renters -- is this too small? 

# *Example from Cromwell Study: Rent Exploitation  ----
#EX1.: Rent exploitation at neighborhood level will vary depending on racial and socioeconomic composition.
#EX2.: Level and variation of exploitation at neighborhood level is affected by the level of residential segregation in larger area (metro or micropolitan area)

# Model notes: 
# - Units = census tracts in counties 
# - SEs are clustered to account for nesting 
# - Dependent variable (Y) is rent exploitation 
# - White/Black Separation Index (we'll use Dissimilarity Index, which is a similar measure of evenness)
# - Tract-level covariates: percent_black, percent_hispanic, percent_pov, and med_hh_income
# - Models also control for med_unit_age, percent_owner_occupied_mort, percent_renter_occupied, and total_pop

# Example model 1: exploitation as a function of poverty 
mod1 <- lm(exploit ~ percent_pov, data = county_rent)
summary(mod1)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.50432 -0.14740 -0.02237  0.12909  0.81660 #pretty balanced 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.471133   0.045879  10.269  < 2e-16 *** #w/o poverty exploitation is .47 on average
#   percent_pov 0.018438   0.003167   5.822 4.24e-08 *** # w/every percentage point increase exploitation increases .018
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2254 on 131 degrees of freedom
# Multiple R-squared:  0.2056,	Adjusted R-squared:  0.1995 
# F-statistic:  33.9 on 1 and 131 DF,  p-value: 4.242e-08

confint(mod1)
# 2.5 %     97.5 %
#   (Intercept) 0.38037254 0.56189373
# percent_pov 0.01217347 0.02470285

# Which variables?
nullmod <- lm(exploit ~ 1, data = county_rent)
anova(nullmod, mod1)

# Res.Df    RSS Df Sum of Sq    F    Pr(>F)    
# 1    132 8.3808                                
# 2    131 6.6579  1    1.7229 33.9 4.242e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# *Eviction ----




