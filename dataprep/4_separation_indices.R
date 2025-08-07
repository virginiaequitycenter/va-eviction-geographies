# Segregation Indices ----
# To maintain parity across tools, this data is pulled & calculated using the same mechanisms as the Equity Atlas:
# https://github.com/virginiaequitycenter/equity-dashboard/blob/main/datacode/addl_county_data.R

library(tidycensus)
library(tidyverse)

# 2023 ----
# Get tract-level info:
seg_tract <- get_acs(geography = "tract", 
                     state = "VA",
                     year = 2023,
                     table = "B03002", 
                     output = "wide", 
                     cache_table = T) # 2023 5-year ACS

seg_tract <- seg_tract %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         hisp = B03002_012E, 
         total = B03002_001E,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,11)) %>% 
  select(white, black, hisp, total, county, tract, NAME)

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
         cohisp = B03002_012E, 
         cototal = B03002_001E,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5)) %>% 
  select(cowhite, coblack, cohisp, cototal, county, NAME)



# Join county totals to tract data to serve as denominator 
seg_tract2 <- left_join(seg_tract, seg_county, by = c("county"))

# Deal with collapsed tracts/counties (for general district courts)
seg_tract2 = seg_tract2 %>%
  mutate(county_collapsed = case_when(
    county %in% c("095", "830") ~ "Williamsburg and James City County",
    county %in% c("195", "720") ~ "Norton and Wise County",
    county %in% c("163", "678") ~ "Lexington and Rockbridge County",
    county %in% c("165", "660") ~ "Harrisonburg and Rockingham County",
    county %in% c("059", "600") ~ "Fairfax City and County",
    county %in% c("703", "700") ~ "Newport News County and City",
    TRUE ~ gsub(",.*", "", NAME.y)
  ))

# *Dissimilarity ----
# This is a measure of evenness, or what of one group that would have to move to another area, in order to equalize the population distribution
# Black/White:
dissim_wb <- seg_tract2 %>%
  mutate(d.wb = abs(white/cowhite - black/coblack)) %>%
  group_by(county_collapsed) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

# Hispanic/White:
dissim_wh <- seg_tract2 %>%
  mutate(d.wh = abs(white/cowhite - hisp/cohisp)) %>%
  group_by(county_collapsed) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T),
            cowhite = sum(unique(cowhite)))

dissim <- left_join(dissim_wb, dissim_wh, by = c("county_collapsed"))

# Save collapsed data
write_csv(dissim, "modeling/data/dissim23_collapsed.csv")


# 2019 ----
# Get tract-level info:
seg_tract <- get_acs(geography = "tract", 
                     state = "VA",
                     year = 2019,
                     table = "B03002", 
                     output = "wide", 
                     cache_table = T) # 2019 5-year ACS

seg_tract <- seg_tract %>%
  mutate(white = B03002_003E,
         black = B03002_004E,
         hisp = B03002_012E, 
         total = B03002_001E,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5),
         tract = substr(GEOID, 6,11)) %>% 
  select(white, black, hisp, total, county, tract, NAME)

# Get county-level info:
seg_county <- get_acs(geography = "county", 
                      year = 2019, 
                      state = "VA",
                      table = "B03002", 
                      geometry = F, 
                      output = "wide", 
                      cache_table = T)

seg_county <- seg_county %>%
  mutate(cowhite = B03002_003E,
         coblack = B03002_004E,
         cohisp = B03002_012E, 
         cototal = B03002_001E,
         state = substr(GEOID, 1,2),
         county = substr(GEOID, 3,5)) %>% 
  select(cowhite, coblack, cohisp, cototal, county, NAME)

# Join county totals to tract data to serve as denominator 
seg_tract19 <- left_join(seg_tract, seg_county, by = c("county"))

# Deal with collapsed tracts/counties (for general district courts)
seg_tract19 = seg_tract19 %>%
  mutate(county_collapsed = case_when(
    county %in% c("095", "830") ~ "Williamsburg and James City County",
    county %in% c("195", "720") ~ "Norton and Wise County",
    county %in% c("163", "678") ~ "Lexington and Rockbridge County",
    county %in% c("165", "660") ~ "Harrisonburg and Rockingham County",
    county %in% c("059", "600") ~ "Fairfax City and County",
    county %in% c("703", "700") ~ "Newport News County and City",
    TRUE ~ gsub(",.*", "", NAME.y)
  ))


# Save (for calculating dissim with collapsed data)
write_csv(seg_tract, "data/seg_tract23.csv")

# *Dissimilarity ----
# This is a measure of evenness, or what of one group that would have to move to another area, in order to equalize the population distribution
# Black/White:
dissim_wb <- seg_tract19 %>%
  mutate(d.wb = abs(white/cowhite - black/coblack)) %>%
  group_by(county_collapsed) %>%
  summarise(dissim_wb = .5*sum(d.wb, na.rm=T))

# Hispanic/White:
dissim_wh <- seg_tract19 %>%
  mutate(d.wh = abs(white/cowhite - hisp/cohisp)) %>%
  group_by(county_collapsed) %>%
  summarise(dissim_wh = .5*sum(d.wh, na.rm=T),
            cowhite = sum(unique(cowhite)))

dissim <- left_join(dissim_wb, dissim_wh, by = c("county_collapsed"))

# Save collapsed data
write_csv(dissim, "modeling/data/dissim19_collapsed.csv")

----


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
# # P = prop_coblack = coblack / (cowhite + coblack)
# 
# s_var_bw <- seg_tract %>%
#   mutate(prop_b = black/(white + black),
#          prop_cob = coblack/(cowhite + coblack)) %>%
#   group_by(county) %>%
#   summarise(var_bw = sum())


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
