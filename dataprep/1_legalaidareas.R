# Build reference map of Legal Aid Service Areas

# Setup ----
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

# Get counties/fips from 2022 ACS----
va_counties <- get_acs(
  geography = "county",
  state = "51",
  variables = "B01001_001",
  survey = "acs5",
  year = 2022, 
  geometry = TRUE)

va_counties <- va_counties %>% 
  select(GEOID, NAME) %>%
  mutate(fips = gsub("^.{0,2}", "", GEOID),
         locality = str_to_title(gsub("(.*),.*", "\\1", NAME))) %>%
  select(-NAME)

# Identify legal aid areas ----
lsnv <- c("51013", "51510", "51610", "51059", "51107", 
          "51153","51685", "51600", 51683) 
# Fairfax city, Manassas City not listed on page but shown on map

law <- c("51179", "51177", "51099", "51033", "51193", 
         "51133", "51159", "51103", "51057", "51097", 
         "51101", "51047", "51137", "51113", "51157", 
         "51061", "51630")

brls <- c("51043", "51069", "51187", "51171", "51139", 
          "51165", "51015", "51790", "51820", "51163", 
          "51530", "51091", "51017", "51005", "51660", 
          "51678", "51580", "51840") 
# Covington, Winchester not listed on page but shown on map
# removed listed localities that overlapped with roanoke valley area

cvlas <- c("51079", "51003", "51125", "51109", "51065", 
           "51075", "51085", "51087", "51145", "51041", 
           "51053", "51570", "51670", "51149", "51181", 
           "51127", "51036", "51760", "51540", "51730")
# Petersburg not listed on page but shown on map

lasev <- c("51119", "51073", "51115", "51830", "51650", 
           "51710", "51700", "51735", "51740", "51199", 
           "51810", "51550", "51001", "51131", "51095")

lasrv <- c("51045", "51023", "51019", "51067", "51770", 
           "51161", "51775")

svlas <- c("51071", "51121", "51063", "51155", "51035", 
           "51021", "51197", "51077", "51185", "51173", 
           "51167", "51027", "51191", "51520", "51195", 
           "51169", "51105", "51720", "51750", "51640",
           "51051") 
# Dickenson not listed on page, but shown on map

vlas <- c("51141", "51089", "51590", "51143", "51031", 
          "51083", "51037", "51009", "51011", "51029", 
          "51147", "51117", "51111", "51049", "51007", 
          "51135", "51025", "51081", "51183", "51175", 
          "51093", "51800", "51595", "51620", "51680", 
          "51690")

# Assign legal aid areas ----
va_counties <- va_counties %>% 
  mutate(legal_aid_service_area = case_when(
    GEOID %in% lsnv ~ "Legal Services of Northern Virginia",
    GEOID %in% law ~ "Legal Aid Works",
    GEOID %in% brls ~ "Blue Ridge Legal Services",
    GEOID %in% cvlas ~ "Central Virginia Legal Aid Society",
    GEOID %in% lasev ~ "Legal Aid Society of Eastern Virginia",
    GEOID %in% lasrv ~ "Legal Aid Society of Roanoke Valley",
    GEOID %in% svlas ~ "Southwest Virginia Legal Aid Society",
    GEOID %in% vlas ~ "Virginia Legal Aid Society")) %>%
  select(-GEOID)

# Save CSV matching counties to service areas (no geometry - the geos pulled earlier are for counties)
service_areas <- va_counties %>% st_drop_geometry()
write_csv(service_areas, "data/service_areas.csv")

# Create and save Legal Aid Area geometries
areas_sf <- va_counties %>%
  group_by(legal_aid_service_area) %>%
  mutate(geometry = st_union(geometry))

write_rds(areas_sf, "data/areas_sf.RDS")

# Build Map ----
my_colors <- c("Southwest Virginia Legal Aid Society" = "#E31A1C",
               "Legal Aid Society of Roanoke Valley" = "#FDBF6F",
               "Blue Ridge Legal Services" = "#1F78B4",
               "Virginia Legal Aid Society" = "#9e6eac",
               "Central Virginia Legal Aid Society" = "#B2DF8A",
               "Legal Services of Northern Virginia" = "#FF7F00",
               "Legal Aid Works" = "#fcb2b2",
               "Legal Aid Society of Eastern Virginia" = "#A6CEE3")

ggplot(areas_sf) +
  geom_sf(aes(fill = legal_aid_service_area)) +
  scale_fill_manual(values = my_colors, guide = "none") +
  annotate("label", x = -78, y = 36.75, label = "VA Legal Aid Society", size = 2.5) +
  annotate("label", x = -82, y = 36.8, label = "Southwest VA Legal Aid Society", size = 2.5) +
  annotate("label", x = -79.5, y = 38.6, label = "Blue Ridge Legal Services", size = 2.5) +
  annotate("label", x = -81, y = 37.9, label = "Legal Aid Society\nof Roanoke Valley", size = 2.5) +
  annotate("segment", x = -81, y = 37.75, xend = -80.4, yend = 37.55, arrow = arrow(length = unit(2, "mm")), linewidth = .25) +
  annotate("label", x = -76.5, y = 39, label = "Legal Aid Services of Northern VA", size = 2.5) +
  annotate("label", x = -78, y = 37.8, label = "Central Virginia\nLegal Aid Society", size = 2.5) +
  annotate("label", x = -76, y = 38.5, label = "Legal Aid Works", size = 2.5) +
  annotate("segment", x = -76, y = 38.42, xend = -76.6, yend = 38.2, arrow = arrow(length = unit(2, "mm")), linewidth = .25) +
  annotate("label", x = -75.7, y = 37.2, label = "Legal Aid Society\nof Eastern VA", size = 2.5) +
  theme_void() +
  ggtitle("     Virginia Legal Aid Service Areas")
