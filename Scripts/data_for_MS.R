# Housekeeping ####
rm(list = ls())
source("Scripts/setup.R")

# Sett data

sett_all <- readRDS("Data/sett_2025_inside_effort.RDS") 

sett_jittered <- sett_all %>% 
  st_jitter(amount = 1000) %>% 
  filter(MAIN_SETT == "Yes") 

st_write(sett_jittered, "sett_data_jittered.csv", layer_options = "GEOMETRY=AS_XY")


# Badger data ####

badgers_all <- readRDS("Data/badgers_thinned.RDS") 


ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(st_crs(badgers_all)) %>% 
  select(County_name = NAME_TAG)

badgers_per_county <- st_join(badgers_all, ireland_counties) %>% 
  group_by(County_name) %>%
  summarise(n_badgers = n()) %>% 
  st_drop_geometry %>% 
  filter(County_name %!in% c("Antrim", "Armagh", "Down", "Fermanagh", 
                             "Londonderry", "Tyrone")) %>% 
  inner_join(ireland_counties)

st_write(badgers_per_county, dsn = "badger_data_aggregated.shp")
