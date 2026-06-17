# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

quart <- read_sf("Data/Raw/just_quartiles.shp") %>% 
  rename(QUARTILE = Q) %>% 
  st_set_crs(29902) %>% 
  st_transform(st_crs(2157))

ireland <- read_sf("Data/Other/Ireland_ITM.shp")

# Sett data ####

# Generalise ####
sett <- readRDS("Data/sett_2025_inside_effort.RDS")

main_setts_per_quartile <-  sett %>% 
  filter(MAIN_SETT == "Yes") %>% 
  st_intersection(quart) %>%  
  st_drop_geometry() %>% 
  group_by(QUARTILE) %>% 
  summarise(n_setts = n_distinct(SETT_ID)) %>% 
  select(QUARTILE, n_setts) %>% 
  right_join(quart)  %>% 
  st_as_sf(sf_column_name = "geometry") %>% 
  select(-QUART)

ggplot() + 
  geom_sf(data = ireland, fill = NA, col = "black") + 
  geom_sf(data = main_setts_per_quartile, aes(fill = n_setts), col = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  theme_bw()
  
# Randomise ####

randomised_main_setts <-  sett %>% 
  filter(MAIN_SETT == "Yes")  %>% 
  st_jitter(amount = 500)
