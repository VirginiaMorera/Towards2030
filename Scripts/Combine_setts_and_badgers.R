rm(list = ls())
source("Scripts/setup.R")

#######################
#### Load datasets ####
#######################

sett_all <- readRDS("Data/sett_all.RDS")
badgers_all <- readRDS("Data/badgers_all.RDS")
ireland <- st_read("Data/ireland_ITM.shp")

##########################
#### Badgers per sett ####
##########################

sett <- sett_all %>% 
  dplyr::select(SETT_ID)

badgers_sum <- badgers_all %>% 
  filter(!is.na(CAPTURE_BLOCK_EVENT)) %>% 
  # filter(!is.na(DATE_CAUGHT)) %>% 
  filter(WEIGHT < 20) %>% 
  group_by(SETT_ID, CAPTURE_BLOCK_EVENT) %>% 
  summarise(group_size = n(), 
            mean_weight = mean(WEIGHT, na.rm = T)) 
  

badgers_loc <- left_join(badgers_sum, sett)  %>% 
  st_as_sf(sf_column_name = "geometry")


sett_all <- left_join(sett_all, badgers_sum)

sett_all %>% 
  filter(group_size >0) %>% 
  ggplot +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(col = "darkgray", size = 1) +
  geom_sf(aes(col = group_size), alpha = 0.7) +  
  scale_colour_viridis(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Number of badgers") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))


badgers_loc %>% 
  filter(!is.na(ECTOPARASITE_TYPE)) %>% 
  filter(ECTOPARASITE_TYPE != "N/A") %>% 
  # filter(mean_weight < 20 & mean_weight > 0) %>% 
  ggplot +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  # geom_sf(col = "lightgray", size = 1) +
  geom_sf(aes(col = ECTOPARASITE_TYPE), alpha = 0.5) +  
  labs(x = "Longitude", y = "Latitude", col = "Ectoparasite type") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))