# farm polygons
farms <- read_sf("raw_data/LPIS/Land_Parcels.shp")

#ireland map
ireland <- st_read("Data/ireland_ITM.shp")

# tb test data
tb_data <- read.csv("raw_data/LPIS/main_TB_testing_merged_lab.csv")

# group all farms that have ever had TB positive animals
positives <- tb_data %>% 
  group_by(herd_no) %>%   
  summarise(any_positive = sum(cows_positive, bulls_positive, calves_positive,
                            heifers_positive, steers_positive, na.rm = T)) %>% 
  dplyr::select(SPH_HERD_N = herd_no, any_positive)

# filter polygons corresponding only to farms with TB positive animals
postive_farms <- farms %>% 
  left_join(positives) %>% 
  mutate(any_positive = replace_na(any_positive, 0)) %>% 
  filter(any_positive != 0)
  
# 2km buffer around positive farms
farm_buffers <- postive_farms %>% 
  st_buffer(dist = 2000) %>% 
  st_union(by_feature = FALSE) 

# crop buffer polygon to actual ireland outlinie
ireland_outline <- st_buffer(ireland, dist = 0)
samplers <- st_intersection(farm_buffers, ireland_outline)

# plot to check
ggplot(ireland) + 
  geom_sf() + 
  geom_sf(data = farm_buffers, fill = NA, col = "red")  +
  # geom_sf(data = sett_all, size = 0.5) + 
  geom_sf(data = samplers, fill = "orange") + 
  theme_bw()

saveRDS(samplers, file = "Data/samplers.RDS")
