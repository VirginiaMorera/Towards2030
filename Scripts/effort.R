# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load data ####
cap <- read_excel("Data/Raw/TBL_CAPTURE_EVENTS_2025.xlsx") # culling capture events
culling_cap <- cap %>% 
  select(CAPTURE_BLOCK_EVENT, DATE_COMMENCED) 

sett_all <- readRDS("Data/sett_all_2025.RDS")

his1 <- read_excel(path = "Data/Raw/tbl_sett_record_history1.xlsx") 

his2 <- read_excel(path = "Data/Raw/tbl_sett_record_history2.xlsx", 
                   col_names = names(his1)) %>% 
  select(SETT_ID, NO_BADGERS_CAPTURED, CAPTURE_BLOCK_EVENT)

his3 <- read_excel(path = "Data/Raw/tbl_sett_record_history3.xlsx", 
                   col_names = names(his1)) %>% 
  select(SETT_ID, NO_BADGERS_CAPTURED, CAPTURE_BLOCK_EVENT)

his1 <- his1 %>% 
  select(SETT_ID, NO_BADGERS_CAPTURED, CAPTURE_BLOCK_EVENT)

sett_history <- bind_rows(his1, his2, his3)


ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp")

vac_cap <- read_excel("Data/Raw/TBL_VACCINE_2025.xlsx") # vaccine capture data
badgers_all <- read_excel("Data/Raw/tbl_vacc_badgers_2023.xlsx")

quart <- read_sf("Data/Raw/just_quartiles.shp") %>% 
  rename(QUARTILE = Q) %>% 
  st_set_crs(29902) %>% 
  st_transform(st_crs(sett_all))

# Culling effort ####

# in sett history, those setts who match capture events from the culling captures are separated
# as those are easy to find the date but a bit harder to match to a quartile 

culling_capture_block_events <- sett_history %>% 
  left_join(culling_cap) %>% 
  arrange(DATE_COMMENCED) %>%  # this gives us the date no problem 
  filter(!is.na(DATE_COMMENCED)) %>% 
  mutate(PROGRAMME = "Culling", 
         NDAYS = 11)

# now for the location we need to assign a location to each sett based on the sett register (sett all)
sett_geom <- sett_all %>% 
  select(SETT_ID)

culling_capture_block_events_sf <- culling_capture_block_events %>% 
  inner_join(sett_geom) %>% 
  st_as_sf(sf_column_name = "geometry") %>% 
  st_set_crs(st_crs(sett_geom))

# then we need to intersect those points with the quartile polygons to assign them a quartile number
culling_capture_block_events <- st_intersection(quart, culling_capture_block_events_sf) %>% 
  st_drop_geometry() %>% 
  select(-QUART) %>% 
  mutate(QUARTILE = as.character(QUARTILE))

# finished! 

# Vaccination effort ####

# those who don't have capture block events matching those of the culling captures are vaccination (mostly)
vacc_capture_block_events <- sett_history %>% 
  left_join(culling_cap) %>% 
  filter(is.na(DATE_COMMENCED)) 

# we now get the vaccination capture events, and generate the capture block event by poasting the Quartile 
# and the event number +1000
vac_cap <- vac_cap %>% 
  mutate(CAPTURE_BLOCK_EVENT = paste0(QUARTILE, "/", EVENT+1000)) %>% 
  select(CAPTURE_BLOCK_EVENT, DATE_COMMENCED)

# and from the sett history we exclude those that are not vaccination (are weird) 
# by checking that the capture block event has not been generated as explained above
# We then left join it to the vac_cap events containing the dates
vacc_capture_block_events <- vacc_capture_block_events %>% 
  separate(SETT_ID, into = c("QUARTILE", NA), sep = 5, remove = F) %>% 
  separate(CAPTURE_BLOCK_EVENT, into = c("QUARTILE_CHECK", NA), sep = "/", remove = F) %>% 
  mutate(vacc_effort = if_else(QUARTILE == QUARTILE_CHECK, "Yes", "No")) %>% 
  filter(vacc_effort == "Yes") %>% 
  select(SETT_ID, QUARTILE, NO_BADGERS_CAPTURED, CAPTURE_BLOCK_EVENT) %>% 
  left_join(vac_cap) %>% 
  filter(!is.na(DATE_COMMENCED)) %>% 
  mutate(PROGRAMME = "Vaccination", 
         NDAYS = 6)

# Bind them ####
effort_with_dates <- bind_rows(vacc_capture_block_events, culling_capture_block_events) %>% 
  arrange(CAPTURE_BLOCK_EVENT) %>% 
  filter(SETT_ID %!in% c("BLANK", "2")) 

saveRDS(effort_with_dates, "Data/Raw/sett_history_with_dates-RDS")  

effort_with_dates <- effort_with_dates %>% 
  select(-SETT_ID) %>% 
  distinct()



# Effort per quartile ####
effort_per_quartile_total <- effort_with_dates %>% 
  group_by(QUARTILE) %>% 
  summarise(NDAYS = sum(NDAYS))

effort_per_quartile_2019 <- effort_with_dates %>% 
  mutate(YEAR = year(DATE_COMMENCED)) %>% 
  filter(YEAR > 2018) %>% 
  group_by(QUARTILE) %>% 
  summarise(NDAYS = sum(NDAYS))

effort_total_sf <- quart %>% 
  mutate(QUARTILE = as.character(QUARTILE)) %>% 
  left_join(effort_per_quartile_total)

effort_2019_sf <- quart %>%  
  mutate(QUARTILE = as.character(QUARTILE)) %>% 
  left_join(effort_per_quartile_2019)

ggplot() + 
  geom_sf(data = effort_total_sf, aes(fill = NDAYS), col = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  geom_sf(data = ireland_counties, fill = NA, col = "black") + 
  theme_bw() + 
  labs(title = "All effort") + 
  
  ggplot() + 
  geom_sf(data = effort_2019_sf, aes(fill = NDAYS), col = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  geom_sf(data = ireland_counties, fill = NA, col = "black") + 
  theme_bw() + 
  labs(title = "Effort since 2019")

saveRDS(effort_total_sf, file = "Data/Inla/all_effort.RDS")
saveRDS(effort_2019_sf, file = "Data/Inla/2019_effort.RDS")

ggplot() + 
  geom_sf(data = effort_2019_sf, aes(fill = NDAYS, col = NDAYS)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  geom_sf(data = ireland_counties, fill = NA, col = "red") + 
  theme_bw() + 
  labs(title = "Sampling effort of culling and vaccination programmes combined", 
       fill = "N days", 
       col = "N days")

# log the nº of days as number of setts found will saturate with effort
all_effort_final_log <- effort_2019_sf %>% 
  mutate(WEIGHT = log(NDAYS))


ggplot(all_effort_final_log) +
  geom_point(aes(x = NDAYS, y = WEIGHT)) + 
  theme_bw()

ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = all_effort_final_log, aes(fill = WEIGHT, col = WEIGHT)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  labs(title = "Sampling effort of culling and vaccination programmes combined", 
       fill = "N days (log)", 
       col = "N days (log)")

saveRDS(effort_2019_sf, file = "Data/Inla/weightedSampler.RDS")
saveRDS(all_effort_final_log, file = "Data/Inla/log_weightedSampler.RDS")

all_effort_final <- readRDS("Data/Inla/weightedSampler.RDS")

ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = all_effort_final, aes(fill = NDAYS, col = NDAYS)) + 
  geom_sf(data = ireland_counties, fill = NA, col = "black") + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  labs(title = "Sampling effort of culling and vaccination programmes combined", 
       fill = "N days", 
       col = "N days") 
