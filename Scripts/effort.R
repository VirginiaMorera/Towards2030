# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load data ####
cap <- read.csv("Data/Raw/tbl_capture_EVENTS_2023.csv") # culling capture events
# badgers_all <- readRDS("Data/badgers_setts.RDS") # badgers georef to capture sett
sett_all <- readRDS("Data/sett_all_2023.RDS")
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
vac_cap <- read.csv("Data/Raw/tbl_vaccine_2023.csv") # vaccine capture data

quart <- read_sf("Data/Raw/just_quartiles.shp") %>% 
  rename(QUARTILE = Q) %>% 
  st_set_crs(29902) %>% 
  st_transform(st_crs(sett_all))


# culling capture events ####

# we need to try and assign a quartile to every capture event by overlapping 
# the events with the quartiles. However, because the sett data only stores the 
# last sett visit, we need to use the actual badgers captured (which means if 
# no badgers were captured in a visit, we won't have them)

str(cap)
str(sett_all)

cap_clean  <-  cap  %>%  
  dplyr::select(-DATE_ENTERED_IN_DATABASE, -diff2) %>% 
  mutate(DATE_COMMENCED = dmy(DATE_COMMENCED), 
         DATE_COMPLETED = dmy(DATE_COMPLETED), 
         year = year(DATE_COMMENCED)) 

# badgers_clean <- badgers_all %>% 
#   st_drop_geometry() %>% 
#   select(SETT_ID, CAPTURE_BLOCK_EVENT_badger = CAPTURE_BLOCK_EVENT, YEAR) %>% 
#   mutate(YEAR= as.numeric(as.character(YEAR))) %>% 
#   distinct()


sett_clean <- sett_all %>% 
  mutate(CAPTURE_BLOCK_EVENT_sett = paste(CAPTURE_BLOCK_ID, CAPTURE_BLOCK_EVENT, sep = "/")) %>% 
  select(SETT_ID, CAPTURE_BLOCK_EVENT_sett, YEAR, geometry) %>% 
  distinct()

# 
# badgers_clean_sf <- badgers_clean %>% 
#   left_join(sett_clean, by = "SETT_ID") %>% 
#   st_as_sf()

quart_clean <- quart %>% 
  mutate(QUART = make.unique(QUART))

quart_geom <- quart %>% 
  select(QUART)

quart2 <- st_intersection(quart, sett_clean) %>% 
  st_drop_geometry()

quart_sum <- quart2 %>% 
  filter(YEAR > 2018) %>% 
  group_by(QUART) %>% 
  summarise(weight = n_distinct(CAPTURE_BLOCK_EVENT_sett)) %>%  
  left_join(quart_geom, by = "QUART") %>% 
  st_as_sf(sf_column_name = "geometry")

p1 <- ggplot() + 
   geom_sf(data = ireland_outline_sf) + 
   geom_sf(data = quart_sum, aes(fill = weight, col = weight)) + 
   # geom_sf(data = sett_all, size = 0.1, col = "white") +
   scale_fill_viridis_c() +
   scale_color_viridis_c() +
   theme_bw() + 
   ggtitle("Culling programme effort in ~11 day units")

# vaccine effort ####


# quartiles

vac_cap <- vac_cap %>% 
  filter(VACCINE_STATUS == "APPROVED") %>% 
  mutate(DATE_COMMENCED = ymd_hm(DATE_COMMENCED), 
         year = year(DATE_COMMENCED))


vac_cap_sum <- vac_cap %>% 
  filter(year > 2018) %>% 
  mutate(CAPTURE_BLOCK_EVENT_vacc = paste(EVENT, DATE_COMMENCED)) %>% 
  group_by(QUARTILE) %>% 
  summarise(weight = n_distinct(CAPTURE_BLOCK_EVENT_vacc)) %>% 
  ungroup()

quart_effort <- full_join(quart, vac_cap_sum)

# combine

head(quart_sum)
head(quart_effort)

vacc_effort <- quart_effort %>% 
  select(QUART, weight)

cul_effort <- quart_sum %>% 
  select(QUART, weight) %>% 
  mutate(weight = weight/2)


all_effort <- bind_rows(vacc_effort, cul_effort)

all_effort_final <- all_effort %>% 
  group_by(QUART) %>% 
  summarise(weight = sum(weight, na.rm = T)) 

ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = all_effort_final %>% filter(weight > 0), aes(fill = weight, col = weight)) + 
  # geom_sf(data = sett_all %>% filter(YEAR > 2018), size = 0.5, col = "red") +
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  ggtitle("Sampling effort of culling and vaccination programmes combined")

  
saveRDS(all_effort_final, file = "Data/Inla/weightedSampler.RDS")
all_effort_final <- readRDS("Data/Inla/weightedSampler.RDS")
