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

# vaccine effort ####

# independent of points. Count number of events per quartile, use that as unit of effort 
# quartiles

vac_cap_clean <- vac_cap %>% 
  filter(VACCINE_STATUS == "APPROVED") %>% 
  mutate(DATE_COMMENCED = ymd_hm(DATE_COMMENCED), 
         DATE_COMPLETED = ymd_hm(DATE_COMPLETED),
         DURATION = DATE_COMPLETED- DATE_COMMENCED, 
         YEAR = year(DATE_COMMENCED)) %>% 
  select(QUARTILE, EVENT, CLUSTER_ID, DATE_COMMENCED, DATE_COMPLETED, CHNG_NO, 
         DURATION, YEAR) %>% 
  drop_na(DATE_COMMENCED) %>% 
  distinct() %>% 
  mutate(DAY_WEIGHT = 1/as.numeric(DURATION))

vac_cap_sum <- vac_cap_clean %>% 
  filter(YEAR > 2018) %>% 
  mutate(CAPTURE_BLOCK_EVENT_vacc = paste(EVENT, DATE_COMMENCED)) %>% 
  group_by(QUARTILE) %>% 
  summarise(NDAYS = as.numeric(sum(DURATION))) %>% 
  ungroup()

quart_effort <- full_join(quart, vac_cap_sum)

p1 <- ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = quart_effort, aes(fill = NDAYS, col = NDAYS)) + 
  # geom_sf(data = sett_all %>% filter(YEAR > 2018), size = 0.5, col = "red") +
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  labs(title = "Sampling effort of the vaccination programme", 
       fill = "N days", 
       col = "N days")

# culling capture events ####

str(cap)
str(sett_all)

# clean capture effort data
cap_clean  <-  cap  %>%  
  dplyr::select(DATE_COMMENCED, DATE_COMPLETED, EVENT_NO, 
                CAPTURE_BLOCK_EVENT, TOTAL_BADGERS) %>% 
  mutate(DATE_COMMENCED = dmy(DATE_COMMENCED), 
         DATE_COMPLETED = dmy(DATE_COMPLETED), 
         YEAR = year(DATE_COMMENCED), 
         DURATION = 11) %>%  # we assume all capture events lasted 11 days 
  filter(YEAR > 2018)

# clean sett data (which will give us the location of the capture events )
sett_clean <- sett_all %>% 
  filter(CAPTURE_BLOCK_ID %!in% c("NOT ASSIGN", "NOT ASSIGNE", "Not Assigne")) %>% 
  mutate(CAPTURE_BLOCK_EVENT_sett = paste(CAPTURE_BLOCK_ID, CAPTURE_BLOCK_EVENT, sep = "/")) %>% 
  select(SETT_ID, CAPTURE_BLOCK_EVENT_sett, YEAR, geometry) %>% 
  distinct()

# intercept setts and quartiles to see in which sett a quartile is 
quart2 <- st_intersection(quart, sett_clean) %>% 
  filter(YEAR > 2018) %>% 
  st_drop_geometry()

# according to the numbers below, we've lost 12588 capture events that had no setts found
length(unique(quart2$CAPTURE_BLOCK_EVENT_sett))
length(unique(cap_clean$CAPTURE_BLOCK_EVENT))

# from 2019 onwards, how many unique capture events in each quartile
quart_sum <- quart2 %>% 
  dplyr::select(-SETT_ID) %>% 
  distinct() %>% 
  mutate(DURATION = 11) %>% # we assume all capture events lasted 11 days
  group_by(QUARTILE) %>% 
  summarise(NDAYS = sum(DURATION)) %>%  
  left_join(quart, by = "QUARTILE") %>% 
  st_as_sf(sf_column_name = "geometry")

p2 <- ggplot() + 
   geom_sf(data = ireland_outline_sf) + 
   geom_sf(data = quart_sum, aes(fill = NDAYS, col = NDAYS)) + 
   # geom_sf(data = sett_all, size = 0.1, col = "white") +
   scale_fill_viridis_c() +
   scale_color_viridis_c() +
   theme_bw() + 
  labs(title = "Sampling effort of the culling programme", 
       fill = "N days", 
       col = "N days")


gridExtra::grid.arrange(p2, p1, nrow = 1)

# combine

head(quart_sum)
head(quart_effort)

vacc_effort <- quart_effort %>% 
  select(QUARTILE, NDAYS)

saveRDS(vacc_effort, file = "Data/Inla/vacc_effort.RDS")

cul_effort <- quart_sum %>% 
  select(QUARTILE, NDAYS) 

saveRDS(cul_effort, file = "Data/Inla/cul_effort.RDS")

all_effort <- bind_rows(vacc_effort, cul_effort)

all_effort_final <- all_effort %>% 
  group_by(QUARTILE) %>%
  summarise(NDAYS = sum(NDAYS, na.rm = T)) %>%
  filter(NDAYS > 0)

ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = all_effort_final, aes(fill = NDAYS, col = NDAYS)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  labs(title = "Sampling effort of culling and vaccination programmes combined", 
       fill = "N days", 
       col = "N days")

# log the nº of days as number of setts found will saturate with effort
all_effort_final <- all_effort_final %>% 
  mutate(WEIGHT = log(NDAYS))


ggplot(all_effort_final) +
  geom_point(aes(x = NDAYS, y = WEIGHT)) + 
  theme_bw()

ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = all_effort_final, aes(fill = WEIGHT, col = WEIGHT)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  labs(title = "Sampling effort of culling and vaccination programmes combined", 
       fill = "N days (log)", 
       col = "N days (log)")

saveRDS(all_effort_final, file = "Data/Inla/weightedSampler.RDS")
all_effort_final <- readRDS("Data/Inla/weightedSampler.RDS")
