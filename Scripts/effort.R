# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load data ####
cap <- read.csv("Data/Raw/tbl_capture_EVENTS_2023.csv") # culling capture events
# badgers_all <- readRDS("Data/badgers_setts.RDS") # badgers georef to capture sett
sett_all <- readRDS("Data/sett_all_2023.RDS")
his1 <- read_excel(path = "Data/Raw/tbl_sett_record_history1.xlsx")
his2 <- read_excel(path = "Data/Raw/tbl_sett_record_history2.xlsx", 
                   col_names = names(his1))
his3 <- read_excel(path = "Data/Raw/tbl_sett_record_history3.xlsx", 
                   col_names = names(his1))
sett_history <- bind_rows(his1, his2, his3)
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp")
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
str(sett_history)
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
sett_geometry <- sett_all %>% 
  select(SETT_ID, geometry) %>% 
  distinct()

# clean sett history data 
sett_history_clean <- sett_history %>% 
  select(SETT_ID, CAPTURE_BLOCK_EVENT, CAPTURE_BLOCK_ID) 

# merge sett_history with capture data to subset only the capture events we're interested in 
cap_clean2 <- cap_clean %>% 
  left_join(sett_history_clean, by = "CAPTURE_BLOCK_EVENT")
# this has repeated rows for every capture block event because there's more than one sett per event

# add sett location
cap_clean2 <- cap_clean2 %>% 
  inner_join(sett_geometry) %>% 
  st_as_sf(sf_column_name = "geometry") %>% 
  st_set_crs(st_crs(quart))

# intercept setts and quartiles to see in which sett a quartile is 
setts_per_quartile <- st_intersection(quart, cap_clean2) %>% 
  filter(YEAR > 2018) %>% 
  st_drop_geometry()


# from 2019 onwards, how many unique capture events in each quartile
quart_sum <- setts_per_quartile %>% 
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

ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = all_effort_final, aes(fill = WEIGHT, col = WEIGHT)) + 
  geom_sf(data = ireland_counties, fill = NA, col = "black") + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  labs(title = "Sampling effort of culling and vaccination programmes combined", 
       fill = "N days (log)", 
       col = "N days (log)") 