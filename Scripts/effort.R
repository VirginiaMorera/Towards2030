# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load data ####
cap <- read.csv("Data/Raw/tbl_capture_EVENTS_2023.csv") # culling capture events
badgers_all <- readRDS("Data/badgers_setts.RDS") # badgers georef to capture sett
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
vac_cap <- read.csv("Data/Raw/tbl_vaccine_2023.csv") # vaccine capture data
quart <- read_sf("Data/Raw/just_quartiles.shp") %>% 
  rename(QUARTILE = Q) %>% 
  st_set_crs(29902) %>% 
  st_transform(st_crs(badgers_all))


# culling capture events ####

str(cap)

# clean capture data

cap_clean  <-  cap  %>%  
  dplyr::select(-DATE_ENTERED_IN_DATABASE, -diff2) %>% 
  mutate(DATE_COMMENCED = dmy(DATE_COMMENCED), 
         DATE_COMPLETED = dmy(DATE_COMPLETED), 
         year = year(DATE_COMMENCED)) 

# clean badger data 

badgers_clean <- badgers_all %>% 
  mutate(YEAR= as.numeric(as.character(YEAR))) %>% 
  filter(YEAR > 2018)
  

# put together 

all <- left_join(badgers_clean, cap_clean, by = "CAPTURE_BLOCK_EVENT") 


# summarise nº of visits per sett

sum <- badgers_clean %>% 
  filter(!is.na(CAPTURE_BLOCK_EVENT)) %>% # this will remove all vaccination badgers and some non-assigned badgers
  group_by(SETT_ID) %>% 
  summarise(n_visits = n_distinct(CAPTURE_BLOCK_EVENT)) %>% 
  ungroup() 

ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = sum, aes(col = n_visits)) + 
  scale_color_viridis_c(trans = scales::pseudo_log_trans(sigma = 0.001)) +
  theme_bw()

# assign setts to quartiles 

sett_quart <- quart %>% st_join(badgers_clean, left=TRUE)

# summarise number of unique capture events per qartile

sum2 <- sett_quart %>% 
  filter(!is.na(CAPTURE_BLOCK_EVENT)) %>% 
  group_by(QUARTILE) %>% 
  summarise(n_events = n_distinct(CAPTURE_BLOCK_EVENT)) %>% 
  ungroup()


p1 <- ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = sum2, aes(col = n_events, fill = n_events)) + 
  scale_color_viridis_c(na.value = "gray50") +
  scale_fill_viridis_c(na.value = "gray50") +
  theme_bw() + 
  ggtitle("Culling programme effort in ~11 day units")


# vaccine effort ####


# quartiles

vac_cap <- vac_cap %>% 
  filter(VACCINE_STATUS == "APPROVED") %>% 
  mutate(DATE_COMMENCED = ymd_hm(DATE_COMMENCED))


vac_cap_sum <- vac_cap %>% 
  group_by(QUARTILE) %>% 
  summarise(events = n_distinct(EVENT)) %>% 
  ungroup()

quart_effort <- full_join(quart, vac_cap_sum)


p2 <- ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = quart_effort, aes(fill = events, col = events)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  theme_bw() + 
  ggtitle("Vaccination programme effort in 5 day units")
  

gridExtra::grid.arrange(p1, p2, nrow = 1)


sett_2015 <- sett_all%>% 
  filter(DATE_OF_FIELD_VISIT >= 2015) %>% 
  filter(MAIN_SETT == "Yes")

sett_2019 <- sett_all%>% 
  filter(DATE_OF_FIELD_VISIT >= 2019) %>% 
  filter(MAIN_SETT == "Yes")


pp1 <- ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = sett_2015, col = "violet") +
  theme_bw() + 
  ggtitle("Setts from 2015 onwards")

pp2 <- ggplot() + 
  geom_sf(data = ireland_outline_sf, fill = "lightgray", col = "black") + 
  geom_sf(data = sett_2019, col = "orange") +
  theme_bw() + 
  ggtitle("Setts from 2019 onwards")

gridExtra::grid.arrange(pp1, pp2, nrow = 1)
