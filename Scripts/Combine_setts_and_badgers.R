rm(list = ls())
source("Scripts/setup.R")

# Load datasets ####

sett_all <- readRDS("Data/sett_all_2025.RDS")
badgers_all <- readRDS("Data/badgers_all_2025.RDS")
# IEC_data <- readRDS("Data/IEC_data_2016-2022.RDS")
ireland <- st_read("Data/Other/ireland_ITM.shp")

# Merge badger and sett data ####

sett_geometry <- sett_all %>% 
  dplyr::select(SETT_ID) %>%
  mutate(x_coord = st_coordinates(.)[,1],
         y_coord = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  distinct(SETT_ID, .keep_all = TRUE) 

all_data <- badgers_all %>% 
  left_join(sett_geometry) %>% 
  distinct() %>% 
  filter(!is.na(x_coord)) %>% 
  st_as_sf(coords = c("x_coord", "y_coord"), crs = st_crs(ireland), remove = F) 

# we've lost 1,115 badgers here, which were assigned to a sett that doesn't 
# appear on the sett register

saveRDS(all_data, file = "Data/culled_both_programmes.RDS")  

# Display badgerXsett data ####

# How many capture events per sett

all_data <- readRDS(file = "Data/badgers_setts_2025.RDS")  


all_data %>% 
  group_by(SETT_ID) %>% 
  summarise(n_capture_events = n_distinct(CAPTURE_BLOCK_EVENT)) %>%  
  ggplot + 
  geom_bar(aes(x = n_capture_events)) + 
  theme_bw() + 
  labs(x = "Number of capture events per sett", y = "Number of setts")

# map capture events per sett and year spatially
all_data %>% 
  group_by(SETT_ID, YEAR) %>% 
  summarise(n_capture_events = n_distinct(CAPTURE_BLOCK_EVENT), 
            x_coord = unique(x_coord), 
            y_coord = unique(y_coord)) %>%  
  filter(!is.na(x_coord)) %>%
  filter(!is.na(YEAR)) %>% 
  st_as_sf(coords = c("x_coord", "y_coord"), crs = st_crs(ireland)) %>% 
  ggplot + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "gray40") + 
  geom_sf(data = . %>% filter(n_capture_events>1), aes(col = n_capture_events), alpha = 0.5, size = 1) + 
  scale_color_viridis() + 
  labs(col = "N of capture events") + 
  theme_bw() + 
  facet_wrap(~YEAR, nrow = 3) + 
  ggtitle("N capture events for setts checked more than once")

ggsave(file = "Outputs/capture_events_sett_year.png", scale = 2)


# map capture events per sett spatially
all_data %>% 
  group_by(SETT_ID) %>% 
  summarise(n_capture_events = n_distinct(CAPTURE_BLOCK_EVENT), 
            x_coord = unique(x_coord), 
            y_coord = unique(y_coord)) %>%  
  filter(!is.na(x_coord)) %>%
  st_as_sf(coords = c("x_coord", "y_coord"), crs = st_crs(ireland)) %>% 
  ggplot + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "gray40") + 
  geom_sf(data = . %>% filter(n_capture_events>1), aes(col = n_capture_events), size = 1) + 
  geom_sf(data = ireland, col = "red", fill = NA) +
  scale_color_viridis() + 
  labs(col = "N of capture events") + 
  theme_bw() + 
  ggtitle("N capture events for setts checked more than once")

# Jitter badger locations to model them as lgcp ####
sampler <- readRDS("Data/Inla/weightedSampler.RDS")

sampler <- sampler %>% 
  filter(NDAYS > 0) %>% 
  st_buffer(0)

all_data_jit <- all_data %>% 
  st_jitter(amount = 500) %>% 
  mutate(YEAR = as.numeric(as.character(YEAR))) %>% 
  filter(YEAR > 2018)
  
filtered = st_filter(all_data_jit, sampler, .pred = st_intersects)


ggplot(filtered) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(data = sampler, fill = NA, col = "red") + 
  geom_sf(col = "black", alpha = 0.5, size = 0.5) + 
  theme_bw()


saveRDS(filtered, "Data/badgers_jittered_filtered_2025.RDS")

badgers_jit <- readRDS("Data/badgers_jittered_filtered_2025.RDS")

badgers_2 <- badgers_jit %>% 
  st_transform(4326) %>% 
  mutate(Lon = st_coordinates(.)[,1], 
         Lat = st_coordinates(.)[,2], 
         Spec = "Badger")

badgers_thin <- spThin::thin(badgers_2, 
                             long.col = "Lon", 
                             lat.col = "Lat",
                             spec.col = "BADGER_ID",
                             thin.par = 0.25, 
                             reps = 1, 
                             out.dir = "Data/") 


badgers_thin <- read.csv("Data/thinned_data_thin1.csv")

badgers_thin <- badgers_thin %>% 
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)  %>% 
  st_transform(st_crs(ireland))

ggplot() + 
  geom_sf(data = ireland) + 
  geom_sf(data = badgers_jit, size = 1) + 
  theme_bw() + 
  
ggplot() + 
  geom_sf(data = ireland) + 
  geom_sf(data = badgers_thin, size = 1) + 
  theme_bw() 
  
saveRDS(badgers_thin, file = "Data/badgers_thin.RDS")  

# Summarise data by sett to model as Poison count data ####

p <- function(v) {
  Reduce(f=paste, x = v)
}



all_data_sum <- all_data %>% 
  st_set_geometry(NULL) %>% 
  mutate(YEAR = as.numeric(levels(YEAR))[YEAR]) %>% 
  filter(YEAR > 2018) %>% 
  group_by(SETT_ID, CAPTURE_BLOCK_EVENT) %>%
  summarise(n_badgers = n()) %>% 
  group_by(SETT_ID) %>% 
  summarise(
    effort = n(), 
    badgers = p(as.character(n_badgers)))
  
# filter setts outside of the effort ####
effort <- readRDS("Data/Inla/weightedSampler.RDS") %>% 
  filter(NDAYS > 0)

setts_in = st_filter(sett_all, effort, .pred = st_intersects)
saveRDS(setts_in, file = "Data/sett_2025_inside_effort.RDS")

# Divide into vaccination and culling badgers to try and model separately ####

all_data <- readRDS(file = "Data/badgers_setts.RDS")

## culling ####
badgers_cull <- all_data %>% 
  subset(PROGRAMME = "Culling") %>% 
  mutate(YEAR = year(DATE_CAUGHT)) %>% 
  filter(YEAR >2018)

cul_effort <- readRDS("Data/Inla/cul_effort.RDS")

cul_sampler <- cul_effort %>% 
  filter(NDAYS > 0) %>%
  st_buffer(0)

# check all culled badgers are within the effort zones

ggplot(badgers_cull) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(data = cul_sampler, aes(fill = NDAYS)) + 
  scale_fill_viridis() + 
  geom_sf(col = "black", alpha = 0.5, size = 0.5) + 
  theme_bw()

badgers_cull_in = st_filter(badgers_cull, cul_sampler, .pred = st_intersects)

culling_jit <- badgers_cull_in %>% 
  st_jitter(amount = 500) 

culling_filtered = st_filter(culling_jit, cul_sampler, .pred = st_intersects)

saveRDS(culling_filtered, "Data/culling_badgers_jittered_filtered.RDS")

## vaccination ####
badgers_vac <- all_data %>% 
  subset(PROGRAMME = "Vaccination") %>% 
  mutate(YEAR = year(DATE_CAUGHT)) %>% 
  filter(YEAR >2018)

vac_effort <- readRDS("Data/Inla/vacc_effort.RDS")

vac_sampler <- vac_effort %>% 
  filter(NDAYS > 0) 

# check all culled badgers are within the effort zones

ggplot(badgers_vac) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(data = vac_sampler, aes(fill = NDAYS)) + 
  scale_fill_viridis() + 
  geom_sf(col = "black", alpha = 0.5, size = 0.5) + 
  theme_bw()

badgers_vac_in = st_filter(badgers_vac, vac_sampler, .pred = st_intersects)

vac_jit <- badgers_vac_in %>% 
  st_jitter(amount = 500) 

vac_filtered = st_filter(vac_jit, vac_sampler, .pred = st_intersects)

ggplot(vac_filtered) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(data = vac_sampler, aes(fill = NDAYS)) + 
  scale_fill_viridis() + 
  geom_sf(col = "black", alpha = 0.5, size = 0.5) + 
  theme_bw()

saveRDS(vac_filtered, "Data/vaccination_badgers_jittered_filtered.RDS")


## revert back to samplers without effort

sett_all <- readRDS("Data/sett_new_data.RDS")
samplers <- readRDS("Data/Inla/samplers.RDS")


ggplot() + 
  geom_sf(data = ireland_counties, fill = "orange") + 
  geom_sf(data = samplers) + 
  geom_sf(data = sett_all, size = 0.5, alpha = 0.5) + 
  theme_bw()

# Culling history ####
all_data <- readRDS("Data/culled_both_programmes.RDS")  
env_vars <- terra::rast("Data/Covars/final_covars_terra_with_setts.grd")

all_data_sub <- st_transform(all_data, st_crs(env_vars))  %>% 
  mutate(YEAR = as.numeric(as.character(YEAR))) %>% 
  filter(YEAR < 2019 & YEAR > 2014)

cull_kde <- sf.kde.new(
  all_data_sub,
  bw = 30,
  ref = env_vars$elevation,
  # res = res(env_vars$elevation),
  standardize = FALSE,
  # scale.factor = 10000,
  scale.factor = 1,
  mask = F)

ggplot() + 
  geom_spatraster(data = cull_kde, aes(fill = z)) + 
  scale_fill_viridis_c() + 
  # geom_sf(data= all_data_sub, size = 0.5) + 
  theme_bw()

ROI <- ireland %>% 
  filter(NAME_TAG %!in% c("Antrim", "Armagh", "Down", "Fermanagh", 
                          "Londonderry", "Tyrone")) 

ROI <- st_transform(ROI, st_crs(cull_kde))
cull_kde_m <- mask(cull_kde, ROI)

plot(cull_kde_m)

writeRaster(cull_kde_m, "Data/Covars/culling_history_unscaled.grd", overwrite = T) 

saveRDS(cull_kde_m, file = "Data/Covars/culling_history_unscaled.RDS")
