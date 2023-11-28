rm(list = ls())
source("Scripts/setup.R")

# Load datasets ####

sett_all <- readRDS("Data/sett_all_2023.RDS")
badgers_all <- readRDS("Data/badgers_all_2023.RDS")
IEC_data <- readRDS("Data/IEC_data_2016-2022.RDS")
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
  st_as_sf(coords = c("x_coord", "y_coord"), crs = st_crs(ireland)) 
  
  
# Display badgerXsett data ####

# How many capture events per sett

all_data %>% 
  group_by(SETT_ID) %>% 
  summarise(n_capture_events = n_distinct(CAPTURE_BLOCK_EVENT)) %>%  
  ggplot + 
  geom_bar(aes(x = n_capture_events)) + 
  theme_bw() + 
  labs(x = "Number of capture events", y = "Number of setts")

table(x$n_capture_events) # ~11000 setts have only been checked once (to keep in mind for CMR)

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
  geom_sf(data = . %>% filter(n_capture_events>1), aes(col = n_capture_events), alpha = 0.5, size = 1) + 
  scale_color_viridis() + 
  labs(col = "N of capture events") + 
  theme_bw() + 
  ggtitle("N capture events for setts checked more than once")

# Jitter badger locations to model them as lgcp ####

all_data_jit <- all_data %>% 
  st_jitter(amount = 1000)

ggplot(all_data_jit) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "black", alpha = 0.5, size = 0.5) + 
  theme_bw()

saveRDS(all_data_jit, "Data/badgers_jittered.RDS")

# Summarise data by sett to model as Poison count data ####

p <- function(v) {
  Reduce(f=paste, x = v)
}



all_data_sum <- all_data %>% 
  st_set_geometry(NULL) %>% 
  mutate(YEAR = as.numeric(levels(YEAR))[YEAR]) %>% 
  filter(YEAR >= 2015) %>% 
  group_by(SETT_ID, CAPTURE_BLOCK_EVENT) %>%
  summarise(n_badgers = n()) %>% 
  group_by(SETT_ID) %>% 
  summarise(
    effort = n(), 
    badgers = p(as.character(n_badgers)))
  
  
  