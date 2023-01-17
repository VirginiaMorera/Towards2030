rm(list = ls())
source("Scripts/setup.R")

#######################
#### Load datasets ####
#######################

sett_all <- readRDS("Data/sett_all.RDS")
badgers_all <- readRDS("Data/badgers_all.RDS")
ireland <- st_read("Data/ireland_ITM.shp")

####################################
#### Merge badger and sett data ####
####################################

sett_geometry <- sett_all %>% 
  # dplyr::select(SETT_ID) %>% 
  mutate(x_coord = st_coordinates(.)[,1],
         y_coord = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  distinct(SETT_ID, .keep_all = TRUE) 

all_data <- badgers_all %>% 
  left_join(sett_geometry) %>% 
  distinct() 
  

##################################
#### Display badgerXsett data ####
##################################

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
  group_by(SETT_ID, year_captured) %>% 
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
  facet_wrap(~year_captured, nrow = 3) + 
  ggtitle("N capture events for setts checked more than once")

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


################################################
#### calculate badgers per capture per sett ####
################################################

badger_count <- all_data %>% 
  filter(!is.na(CAPTURE_BLOCK_EVENT)) %>%  # 16% (>17000 rows) of badger obs don't have capture block event
  group_by(SETT_ID, CAPTURE_BLOCK_EVENT) %>% 
  summarise(N_badgers = n()) %>% 
  ungroup() %>% 
  group_by(SETT_ID) %>% 
  summarise(Avg_badgers = mean(N_badgers, na.rm = T)) %>% 
  left_join(sett_geometry[,c("SETT_ID", "x_coord", "y_coord")]) %>% 
  filter(!is.na(x_coord)) %>% 
  st_as_sf(coords = c("x_coord", "y_coord"), crs = st_crs(ireland))

ggplot(badger_count) + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(aes(col = Avg_badgers)) + 
  scale_color_viridis_c() + 
  theme_bw()



#################################################################
#### calculate avg badger weight per year and sett ####
#################################################################

badger_weight <- all_data %>% 
  filter(!is.na(CAPTURE_BLOCK_EVENT)) %>%  # 16% (>17000 rows) of badger obs don't have capture block event
  group_by(year_captured, SETT_ID) %>% 
  summarise(Avg_weight = mean(WEIGHT, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(sett_geometry[,c("SETT_ID", "x_coord", "y_coord")]) %>% 
  filter(!is.na(x_coord)) %>% 
  st_as_sf(coords = c("x_coord", "y_coord"), crs = st_crs(ireland))

ggplot(badger_weight) + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(aes(col = Avg_weight)) + 
  scale_color_viridis_c() + 
  facet_wrap(~year_captured, nrow = 3) + 
  theme_bw()



