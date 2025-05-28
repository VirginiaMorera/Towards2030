rm(list = ls())
source("Scripts/setup.R")
ireland <- st_read("Data/Other/ireland_ITM.shp")


# Sett database ####
# this contains all setts ever found, environmental info, and date they were first found

## load sett database ####
sett <- read_xlsx("Data/Raw/TBL_SETT_RECORD_2025.xlsx")

## clean sett database ####
sett_clean <- sett %>% 
  # remove unnecessary or empty variables
  dplyr::select(
    SETT_ID, DATE_OF_FIELD_VISIT, 
    MAIN_SETT, ACTIVITY, OTHERSETTTYPE, RESTRAINTS_LAID, RESTRAINTS, RESTRAINTS_RECOVERED,
    HEDGEROW:SCRUB, GREENFIELD_SITE,
    NO_BADGERS_CAPTURED,
    CAPTURE_BLOCK_ID, CAPTURE_BLOCK_EVENT, 
    CLUSTER_ID, QUARTILE, SETT_NO, X_COORDINATE, Y_COORDINATE) %>% 
  # check how many badgers have at least one habitat
  rowwise() %>%
  mutate(AnyHabitat = sum(c_across(c(HEDGEROW:SCRUB, GREENFIELD_SITE)))) %>%
  ungroup() %>%
  # turn main sett into factor
  mutate(MAIN_SETT = if_else(MAIN_SETT == 1, "Yes", "No"), 
         MAIN_SETT = as.factor(MAIN_SETT), 
         # convert other sett type into what they mean
         OTHERSETTTYPE = recode(OTHERSETTTYPE, '0' = "NA", '1' = "Former", 
                                '2' = "Disused", '3' = "Dormant", 
                                '4' = "Annexed", '5' = "Subsidiary", 
                                '6' = "Other"), 
         OTHERSETTTYPE = as.factor(OTHERSETTTYPE), 
         # turn date into date format, add month and year
         DATE_FOUND = dmy(DATE_OF_FIELD_VISIT), 
         YEAR_FOUND = year(DATE_FOUND),
         MONTH_FOUND = month(DATE_FOUND, label = TRUE),
         # sett all empty OTHERSETTTYPE as NA
         OTHERSETTTYPE = na_if(OTHERSETTTYPE, "NA"), 
         RESTRAINTS = as.numeric(RESTRAINTS), 
         RESTRAINTS_LAID = as.numeric(RESTRAINTS_LAID), 
         RESTRAINTS_RECOVERED = as.numeric(RESTRAINTS_RECOVERED), 
         NO_BADGERS_CAPTURED = as.numeric(NO_BADGERS_CAPTURED)) %>% 
  # filter(RESTRAINTS > 0 & RESTRAINTS < 100, 
  #        RESTRAINTS_LAID > 0 & RESTRAINTS_LAID < 100, 
  #        RESTRAINTS_RECOVERED > 0 & RESTRAINTS_RECOVERED < 100) %>% 
  # turn into spatial 
  st_as_sf(coords = c("X_COORDINATE", "Y_COORDINATE"), 
           crs = 29903) %>% # Irish Grid 
  # transform to ITM
  st_transform(st_crs(ireland)) %>% #IRENET
  # keep coordinates but remove geometry
  mutate(x_coordinate = sf::st_coordinates(.)[,1],
         y_coordinate = sf::st_coordinates(.)[,2])  
  # st_set_geometry(NULL) %>% 
  
attributes(sett_clean$x_coordinate) <- NULL
attributes(sett_clean$y_coordinate) <- NULL

ggplot() + 
  geom_sf(data = ireland) + 
  geom_sf(data = sett_clean) + 
  theme_bw()

#remove weird point out at sea

sett_clean <- sett_clean %>% 
  mutate(Lon = st_coordinates(.)[,2]) %>%
  filter(Lon > min(Lon, na.rm = T)) %>%
  select(-Lon) 


# saveRDS(sett_clean, file = "Data/sett_all_2025.RDS")
sett_all <- readRDS("Data/sett_all_2025.RDS")

## Visualise sett database ####

## main setts
sett_all %>% 
  distinct(SETT_ID, .keep_all = T) %>% 
  ggplot +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(aes(col = MAIN_SETT), alpha = 0.5, size = 0.75) + 
  scale_color_viridis_d() + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))

## setts by year 

ggplot(sett_all) +
  geom_bar(aes(x = YEAR_FOUND, fill = MAIN_SETT), stat = "count") + 
  theme_bw()

# some areas have not been sampled. Are there no setts? or no farms? 

## active setts 
sett_all %>% 
  distinct(SETT_ID, .keep_all = T) %>% 
  ggplot + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(aes(col = ACTIVITY), alpha = 0.5, size = 1) + 
  scale_color_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", col = "Activity score") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))


## number of restrains
sett_all %>% 
  filter(RESTRAINTS < 40 & RESTRAINTS > 0) %>%
  ggplot +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(aes(col = RESTRAINTS), alpha = 0.5, size = 1) +  
  scale_color_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", col = "N of restrains") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))


## number of badgers
ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "darkgray") +  
  geom_sf(data = . %>% filter(NO_BADGERS_CAPTURED >= 0), aes(col = NO_BADGERS_CAPTURED), 
          size = 1, alpha = 0.5) +  
  scale_colour_viridis_c() + 
  labs(x = "Longitude", y = "Latitude", col = "Number of badgers") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1)))
# we don't really know what this data is at all


## habitats
sett_all %>%
  pivot_longer(GREENFIELD_SITE:SCRUB, names_to = "Habitat_type", 
               values_to = "Habitat_presence") %>%
  filter(!is.na(Habitat_presence)) %>%
  ggplot +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(aes(col = as.factor(Habitat_presence)), alpha = 0.7, size = 0.5) +
  facet_wrap(~Habitat_type) +
  labs(x = "Longitude", y = "Latitude", col = "Habitat present") +
  theme_bw()

# ggsave(filename = "Outputs/habitat_plot.png", scale = 2)

img = readPNG("Outputs/habitat_plot.png")
grid::grid.raster(img)


#### visualise setts by year ####

## before 2000
p1 <- ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "darkgray", size = 0.5) +  
  geom_sf(data = . %>% filter(YEAR_FOUND < 2000), 
          aes(col = MAIN_SETT), size = 1, alpha = 0.5) +  
  scale_colour_viridis_d(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("Before 2000")

# these can very clearly be removed

## 2000 - 2005
p2 <- ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "darkgray", size = 0.5) +  
  geom_sf(data = . %>% filter(YEAR_FOUND < 2005 & YEAR_FOUND > 2000), 
          aes(col = MAIN_SETT), size = 1, alpha = 0.5) +  
  scale_colour_viridis_d(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("2000 - 2005")

# until 2005 all year info comes from the sett dataset (so no issue with data entering dates)

## 2005 - 2010
p3 <- ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "darkgray", size = 0.5) +  
  geom_sf(data = . %>% filter(YEAR_FOUND < 2010 & YEAR_FOUND > 2005), 
          aes(col = MAIN_SETT), size = 1, alpha = 0.5) +  
  scale_colour_viridis_d(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("2005 - 2010")

## 2010 - 2015
p4 <- ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "darkgray", size = 0.5) +  
  geom_sf(data = . %>% filter(YEAR_FOUND < 2015 & YEAR_FOUND > 2010), 
          aes(col = MAIN_SETT), size = 1, alpha = 0.5) +  
  scale_colour_viridis_d(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") +  
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("2010 - 2015")

## 2015 - 2020
p5 <- ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "darkgray", size = 0.5) +  
  geom_sf(data = . %>% filter(YEAR_FOUND < 2020 & YEAR_FOUND > 2015), 
          aes(col = MAIN_SETT), size = 1, alpha = 0.5) +  
  scale_colour_viridis_d(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") +  
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("2015 - 2020")

## after 2020
p6 <- ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(col = "darkgray", size = 0.5) +  
  geom_sf(data = . %>% filter(YEAR_FOUND > 2020), 
          aes(col = MAIN_SETT), size = 1, alpha = 0.5) +  
  scale_colour_viridis_d(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") +  
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1))) + 
  ggtitle("After 2020")


gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

# Sett visit history ####

## load sett history ####
sett_history <- readRDS("Data/Raw/sett_history_with_dates-RDS")

sett_info <- sett_all %>% 
  select(-NO_BADGERS_CAPTURED, -CAPTURE_BLOCK_ID, -CAPTURE_BLOCK_EVENT, -QUARTILE)


setts_2019 <- sett_history %>% 
  mutate(YEAR = year(DATE_COMMENCED)) %>% 
  filter(YEAR > 2018) %>% 
  inner_join(sett_info) %>% 
  st_as_sf(sf_column_name = "geometry", crs = st_crs(sett_all)) %>% 
  select(SETT_ID, MAIN_SETT) %>% 
  distinct()

saveRDS(setts_2019, file = "Data/setts_2025.RDS")

# Plot final dataset for modelling
ggplot() + 
  geom_sf(data = ireland, fill = NA) + 
  geom_sf(data = setts_2019, aes(col = MAIN_SETT)) + 
  theme_bw() + labs(col = "Main")
 


