rm(list = ls())
source("Scripts/setup.R")

##################################
#### Load and clean sett data ####
##################################

# load
sett <- read_csv("raw_data/forR_SETT_SUMMARY_111121.csv")
sett_spatial <- read_sf("raw_data/Sett_Summary_111121.shp")
ireland <- st_read("Data/ireland_ITM.shp")

# remove unnecessary columns and clean
sett_clean <- sett %>% 
  dplyr::select(-DVO_CODE, -SPECIAL_SETT, -SETT2_OPENINGS_USED,
                -SETT2_RESTRAINTS, -SETT3_OPENINGS_USED, -SETT3_RESTRAINTS, 
                -SETT_LOCATION_ADDED, -LANDS_RENTED, -OTHER_LOCATION) %>% 
  rename(EVIDENCE_OF_INTERFERENCE = EVIDENCCE_OF_INTERFERENCE) %>% 
  # filter "legacy" obs that Philip told us to disregard
  filter(ACTIVE <= 1) %>% 
  filter(EVIDENCE_OF_INTERFERENCE <= 1) %>% 
  # check how many badgers have at least one habitat
  rowwise() %>% mutate(AnyHabitat = sum(c_across((GREENFIELD_SITE:SCRUB)))) %>% 
  # turn main sett into factor
  mutate(MAIN_SETT = if_else(MAIN_SETT == 1, "Yes", "No"), 
         MAIN_SETT = as.factor(MAIN_SETT), 
         # convert other sett type into what they mean
         OTHERSETTTYPE = recode(OTHERSETTTYPE, '0' = "NA", '1' = "Former", '2' = "Disused", '3' = "Dormant", 
                                '4' = "Annexed", '5' = "Subsidiary", '6' = "Other"), 
         OTHERSETTTYPE = as.factor(OTHERSETTTYPE), 
         # convert activity into a factor
         ACTIVE = if_else(ACTIVE == 1, "Yes", "No"), 
         ACTIVE = as.factor(ACTIVE), 
         # turn date into date format
         DATE_OF_FIELD_VISIT = dmy(DATE_OF_FIELD_VISIT), 
         YEAR = year(DATE_OF_FIELD_VISIT),
         MONTH = month(DATE_OF_FIELD_VISIT, label = TRUE),
         SETT_ID = as.numeric(SETT_ID), 
         OTHERSETTTYPE = na_if(OTHERSETTTYPE, "NA"))


sett_spatial <- sett_spatial %>% 
  dplyr::select(-TOTAL_BADG, -CAPT_BADGE, -VACC_BADGE, -DVO, -X_COORDINA, -Y_COORDINA) %>%
  mutate(SETT_ID = as.numeric(SETT_ID), 
         BADGERS = as.numeric(BADGERS), 
         RESTRAINTS = gsub("`", "", RESTRAINTS),
         RESTRAINTS = as.numeric(RESTRAINTS), 
         MAIN_SETT = if_else(MAIN_SETT == 1, "Yes", "No"), 
         MAIN_SETT = as.factor(MAIN_SETT)) %>% 
  rename(MAIN_SETT_sp = MAIN_SETT, RESTRAINTS_sp = RESTRAINTS, DATE_OF_FIELD_VISIT_sp = VISITDATE)

sett_all <- full_join(sett_clean, sett_spatial)

sett_all <- sett_all %>% 
  relocate(MAIN_SETT_sp, .after = MAIN_SETT) %>% 
  relocate(RESTRAINTS_sp, .after = RESTRAINTS) %>% 
  relocate(DATE_OF_FIELD_VISIT_sp, .after = DATE_OF_FIELD_VISIT)

table(sett_all$MAIN_SETT, sett_all$OTHERSETTTYPE)

sett_all <- sett_all %>% 
  st_as_sf(sf_column_name = "geometry")

#saveRDS(sett_all, file = "Data/sett_all.RDS")

#############################
#### Visualise sett data ####
#############################

# temporal trends of sett visits

ggplot(sett_all) + 
  geom_histogram(aes(x = YEAR), binwidth = 1) + 
  theme_bw() + 
  ggtitle("Sett visits by year")

ggplot(sett_all) + 
  geom_bar(aes(x = MONTH), stat = "count") + 
  theme_bw() + 
  ggtitle("Sett visits by month")

# other characteristics

sett_all%>% 
  # filter(OPENINGS < 40) %>%
  ggplot + 
  geom_bar(aes(x = OPENINGS), stat = "count") + 
  theme_bw() + 
  ggtitle("Number of openings")

sett_all%>% 
  # filter(OPENINGS_USED < 20) %>%
  ggplot + 
  geom_bar(aes(x = OPENINGS_USED), stat = "count") + 
  theme_bw() + 
  ggtitle("Number of openings used")

sett_all%>% 
  # filter(SPOIL_HEAPS < 20) %>%
  ggplot + 
  geom_bar(aes(x = SPOIL_HEAPS), stat = "count") + 
  theme_bw() + 
  ggtitle("Spoil heaps")
  
sett_all%>% 
  # filter(DISTANCE < 150) %>% 
  ggplot + 
  geom_histogram(aes(x = DISTANCE), binwidth = 10) + 
  theme_bw() + 
  ggtitle("Search distance")

sett_all%>% 
  # filter(RESTRAINTS < 20 & RESTRAINTS > 0) %>%
  ggplot + 
  geom_bar(aes(x = RESTRAINTS), stat = "count") + 
  theme_bw() + 
  ggtitle("Number of restraints used")

sett_all%>% 
  filter(RESTRAINTS < 20 & RESTRAINTS > 0) %>%
  ggplot + 
  geom_histogram(aes(x = RESTRAINTS/DISTANCE), binwidth = 1) + 
  theme_bw() + 
  ggtitle("Restraings per unit of distance (proxy for effort?)")
  
# Spatial visualisations

## main setts

ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(aes(col = MAIN_SETT), alpha = 0.5, size = 0.75) + 
  scale_color_viridis_d() + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))

# some areas have not been sampled. Are there no setts? or no farms? 


## active setts 
ggplot(sett_all) +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(aes(col = ACTIVE), alpha = 0.5, size = 1) + 
  scale_color_viridis_d() + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))


## number of restrains
sett_all %>% 
  filter(RESTRAINTS < 20 & RESTRAINTS > 0) %>%
  ggplot +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = sett_all, col = "lightgray", size = 1) +
  geom_sf(aes(col = RESTRAINTS)) +  
  scale_colour_viridis(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Number of restrains") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))

## number of distance
sett_all %>% 
  filter(RESTRAINTS < 20 & RESTRAINTS > 0)  %>% 
  filter(DISTANCE < 150) %>% 
  ggplot +
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") +
  geom_sf(aes(col = RESTRAINTS, size = DISTANCE)) +  
  scale_colour_viridis(option = "C") + 
  labs(x = "Longitude", y = "Latitude", col = "Number of restrains", size = "Distance") + 
  theme_bw() + 
  guides(col = guide_legend(override.aes = list(alpha = 1)))


## habitats
# sett_all %>% 
#   pivot_longer(GREENFIELD_SITE:SCRUB, names_to = "Habitat_type", values_to = "Habitat_presence") %>% 
#   filter(!is.na(Habitat_presence)) %>%
#   ggplot +
#   geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
#   geom_sf(aes(col = as.factor(Habitat_presence)), alpha = 0.7, size = 0.5) + 
#   facet_wrap(~Habitat_type) +
#   labs(x = "Longitude", y = "Latitude", col = "Habitat present") + 
#   theme_bw() 

# ggsave(filename = "Outputs/habitat_plot.png", scale = 2)

img = readPNG("Outputs/habitat_plot.png")
grid::grid.raster(img)
