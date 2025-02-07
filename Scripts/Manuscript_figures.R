# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load ireland maps ####
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS") %>% 
  st_transform(crs = projKM) 

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 


# load sett data ####
sett_all <- readRDS("Data/sett_all_inside_effort.RDS") %>% 
  st_transform(crs = projKM) 

sett_subset <- sett_all %>% 
  mutate(year = lubridate::year(DATE_OF_FIELD_VISIT)) %>% 
  filter(year > 2018) %>% 
  filter(ACTIVITY > 1) %>% 
  filter(MAIN_SETT == "Yes") %>%
  filter(CAPTURE_BLOCK_ID %!in% c("NOT ASSIGN", "NOT ASSIGNE")) %>% 
  select(SETT_ID, geometry)


# load badger data ####
badgers_all <- readRDS("Data/badgers_jittered_filtered.RDS") %>% 
  st_transform(crs = projKM) 


# load effort data ####
effort <- readRDS("Data/Inla/weightedSampler.RDS") %>% 
  st_transform(crs = projKM) 


# Fig 1: data maps ####
pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_1s.pdf", width = 12, height = 10)
ggplot() + 
  geom_sf(data = ireland_counties, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = sett_subset, alpha = 0.5) + 
  theme_bw() +
  
ggplot() +   
  geom_sf(data = ireland_counties, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = badgers_all, alpha = 0.5, shape = 1, size = 1, stroke = 0.1) + 
  theme_bw() +

ggplot() + 
  geom_sf(data = effort, aes(fill = NDAYS, col = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "darkgray", fill = NA) + 
  scale_fill_viridis_c(na.value = NA, name = "N. days") + 
  scale_colour_viridis_c(na.value = NA, name = "N. days") + 
  theme_bw() +
  
plot_layout(nrow = 2) + 
plot_annotation(tag_levels = 'a')

dev.off()


