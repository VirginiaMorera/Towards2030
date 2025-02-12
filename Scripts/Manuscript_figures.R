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

# Fig. 2: covariate effects ####
# For this to work the code in the sett model script that obtains the non spatial evaluation of each covariate needs to have been run 
pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_2.pdf", 
    width = 8, height = 8)
multiplot(
  eval.elev,
  eval.grasslandsPastures,
  eval.hfi,
  
  eval.slope,
  eval.forestdist,
  eval.topo,
  cols = 2 
)
dev.off()


# Fig. 3: sett distribution map ####
rp4 <- readRDS("Outputs/sett_model/response_predictor.RDS")
inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_3.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = y, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "a)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D", 
                       breaks = c(0.005, 0.010, 0.015, 0.020, 0.025)) +
  NULL + 
  
  ggplot() + 
  gg(data = y, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "95% CI width", 
       title = "b)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D", 
                       breaks = c(0.005, 0.010, 0.015, 0.020, 0.025, 0.030)) +
  NULL

dev.off()


# Fig S1. Spatial covariate effects ####
rp4 <- readRDS("Outputs/sett_model/response_predictor.RDS")
inside = sapply(st_intersects(lp4$elevation, ireland_outline_sf), function(x){length(x)==0})

elev_df <- lp4$elevation[!inside,] 
elev_df <- elev_df %>% 
  mutate(Variable = "Elevation")

slope_df <- lp4$slope[!inside,] 
slope_df <- slope_df %>% 
  mutate(Variable = "Slope")

grass_df <- lp4$grassland[!inside,] 
grass_df <- grass_df %>% 
  mutate(Variable = "Grasslands and pastures")

topo_df <- lp4$topoWetness[!inside,]
topo_df <- topo_df %>%
  mutate(Variable = "Topographic wetness index")

hfi_df <- lp4$hfi[!inside,]
hfi_df <- hfi_df %>%
  mutate(Variable = "Human footprint index")

forestdist_df <- lp4$forestDistance[!inside,]
forestdist <- forestdist_df %>%
  mutate(Variable = "Distance to forest edge")

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S1.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
  ggplot() + 
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
  ggplot() + 
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Pasture and grasslands effect", x = "", y = "", fill = "Median") +
  
  ggplot() +
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Topographic wetness index effect", x = "", y = "", fill = "Mean") +
  
  ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Human footprint index effect", x = "", y = "", fill = "Mean") +
  
  ggplot() +
  gg(data = forestdist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Distance to forest edge", x = "", y = "", fill = "Mean") +
  
  plot_layout(ncol = 3)

dev.off()

# Fig S2: spatial field ####
lp4 <- readRDS("Outputs/sett_model/linear_predictor.RDS")

inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S2.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", title = "Spatial random field") + 
  
  ggplot() + 
  gg(data = spb, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_viridis_c() +
  labs(x = "", y = "", fill = "95% CI", title = "Uncertainty of the spatial random field") 

dev.off()
