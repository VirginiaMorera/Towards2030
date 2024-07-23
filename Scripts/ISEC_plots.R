# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# sett data plots ####

sett_all <- readRDS("Data/sett_all_2023.RDS") %>% 
  st_transform(crs = projKM) 

ireland <- readRDS("Data/Inla/ireland_outline_km.RDS")

quart <- read_sf("Data/Raw/just_quartiles.shp") %>% 
  rename(QUARTILE = Q) %>% 
  st_set_crs(29902) %>% 
  st_transform(st_crs(sett_all))

effort <- readRDS("Data/Inla/weightedSampler.RDS")
cul_effort <- readRDS("Data/Inla/cul_effort.RDS")
vac_effort <- readRDS("Data/Inla/vacc_effort.RDS")

sett_subset <- sett_all %>% 
  mutate(year = lubridate::year(DATE_OF_FIELD_VISIT)) %>% 
  filter(year > 2018) %>% 
  filter(MAIN_SETT == "Yes") %>%
  filter(CAPTURE_BLOCK_ID %!in% c("NOT ASSIGN", "NOT ASSIGNE")) %>% 
  select(SETT_ID, CAPTURE_BLOCK_ID, geometry)

png("Outputs/ISEC_Figures/Ireland_map.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  theme_bw() +
  ggtitle("") + 
  NULL
dev.off()

png("Outputs/ISEC_Figures/Quartiles.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  geom_sf(data = quart, col = "lightgray", fill = NA) + 
  theme_bw() +
  ggtitle("Vaccination and culling programme Quartiles") + 
  NULL
dev.off()

png("Outputs/ISEC_Figures/two_efforts.jpg", width = 7.46*2, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  geom_sf(data = vac_effort %>% filter(weight > 0), aes(col = weight, fill = weight)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  ggtitle("Vaccination programme effort") + 
  theme_bw() +
  

ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  geom_sf(data = cul_effort %>% filter(weight > 0), aes(col = weight, fill = weight)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  ggtitle("Culling programme effort") + 
  theme_bw() +
  
plot_layout(nrow = 1)
dev.off()


png("Outputs/ISEC_Figures/Total_effort.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  geom_sf(data = effort %>% filter(weight > 0), aes(col = weight, fill = weight)) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  ggtitle("Total effort") + 
  theme_bw()
dev.o

png("Outputs/ISEC_Figures/Effort_and_setts.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  geom_sf(data = effort %>% filter(weight > 0), aes(col = weight, fill = weight), 
          alpha = 0.5) + 
  geom_sf(data = sett_subset, size = 1, col = "orange") + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  ggtitle("Main setts") + 
  theme_bw()
dev.off()

# badger data plots ####

badgers_all <- readRDS(file = "Data/badgers_setts.RDS") %>% 
  mutate(YEAR = year(DATE_CAUGHT)) %>% 
  filter(YEAR > 2018)


png("Outputs/ISEC_Figures/badgers_programme.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  geom_sf(data = badgers_all, aes(col = PROGRAMME)) + 
  scale_color_viridis_d(na.value = NA) + 
  labs(title = "Badgers", col = "Programme") + 
  theme_bw()
dev.off()


badgers_sum <- badgers_all %>% 
  group_by(SETT_ID) %>% 
  summarise(n_badgers = n())

png("Outputs/ISEC_Figures/badgers_per_sett.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland, col = "black", fill = NA) + 
  geom_sf(data = badgers_sum, aes(col = n_badgers)) + 
  scale_colour_viridis_c(trans = "log", 
                         breaks = c(1, 2.5, 5, 10, 20), 
                         labels = c(1, 2.5, 5, 10, 20)) + 
  # scale_color_binned(type = "viridis", na.value = NA) + 
  labs(title = "Badgers per sett", col = "No. of badgers") + 
  theme_bw()
dev.off()

badgers_jit <- readRDS("Data/badgers_jittered_filtered.RDS") %>% 
  st_transform(crs = projKM) 

png("Outputs/ISEC_Figures/jittered.jpg", width = 5, height = 5, res = 600, units = "in")
ggplot() + 
  geom_sf(data = sett_all %>% filter(SETT_ID == 2072301), col = "orange", size = 2) + 
  geom_sf(data = badgers_jit %>% filter(SETT_ID == 2072301)) + 
  # geom_sf(data = ireland, col = "black", fill = NA) +
  # coord_sf(xlim = st_bbox(badgers_jit %>% filter(SETT_ID == 2072301))[c(1,3)], 
  #          ylim = st_bbox(badgers_jit %>% filter(SETT_ID == 2072301))[c(2,4)], 
  #          expand = T) + 
  labs(title = "Jittered badger locations") + 
  theme_bw()
dev.off()


# covar plots ####
env_vars <- terra::rast("Data/Covars/final_covars_terra.grd")
env_vars <- trim(env_vars)
env_vars$GrasslandPastures <- sum(env_vars$Naturalgrasslands, env_vars$Pastures)

png("Outputs/ISEC_Figures/env_Vars.jpg", height = 8.3, width = 11.7, res = 600, units = "in")
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = elevation)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Elevation") +
  theme_bw() + 
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = forest_distances)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Distance to \nforest edge (m)") +
  theme_bw() +
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = GrasslandPastures)) + 
  scale_fill_viridis_c(na.value = NA, labels = scales::percent_format(accuracy = 1)) + 
  labs(fill = "Grasslands \nand pastures") +
  theme_bw() +
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = slope)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Slope") +
  theme_bw() +
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = human_footprint_index)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Human footprint index") +
  theme_bw() +

ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = small_woody_features)) + 
  scale_fill_viridis_c(na.value = NA, labels = scales::percent_format(accuracy = 1)) + 
  labs(fill = "Small woody features") +
  theme_bw() +
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = topographic_wetness_index)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Topographic \nwetness index") +
  theme_bw() +
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = dist_to_paths)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Distance to \nOSM 'paths' (m)") +
  theme_bw() +
  
plot_layout(ncol = 3, byrow = T)
dev.off()


# Mesh figures 

meshes <- readRDS("Data/Inla/meshes.RDS")
ipoints1 <- fm_int(meshes[[1]])
ipoints2 <- fm_int(meshes[[2]])
ipoints4 <- fm_int(meshes[[4]])

png("Outputs/ISEC_Figures/mesh1.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = elevation)) + 
  scale_fill_viridis_c(na.value = NA) + 
  gg(meshes[[1]]) + 
  geom_sf(data = ipoints1, col = "red") + 
  coord_sf(xlim = st_bbox(env_vars)[c(1,3)], 
            ylim = st_bbox(env_vars)[c(2,4)], 
           expand = F) + 
  theme_bw() + 
  labs(title = "Elevation", x = "", y = "", fill = "Elevation")
dev.off()

png("Outputs/ISEC_Figures/mesh2.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = elevation)) + 
  scale_fill_viridis_c(na.value = NA) + 
  gg(meshes[[2]]) + 
  geom_sf(data = ipoints2, col = "red") + 
  coord_sf(xlim = st_bbox(env_vars)[c(1,3)], 
           ylim = st_bbox(env_vars)[c(2,4)], 
           expand = F) + 
  labs(title = "Elevation", x = "", y = "", fill = "Elevation") + 
  theme_bw()
dev.off()

png("Outputs/ISEC_Figures/mesh3.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = elevation)) + 
  scale_fill_viridis_c(na.value = NA) + 
  gg(meshes[[1]]) + 
  geom_sf(data = ipoints4, col = "red", size = 0.5) + 
  coord_sf(xlim = st_bbox(env_vars)[c(1,3)], 
           ylim = st_bbox(env_vars)[c(2,4)], 
           expand = F) + 
  labs(title = "Elevation", x = "", y = "", fill = "Elevation") + 
  theme_bw()
dev.off()

png("Outputs/ISEC_Figures/mesh4.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = GrasslandPastures)) + 
  scale_fill_viridis_c(na.value = NA) + 
  gg(meshes[[1]]) + 
  geom_sf(data = ipoints4, col = "red", size = 0.5) + 
  coord_sf(xlim = st_bbox(env_vars)[c(1,3)], 
           ylim = st_bbox(env_vars)[c(2,4)], 
           expand = F) + 
  labs(title = "Grassland and pastures", x = "", y = "", fill = "% Coverage") + 
  theme_bw()
dev.off()

# Sett model ####

## predictions ####

lp4 <- readRDS("Outputs/sett_model/linear_predictor.RDS")

inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]
x <- lp4$all[!inside,]

png("Outputs/ISEC_Figures/sett_prediction.jpg", width = 7.46*2, height = 7.79, res = 600, units = "in")
ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  labs(x = "", y = "", fill = "Median", 
       title = "Main sett distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL + 

ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", 
       title = "Spatially structured random field") +  
  NULL  
dev.off()


## covar effects spatial ####
covars_spatial_sett <- readRDS("Outputs/sett_model/covar_effects.RDS")

png("Outputs/ISEC_Figures/sett_covars_effect.jpg", height = 10, width = 8.5, res = 600, units = "in")
ggplot() +
  gg(data = covars_spatial_sett %>% filter(Variable == "Elevation"), 
     aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = covars_spatial_sett %>% filter(Variable == "Grasslands and pastures"), 
     aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Grasslands and \npastures effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = covars_spatial_sett %>% filter(Variable == "Slope"), 
     aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = covars_spatial_sett %>% filter(Variable == "Human footprint index"), 
     aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Human footprint \nindex effect", x = "", y = "", fill = "Median") +

ggplot() + 
  gg(data = covars_spatial_sett %>% filter(Variable == "Topographic wetness index"), 
     aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Topographic wetness \nindex effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = covars_spatial_sett %>% filter(Variable == "Distance to paths"), 
     aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Distance to 'OSM' \npaths effect", x = "", y = "", fill = "Median") +

plot_layout(nrow = 3)
dev.off()  
beepr::beep(sound = 4)

## covar eval effects ####
elev.pred <-  readRDS("Outputs/sett_model/elev.pred.RDS")
gp.pred <-  readRDS("Outputs/sett_model/grasslandPastures.pred.RDS")
slope.pred <-  readRDS("Outputs/sett_model/slope.pred.RDS")
pd.pred <-  readRDS("Outputs/sett_model/pathDist.pred.RDS")
topo.pred <-  readRDS("Outputs/sett_model/topo.pred.RDS")
hfi.pred <-  readRDS("Outputs/sett_model/hfi.pred.RDS")

png("Outputs/ISEC_Figures/sett_covars_eval.jpg", height = 10, width = 10, res = 600, units = "in")
ggplot(elev.pred) +
  geom_line(aes(elevation, mean)) +
  geom_ribbon(aes(elevation,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  geom_rug(data = elevation_scaled, aes(x = elevation), inherit.aes = F) + 
  geom_rug(data = elevation_ipoints, aes(x = elevation), inherit.aes = F, 
           col = "darkgray", sides = "t") + 
  scale_x_continuous(breaks = seq(min(elevation[], na.rm = T), 
                                  quantile(elevation[], probs = 0.99, na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$elevation[], na.rm = T), 
                                        quantile(env_vars$elevation[], probs = 0.99, na.rm = T), 
                                        length.out = 10), 0), 
                     limits = c(min(elevation[], na.rm = T), 
                                quantile(elevation[], probs = 0.99, na.rm = T))) + 
  # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7),
  #                    limits = c(-1.5, 1.5)) + 
  labs(x = "Elevation", y = "Effect") + 
  theme_bw() + 
 
ggplot(gp.pred) +
  geom_line(aes(grasslandsPastures, mean)) +
  geom_ribbon(aes(grasslandsPastures,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  geom_rug(data = grasslandPastures_scaled, aes(x = GrasslandPastures)) + 
  geom_rug(data = grasslandPastures_ipoints, aes(x = GrasslandPastures), inherit.aes = F, 
           col = "darkgray", sides = "t") + 
  scale_x_continuous(breaks = seq(min(grasslandsPastures[], na.rm = T), 
                                  max(grasslandsPastures[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$GrasslandPastures[], na.rm = T), 
                                             max(env_vars$GrasslandPastures[], na.rm = T), 
                                             length.out = 10)), 0), 
                     limits = c(min(grasslandsPastures[], na.rm = T),
                                quantile(grasslandsPastures[], 0.99, na.rm = T))) + 
  labs(x = "Percentage of pastures and grasslands", y = "Effect") + 
  # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
  #                    limits = c(-1.5, 1.5)) + 
  theme_bw() + 
  
ggplot(slope.pred) +
  geom_line(aes(slope, mean)) +
  geom_ribbon(aes(slope,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = slope_scaled, aes(x = slope)) + 
  geom_rug(data = slope_ipoints, aes(x = slope), inherit.aes = F, 
           col = "darkgray", sides = "t") + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$slope[], na.rm = T), 
                                  max(env_vars_scaled$slope[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$slope[], na.rm = T), 
                                        max(env_vars$slope[], na.rm = T), 
                                        length.out = 10)*100, 0), 
                     limits = c(min(env_vars_scaled$slope[], na.rm = T), 
                                quantile(env_vars_scaled$slope[], 0.99, na.rm = T))) + 
  labs(x = "Slope", y = "Effect") +
  # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
  #                    limits = c(-1.5, 1.5)) + 
  theme_bw() + 
  
ggplot(hfi.pred) +
  geom_line(aes(hfi, mean)) +
  geom_ribbon(aes(hfi,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = hfi_scaled, aes(x = human_footprint_index)) + 
  geom_rug(data = hfi_ipoints, aes(x = human_footprint_index), inherit.aes = F, 
           col = "darkgray", sides = "t") + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$human_footprint_index[], na.rm = T), 
                                  max(env_vars_scaled$human_footprint_index[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$human_footprint_index[], na.rm = T), 
                                        max(env_vars$human_footprint_index[], na.rm = T), 
                                        length.out = 10), 0), 
                     limits = c(min(human_footprint[], na.rm = T), 
                                quantile(human_footprint[], 0.99, na.rm = T))) + 
  labs(x = "Human footprint index", y = "Effect") +
  # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
  #                    limits = c(-1.5, 1.5)) + 
  theme_bw() + 
  
ggplot(topo.pred) +
  geom_line(aes(topo, mean)) +
  geom_ribbon(aes(topo,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = topo_scaled, aes(x = topographic_wetness_index)) + 
  geom_rug(data = topo_ipoints, aes(x = topographic_wetness_index), inherit.aes = F, 
           col = "darkgray", sides = "t") + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$topographic_wetness_index[], na.rm = T), 
                                  max(env_vars_scaled$topographic_wetness_index[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$topographic_wetness_index[], na.rm = T), 
                                        max(env_vars$topographic_wetness_index[], na.rm = T), 
                                        length.out = 10), 0), 
                     limits = c(min(topo_wetness[], na.rm = T), 
                                quantile(topo_wetness[], 0.99, na.rm = T))) + 
  labs(x = "Topographic wetness index", y = "Effect") +
  # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
  #                    limits = c(-1.5, 1.5)) + 
  theme_bw() + 
  
ggplot(pd.pred) +
  geom_line(aes(path_dist, mean)) +
  geom_ribbon(aes(path_dist,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = pathdist_scaled, aes(x = dist_to_paths)) + 
  geom_rug(data = pathdist_ipoints, aes(x = dist_to_paths), inherit.aes = F, 
           col = "darkgray", sides = "t") + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$dist_to_paths[], na.rm = T), 
                                  max(env_vars_scaled$dist_to_paths[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$dist_to_paths[], na.rm = T), 
                                        max(env_vars$dist_to_paths[], na.rm = T), 
                                        length.out = 10), 0), 
                     limits = c(min(env_vars_scaled$dist_to_paths[], na.rm = T), 
                                quantile(env_vars_scaled$dist_to_paths[], 0.99, na.rm = T))) + 
  labs(x = "Distance to paths", y = "Effect") +
  # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
  #                    limits = c(-1.5, 1.5)) + 
  theme_bw() + 
  
plot_layout(nrow = 3)
dev.off()


# Badger model ####

## Predictions ####
lp4 <- readRDS("Outputs/badgers_lgcp_model_1km/linear_predictor.RDS")

inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]
x <- lp4$all[!inside,]

png("Outputs/ISEC_Figures/badger_prediction.jpg", width = 7.46*2, height = 7.79, res = 600, units = "in")
ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  labs(x = "", y = "", fill = "Median", 
       title = "Badger distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL + 
  
  ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", 
       title = "Spatially structured random field") +  
  NULL  
dev.off()

## covar effects spatial ####

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

swf_df <- lp4$swf[!inside,] 
swf_df <- swf_df %>% 
  mutate(Variable = "Small woodie features")

hfi_df <- lp4$hfi[!inside,]
hfi_df <- hfi_df %>% 
  mutate(Variable = "Human footprint index")

# pathdist_df <- lp4$pathDistance[!inside,]
# pathdist_df <- pathdist_df %>% 
#   mutate(Variable = "Distance to paths")

fordist_df <- lp4$forestDistance[!inside,]
fordist_df <- fordist_df %>% 
  mutate(Variable = "Distance to forest edge")

topo_df <- lp4$topoWetness[!inside,]
topo_df <- topo_df %>% 
  mutate(Variable = "Topographic wetness index")


png("Outputs/ISEC_Figures/badger_covars_effect.jpg", height = 10, width = 8.5, res = 600, units = "in")
ggplot() +
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Elevation effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Slope effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Grasslands and \npastures effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = swf_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Small woodie \nfeatures effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Human footprint \nindex effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = fordist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Dist. forest \nedge effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Topographic wetness \nindex effect", x = "", y = "", fill = "Mean") +
  
plot_layout(ncol = 3)
dev.off()

## covar eval effects ####
png("Outputs/ISEC_Figures/badger_covars_eval.jpg", height = 8.5, width = 10, res = 600, units = "in")
eval.elev +
  eval.slope + 
  eval.grasslandsPastures + 
  eval.swf + 
  eval.hfi + 
  eval.forestDist + 
  eval.topo + 
  plot_layout(ncol = 3)
dev.off()

# response plots ####

rp4_sett <- readRDS("Outputs/sett_model/response_predictor.RDS")

inside = sapply(st_intersects(rp4_sett$all, ireland_outline_sf), function(x){length(x)==0})
y_sett <- rp4_sett$all[!inside,]

rp4 <- readRDS("Outputs/badgers_lgcp_model_1km/response_predictor.RDS")

inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

png("Outputs/ISEC_Figures/response_predictions.jpg", width = 7.46*2, height = 7.79, res = 600, units = "in")
ggplot() + 
  gg(data = y_sett, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  ggtitle("Sett distribution (response scale)") +
  labs(x = "", y = "", fill = "Median") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL + 

ggplot() + 
  gg(data = y, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  ggtitle("Badger distribution (response scale)") +
  labs(x = "", y = "", fill = "Median") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL + 
  
plot_layout(nrow = 1)
dev.off()


y$diff <- y$q0.5/y_sett$q0.5

png("Outputs/ISEC_Figures/diff.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  gg(data = y, aes(fill = diff), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  ggtitle("Badger - sett ratio by km") +
  labs(x = "", y = "", fill = "Median") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL
dev.off()

png("Outputs/ISEC_Figures/badger_pred_uncert.jpg", width = 7.46*2, height = 7.79, res = 600, units = "in")
ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  labs(x = "", y = "", fill = "Median", 
       title = "Badger distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL + 
  
ggplot() + 
  gg(data = x, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  labs(x = "", y = "", fill = "Median", 
       title = "Credible interval width") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL 
dev.off()
