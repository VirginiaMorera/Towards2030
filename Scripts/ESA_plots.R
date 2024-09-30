# To be used with the model scripts

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 

## Effort ####

effort <- readRDS("Data/Inla/weightedSampler.RDS")
cul_effort <- readRDS("Data/Inla/cul_effort.RDS")
vac_effort <- readRDS("Data/Inla/vacc_effort.RDS")


png("Outputs/ESA_Figures/two_efforts.jpg", width = 7.46*2, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = cul_effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  labs(title = "Culling programme effort", 
       col = "Number of days", fill = "Number of days", x = "", y = "") + 
  theme_bw() +

ggplot() + 
  geom_sf(data = vac_effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  labs(title = "Vaccination programme effort", 
       col = "Number of days", fill = "Number of days", x = "", y = "") + 
  theme_bw() +

plot_layout(nrow = 1)
dev.off()

png("Outputs/ESA_Figures/final_effort.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  labs(title = "Total effort in number of days", 
       col = "Number of days", fill = "Number of days", x = "", y = "") + 
  theme_bw()
dev.off()

png("Outputs/ESA_Figures/effort_trans.jpg", width = 6, height = 4, res = 600, units = "in")
ggplot(effort) + 
  geom_point(aes(x = NDAYS, y = WEIGHT, col = NDAYS)) + 
  scale_color_viridis() + 
  labs(x = "Number of days", y = "Log of number of days", 
       col = "Number of days", title = "Transformation of effort") + 
  theme_bw()
dev.off()

## datasets ####

### setts ####
sett_all <- readRDS("Data/sett_all_inside_effort.RDS") %>% 
  st_transform(crs = projKM) 

sett_subset <- sett_all %>% 
  mutate(year = lubridate::year(DATE_OF_FIELD_VISIT)) %>% 
  filter(year > 2018) %>% 
  filter(MAIN_SETT == "Yes") %>%
  filter(CAPTURE_BLOCK_ID %!in% c("NOT ASSIGN", "NOT ASSIGNE")) %>% 
  select(SETT_ID, geometry)


png("Outputs/ESA_Figures/Effort_and_setts.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = sett_subset, size = 1, shape = 21, 
          col = "black", fill = "orange") + 
  scale_fill_viridis_c(na.value = NA, trans = "log", 
                       breaks = c(10, 30, 75, 200, 400),
                       labels = round(c(10, 30, 75, 200, 400), 1)) + 
  scale_color_viridis_c(na.value = NA, trans = "log", 
                        breaks = c(10, 30, 75, 200, 400),
                        labels = round(c(10, 30, 75, 200, 400), 1)) +
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  labs(title = "Main setts", fill = "Number of days", color = "Number of days") + 
  theme_bw()
dev.off()

### badgers ####

badgers <- readRDS(file = "Data/badgers_jittered_filtered.RDS") %>% 
  mutate(YEAR = year(DATE_CAUGHT)) %>% 
  filter(YEAR > 2018)

png("Outputs/ESA_Figures/badgers_programme.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  geom_sf(data = badgers, shape = 21, col = "black", size = 1,
          aes(fill = PROGRAMME)) + 
  labs(title = "Badgers captured", fill = "Programme") + 
  theme_bw()
dev.off()

png("Outputs/ESA_Figures/badgers_culling.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  geom_sf(data = badgers %>% filter(PROGRAMME == "Culling"), shape = 21, col = "black", size = 1,
          aes(fill = PROGRAMME)) + 
  labs(title = "Badgers captured", fill = "Programme") + 
  theme_bw()
dev.off()

### Covariates ####

env_vars <- terra::rast("Data/Covars/final_covars_terra.grd")
env_vars <- trim(env_vars)
env_vars$GrasslandPastures <- sum(env_vars$Naturalgrasslands, env_vars$Pastures)
env_vars$swf <- env_vars$small_woody_features/100

png("Outputs/ESA_Figures/env_Vars.jpg", height = 8.3, width = 11.7, res = 600, units = "in")
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = elevation)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Elevation (m)") +
  theme_bw() + 
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = GrasslandPastures)) + 
  scale_fill_viridis_c(na.value = NA, labels = scales::percent_format(accuracy = 1)) + 
  labs(fill = "Grasslands \nand pastures") +
  theme_bw() +
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = forest_distances)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Distance to \nforest edge (m)") +
  theme_bw() +
  

  
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = slope)) + 
  scale_fill_viridis_c(na.value = NA, labels = scales::percent_format(accuracy = 1)) + 
  labs(fill = "Slope") +
  theme_bw() +
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = Transitionalwoodland.shrub)) + 
  scale_fill_viridis_c(na.value = NA, 
                       labels = scales::percent_format(accuracy = 1)) + 
  labs(fill = "Transitional woodland \nand shrub") +
  theme_bw() +  
  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = swf)) + 
  scale_fill_viridis_c(na.value = NA, 
                       labels = scales::percent_format(accuracy = 1)) + 
  labs(fill = "Small woody \nfeatures") +
  theme_bw() +
  

  
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = human_footprint_index)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Human footprint \nindex") +
  theme_bw() +

ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = topographic_wetness_index)) + 
  scale_fill_viridis_c(na.value = NA) + 
  labs(fill = "Topographic \nwetness index") +
  theme_bw() +  
  

plot_layout(ncol = 3, byrow = T)
dev.off()

### Mesh figures  ####

meshes <- readRDS("Data/Inla/meshes.RDS")
ipoints1 <- fm_int(meshes[[1]])
ipoints2 <- fm_int(meshes[[2]])
ipoints4 <- fm_int(meshes[[4]])

png("Outputs/ESA_Figures/mesh1.jpg", width = 7.46, height = 7.79, res = 600, units = "in")

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

png("Outputs/ESA_Figures/mesh2.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_spatraster(data = env_vars, aes(fill = GrasslandPastures)) + 
  scale_fill_viridis_c(na.value = NA) + 
  gg(meshes[[2]]) + 
  geom_sf(data = ipoints2, col = "red") + 
  coord_sf(xlim = st_bbox(env_vars)[c(1,3)], 
           ylim = st_bbox(env_vars)[c(2,4)], 
           expand = F) + 
  labs(title = "Elevation", x = "", y = "", fill = "Pastures") + 
  theme_bw()
dev.off()


## Sett ####

### predictions ####

png("Outputs/ESA_Figures/sett_prediction.jpg", width = 7.46*2, height = 7.79, 
    res = 600, units = "in")
ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "Main sett distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL + 
  
ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", title = "Spatial random field")   
dev.off()


### covariate effect ####
png("Outputs/ESA_Figures/sett_covars_effect.jpg", height = 10, width = 8.5, res = 600, units = "in")
ggplot() + 
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = swf_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Small woody features effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Pasture and grasslands effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = shrub_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Transitional woodland and shrub", x = "", y = "", fill = "Median") +

plot_layout(ncol = 2)
dev.off()

### evaluate effects ####
png("Outputs/Aims/sett_covars_eval.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
multiplot(
  eval.elev,
  eval.swf,
  eval.shrub, 
  eval.slope,
  eval.grasslandsPastures,
  cols = 2
)
dev.off()

## Badgers ####

### predictions ####

png("Outputs/ESA_Figures/new_badger_prediction.jpg", width = 7.46*2, height = 7.79, 
    res = 600, units = "in")
ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  labs(x = "", y = "", fill = "Median", 
       title = "Badger distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL + 
  
  ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", title = "Spatial random field")   
dev.off()


### covariate effect ####
png("Outputs/ESA_Figures/new_badger_covars_effect.jpg", height = 10, width = 8.5, res = 600, units = "in")
ggplot() + 
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Pasture and grasslands effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = fordist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "distance to forest edge effect", 
       x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Human footprint index effect", x = "", y = "", fill = "Median") +
  
  
  ggplot() + 
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Topographic wetness index shrub", x = "", y = "", fill = "Median") +
  
  plot_layout(ncol = 2)
dev.off()

### evaluate effects ####
png("Outputs/aims/badger_covars_eval.jpg", 
    width = 7.46, height = 7.79, res = 600, units = "in")
multiplot(
  eval.elev,
  eval.grasslandsPastures,
  eval.hfi,
  eval.slope,
  eval.forestDist, 
  eval.topo,
  cols = 2 
)
dev.off()


### Badgers per sett
badgers_r <- readRDS("Outputs/badgers_all_model/response_predictor.RDS")
sett_r <- readRDS("Outputs/sett_model/response_predictor.RDS")

inside = sapply(st_intersects(badgers_r$all, ireland_outline_sf), function(x){length(x)==0})
badgers_r <- badgers_r$all[!inside,]

inside = sapply(st_intersects(sett_r$all, ireland_outline_sf), function(x){length(x)==0})
sett_r <- sett_r$all[!inside,]


sett_r <- sett_r %>% 
  mutate(scaledq0.5 = scale01(q0.5))

badgers_r <- badgers_r %>% 
  mutate(scaledq0.5 = scale01(q0.5), 
         ratio = q0.5/sett_r$q0.5)

png("Outputs/response_prediction_with_Vacc.jpg", width = 7.46*2, height = 7.79,  res = 600, units = "in")
ggplot() + 
  gg(data = sett_r, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "lightgray") + 
  ggtitle("Sett distribution (response scale)") +
  labs(x = "", y = "", fill = "Median") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +

ggplot() + 
  gg(data = badgers_r, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "lightgray") + 
  ggtitle("Badger distribution (response scale)") +
  labs(x = "", y = "", fill = "Median") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +

plot_layout(ncol = 2)
dev.off()

png("Outputs/Aims/badgers_per_sett_with_Vacc.jpg", 
    width = 7.46, height = 7.79,  res = 600, units = "in")
ggplot() + 
  gg(data = badgers_r, aes(fill = ratio), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "lightgray") + 
  ggtitle("Badgers per sett") +
  labs(x = "", y = "", fill = "Median") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL
dev.off()


png("Outputs/Aims/badgers_all_response.jpg", 
    width = 7.46, height = 7.79,  res = 600, units = "in")
ggplot() + 
  gg(data = badgers_r, aes(fill = ratio), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "lightgray") + 
  ggtitle("Badgers per sett") +
  labs(x = "", y = "", fill = "Median") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL
dev.off()
