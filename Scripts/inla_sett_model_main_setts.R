# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

bru_options_set(bru_verbose = TRUE,
#                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.inla = list(int.strategy="eb"))

# Preparation of data ####

## load sett data and covariates ####
sett_all <- readRDS("Data/sett_all_2023.RDS") %>% 
  st_transform(crs = projKM) 

sett_subset <- sett_all %>% 
  mutate(year = lubridate::year(DATE_OF_FIELD_VISIT)) %>% 
  filter(year > 2018) %>% 
  filter(MAIN_SETT == "Yes") %>%
  filter(CAPTURE_BLOCK_ID %!in% c("NOT ASSIGN", "NOT ASSIGNE")) %>% 
  select(SETT_ID, geometry)

env_vars <- stack("Data/Covars/final_covars.grd")

env_vars$PeatbogsandMoors <- sum(env_vars$Peatbogs, env_vars$Moorsandheathland)
env_vars$GrasslandPastures <- sum(env_vars$Naturalgrasslands, env_vars$Pastures)
env_vars$forested <- sum(env_vars$tree_cover_density, env_vars$small_woody_features, 
                         env_vars$Transitionalwoodland.shrub, na.rm = T)

env_vars_scaled <- raster::scale(env_vars)


## load mesh boundaries and samplers ####
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
ireland_outline_simple <- st_simplify(ireland_outline_sf, dTolerance = 5)

mesh <- readRDS("Data/Inla/meshes.RDS")[[2]]
mesh$crs <- projKM

int_pointsw <- readRDS("Data/Inla/int_points4_weighted.RDS")
int_points <- readRDS("Data/Inla/int_points4_nonweighted.RDS")


inner_boundary <- st_as_sf(readRDS("Data/Inla/inner_boundary.RDS"))

## create different integration points dfs ####
df <- fm_pixels(mesh, mask = inner_boundary,
                 dims = c(72, 81),
                 format = "sf")

df2 <- fm_pixels(mesh,
                dims = c(72*10, 81*10),
                mask = inner_boundary,
                format = "sf")

# Prepare covars ####

elevation <- as(env_vars_scaled$elevation, "SpatRaster")
slope <- as(env_vars_scaled$slope, "SpatRaster")
tcd <- as(env_vars_scaled$tree_cover_density, "SpatRaster")
swf <- as(env_vars_scaled$small_woody_features, "SpatRaster")
crops <- as(env_vars_scaled$Plantedvegetationandcrops, "SpatRaster")
moors <- as(env_vars_scaled$Moorsandheathland, "SpatRaster")
shrub <- as(env_vars_scaled$Transitionalwoodland.shrub, "SpatRaster")
peatbogsandMoors <- as(env_vars_scaled$PeatbogsandMoors, "SpatRaster")
grasslandsPastures <- as(env_vars_scaled$GrasslandPastures, "SpatRaster")
forested <- as(env_vars_scaled$forested, "SpatRaster")
roadDist <- as(env_vars_scaled$dist_to_roads, "SpatRaster")
pathDist <- as(env_vars_scaled$dist_to_paths, "SpatRaster")
# OSM query "SELECT * FROM 'lines' WHERE highway IN ('path','track','footway')"
#https://wiki.openstreetmap.org/wiki/Key:highway#Paths
forestDist <- as(env_vars_scaled$Forest_distance, "SpatRaster")
heat_loading <- as(env_vars_scaled$heat_loading_index, "SpatRaster")
topo_wetness <- as(env_vars_scaled$topographic_wetness_index, "SpatRaster")
human_footprint <- as(env_vars_scaled$human_footprint_index, "SpatRaster")


## Prepare 1D meshes for covars ####

### 1d mesh for elevation ####
mesh1D_elev <- inla.mesh.1d(seq(min(elevation[], na.rm = T)-1, 
                                max(elevation[], na.rm = T)+1, length.out = 10),
                            degree = 2)

diff(range(elevation[], na.rm = T))/3
matern1D_elev <- inla.spde2.pcmatern(mesh1D_elev,
                                     prior.range = c(6, 0.1), # 1 third range mesh
                                     prior.sigma = c(0.1, 0.1))

### 1d mesh for slope ####
mesh1D_slope <- inla.mesh.1d(seq(min(slope[], na.rm = T)-1, max(slope[], na.rm = T)+1, 
                                 length.out = 10), 
                             degree = 2) 

diff(range(slope[], na.rm = T))/3
matern1D_slope <- inla.spde2.pcmatern(mesh1D_slope,
                                      prior.range = c(3, 0.1), # 1 third range mesh
                                      prior.sigma = c(1, 0.1))

### 1d mesh for crops ####
mesh1D_crops <- inla.mesh.1d(seq(min(crops[], na.rm = T)-1, 
                                 max(crops[], na.rm = T)+1, 
                                 length.out = 10), 
                             degree = 2) 

diff(range(crops[], na.rm = T))/3

matern1D_crops <- inla.spde2.pcmatern(mesh1D_crops,
                                      prior.range = c(2.5, 0.5), # 1 third range mesh
                                      prior.sigma = c(0.1, 0.5))


### 1d mesh for tree cover density ####
mesh1D_tcd <- inla.mesh.1d(seq(min(tcd[], na.rm = T)-1, 
                               max(tcd[], na.rm = T)+1, 
                               length.out = 10), 
                           degree = 2) 

diff(range(tcd[], na.rm = T))/3
matern1D_tcd <- inla.spde2.pcmatern(mesh1D_tcd,
                                    prior.range = c(2.5, 0.5), # 1 third range mesh
                                    prior.sigma = c(1, 0.5))

### 1d mesh for small woody features ####
mesh1D_swf <- inla.mesh.1d(seq(min(swf[], na.rm = T)-1, 
                               max(swf[], na.rm = T)+1, 
                               length.out = 10), 
                           degree = 2) 

diff(range(swf[], na.rm = T))/3
matern1D_swf <- inla.spde2.pcmatern(mesh1D_swf,
                                    prior.range = c(5, 0.9), # 1 third range mesh
                                    prior.sigma = c(0.1, 0.1))

### 1d mesh for peatbogs and moors together ####
mesh1D_peatbogsandMoors <- inla.mesh.1d(seq(min(peatbogsandMoors[], na.rm = T)-1, 
                                    max(peatbogsandMoors[], na.rm = T)+1, 
                                    length.out = 10), 
                                degree = 2) 

diff(range(peatbogsandMoors[], na.rm = T))/3
matern1D_peatbogsandMoors  <- inla.spde2.pcmatern(mesh1D_peatbogsandMoors,
                                         prior.range = c(10, 0.1), # force a longer range, and same as grasslands
                                         prior.sigma = c(1, 0.1))

### 1d mesh for shrub ####
mesh1D_shrub <- inla.mesh.1d(seq(min(shrub[], na.rm = T)-1,
                                 max(shrub[], na.rm = T)+1,
                                 length.out = 10),
                             degree = 2)

diff(range(shrub[], na.rm = T))/3
matern1D_shrub <- inla.spde2.pcmatern(mesh1D_shrub,
                                      prior.range = c(5, 0.9), # 1 third range mesh
                                      prior.sigma = c(0.5, 0.1))

### 1d mesh for grasslands and pastures ####
mesh1D_grassPast <- inla.mesh.1d(seq(min(grasslandsPastures[], na.rm = T)-1,
                                 max(grasslandsPastures[], na.rm = T)+1,
                                 length.out = 10),
                             degree = 2)

diff(range(grasslandsPastures[], na.rm = T))/3
matern1D_grassPast <- inla.spde2.pcmatern(mesh1D_grassPast,
                                          prior.range = c(2, 0.1), # 1 third range mesh
                                          prior.sigma = c(1, 0.1))

### 1d mesh for distance to roads ####
mesh1D_distRoads <- inla.mesh.1d(seq(min(roadDist[], na.rm = T)-1,
                                     max(roadDist[], na.rm = T)+1,
                                     length.out = 10),
                                 degree = 2)

diff(range(roadDist[], na.rm = T))/3
matern1D_distRoads <- inla.spde2.pcmatern(mesh1D_distRoads,
                                          prior.range = c(4, 0.1), # 1 third range mesh
                                          prior.sigma = c(0.5, 0.1))

### 1d mesh for distance to paths ####
mesh1D_distPaths <- inla.mesh.1d(seq(min(pathDist[], na.rm = T)-1,
                                     max(pathDist[], na.rm = T)+1,
                                     length.out = 10),
                                 degree = 2)

diff(range(pathDist[], na.rm = T))/3
matern1D_distPaths <- inla.spde2.pcmatern(mesh1D_distPaths,
                                          prior.range = c(3, 0.1), # 1 third range mesh
                                          prior.sigma = c(0.5, 0.1))


### 1d mesh for heat loading index ####
mesh1D_heat <- inla.mesh.1d(seq(min(heat_loading[], na.rm = T)-1,
                                     max(heat_loading[], na.rm = T)+1,
                                     length.out = 10),
                                 degree = 2)

diff(range(heat_loading[], na.rm = T))/3
matern1D_heat <- inla.spde2.pcmatern(mesh1D_heat,
                                          prior.range = c(6.2, 0.5), # 1 third range mesh
                                          prior.sigma = c(0.5, 0.5))

### 1d mesh for topographic wetness index ####
mesh1D_topo <- inla.mesh.1d(seq(min(topo_wetness[], na.rm = T)-1,
                                max(topo_wetness[], na.rm = T)+1,
                                length.out = 10),
                            degree = 2)

diff(range(topo_wetness[], na.rm = T))/3
matern1D_topo <- inla.spde2.pcmatern(mesh1D_topo,
                                     prior.range = c(10, 0.1), # 1 third range mesh
                                     prior.sigma = c(0.1, 0.1))


### 1d mesh for human footprint index ####
mesh1D_hfi <- inla.mesh.1d(seq(min(human_footprint[], na.rm = T)-1,
                                max(human_footprint[], na.rm = T)+1,
                                length.out = 10),
                            degree = 2)

diff(range(human_footprint[], na.rm = T))/3
matern1D_hfi <- inla.spde2.pcmatern(mesh1D_hfi,
                                    prior.range = c(8, 0.9), # 1 third range mesh
                                    prior.sigma = c(0.5, 0.9))

### 1d mesh for forest distances ####
mesh1D_forest <- inla.mesh.1d(seq(min(forestDist[], na.rm = T)-1,
                                max(forestDist[], na.rm = T)+1,
                                length.out = 10),
                            degree = 2)

diff(range(forestDist[], na.rm = T))/3
matern1D_forest <- inla.spde2.pcmatern(mesh1D_forest,
                                       prior.range = c(10, 0.9), # 1 third range mesh
                                       prior.sigma = c(0.5, 0.5))


# M4 non-linear covar effects + spde ####

## Set up spde ####

matern2D_small <- inla.spde2.pcmatern(mesh,
                                prior.range = c(30, 0.1),  #1/3 y coordinate 90
                                prior.sigma = c(0.01, 0.1)) #0.001

matern2D_big <- inla.spde2.pcmatern(mesh,
                                    prior.range = c(100, 0.1),  #1/3 y coordinate 90
                                    prior.sigma = c(0.02, 0.01)) #0.01 at p 0.1 works

## Formula ####

nonlinear_SPDE <- geometry ~  Intercept(1)  +
  
  # Eff.elevation(elevation, model = "linear") +
  Eff.elevation(elevation, model = matern1D_elev) +
  
  # Eff.slope(slope, model = "linear") +
  Eff.slope(slope, model = matern1D_slope) +
  
  # Eff.tcd(tcd, model = "linear") +
  # Eff.tcd(tcd, model = matern1D_tcd) +
  
  # Eff.swf(swf, model = "linear") +
  # Eff.swf(swf, model = matern1D_swf) +
  
  # Eff.crops(crops, model = "linear") +
  # Eff.crops(crops, model = matern1D_crops) +
  
  Eff.grassPast(grasslandsPastures, model = matern1D_grassPast) + 
  
  # Eff.shrub(shrub, model = "linear") + # linear because is 0 in most of the domain and it'd be a problem for integrating
  # Eff.shrub(shrub, model = "linear") +
  
  # Eff.peatbogsandMoors(peatbogsandMoors, model = "linear") +
  
  # Eff.roaddist(roadDist, model = matern1D_distRoads) + 
  
  Eff.pathdist(pathDist, model = matern1D_distPaths) +
  # Eff.pathdist(pathDist, model = "linear") +
  
  # Eff.heat(heat_loading, model = matern1D_heat) + 
  # Eff.heat(heat_loading, model = "linear") + 
  
  # Eff.topo(topo_wetness, model = matern1D_topo) +
  Eff.topo(topo_wetness, model = "linear") +
  
  # Eff.hfi(human_footprint, model = matern1D_hfi) +
  Eff.hfi(human_footprint, model = "linear") +
  
  # Eff.forestdist(forestDist, model = matern1D_forest) +
  # Eff.forestdist(forestDist, model = "linear") +
  
  Eff.smooth_big(geometry, model = matern2D_big) +
  
  # Eff.smooth_small(geometry, model = matern2D_small) +
  NULL


## Run model ####
m4 <- lgcp(components = nonlinear_SPDE,
           data = sett_subset,
           ips = int_pointsw)

# saveRDS(m4, file = "Outputs/sett_model/final_main_sett_model.RDS")
# m4 <- readRDS("Outputs/main_sett_model_1km/final_main_sett_model.RDS")
summary(m4)
beepr::beep(sound = 4)

## predict ####

### Linear scale ####

lp4 <- predict(
  object = m4, 
  newdata = df2, 
  samples = 1000,
  formula = ~ list(
    elevation = Eff.elevation,
    slope = Eff.slope,
    grassland = Eff.grassPast,
    hfi = Eff.hfi, 
    # roadDistance = Eff.roaddist,
    pathDistance = Eff.pathdist,
    topoWetness = Eff.topo,
    # forestDistance = Eff.forestdist,
    spfield_big = Eff.smooth_big,
      
    all = Intercept +
      Eff.elevation +
      Eff.slope +
      Eff.grassPast +
      Eff.hfi +
      # Eff.roaddist +
      Eff.pathdist + 
      Eff.topo + 
      # Eff.forestdist + 
      Eff.smooth_big
    ))
saveRDS(lp4, file = "Outputs/sett_model/linear_predictor.RDS")

### Response scale ####

rp4 <- predict(
  object = m4, 
  newdata = df2, 
  samples = 1000,
  formula = ~ list(
    all = exp(Intercept +
                Eff.elevation +
                Eff.slope +
                Eff.grassPast +
                Eff.hfi +
                # Eff.roaddist +
                Eff.pathdist + 
                Eff.topo + 
                # Eff.forestdist + 
                Eff.smooth_big))
)
saveRDS(rp4, file = "Outputs/sett_model/response_predictor.RDS")

### plot #### 
inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

ggplot() + 
  gg(data = spb, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu') + 
  ggtitle("Spatial random field") + 
  NULL

inside = sapply(st_intersects(lp4$all, ireland_outline_sf), function(x){length(x)==0})
x <- lp4$all[!inside,]

ggplot() + 
  gg(data = x, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  ggtitle("Main sett distribution (linear scale)") +
  labs(x = "", y = "", fill = "Mean") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL

inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

ggplot() + 
  gg(data = y, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  ggtitle("Main sett distribution response") +
  labs(x = "", y = "", fill = "Mean") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL

inside = sapply(st_intersects(lp4$elevation, ireland_outline_sf), function(x){length(x)==0})
elev <- lp4$elevation[!inside,] 
elev <- elev %>% 
  mutate(Variable = "Elevation")

slop <- lp4$slope[!inside,] 
slop <- slop %>% 
  mutate(Variable = "Slope")

grass <- lp4$grassland[!inside,] 
grass <- grass %>% 
  mutate(Variable = "Grasslands and pastures")

hfi <- lp4$hfi[!inside,]
hfi <- hfi %>% 
  mutate(Variable = "Human footprint index")

pathdist <- lp4$pathDistance[!inside,]
pathdist <- pathdist %>% 
  mutate(Variable = "Distance to paths")

topo <- lp4$topoWetness[!inside,]
topo <- topo %>% 
  mutate(Variable = "Topographic wetness index")

all_covars <- bind_rows(elev, slop, grass, hfi, pathdist, topo) 
saveRDS(all_covars, file = "Outputs/sett_model/covar_effects.RDS")

ggplot() + 
  gg(data = all_covars, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Covariate effects", x = "", y = "", fill = "Mean") +
  facet_wrap(~Variable, ncol = 3) +
  NULL

## Spde and cor covariance plots ####

spde.range <- spde.posterior(m4, "Eff.smooth_big", what = "range")
spde.logvar <- spde.posterior(m4, "Eff.smooth_big", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)

corplot <- plot(spde.posterior(m4, "Eff.smooth_big", what = "matern.correlation"))
covplot <- plot(spde.posterior(m4, "Eff.smooth_big", what = "matern.covariance"))

multiplot(range.plot, var.plot, covplot, corplot)

## Evaluate effects ####

#### Elevation ####

elevation_scaled <- extract(elevation, sett_subset)
elevation_ipoints <- extract(elevation, int_pointsw)

elev.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(elevation = seq(min(elevation[], na.rm = T), 
                                    quantile(elevation[], probs = 0.99, na.rm = T), 
                                    length.out = 1000)),
  formula = ~ Eff.elevation_eval(elevation), 
  exclude = c(
    # "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast",
    "Eff.roaddist",
    "Eff.pathdist",
    "Eff.topo", 
    "Eff.forestdist", 
    "Eff.hfi", 
    "Eff.smooth_big"
  )) 

saveRDS(elev.pred, file = "Outputs/sett_model/elev.pred.RDS")

(eval.elev <- ggplot(elev.pred) +
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
    theme_bw())


#### Grasslands ####

grasslandPastures_scaled <- extract(grasslandsPastures, sett_subset)
grasslandPastures_ipoints <- extract(grasslandsPastures, int_pointsw)

grasslandPastures.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(grasslandsPastures = 
                         seq(min(grasslandsPastures[], na.rm = T),
                             quantile(grasslandsPastures[], 0.99, na.rm = T),
                             length.out = 1000)),
  formula = ~ Eff.grassPast_eval(grasslandsPastures),
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    # "Eff.grassPast",
    "Eff.roaddist",
    "Eff.pathdist",
    "Eff.topo", 
    "Eff.forestdist", 
    "Eff.hfi", 
    "Eff.smooth_big"
  )) 

saveRDS(grasslandPastures.pred, file = "Outputs/sett_model/grasslandPastures.pred.RDS")

(eval.grasslandsPastures <- ggplot(grasslandPastures.pred) +
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
  theme_bw()) 

#### Slope ####

slope_scaled <- extract(slope, sett_subset)
slope_ipoints <- extract(slope, int_pointsw)

slope.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(slope = seq(min(slope[], na.rm = T), 
                                quantile(slope[], 0.99, na.rm = T), 
                                length.out = 1000)),
  formula = ~ Eff.slope_eval(slope), 
  exclude = c(
    "Eff.elevation",
    # "Eff.slope",
    "Eff.grassPast",
    "Eff.roaddist",
    "Eff.pathdist",
    "Eff.topo", 
    "Eff.forestdist", 
    "Eff.hfi", 
    "Eff.smooth_big"
  )) 

saveRDS(slope.pred, file = "Outputs/sett_model/slope.pred.RDS")


(eval.slope <- ggplot(slope.pred) +
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
    theme_bw())


#### distance to paths ####

pathdist_scaled <- extract(pathDist, sett_subset)
pathdist_ipoints <- extract(pathDist, int_pointsw)

pathDist.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(path_dist = seq(min(pathDist[], na.rm = T), 
                                       quantile(pathDist[], 0.99, na.rm = T), 
                                       length.out = 1000)),
  formula = ~ Eff.pathdist_eval(path_dist), 
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast",
    "Eff.roaddist",
    # "Eff.pathdist",
    "Eff.topo", 
    "Eff.forestdist", 
    "Eff.hfi", 
    "Eff.smooth_big"
  )) 

saveRDS(pathDist.pred, file = "Outputs/sett_model/pathDist.pred.RDS")

(eval.pathDist <- ggplot(pathDist.pred) +
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
    theme_bw()) 

#### Topographic wetness index ####

topo_scaled <- extract(topo_wetness, sett_subset)
topo_ipoints <- extract(topo_wetness, int_pointsw)

topo.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(topo = seq(min(topo_wetness[], na.rm = T), 
                                  quantile(topo_wetness[], 0.99, na.rm = T), 
                                  length.out = 1000)),
  formula = ~ Eff.topo_eval(topo), 
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast",
    "Eff.roaddist",
    "Eff.pathdist",
    # "Eff.topo", 
    "Eff.forestdist", 
    "Eff.hfi", 
    "Eff.smooth_big")) 

saveRDS(topo.pred, file = "Outputs/sett_model/topo.pred.RDS")

(eval.topo <- ggplot(topo.pred) +
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
    theme_bw()) 

#### Human footprint index ####

hfi_scaled <- extract(human_footprint, sett_subset)
hfi_ipoints <- extract(human_footprint, int_pointsw)

hfi.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(hfi = seq(min(human_footprint[], na.rm = T), 
                                 quantile(human_footprint[], 0.99, na.rm = T), 
                                 length.out = 1000)),
  formula = ~ Eff.hfi_eval(hfi), 
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast",
    "Eff.roaddist",
    "Eff.pathdist",
    "Eff.topo", 
    "Eff.forestdist",
    # "Eff.hfi", 
    "Eff.smooth_big")) 

saveRDS(hfi.pred, file = "Outputs/sett_model/hfi.pred.RDS")

(eval.hfi <- ggplot(hfi.pred) +
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
    theme_bw()) 

### plot ####

multiplot(
  eval.elev,
  eval.slope,
  eval.grasslandsPastures,
  # eval.fdist, 
  # eval.roadDist, 
  eval.pathDist,
  eval.topo, 
  eval.hfi,
  cols = 3 
)

beepr::beep(sound = 3)
# ggsave("Outputs/sett_model/setts_covars.png")


