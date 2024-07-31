# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# bru_options_set(bru_verbose = FALSE,
#                 # control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
#                 control.inla = list(int.strategy="eb"))

# Preparation of data ####

## load sett data and covariates ####
badgers_all <- readRDS("Data/badgers_thin.RDS") %>% 
  st_transform(crs = projKM) 

# badger_subset <- badgers_all %>% 
#   filter(YEAR > 2018)

env_vars <- terra::rast("Data/Covars/final_covars_terra.grd")

env_vars$PeatbogsandMoors <- sum(env_vars$Peatbogs, env_vars$Moorsandheathland)
env_vars$GrasslandPastures <- sum(env_vars$Naturalgrasslands, env_vars$Pastures)

env_vars_scaled <- terra::scale(env_vars)


## load mesh boundaries and samplers ####
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
ireland_outline_simple <- st_simplify(ireland_outline_sf, dTolerance = 5)

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 

mesh <- readRDS("Data/Inla/meshes.RDS")[[2]]
mesh$crs <- projKM

int_pointsw <- readRDS("Data/Inla/int_points4_weighted_NDAYS.RDS")

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

elevation <- env_vars_scaled$elevation
slope <- env_vars_scaled$slope
tcd <- env_vars_scaled$tree_cover_density
swf <- env_vars_scaled$small_woody_features
shrub <- env_vars_scaled$Transitionalwoodland.shrub
peatbogsandMoors <- env_vars_scaled$PeatbogsandMoors
grasslandsPastures <- env_vars_scaled$GrasslandPastures
roadDist <- env_vars_scaled$dist_to_roads
pathDist <- env_vars_scaled$dist_to_paths
# OSM query "SELECT * FROM 'lines' WHERE highway IN ('path','track','footway')"
#https://wiki.openstreetmap.org/wiki/Key:highway#Paths
forestDist <- env_vars_scaled$forest_distances
heat_loading <- env_vars_scaled$heat_loading_index
topo_wetness <- env_vars_scaled$topographic_wetness_index
human_footprint <- env_vars_scaled$human_footprint_index


## Prepare 1D meshes for covars ####

### 1d mesh for elevation ####
mesh1D_elev <- inla.mesh.1d(seq(min(elevation[], na.rm = T)-1, 
                                max(elevation[], na.rm = T)+1, 
                                length.out = 20),
                            degree = 2)

diff(range(elevation[], na.rm = T))/3
matern1D_elev <- inla.spde2.pcmatern(mesh1D_elev,
                                     prior.range = c(3, 0.1), # 1 third range mesh
                                     prior.sigma = c(0.5, 0.1))

### 1d mesh for slope ####
mesh1D_slope <- inla.mesh.1d(seq(min(slope[], na.rm = T)-1, 
                                 max(slope[], na.rm = T)+1, 
                                 length.out = 20), 
                             degree = 2) 

diff(range(slope[], na.rm = T))/3
matern1D_slope <- inla.spde2.pcmatern(mesh1D_slope,
                                      prior.range = c(3.8, 0.1), # 1 third range mesh
                                      prior.sigma = c(0.5, 0.1))

### 1d mesh for tree cover density ####
mesh1D_tcd <- inla.mesh.1d(seq(min(tcd[], na.rm = T)-1, 
                               max(tcd[], na.rm = T)+1, 
                               length.out = 20), 
                           degree = 2) 

diff(range(tcd[], na.rm = T))/3
matern1D_tcd <- inla.spde2.pcmatern(mesh1D_tcd,
                                    prior.range = c(2.6, 0.5), # 1 third range mesh
                                    prior.sigma = c(1, 0.5))

### 1d mesh for small woody features ####
mesh1D_swf <- inla.mesh.1d(seq(min(swf[], na.rm = T)-1, 
                               max(swf[], na.rm = T)+1, 
                               length.out = 20), 
                           degree = 2) 

diff(range(swf[], na.rm = T))/3
matern1D_swf <- inla.spde2.pcmatern(mesh1D_swf,
                                    prior.range = c(2.6, 0.5), # 1 third range mesh
                                    prior.sigma = c(1, 0.5))

### 1d mesh for transitional woodland and shrubs ####
mesh1D_shrub <- inla.mesh.1d(seq(min(shrub[], na.rm = T)-1, 
                                 max(shrub[], na.rm = T)+1, 
                                 length.out = 20), 
                             degree = 2) 

diff(range(shrub[], na.rm = T))/3
matern1D_shrub <- inla.spde2.pcmatern(mesh1D_shrub,
                                    prior.range = c(3.8, 0.5), # 1 third range mesh
                                    prior.sigma = c(1, 0.5))

### 1d mesh for peatbogs and moors ####
mesh1D_peatbogs <- inla.mesh.1d(seq(min(peatbogsandMoors[], na.rm = T)-1, 
                                    max(peatbogsandMoors[], na.rm = T)+1, 
                                    length.out = 20), 
                                degree = 2) 

diff(range(peatbogsandMoors[], na.rm = T))/3
matern1D_peatbogs <- inla.spde2.pcmatern(mesh1D_peatbogs,
                                         prior.range = c(1.2, 0.5), # 1 third range mesh
                                         prior.sigma = c(1, 0.5))

### 1d mesh for grasslands and pastures ####
mesh1D_grassPast <- inla.mesh.1d(seq(min(grasslandsPastures[], na.rm = T)-1,
                                     max(grasslandsPastures[], na.rm = T)+1,
                                     length.out = 20),
                                 degree = 2)

diff(range(grasslandsPastures[], na.rm = T))/3
matern1D_grassPast <- inla.spde2.pcmatern(mesh1D_grassPast,
                                          prior.range = c(2, 0.01), # 1 third range mesh
                                          prior.sigma = c(0.1, 0.01))

### 1d mesh for distance to roads ####
mesh1D_distRoads <- inla.mesh.1d(seq(min(roadDist[], na.rm = T)-1,
                                     max(roadDist[], na.rm = T)+1,
                                     length.out = 20),
                                 degree = 2)

diff(range(roadDist[], na.rm = T))/3
matern1D_distRoads <- inla.spde2.pcmatern(mesh1D_distRoads,
                                          prior.range = c(4.6, 0.5), # 1 third range mesh
                                          prior.sigma = c(1, 0.5))

### 1d mesh for distance to paths ####
mesh1D_distPaths <- inla.mesh.1d(seq(min(pathDist[], na.rm = T)-1,
                                     max(pathDist[], na.rm = T)+1,
                                     length.out = 20),
                                 degree = 2)

diff(range(pathDist[], na.rm = T))/3
matern1D_distPaths <- inla.spde2.pcmatern(mesh1D_distPaths,
                                          prior.range = c(1.5, 0.1), # 1 third range mesh
                                          prior.sigma = c(0.3, 0.1))

### 1d mesh for distance to forests ####
mesh1D_distForests <- inla.mesh.1d(seq(min(forestDist[], na.rm = T)-1,
                                       max(forestDist[], na.rm = T)+1,
                                       length.out = 20),
                                   degree = 2)

diff(range(forestDist[], na.rm = T))/3
matern1D_distForests <- inla.spde2.pcmatern(mesh1D_distForests,
                                            prior.range = c(4, 0.1), # 1 third range mesh
                                            prior.sigma = c(0.5, 0.01))

### 1d mesh for heat loading index ####
mesh1D_heat <- inla.mesh.1d(seq(min(heat_loading[], na.rm = T)-1,
                                max(heat_loading[], na.rm = T)+1,
                                length.out = 20),
                            degree = 2)

diff(range(heat_loading[], na.rm = T))/3
matern1D_heat <- inla.spde2.pcmatern(mesh1D_heat,
                                     prior.range = c(8.8, 0.5), # 1 third range mesh
                                     prior.sigma = c(1, 0.5))

### 1d mesh for topographic wetness index ####
mesh1D_topo <- inla.mesh.1d(seq(min(topo_wetness[], na.rm = T)-1,
                                max(topo_wetness[], na.rm = T)+1,
                                length.out = 20),
                            degree = 2)

diff(range(topo_wetness[], na.rm = T))/3
matern1D_topo <- inla.spde2.pcmatern(mesh1D_topo,
                                     prior.range = c(1.5, 0.1), # 1 third range mesh
                                     prior.sigma = c(0.5, 0.1))


### 1d mesh for human footprint index ####
mesh1D_hfi <- inla.mesh.1d(seq(min(human_footprint[], na.rm = T)-1,
                               max(human_footprint[], na.rm = T)+1,
                               length.out = 20),
                           degree = 2)

diff(range(human_footprint[], na.rm = T))/3
matern1D_hfi <- inla.spde2.pcmatern(mesh1D_hfi,
                                    prior.range = c(2.14, 0.1), # 1 third range mesh
                                    prior.sigma = c(0.5, 0.1))

# M4 non-linear covar effects + spde ####

## Set up spde ####

matern2D_small <- inla.spde2.pcmatern(mesh,
                                      prior.range = c(30, 0.1),  #1/3 y coordinate 90
                                      prior.sigma = c(0.01, 0.1)) #0.001

matern2D_big <- inla.spde2.pcmatern(mesh,
                                    prior.range = c(100, 0.01),  #1/3 y coordinate 90
                                    prior.sigma = c(0.001, 0.01)) #0.01 at p 0.1 works

## Formula ####

nonlinear_SPDE <- geometry ~  Intercept(1)  +
  
  # Eff.elevation(elevation, model = "linear") +
  Eff.elevation(elevation, model = matern1D_elev) +

  # Eff.slope(slope, model = "linear") +
  Eff.slope(slope, model = matern1D_slope) +

  # Eff.tcd(tcd, model = "linear") +
  # Eff.tcd(tcd, model = matern1D_tcd) +

  Eff.swf(swf, model = "linear") +
  # Eff.swf(swf, model = matern1D_swf) +

  # Eff.shrub(shrub, model = "linear") +
  # Eff.shrub(shrub, model = matern1D_shrub) +

  # Eff.grassPast(peatbogsandMoors, model = "linear") +
  # Eff.peatbogs(peatbogsandMoors, model = matern1D_peatbogs) +

  # Eff.grassPast(grasslandsPastures, model = "linear") +
  Eff.grassPast(grasslandsPastures, model = matern1D_grassPast) +

  # Eff.roaddist(roadDist, model = "linear") +
  # Eff.roaddist(roadDist, model = matern1D_distRoads) +

  # Eff.pathdist(pathDist, model = "linear") +
  Eff.pathdist(pathDist, model = matern1D_distPaths) +

  # Eff.forestdist(forestDist, model = "linear") +
  Eff.forestdist(forestDist, model = matern1D_distForests) +

  # Eff.heat(heat_loading, model = "linear") +
  # Eff.heat(heat_loading, model = matern1D_heat) +

  # Eff.topo(topo_wetness, model = "linear") +
  Eff.topo(topo_wetness, model = matern1D_topo) +

  # Eff.hfi(human_footprint, model = "linear") +
  Eff.hfi(human_footprint, model = matern1D_hfi) +
  
  Eff.smooth_big(geometry, model = matern2D_big) +
  # Eff.smooth_small(geometry, model = matern2D_small) +
  
  NULL

## Run model ####
m4 <- lgcp(nonlinear_SPDE,
           badgers_all,
           ips = int_pointsw)

# saveRDS(m4, file = "Outputs/badgers_lgcp_model_1km/final_badger_lgcp_model.RDS")
# m4 <- readRDS("Outputs/badgers_lgcp_model_1km/final_badger_lgcp_model.RDS")

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
    swf = Eff.swf,
    # shrub = Eff.shrub,
    hfi = Eff.hfi,
    pathDistance = Eff.pathdist,
    # heat = Eff.heat,
    topoWetness = Eff.topo,
    forestDistance = Eff.forestdist,
    spfield_big = Eff.smooth_big,
    
    all = Intercept +
      Eff.elevation +
      Eff.slope +
      Eff.grassPast +
      Eff.swf +
      # Eff.shrub +
      Eff.hfi +
      Eff.pathdist +
      Eff.topo +
      # Eff.heat +
      Eff.forestdist +
      Eff.smooth_big
    ))

# saveRDS(lp4, file = "Outputs/badgers_lgcp_model_1km/linear_predictor.RDS")
# lp4 <- readRDS("Outputs/badgers_lgcp_model_1km/linear_predictor.RDS")
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
                Eff.swf +
                # Eff.shrub +
                Eff.hfi +
                Eff.pathdist +
                Eff.topo +
                # Eff.heat +
                Eff.forestdist +
                Eff.smooth_big
              )))

# saveRDS(rp4, file = "Outputs/badgers_lgcp_model_1km/response_predictor.RDS")

### plot #### 
inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

inside = sapply(st_intersects(lp4$all, ireland_outline_sf), function(x){length(x)==0})
x <- lp4$all[!inside,]

ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badger_subset, alpha = 0.5, size = 1) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu') + 
  ggtitle("Spatial random field") + 
  NULL + 

ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  # geom_sf(data = badger_subset, alpha = 0.5, size = 1) +
  ggtitle("Badger distribution (linear scale)") +
  labs(x = "", y = "", fill = "Mean") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL

inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

ggplot() + 
  gg(data = y, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  ggtitle("Main sett distribution response") +
  labs(x = "", y = "", fill = "Mean") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL

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

pathdist_df <- lp4$pathDistance[!inside,]
pathdist_df <- pathdist_df %>%
  mutate(Variable = "Distance to paths")

fordist_df <- lp4$forestDistance[!inside,]
fordist_df <- fordist_df %>% 
  mutate(Variable = "Distance to forest edge")

topo_df <- lp4$topoWetness[!inside,]
topo_df <- topo_df %>% 
  mutate(Variable = "Topographic wetness index")


ggplot() +
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Elevation", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Slope", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Grasslands and pastures", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = swf_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Small woodie features", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Human footprint index", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = pathdist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Distance to paths", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = fordist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Distance to the forest edge", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1) +
  labs(title = "Topographic wetness index", x = "", y = "", fill = "Mean") +

  plot_layout(ncol = 4)

  
  
## Spde and cor covariance plots ####

range.plot <- plot(spde.posterior(m4, "Eff.smooth_big", what = "range"))
var.plot <- plot(spde.posterior(m4, "Eff.smooth_big", what = "log.variance"))

corplot <- plot(spde.posterior(m4, "Eff.smooth_big", what = "matern.correlation"))
covplot <- plot(spde.posterior(m4, "Eff.smooth_big", what = "matern.covariance"))

multiplot(range.plot, var.plot, covplot, corplot)

## Evaluate effects ####


#### Elevation ####

elevation_scaled <- extract(elevation, badgers_all)
elevation_ipoints <- extract(elevation, int_pointsw)

elev.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(elevation_var = seq(min(elevation[], na.rm = T), 
                                       quantile(elevation[], probs = 0.99, na.rm = T), 
                                       length.out = 1000)),
  formula = ~ Eff.elevation_eval(elevation_var), 
  exclude = c(
    # "Eff.elevation",
    "Eff.slope", 
    "Eff.grassPast", 
    "Eff.swf",
    "Eff.shrub", 
    "Eff.hfi", 
    "Eff.pathdist", 
    "Eff.topo", 
    "Eff.heat",  
    "Eff.forestdist", 
    "Eff.smooth_big"
  )) 

# saveRDS(elev.pred, file = "Outputs/sett_model/elev.pred.RDS")

eval.elev <- ggplot(elev.pred) +
    geom_line(aes(elevation_var, q0.5)) +
    geom_ribbon(aes(elevation_var,
                    ymin = q0.025,
                    ymax = q0.975),
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
    theme_bw()

#### Slope ####

slope_scaled <- extract(slope, badgers_all)
slope_ipoints <- extract(slope, int_pointsw)

slope.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(slope_var = seq(min(slope[], na.rm = T), 
                                   quantile(slope[], 0.99, na.rm = T), 
                                   length.out = 1000)),
  formula = ~ Eff.slope_eval(slope_var), 
  exclude = c(
    "Eff.elevation",
    # "Eff.slope", 
    "Eff.grassPast", 
    "Eff.swf",
    "Eff.shrub", 
    "Eff.hfi", 
    "Eff.pathdist", 
    "Eff.topo", 
    "Eff.heat",  
    "Eff.forestdist", 
    "Eff.smooth_big"
  )) 

# saveRDS(slope.pred, file = "Outputs/sett_model/slope.pred.RDS")


eval.slope <- ggplot(slope.pred) +
    geom_line(aes(slope_var, q0.5)) +
    geom_ribbon(aes(slope_var,
                    ymin = q0.025,
                    ymax = q0.975),
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
    theme_bw()


#### Grasslands ####

grasslandPastures_scaled <- extract(grasslandsPastures, badgers_all)
grasslandPastures_ipoints <- extract(grasslandsPastures, int_pointsw)


grasslandPastures.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(grass_var = 
                         seq(min(grasslandsPastures[], na.rm = T),
                             quantile(grasslandsPastures[], 0.99, na.rm = T),
                             length.out = 1000)),
  formula = ~ Eff.grassPast_eval(grass_var),
  exclude = c(
    "Eff.elevation",
    "Eff.slope", 
    # "Eff.grassPast", 
    "Eff.swf",
    "Eff.shrub", 
    "Eff.hfi", 
    "Eff.pathdist", 
    "Eff.topo", 
    "Eff.heat",  
    "Eff.forestdist", 
    "Eff.smooth_big"
  )) 

# saveRDS(grasslandPastures.pred, file = "Outputs/sett_model/grasslandPastures.pred.RDS")

eval.grasslandsPastures <- ggplot(grasslandPastures.pred) +
    geom_line(aes(grass_var, q0.5)) +
    geom_ribbon(aes(grass_var,
                    ymin = q0.025,
                    ymax = q0.975),
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
    theme_bw()


#### Small woodie features ####

swf_scaled <- extract(swf, badgers_all)
swf_ipoints <- extract(swf, int_pointsw)

swf.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(swf_var = 
                         seq(min(swf[], na.rm = T),
                             quantile(swf[], 0.99, na.rm = T),
                             length.out = 1000)),
  formula = ~ Eff.swf_eval(swf_var),
  exclude = c(
    "Eff.elevation",
    "Eff.slope", 
    "Eff.grassPast",
    # "Eff.swf",
    "Eff.shrub", 
    "Eff.hfi", 
    "Eff.pathdist", 
    "Eff.topo", 
    "Eff.heat",  
    "Eff.forestdist", 
    "Eff.smooth_big"
  )) 

# saveRDS(grasslandPastures.pred, file = "Outputs/sett_model/grasslandPastures.pred.RDS")

eval.swf <- ggplot(swf.pred) +
    geom_line(aes(swf_var, q0.5)) +
    geom_ribbon(aes(swf_var,
                    ymin = q0.025,
                    ymax = q0.975),
                alpha = 0.2) + 
    geom_rug(data = swf_scaled, aes(x = small_woody_features)) + 
    geom_rug(data = swf_scaled, aes(x = small_woody_features), inherit.aes = F, 
             col = "darkgray", sides = "t") + 
    scale_x_continuous(breaks = seq(min(swf[], na.rm = T), 
                                    max(swf[], na.rm = T), 
                                    length.out = 10), 
                       labels = round((seq(min(env_vars$small_woody_features[], na.rm = T), 
                                               max(env_vars$small_woody_features[], na.rm = T), 
                                               length.out = 10)), 0), 
                       limits = c(min(swf[], na.rm = T),
                                  quantile(swf[], 0.99, na.rm = T))) + 
    labs(x = "Percentage of small woodie features", y = "Effect") + 
    # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
    #                    limits = c(-1.5, 1.5)) + 
    theme_bw()


#### Human footprint index ####

hfi_scaled <- extract(human_footprint, badgers_all)
hfi_ipoints <- extract(human_footprint, int_pointsw)

hfi.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(hfi_var = seq(
    min(human_footprint[], na.rm = T), 
    quantile(human_footprint[], 0.99, na.rm = T), 
    length.out = 1000)),
  formula = ~ Eff.hfi_eval(hfi_var), 
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast", 
    "Eff.swf",
    "Eff.shrub",
    # "Eff.hfi", 
    "Eff.pathdist", 
    "Eff.topo", 
    "Eff.heat",  
    "Eff.forestdist", 
    "Eff.smooth_big"
    )) 

# saveRDS(hfi.pred, file = "Outputs/sett_model/hfi.pred.RDS")

eval.hfi <- ggplot(hfi.pred) +
    geom_line(aes(hfi_var, q0.5)) +
    geom_ribbon(aes(hfi_var,
                    ymin = q0.025,
                    ymax = q0.975),
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
    theme_bw()


#### distance to paths ####

pathdist_scaled <- extract(pathDist, badgers_all)
pathdist_ipoints <- extract(pathDist, int_pointsw)

pathDist.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(path_var = seq(min(pathDist[], na.rm = T),
                                       quantile(pathDist[], 0.99, na.rm = T),
                                       length.out = 1000)),
  formula = ~ Eff.pathdist_eval(path_var),
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast",
    "Eff.swf",
    "Eff.shrub",
    "Eff.hfi",
    # "Eff.pathdist",
    "Eff.topo",
    "Eff.heat",
    "Eff.forestdist",
    "Eff.smooth_big"
  ))

eval.pathDist <- ggplot(pathDist.pred) +
    geom_line(aes(path_var, mean)) +
    geom_ribbon(aes(path_var,
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
    theme_bw()

#### Topographic wetness index ####

topo_scaled <- extract(topo_wetness, badgers_all)
topo_ipoints <- extract(topo_wetness, int_pointsw)

topo.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(topo_var = seq(
    quantile(topo_wetness[], 0.01, na.rm = T), 
    quantile(topo_wetness[], 0.99, na.rm = T), 
    length.out = 1000)),
  formula = ~ Eff.topo_eval(topo_var), 
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast", 
    "Eff.swf",
    "Eff.shrub",
    "Eff.hfi",
    "Eff.pathdist",
    # "Eff.topo", 
    "Eff.heat",  
    "Eff.forestdist", 
    "Eff.smooth_big"
    )) 

# saveRDS(topo.pred, file = "Outputs/sett_model/topo.pred.RDS")

eval.topo <- ggplot(topo.pred) +
    geom_line(aes(topo_var, q0.5)) +
    geom_ribbon(aes(topo_var,
                    ymin = q0.025,
                    ymax = q0.975),
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
                       limits = c(quantile(topo_wetness[], 0.01, na.rm = T), 
                                  quantile(topo_wetness[], 0.99, na.rm = T))) + 
    labs(x = "Topographic wetness index", y = "Effect") +
    # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
    #                    limits = c(-1.5, 1.5)) + 
    theme_bw() 

#### Distance to forest edge ####

forestDist_scaled <- extract(forestDist, badgers_all)
forestDist_ipoints <- extract(forestDist, int_pointsw)

forestDist.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(dist_var = seq(
    quantile(forestDist[], 0.001, na.rm = T), 
    quantile(forestDist[], 0.99, na.rm = T), 
    length.out = 1000)),
  formula = ~ Eff.forestdist_eval(dist_var), 
  exclude = c(
    "Eff.elevation",
    "Eff.slope",
    "Eff.grassPast", 
    "Eff.swf",
    # "Eff.shrub",
    "Eff.hfi",
    "Eff.pathdist",
    "Eff.topo",
    # "Eff.heat",
    # "Eff.forestdist", 
    "Eff.smooth_big"
  )) 

# saveRDS(topo.pred, file = "Outputs/sett_model/topo.pred.RDS")

eval.forestDist <- ggplot(forestDist.pred) +
    geom_line(aes(dist_var, q0.5)) +
    geom_ribbon(aes(dist_var,
                    ymin = q0.025,
                    ymax = q0.975),
                alpha = 0.2) +
    geom_rug(data = forestDist_scaled, aes(x = forest_distances)) + 
    geom_rug(data = forestDist_ipoints, aes(x = forest_distances), inherit.aes = F, 
             col = "darkgray", sides = "t") + 
    scale_x_continuous(breaks = seq(min(env_vars_scaled$forest_distances[], na.rm = T), 
                                    max(env_vars_scaled$forest_distances[], na.rm = T), 
                                    length.out = 10), 
                       labels = round(seq(min(env_vars$forest_distances[], na.rm = T), 
                                          max(env_vars$forest_distances[], na.rm = T), 
                                          length.out = 10), 0), 
                       limits = c(quantile(forestDist[], 0.001, na.rm = T), 
                                  quantile(forestDist[], 0.99, na.rm = T))) + 
    labs(x = "Distance to forest edge", y = "Effect") +
    # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7), 
    #                    limits = c(-1.5, 1.5)) + 
    theme_bw()


### plot ####

multiplot(
  eval.elev,
  eval.slope,
  eval.grasslandsPastures, 
  eval.swf,
  # eval.shrub,
  eval.hfi,
  eval.pathDist,
  eval.topo,
  # eval.heat,
  eval.forestDist, 
  cols = 3 
)

beepr::beep(sound = 3)
# ggsave("Outputs/sett_model/setts_covars.png")


