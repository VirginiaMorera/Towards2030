# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

bru_options_set(bru_verbose = TRUE,
#                 control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.inla = list(int.strategy="auto"))

# Preparation of data ####

## load sett data and covariates ####
sett_all <- readRDS("Data/sett_2025_inside_effort.RDS") %>% 
  st_transform(crs = projKM) 

sett_subset <- sett_all %>% 
  filter(MAIN_SETT == "Yes") %>%
  dplyr::select(SETT_ID, geometry) %>% 
  distinct() 

env_vars <- terra::rast("Data/Covars/final_covars_terra.grd")
env_vars$forest_distances <- env_vars$forest_distances/1000

env_vars$PeatbogsandMoors <- sum(env_vars$Peatbogs, env_vars$Moorsandheathland)
env_vars$GrasslandPastures <- sum(env_vars$Naturalgrasslands, env_vars$Pastures)

env_vars_scaled <- terra::scale(env_vars) 

scaling_parameters <- data.frame(
  names = names(env_vars), 
  means = global(env_vars, mean, na.rm=TRUE)[,1],
  sds = global(env_vars, sd, na.rm=TRUE)[,1])

## load mesh boundaries and samplers ####
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
ireland_outline_simple <- st_simplify(ireland_outline_sf, dTolerance = 5)

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 

mesh <- readRDS("Data/Inla/meshes.RDS")[[2]]
mesh$crs <- projKM

int_pointsw <- readRDS("Data/Inla/weighted_int_points4.RDS")

inner_boundary <- st_as_sf(readRDS("Data/Inla/inner_boundary.RDS"))

## create different integration points dfs ####
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
                                     prior.range = c(2, 0.99), # 1 third range mesh
                                     prior.sigma = c(2, 0.1))

### 1d mesh for slope ####
mesh1D_slope <- inla.mesh.1d(seq(min(slope[], na.rm = T)-1, 
                                 max(slope[], na.rm = T)+1, 
                                 length.out = 20), 
                             degree = 2) 

diff(range(slope[], na.rm = T))/3
matern1D_slope <- inla.spde2.pcmatern(mesh1D_slope,
                                      prior.range = c(1, 0.1), # 1 third range mesh
                                      prior.sigma = c(2, 0.1))


### 1d mesh for grasslands and pastures ####
mesh1D_grassPast <- inla.mesh.1d(seq(min(grasslandsPastures[], na.rm = T)-1,
                                     max(grasslandsPastures[], na.rm = T)+1,
                                     length.out = 20),
                                 degree = 2)

diff(range(grasslandsPastures[], na.rm = T))/3
matern1D_grassPast <- inla.spde2.pcmatern(mesh1D_grassPast,
                                          prior.range = c(2, 0.99), # 1 third range mesh
                                          prior.sigma = c(2, 0.1))


### 1d mesh for distance to forests ####
mesh1D_distForests <- inla.mesh.1d(seq(min(forestDist[], na.rm = T)-1,
                                       max(forestDist[], na.rm = T)+1,
                                       length.out = 20),
                                   degree = 2)

diff(range(forestDist[], na.rm = T))/3
matern1D_distForests <- inla.spde2.pcmatern(mesh1D_distForests,
                                            prior.range = c(1, 0.1), # 1 third range mesh
                                            prior.sigma = c(2, 0.1))

### 1d mesh for topographic wetness index ####
mesh1D_topo <- inla.mesh.1d(seq(min(topo_wetness[], na.rm = T)-1,
                                max(topo_wetness[], na.rm = T)+1,
                                length.out = 20),
                            degree = 2)

diff(range(topo_wetness[], na.rm = T))/3
matern1D_topo <- inla.spde2.pcmatern(mesh1D_topo,
                                     prior.range = c(1, 0.1), # 1 third range mesh
                                     prior.sigma = c(2, 0.1))


### 1d mesh for human footprint index ####
mesh1D_hfi <- inla.mesh.1d(seq(min(human_footprint[], na.rm = T)-1,
                               max(human_footprint[], na.rm = T)+1,
                               length.out = 20),
                           degree = 2)

diff(range(human_footprint[], na.rm = T))/3
matern1D_hfi <- inla.spde2.pcmatern(mesh1D_hfi,
                                    prior.range = c(1, 0.1), # 1 third range mesh
                                    prior.sigma = c(2, 0.1))

# M4 non-linear covar effects + spde ####

## Set up spde ####

matern2D_big <- inla.spde2.pcmatern(mesh,      
                                    # alpha = 3/2,
                                    prior.range = c(100, 0.01),  #1/3 y coordinate 90
                                    prior.sigma = c(0.01, NA)) #0.02 at p 0.1 works

## Formula ####

nonlinear_SPDE <- geometry ~  Intercept(1)  +
  Eff.elevation(elevation, model = "linear") +
  Eff.slope(slope, model = matern1D_slope) +
  Eff.grassPast(grasslandsPastures, model = matern1D_grassPast) +
  Eff.forestdist(forestDist, model = matern1D_distForests) +
  Eff.topo(topo_wetness, model = "linear") +
  Eff.hfi(human_footprint, model = matern1D_hfi) +
  Eff.smooth_big(geometry, model = matern2D_big) +
  NULL


## Run model ####
m4 <- lgcp(components = nonlinear_SPDE,
           data = sett_subset,
           ips = int_pointsw)

# saveRDS(m4, file = "Outputs/sett_model/final_main_sett_model.RDS")
# m4 <- readRDS("Outputs/sett_model/final_main_sett_model.RDS")
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
    topoWetness = Eff.topo,
    hfi = Eff.hfi,
    forestDistance = Eff.forestdist,
    # spfield_small = Eff.smooth_small,
    spfield_big = Eff.smooth_big,
      
    all = Intercept +
      Eff.elevation +
      Eff.slope +
      Eff.grassPast +
      Eff.topo +
      Eff.hfi +
      Eff.forestdist +
      # Eff.smooth_small +
      Eff.smooth_big
    ))

# saveRDS(lp4, file = "Outputs/sett_model/linear_predictor.RDS")
# lp4 <- readRDS("Outputs/sett_model/linear_predictor.RDS")

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
                Eff.topo +
                Eff.hfi +
                Eff.forestdist +
                Eff.smooth_big
              ))
)

# saveRDS(rp4, file = "Outputs/sett_model/response_predictor.RDS")
# rp4 <- readRDS("Outputs/sett_model/response_predictor.RDS")

### plot #### 
inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

inside = sapply(st_intersects(lp4$all, ireland_outline_sf), function(x){length(x)==0})
x <- lp4$all[!inside,]

#### prediction ####
ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1, col = "white") +
  labs(x = "", y = "", fill = "Median", 
       title = "Main sett distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL +

ggplot() + 
  gg(data = x, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "95% CI", 
       title = "Main sett distribution uncertainty") +  
  theme_bw() + 
  scale_fill_viridis_c() +
  NULL 
  
#### spatial field ####
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

#### response scale prediction ####

inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

ggplot() + 
  gg(data = y, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "Main sett distribution (response scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL + 

ggplot() + 
  gg(data = y, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "Main sett distribution (response scale)") +  
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

topo_df <- lp4$topoWetness[!inside,]
topo_df <- topo_df %>%
  mutate(Variable = "Topographic wetness index")

hfi_df <- lp4$hfi[!inside,]
hfi_df <- hfi_df %>%
  mutate(Variable = "Human footprint index")

forestdist_df <- lp4$forestDistance[!inside,]
forestdist <- forestdist_df %>%
  mutate(Variable = "Distance to forest edge")

limit <- max(abs(c(elev_df$q0.5, slope_df$q0.5, grass_df$q0.5, hfi_df$q0.5,
                   forestdist_df$q0.5, topo_df$q0.5))) * c(-1, 1)

ggplot() + 
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) + 
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) + 
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) + 
  labs(title = "Pasture and grasslands effect", x = "", y = "", fill = "Median") +

ggplot() +
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) +
  labs(title = "Topographic wetness index effect", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) +
  labs(title = "Human footprint index effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = forestdist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) +
  labs(title = "Distance to forest edge", x = "", y = "", fill = "Mean") +
 
plot_layout(ncol = 3)

  
## Spde and cor covariance plots ####

multiplot(plot(spde.posterior(m4, "Eff.smooth_big", what = "range")), 
          plot(spde.posterior(m4, "Eff.smooth_big", what = "log.variance")), 
          plot(spde.posterior(m4, "Eff.smooth_big", what = "matern.correlation")), 
          plot(spde.posterior(m4, "Eff.smooth_big", what = "matern.covariance")))

## Evaluate effects ####

#### Elevation ####

elevation_scaled <- extract(elevation, sett_subset)
elevation_ipoints <- extract(elevation, int_pointsw)

elev.pred <- predict(
  m4,
  n.samples = 100,
  newdata = data.frame(elevation = seq(min(elevation[], na.rm = T), 
                                    quantile(elevation[], probs = 0.99, na.rm = T), 
                                    length.out = 100)),
  formula = ~ Eff.elevation_eval(elevation)) 

eval.elev <- ggplot(elev.pred) +
    geom_line(aes(elevation, q0.5)) +
    geom_ribbon(aes(elevation,
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

slope_scaled <- extract(slope, sett_subset)
slope_ipoints <- extract(slope, int_pointsw)

slope.pred <- predict(
  m4,
  n.samples = 100,
  newdata = data.frame(slope = seq(min(slope[], na.rm = T), 
                                   quantile(slope[], 0.99, na.rm = T), 
                                   length.out = 100)),
  formula = ~ Eff.slope_eval(slope)) 

eval.slope <- ggplot(slope.pred) +
    geom_line(aes(slope, q0.5)) +
    geom_ribbon(aes(slope,
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

grasslandPastures_scaled <- extract(grasslandsPastures, sett_subset)
grasslandPastures_ipoints <- extract(grasslandsPastures, int_pointsw)

grasslandPastures.pred <- predict(
  m4,
  n.samples = 100,
  newdata = data.frame(grasslandsPastures = 
                         seq(min(grasslandsPastures[], na.rm = T),
                             quantile(grasslandsPastures[], 0.99, na.rm = T),
                             length.out = 100)),
  formula = ~ Eff.grassPast_eval(grasslandsPastures)) 

eval.grasslandsPastures <- ggplot(grasslandPastures.pred) +
  geom_line(aes(grasslandsPastures, q0.5)) +
  geom_ribbon(aes(grasslandsPastures,
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

#### Topographic wetness index ####

topo_scaled <- extract(topo_wetness, sett_subset)
topo_ipoints <- extract(topo_wetness, int_pointsw)

topo.pred <- predict(
  m4,
  n.samples = 100,
  # n.samples = 100,
  newdata = data.frame(topo = seq(min(topo_wetness[], na.rm = T),
                                  quantile(topo_wetness[], 0.99, na.rm = T),
                                  length.out = 100)),
  formula = ~ Eff.topo_eval(topo))
  
eval.topo <- ggplot(topo.pred) +
    geom_line(aes(topo, q0.5)) +
    geom_ribbon(aes(topo,
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
                       limits = c(min(topo_wetness[], na.rm = T),
                                  quantile(topo_wetness[], 0.99, na.rm = T))) +
    labs(x = "Topographic wetness index", y = "Effect") +
    # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7),
    #                    limits = c(-1.5, 1.5)) +
    theme_bw()

#### Human footprint index ####

hfi_scaled <- extract(human_footprint, sett_subset)
hfi_ipoints <- extract(human_footprint, int_pointsw)

hfi.pred <- predict(
  m4,
  n.samples = 100,
  # n.samples = 100,
  newdata = data.frame(hfi = seq(min(human_footprint[], na.rm = T),
                                 quantile(human_footprint[], 0.99, na.rm = T),
                                 length.out = 100)),
  formula = ~ Eff.hfi_eval(hfi))

eval.hfi <- ggplot(hfi.pred) +
    geom_line(aes(hfi, q0.5)) +
    geom_ribbon(aes(hfi,
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

#### Distance to forest edge ####

forestdist_scaled <- extract(forestDist, sett_subset)
forestdist_ipoints <- extract(forestDist, int_pointsw)

forestdist.pred <- predict(
  m4,
  n.samples = 100,
  # n.samples = 100,
  newdata = data.frame(forestdist = seq(min(forestDist[], na.rm = T),
                                 quantile(forestDist[], 0.99, na.rm = T),
                                 length.out = 100)),
  formula = ~ Eff.forestdist_eval(forestdist))

eval.forestdist <- ggplot(forestdist.pred) +
  geom_line(aes(forestdist, q0.5)) +
  geom_ribbon(aes(forestdist,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  geom_rug(data = forestdist_scaled, aes(x = forest_distances)) +
  geom_rug(data = forestdist_ipoints, aes(x = forest_distances), inherit.aes = F,
           col = "darkgray", sides = "t") +
  scale_x_continuous(breaks = seq(min(env_vars_scaled$forest_distances[], na.rm = T),
                                  max(env_vars_scaled$forest_distances[], na.rm = T),
                                  length.out = 10),
                     labels = round(seq(min(env_vars$forest_distances[], na.rm = T),
                                        max(env_vars$forest_distances[], na.rm = T),
                                        length.out = 10), 0),
                     limits = c(min(forestDist[], na.rm = T),
                                quantile(forestDist[], 0.99, na.rm = T))) +
  labs(x = "Distance to forest edge", y = "Effect") +
  # scale_y_continuous(breaks = seq(-1.5, 1.5, length.out = 7),
  #                    limits = c(-1.5, 1.5)) +
  theme_bw()

### plot ####

multiplot(
  eval.elev,
  eval.grasslandsPastures,
  eval.hfi,
  
  eval.slope,
  eval.forestdist,
  eval.topo,
  cols = 2 
)


beepr::beep(sound = 3)
# ggsave("Outputs/sett_model/setts_covars.png")

