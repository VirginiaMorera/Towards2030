# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

bru_options_set(bru_verbose = FALSE,
                # control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.inla = list(int.strategy="auto"))

# Preparation of data ####

## load sett data and covariates ####
badgers_all <- readRDS("Data/badgers_jittered_filtered.RDS") %>% 
  st_transform(crs = projKM) 

# badger_subset <- badgers_all %>% 
#   filter(YEAR > 2018)

env_vars <- terra::rast("Data/Covars/final_covars_terra_with_setts.grd")


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
setts <- env_vars_scaled$setts

## Prepare 1D meshes for covars ####

### 1d mesh for elevation ####
mesh1D_elev <- inla.mesh.1d(seq(min(elevation[], na.rm = T)-1, 
                                max(elevation[], na.rm = T)+1, 
                                length.out = 20),
                            degree = 2)

diff(range(elevation[], na.rm = T))/3
matern1D_elev <- inla.spde2.pcmatern(mesh1D_elev,
                                     prior.range = c(2, 0.1), # 1 third range mesh
                                     prior.sigma = c(0.1, 0.1))

### 1d mesh for slope ####
mesh1D_slope <- inla.mesh.1d(seq(min(slope[], na.rm = T)-1, 
                                 max(slope[], na.rm = T)+1, 
                                 length.out = 20), 
                             degree = 2) 

diff(range(slope[], na.rm = T))/3
matern1D_slope <- inla.spde2.pcmatern(mesh1D_slope,
                                      prior.range = c(3, 0.01), # 1 third range mesh
                                      prior.sigma = c(0.1, 0.01))


### 1d mesh for grasslands and pastures ####
mesh1D_grassPast <- inla.mesh.1d(seq(min(grasslandsPastures[], na.rm = T)-1,
                                     max(grasslandsPastures[], na.rm = T)+1,
                                     length.out = 10),
                                 degree = 2)

diff(range(grasslandsPastures[], na.rm = T))/3
matern1D_grassPast <- inla.spde2.pcmatern(mesh1D_grassPast,
                                          prior.range = c(0.3, 0.1), # 1 third range mesh
                                          prior.sigma = c(1, 0.1))


### 1d mesh for distance to forests ####
mesh1D_distForests <- inla.mesh.1d(seq(min(forestDist[], na.rm = T)-1,
                                       max(forestDist[], na.rm = T)+1,
                                       length.out = 20),
                                   degree = 2)

diff(range(forestDist[], na.rm = T))/3
matern1D_distForests <- inla.spde2.pcmatern(mesh1D_distForests,
                                            prior.range = c(4, 0.9), # 1 third range mesh
                                            prior.sigma = c(0.1, 0.1))

### 1d mesh for topographic wetness index ####
mesh1D_topo <- inla.mesh.1d(seq(min(topo_wetness[], na.rm = T)-1,
                                max(topo_wetness[], na.rm = T)+1,
                                length.out = 20),
                            degree = 2)

diff(range(topo_wetness[], na.rm = T))/3
matern1D_topo <- inla.spde2.pcmatern(mesh1D_topo,
                                     prior.range = c(2, 0.1), # 1 third range mesh
                                     prior.sigma = c(0.5, 0.1))


### 1d mesh for human footprint index ####
mesh1D_hfi <- inla.mesh.1d(seq(min(human_footprint[], na.rm = T)-1,
                               max(human_footprint[], na.rm = T)+1,
                               length.out = 20),
                           degree = 2)

diff(range(human_footprint[], na.rm = T))/3
matern1D_hfi <- inla.spde2.pcmatern(mesh1D_hfi,
                                    prior.range = c(3, 0.1), # 1 third range mesh
                                    prior.sigma = c(0.5, 0.1))

### 1d mesh for sett distribution ####
mesh1D_sett <- inla.mesh.1d(seq(min(setts[], na.rm = T)-1,
                               max(setts[], na.rm = T)+1,
                               length.out = 20),
                           degree = 2)

diff(range(setts[], na.rm = T))/3
matern1D_sett <- inla.spde2.pcmatern(mesh1D_sett,
                                    prior.range = c(4, 0.01), # 1 third range mesh
                                    prior.sigma = c(0.1, 0.1))


# M4 non-linear covar effects + spde ####

## Set up spde ####

matern2D_small <- inla.spde2.pcmatern(mesh,
                                      prior.range = c(30, 0.1),  #1/3 y coordinate 90
                                      prior.sigma = c(0.01, 0.1)) #0.001

matern2D_big <- inla.spde2.pcmatern(mesh,
                                    prior.range = c(90, 0.1),  #1/3 y coordinate 90
                                    prior.sigma = c(0.1, NA)) #

## Formula ####

nonlinear_SPDE <- geometry ~  Intercept(1)  +
  
  Eff.elevation(elevation, model = matern1D_elev) +
  Eff.slope(slope, model = matern1D_slope) +
  Eff.grassPast(grasslandsPastures, model = "linear") +
  Eff.forestdist(forestDist, model = matern1D_distForests) +
  Eff.topo(topo_wetness, model = matern1D_topo) +
  Eff.hfi(human_footprint, model = matern1D_hfi) +
  Eff.sett(setts, model = matern1D_sett) +
  Eff.smooth_big(geometry, model = matern2D_big) +
  NULL

## Run model ####
m4 <- lgcp(nonlinear_SPDE,
           badgers_all,
           ips = int_pointsw)

# saveRDS(m4, file = "Outputs/badgers_all_model/final_model.RDS")
# m4 <- readRDS("Outputs/badgers_all_model/final_model.RDS")

summary(m4)
beepr::beep(sound = 4)

## predict ####

### Linear scale ####

lp4 <- predict(
  object = m4, 
  newdata = df2, 
  samples = 100,
  formula = ~ list(
    elevation = Eff.elevation,
    slope = Eff.slope,
    grassland = Eff.grassPast,
    hfi = Eff.hfi,
    topoWetness = Eff.topo,
    forestDistance = Eff.forestdist,
    setts = Eff.sett,
    spfield_big = Eff.smooth_big,
    
    all = Intercept +
      Eff.elevation +
      Eff.slope +
      Eff.grassPast +
      Eff.hfi +
      Eff.topo +
      Eff.forestdist +
      Eff.sett + 
      Eff.smooth_big
    ))

# saveRDS(lp4, file = "Outputs/badgers_all_model/linear_predictor.RDS")
# lp4 <- readRDS("Outputs/badgers_all_model/linear_predictor.RDS")
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
                Eff.topo +
                Eff.sett + 
                Eff.forestdist +
                Eff.smooth_big
              )))

# saveRDS(rp4, file = "Outputs/badgers_all_model/response_predictor.RDS")
# rp4 <- readRDS("Outputs/badgers_all_model/response_predictor.RDS")

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
       title = "Badger distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL +
  
ggplot() + 
  gg(data = x, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "95% CI", 
       title = "Badger distribution uncertainty") +  
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
       title = "Badger distribution (response scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL + 
  
  ggplot() + 
  gg(data = y, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "Badger distribution uncertainty") +  
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

hfi_df <- lp4$hfi[!inside,]
hfi_df <- hfi_df %>% 
  mutate(Variable = "Human footprint index")

fordist_df <- lp4$forestDistance[!inside,]
fordist_df <- fordist_df %>%
  mutate(Variable = "Distance to forest edge")

topo_df <- lp4$topoWetness[!inside,]
topo_df <- topo_df %>% 
  mutate(Variable = "Topographic wetness index")

setts_df <- lp4$setts[!inside,]
setts_df <- setts_df %>% 
  mutate(Variable = "Sett relative density")

limit <- max(abs(c(elev_df$q0.5, slope_df$q0.5, grass_df$q0.5, hfi_df$q0.5,
                   fordist_df$q0.5, topo_df$q0.5, setts_df$q0.5))) * c(-1, 1)

ggplot() +
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Elevation", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Slope", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Grasslands and pastures", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Human footprint index", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = fordist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Distance to the forest edge", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Topographic wetness index", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = setts_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Sett relative density", x = "", y = "", fill = "Mean") +
  
plot_layout(ncol = 3)

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
  n.samples = 100,
  newdata = data.frame(elevation_var = seq(min(elevation[], na.rm = T), 
                                       quantile(elevation[], probs = 0.99, na.rm = T), 
                                       length.out = 100)),
  formula = ~ Eff.elevation_eval(elevation_var)) 

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
  n.samples = 100,
  newdata = data.frame(slope_var = seq(min(slope[], na.rm = T), 
                                   quantile(slope[], 0.99, na.rm = T), 
                                   length.out = 100)),
  formula = ~ Eff.slope_eval(slope_var)) 

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
  n.samples = 100,
  newdata = data.frame(grass_var = 
                         seq(min(grasslandsPastures[], na.rm = T),
                             quantile(grasslandsPastures[], 0.99, na.rm = T),
                             length.out = 100)),   
  formula = ~ Eff.grassPast_eval(grass_var)) 
 

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

#### Human footprint index ####

hfi_scaled <- extract(human_footprint, badgers_all)
hfi_ipoints <- extract(human_footprint, int_pointsw)

hfi.pred <- predict(
  m4,
  n.samples = 100,
  newdata = data.frame(hfi_var = seq(
    min(human_footprint[], na.rm = T), 
    quantile(human_footprint[], 0.99, na.rm = T), 
    length.out = 100)),
  formula = ~ Eff.hfi_eval(hfi_var)) 

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


#### Topographic wetness index ####

topo_scaled <- extract(topo_wetness, badgers_all)
topo_ipoints <- extract(topo_wetness, int_pointsw)

topo.pred <- predict(
  m4,
  n.samples = 100, 
  newdata = data.frame(topo_var = seq(
    quantile(topo_wetness[], 0.01, na.rm = T), 
    quantile(topo_wetness[], 0.99, na.rm = T), 
    length.out = 100)),
  formula = ~ Eff.topo_eval(topo_var)) 

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
  n.samples = 100, 
  newdata = data.frame(dist_var = seq(
    quantile(forestDist[], 0.001, na.rm = T), 
    quantile(forestDist[], 0.99, na.rm = T), 
    length.out = 100)),
  formula = ~ Eff.forestdist_eval(dist_var)) 

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


#### Sett distribution ####

sett_scaled <- extract(setts, badgers_all)
sett_ipoints <- extract(setts, int_pointsw)

sett.pred <- predict(
  m4,
  n.samples = 100,
  newdata = data.frame(sett_var = seq(
    quantile(setts[], 0.001, na.rm = T), 
    quantile(setts[], 0.99, na.rm = T), 
    length.out = 100)),
  formula = ~ Eff.sett_eval(sett_var)) 

eval.sett <- ggplot(sett.pred) +
  geom_line(aes(sett_var, q0.5)) +
  geom_ribbon(aes(sett_var,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  geom_rug(data = sett_scaled, aes(x = setts)) + 
  geom_rug(data = sett_ipoints, aes(x = setts), inherit.aes = F, 
           col = "darkgray", sides = "t") + 
  scale_x_continuous(breaks = seq(min(setts[], na.rm = T), 
                                  max(setts[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(setts_raster[], na.rm = T),
                                        max(setts_raster[], na.rm = T),
                                        length.out = 10), 3),
                     limits = c(quantile(setts[], 0.001, na.rm = T), 
                                quantile(setts[], 0.99, na.rm = T))) + 
  # scale_y_continuous(labels = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5)) +
  labs(x = "Relative abundance of setts", y = "Effect") +
  theme_bw()

### plot ####

multiplot(
  eval.elev,
  eval.slope,
  eval.grasslandsPastures,
  eval.hfi,
  eval.topo,
  eval.forestDist,
  eval.sett,
  cols = 2 
)

beepr::beep(sound = 3)
# ggsave("Outputs/badgers_all_model/eval_covars.png")


