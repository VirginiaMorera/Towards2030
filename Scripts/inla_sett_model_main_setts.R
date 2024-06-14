# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

bru_options_set(bru_verbose = TRUE,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
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

env_vars <- stack("Data/all_covars_1km_smooth5.grd")

env_vars$PeatbogsandMoors <- sum(env_vars$Peatbogs, env_vars$Moorsandheathland)

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
northness <- as(env_vars_scaled$northness, "SpatRaster")
eastness <- as(env_vars_scaled$eastness, "SpatRaster")
tcd <- as(env_vars_scaled$tree_cover_density, "SpatRaster")
swf <- as(env_vars_scaled$small_woody_features, "SpatRaster")
artificial <- as(env_vars_scaled$Artificialsurfaces, "SpatRaster")
crops <- as(env_vars_scaled$Plantedvegetationandcrops, "SpatRaster")
pastures <- as(env_vars_scaled$Pastures, "SpatRaster")
grassland <- as(env_vars_scaled$Naturalgrasslands, "SpatRaster")
moors <- as(env_vars_scaled$Moorsandheathland, "SpatRaster")
shrub <- as(env_vars_scaled$Transitionalwoodland.shrub, "SpatRaster")
peatbogs <- as(env_vars_scaled$Peatbogs, "SpatRaster")
peatbogsandMoors <- as(env_vars_scaled$PeatbogsandMoors, "SpatRaster")

## Prepare 1D meshes for covars ####

### 1d mesh for elevation ####
mesh1D_elev <- inla.mesh.1d(seq(min(elevation[], na.rm = T)-1, 
                                max(elevation[], na.rm = T)+1, length.out = 10),
                            degree = 2)

diff(range(elevation[], na.rm = T))/3
matern1D_elev <- inla.spde2.pcmatern(mesh1D_elev,
                                     prior.range = c(2.6, 0.1), # 1 third range mesh
                                     prior.sigma = c(0.1, 0.1))

### 1d mesh for slope ####
mesh1D_slope <- inla.mesh.1d(seq(min(slope[], na.rm = T)-1, max(slope[], na.rm = T)+1, 
                                 length.out = 10), 
                             degree = 2) 

diff(range(slope[], na.rm = T))/3
matern1D_slope <- inla.spde2.pcmatern(mesh1D_slope,
                                      prior.range = c(2, 0.1), # 1 third range mesh
                                      prior.sigma = c(0.1, 0.1))

### 1d mesh for northness ####
mesh1D_northness <- inla.mesh.1d(seq(min(northness[], na.rm = T)-1,
                                  max(northness[], na.rm = T)+1,
                                  length.out = 10),
                              degree = 2)

diff(range(northness[], na.rm = T))/3
matern1D_northness <- inla.spde2.pcmatern(mesh1D_northness,
                                       prior.range = c(1.4, 0.5), # 1 third range mesh
                                       prior.sigma = c(0.1, 0.5))

### 1d mesh for eastness ####
mesh1D_eastness <- inla.mesh.1d(seq(min(eastness[], na.rm = T)-1, 
                                    max(eastness[], na.rm = T)+1, 
                                    length.out = 10), 
                                degree = 2) 

diff(range(eastness[], na.rm = T))/3
matern1D_eastness <- inla.spde2.pcmatern(mesh1D_eastness,
                                         prior.range = c(1.4, 0.5), # 1 third range mesh
                                         prior.sigma = c(0.1, 0.5))

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

### 1d mesh for pastures ####
mesh1D_pastures <- inla.mesh.1d(seq(min(pastures[], na.rm = T)-1, 
                                    max(pastures[], na.rm = T)+1, 
                                    length.out = 10), 
                                degree = 2) 

diff(range(pastures[], na.rm = T))/3
matern1D_pastures <- inla.spde2.pcmatern(mesh1D_pastures,
                                         prior.range = c(5, 0.9), # force a longer range, and same as grasslands
                                         prior.sigma = c(0.5, 0.1))

### 1d mesh for peatbogs and moors together ####
mesh1D_peatbogsandMoors <- inla.mesh.1d(seq(min(peatbogsandMoors[], na.rm = T)-1, 
                                    max(peatbogsandMoors[], na.rm = T)+1, 
                                    length.out = 10), 
                                degree = 2) 

diff(range(peatbogsandMoors[], na.rm = T))/3
matern1D_peatbogsandMoors  <- inla.spde2.pcmatern(mesh1D_peatbogsandMoors,
                                         prior.range = c(10, 0.1), # force a longer range, and same as grasslands
                                         prior.sigma = c(1, 0.1))

### 1d mesh for artificial surfaces ####
mesh1D_artificial <- inla.mesh.1d(seq(min(artificial[], na.rm = T)-1,
                                      max(artificial[], na.rm = T)+1,
                                      length.out = 10),
                                  degree = 2)

diff(range(artificial[], na.rm = T))/3
matern1D_artificial <- inla.spde2.pcmatern(mesh1D_artificial,
                                           prior.range = c(3.6, 0.5), # 1 third range mesh
                                           prior.sigma = c(0.1, 0.5))

### 1d mesh for grasslands ####
mesh1D_grassland <- inla.mesh.1d(seq(min(grassland[], na.rm = T)-1,
                                     max(grassland[], na.rm = T)+1,
                                     length.out = 10),
                                 degree = 2)

diff(range(grassland[], na.rm = T))/3
matern1D_grassland <- inla.spde2.pcmatern(mesh1D_grassland,
                                          prior.range = c(4.4, 0.5), # force a longer range together with pastures
                                          prior.sigma = c(0.1, 0.5))

### 1d mesh for shrub ####
mesh1D_shrub <- inla.mesh.1d(seq(min(shrub[], na.rm = T)-1,
                                 max(shrub[], na.rm = T)+1,
                                 length.out = 10),
                             degree = 2)

diff(range(shrub[], na.rm = T))/3
matern1D_shrub <- inla.spde2.pcmatern(mesh1D_shrub,
                                      prior.range = c(30, 0.9), # 1 third range mesh
                                      prior.sigma = c(1, 0.1))


# M4 non-linear covar effects + spde ####

## Set up spde ####

matern2D_small <- inla.spde2.pcmatern(mesh,
                                prior.range = c(30, 0.1),  #1/3 y coordinate 90
                                prior.sigma = c(0.01, 0.1)) #0.001

matern2D_big <- inla.spde2.pcmatern(mesh,
                                    prior.range = c(100, 0.1),  #1/3 y coordinate 90
                                    prior.sigma = c(0.029, 0.001)) #0.001

## Formula ####

nonlinear_SPDE <- geometry ~  Intercept(1)  +
  
  # Eff.elevation(elevation, model = "linear") +
  Eff.elevation(elevation, model = matern1D_elev) +
  
  # Eff.slope(slope, model = "linear") +
  Eff.slope(slope, model = matern1D_slope) +
  
  # Eff.eastness(eastness, model = "linear") +
  
  # Eff.northness(northness, model = "linear") +
  
  # Eff.tcd(tcd, model = "linear") +
  # Eff.tcd(tcd, model = matern1D_tcd) +
  
  # Eff.swf(swf, model = "linear") +
  # Eff.swf(swf, model = matern1D_swf) +
  
  # Eff.crops(crops, model = "linear") +
  Eff.crops(crops, model = matern1D_crops) +
  
  # Eff.pasture(pastures, model = "linear") +
  Eff.pasture(pastures, model = matern1D_pastures) +
  
  # Eff.artificial(artificial, model = "linear") + # linear because is 0 in most of the domain and it'd be a problem for integrating
  # Eff.artificial(artificial, model = matern1D_artificial) +
  
  # Eff.grasslands(grassland, model = "linear") + # linear because is 0 in most of the domain and it'd be a problem for integrating
  Eff.grasslands(grassland, model = matern1D_grassland) +
  
  Eff.shrub(shrub, model = "linear") + # linear because is 0 in most of the domain and it'd be a problem for integrating
  # Eff.shrub(shrub, model = matern1D_shrub) +
  
  Eff.peatbogsandMoors(peatbogsandMoors, model = "linear") +
  
  Eff.smooth_big(geometry, model = matern2D_big) +
  
  # Eff.smooth_small(geometry, model = matern2D_small) +
  # Eff.iid(SETT_ID, model = "iid") +
  NULL


## Run model ####
m4 <- lgcp(components = nonlinear_SPDE,
           data = sett_subset,
           ips = int_pointsw)

# saveRDS(m4, file = "Outputs/main_sett_model_1km_23/final_main_sett_model.RDS")
# m4 <- readRDS("Outputs/main_sett_model_1km/final_main_sett_model.RDS")

summary(m4)
beepr::beep(sound = 4)

## Predict linear scale ####
### predict ####

lp4 <- predict(
  object = m4, 
  newdata = df2, 
  samples = 1000,
  formula = ~ list(
    elevation = Eff.elevation,
    slope = Eff.slope,
    # northness = Eff.northness,
    # eastness = Eff.eastness,
    crops = Eff.crops,
    # tcd = Eff.tcd,
    # swf = Eff.swf,
    pastures = Eff.pasture,
    # artificial = Eff.artificial,
    grassland = Eff.grasslands,
    shrub = Eff.shrub,
    peatbogsandMoors = Eff.peatbogsandMoors,
    spfield_big = Eff.smooth_big,
    # spfield_small = Eff.smooth_small,
    
    just_covars = Intercept +
      Eff.elevation +
      Eff.slope +
      # Eff.northness +
      # Eff.eastness +
      Eff.crops +
      # Eff.tcd +
      # Eff.swf +
      Eff.pasture +
      # Eff.artificial +
      Eff.grasslands +
      Eff.shrub +
      # Eff.smooth_big + 
      # Eff.smooth_small +
      Eff.peatbogsandMoors,
      
    all = Intercept +
      Eff.elevation +
      Eff.slope +
      # Eff.northness +
      # Eff.eastness +
      Eff.crops +
      # Eff.tcd +
      # Eff.swf +
      Eff.pasture +
      # Eff.artificial +
      Eff.grasslands +
      Eff.shrub +
      Eff.smooth_big +
      # Eff.smooth_small +
      Eff.peatbogsandMoors)
)

# saveRDS(lp4, file = "Outputs/main_sett_model_1km_23/m4_lp.RDS")
# lp4 <- readRDS("Outputs/main_sett_model_1km/m4_lp.RDS")

### plot #### 
inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

ggplot() + 
  gg(data = spb, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu') + 
  ggtitle("Large scale SPDE linear scale") + 
  NULL


inside = sapply(st_intersects(lp4$all, ireland_outline_sf), function(x){length(x)==0})
x <- lp4$all[!inside,]

ggplot() + 
  gg(data = x, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  ggtitle("Main sett distribution") +
  labs(x = "", y = "", fill = "Mean") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  
  NULL


inside = sapply(st_intersects(lp4$elevation, ireland_outline_sf), function(x){length(x)==0})
elev <- lp4$elevation[!inside,]

lp4_elev <- ggplot() + 
  gg(data = elev, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  labs(title = "Elevation", x = "", y = "", fill = "Mean") +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  NULL

slop <- lp4$slope[!inside,]

lp4_slope <- ggplot() + 
  gg(data = slop, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Slope", x = "", y = "", fill = "Mean") +
  NULL

grass <- lp4$grassland[!inside,]

lp4_grassland <- ggplot() + 
  gg(data = grass, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Grasslands", x = "", y = "", fill = "Mean") +
  NULL

crop <- lp4$crops[!inside,]

lp4_crops <- ggplot() + 
  gg(data = crop, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Planted vegetation and crops", x = "", y = "", fill = "Mean") +
  NULL

past <- lp4$pastures[!inside,]

lp4_pastures <- ggplot() + 
  gg(data = past, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Pastures", x = "", y = "", fill = "Mean") +
  NULL

shru <- lp4$shrub[!inside,]

lp4_shrub <- ggplot() + 
  gg(data = shru, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Transitional woodland\n and shrub", x = "", y = "", fill = "Mean") +
  NULL

bog <- lp4$peatbogsandMoors[!inside,]

lp4_peatbog <- ggplot() + 
  gg(data = bog, aes(fill = mean), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  #  scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Peatbogs, moors,\n and heathland", x = "", y = "", fill = "Mean") +
  NULL

# put all the dataframes in 1 and use facet_wrap so all colours are in the same scale
multiplot(lp4_elev,
          lp4_slope,
          lp4_grassland, 
          lp4_pastures,
          lp4_peatbog, 
          lp4_shrub,
          lp4_crops, 
          cols = 4)


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

elev.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(elevation = seq(min(elevation[], na.rm = T), 
                                    max(elevation[], na.rm = T), 
                                    length.out = 1000)),
  formula = ~ Eff.elevation_eval(elevation), 
  exclude = c(
    # "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub", 
    # "Eff.moors",
    "Eff.peatbogsandMoors",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  )) 

eval.elev <- ggplot(elev.pred) +
  geom_line(aes(elevation, mean)) +
  geom_ribbon(aes(elevation,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  geom_rug(data = elevation_scaled, aes(x = elevation), inherit.aes = F) + 
  scale_x_continuous(breaks = seq(min(elevation[], na.rm = T), 
                                  max(elevation[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$elevation[], na.rm = T), 
                                        max(env_vars$elevation[], na.rm = T), 
                                        length.out = 10), 0)) + 
  labs(x = "Elevation", y = "Effect") + 
  theme_bw() 

#### Pastures ####

pasture_scaled <- extract(pastures, sett_subset)

pasture.pred <- predict(
  m4, 
  n.samples = 1000,
  newdata = data.frame(pastures = seq(min(pastures[], na.rm = T), 
                                   max(pastures[], na.rm = T), 
                                   length.out = 1000)),
  formula = ~ Eff.pasture_eval(pastures), 
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    # "Eff.pasture",
    "Eff.shrub", 
    "Eff.moors",
    "Eff.peatbogsandMoors",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  )) 

eval.pasture <- ggplot(pasture.pred) +
  geom_line(aes(pastures, mean)) +
  geom_ribbon(aes(pastures,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = pasture_scaled, aes(x = Pastures)) + 
  scale_x_continuous(breaks = seq(min(pastures[], na.rm = T), 
                                  max(pastures[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Pastures[], na.rm = T), 
                                             max(env_vars$Pastures[], na.rm = T), 
                                             length.out = 10)), 0)) + 
  labs(x = "Percentage of pastures", y = "Effect") + 
  theme_bw() 

#### Peatbogs and moors ####

peatbog_moors_scaled <- extract(peatbogsandMoors, sett_subset)

peatbogsandmoors.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(peatbogsandMoors = 
                         seq(min(peatbogsandMoors[], na.rm = T),
                             max(peatbogsandMoors[], na.rm = T),
                             length.out = 1000)),
  formula = ~ Eff.peatbogsandMoors_eval(peatbogsandMoors),
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub",
    # "Eff.moors",
    # "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial",
    "Eff.smooth"
  )) 

eval.peatbogsmoors <- ggplot(peatbogsandmoors.pred) +
  geom_line(aes(peatbogsandMoors, mean)) +
  geom_ribbon(aes(peatbogsandMoors,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = peatbog_moors_scaled, aes(x = PeatbogsandMoors)) + 
  scale_x_continuous(breaks = seq(min(peatbogsandMoors[], na.rm = T),
                                  max(peatbogsandMoors[], na.rm = T),
                                  length.out = 10),
                     labels = round(100*(seq(min(env_vars$PeatbogsandMoors[], na.rm = T),
                                             max(env_vars$PeatbogsandMoors[], na.rm = T),
                                             length.out = 10)), 0)) +
  labs(x = "Percentage of peatbogs and moors", y = "Effect") +
  theme_bw()

#### Grasslands ####

grassland_scaled <- extract(grassland, sett_subset)

grassland.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(grassland = seq(min(grassland[], na.rm = T),
                                    max(grassland[], na.rm = T),
                                    length.out = 1000)),
  formula = ~ Eff.grasslands_eval(grassland),
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub", 
    # "Eff.moors",
    "Eff.peatbogsandMoors",
    # "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  )) 

eval.grasslands <- ggplot(grassland.pred) +
  geom_line(aes(grassland, mean)) +
  geom_ribbon(aes(grassland,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  geom_rug(data = grassland_scaled, aes(x = Naturalgrasslands)) + 
  scale_x_continuous(breaks = seq(min(grassland[], na.rm = T), 
                                  max(grassland[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Naturalgrasslands[], na.rm = T), 
                                             max(env_vars$Naturalgrasslands[], na.rm = T), 
                                             length.out = 10)), 0)) + 
  labs(x = "Percentage of grasslands", y = "Effect") + 
  theme_bw() 

#### Artificial surfaces ####

# artificial_scaled <- extract(artificial, sett_subset)
# 
# artificial.pred <- predict(
#   m4,
#   n.samples = 1000,
#   newdata = data.frame(artificial = seq(min(artificial[], na.rm = T), 
#                                      max(artificial[], na.rm = T), 
#                                      length.out = 1000)),
#   formula = ~ Eff.artificial_eval(artificial), 
#   exclude = c(
#     "Eff.elevation",
#     "Eff.eastness",
#     "Eff.northness",
#     "Eff.slope",
#     "Eff.crops",
#     "Eff.tcd",
#     "Eff.swf",
#     "Eff.pasture",
#     "Eff.shrub", 
#     # "Eff.moors",
#     "Eff.peatbogsandMoors",
#     "Eff.grasslands",
#     # "Eff.artificial", 
#     "Eff.smooth"
#   )) 
# 
# eval.artificial <- ggplot(artificial.pred) +
#   geom_line(aes(artificial, mean)) +
#   geom_ribbon(aes(artificial,
#                   ymin = mean - 1 * sd,
#                   ymax = mean + 1 * sd),
#               alpha = 0.2) + 
#   geom_rug(data = artificial_scaled, aes(x = Artificialsurfaces)) + 
#   scale_x_continuous(breaks = seq(min(env_vars_scaled$Artificialsurfaces[], na.rm = T), 
#                                   max(env_vars_scaled$Artificialsurfaces[], na.rm = T), 
#                                   length.out = 10), 
#                      labels = round(100*(seq(min(env_vars$Artificialsurfaces[], na.rm = T), 
#                                              max(env_vars$Artificialsurfaces[], na.rm = T), 
#                                              length.out = 10)), 0)) + 
#     labs(x = "Percentage of artificial surfaces", y = "Effect") + 
# theme_bw() 

#### Crops ####

crops_scaled <- extract(crops, sett_subset)

crops.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(crops = seq(min(crops[], na.rm = T), 
                                max(crops[], na.rm = T), 
                                length.out = 1000)),
  formula = ~ Eff.crops_eval(crops), 
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    # "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub", 
    # "Eff.moors",
    "Eff.peatbogsandMoors",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

eval.crops <- ggplot(crops.pred) +
  geom_line(aes(crops, mean)) +
  labs(x = "Percentage of crops", y = "Effect") + 
  geom_ribbon(aes(crops,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  geom_rug(data = crops_scaled, aes(x = Plantedvegetationandcrops)) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Plantedvegetationandcrops[], na.rm = T), 
                                  max(env_vars_scaled$Plantedvegetationandcrops[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Plantedvegetationandcrops[], na.rm = T), 
                                             max(env_vars$Plantedvegetationandcrops[], na.rm = T), 
                                             length.out = 10)), 0)) +
  theme_bw() 

#### Small woody features ####

# swf_scaled <- extract(swf, sett_subset)
# 
# swf.pred <- predict(
#   m4,
#   n.samples = 1000,
#   newdata = data.frame(swf = seq(min(swf[], na.rm = T), 
#                               max(swf[], na.rm = T), 
#                               length.out = 1000)),
#   formula = ~ Eff.swf_eval(swf), 
#   exclude = c(
#     "Eff.elevation",
#     "Eff.eastness",
#     "Eff.northness",
#     "Eff.slope",
#     "Eff.crops",
#     "Eff.tcd",
#     # "Eff.swf",
#     "Eff.pasture",
#     "Eff.shrub", 
#     "Eff.peatbogsandMoors",
#     "Eff.grasslands",
#     "Eff.artificial", 
#     "Eff.smooth"
#   )) 
# 
# eval.swf <- ggplot(swf.pred) +
#   geom_line(aes(swf, mean)) +
#   # geom_ribbon(aes(swf,
#   #                 ymin = q0.025,
#   #                 ymax = q0.975),
#   #             alpha = 0.2) +
#   labs(x = "Percentage of small woody features", y = "Effect") + 
#   geom_ribbon(aes(swf,
#                   ymin = mean - 1 * sd,
#                   ymax = mean + 1 * sd),
#               alpha = 0.2) + 
#   geom_rug(data = swf_scaled, aes(x = small_woody_features)) + 
#   scale_x_continuous(breaks = seq(min(env_vars_scaled$small_woody_features[], na.rm = T), 
#                                   max(env_vars_scaled$small_woody_features[], na.rm = T), 
#                                   length.out = 10), 
#                      labels = round(seq(min(env_vars$small_woody_features[], na.rm = T), 
#                                         max(env_vars$small_woody_features[], na.rm = T), 
#                                         length.out = 10), 0)) + 
#   theme_bw() 

#### Tree Cover Density ####

# tcd_scaled <- extract(tcd, sett_subset)
# 
# tcd.pred <- predict(
#   m4,
#   n.samples = 1000,
#   newdata = data.frame(tcd = seq(min(tcd[], na.rm = T),
#                               max(tcd[], na.rm = T),
#                               length.out = 1000)),
#   formula = ~ Eff.tcd_eval(tcd),
#   exclude = c(
#     "Eff.elevation",
#     "Eff.eastness",
#     "Eff.northness",
#     "Eff.slope",
#     "Eff.crops",
#     # "Eff.tcd",
#     "Eff.swf",
#     "Eff.pasture",
#     "Eff.shrub",
#     # "Eff.moors",
#     "Eff.peatbogsandMoors",
#     "Eff.grasslands",
#     "Eff.artificial",
#     "Eff.smooth"
#   )) 
# 
# eval.tcd <- ggplot(tcd.pred) +
#   geom_line(aes(tcd, mean)) +
#   geom_ribbon(aes(tcd,
#                   ymin = mean - 1 * sd,
#                   ymax = mean + 1 * sd),
#               alpha = 0.2) +
#   geom_rug(data = tcd_scaled, aes(x = tree_cover_density)) +
#   scale_x_continuous(breaks = seq(min(env_vars_scaled$tree_cover_density[], na.rm = T), 
#                                   max(env_vars_scaled$tree_cover_density[], na.rm = T), 
#                                   length.out = 10), 
#                      labels = round(seq(min(env_vars$tree_cover_density[], na.rm = T), 
#                                         max(env_vars$tree_cover_density[], na.rm = T), 
#                                         length.out = 10), 0)) + 
#   labs(x = "Tree cover density", y = "Effect") + 
#   theme_bw() 

#### Northness ####

# northness_scaled <- extract(northness, sett_subset)
# 
# northness.pred <- predict(
#   m4,
#   n.samples = 1000,
#   newdata = data.frame(northness = seq(min(northness[], na.rm = T),
#                                     max(northness[], na.rm = T),
#                                     length.out = 1000)),
#   formula = ~ Eff.northness_eval(northness),
#   exclude = c(
#     "Eff.elevation",
#     "Eff.eastness",
#     # "Eff.northness",
#     "Eff.slope",
#     "Eff.crops",
#     "Eff.tcd",
#     "Eff.swf",
#     "Eff.pasture",
#     "Eff.shrub",
#     "Eff.peatbogsandMoors",
#     "Eff.grasslands",
#     "Eff.artificial",
#     "Eff.smooth"
#   )) 
# 
# eval.northness <- ggplot(northness.pred) +
#   geom_line(aes(northness, mean)) +
#   geom_ribbon(aes(northness,
#                   ymin = mean - 1 * sd,
#                   ymax = mean + 1 * sd),
#               alpha = 0.2) +
#   geom_rug(data = northness_scaled, aes(x = northness)) + 
#   scale_x_continuous(breaks = seq(min(northness[], na.rm = T),
#                                   max(northness[], na.rm = T),
#                                   length.out = 10),
#                      labels = round(seq(min(northness[], na.rm = T),
#                                         max(northness[], na.rm = T),
#                                         length.out = 10), 2)) +
#   labs(x = "Northness", y = "Effect") +
#   theme_bw()

#### Slope ####

slope_scaled <- extract(slope, sett_subset)

slope.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(slope = seq(min(slope[], na.rm = T), 
                                max(slope[], na.rm = T), 
                                length.out = 1000)),
  formula = ~ Eff.slope_eval(slope), 
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    # "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub", 
    # "Eff.moors",
    "Eff.peatbogsandMoors",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  )) 

eval.slope <- ggplot(slope.pred) +
  geom_line(aes(slope, mean)) +
  geom_ribbon(aes(slope,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = slope_scaled, aes(x = slope)) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$slope[], na.rm = T), 
                                  max(env_vars_scaled$slope[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$slope[], na.rm = T), 
                                        max(env_vars$slope[], na.rm = T), 
                                        length.out = 10)*100, 0)) + 
  labs(x = "Slope", y = "Effect") +
  theme_bw() 

#### Shrub ####

shrub_scaled <- extract(shrub, sett_subset)

shrub.pred <- predict(
  m4,
  n.samples = 1000,
  newdata = data.frame(shrub = seq(min(shrub[], na.rm = T),
                                max(shrub[], na.rm = T),
                                length.out = 1000)),
  formula = ~ Eff.shrub_eval(shrub),
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    # "Eff.shrub",
    # "Eff.moors",
    "Eff.peatbogsandMoors",
    "Eff.grasslands",
    "Eff.artificial",
    "Eff.smooth"
  )) 

eval.shrub <- ggplot(shrub.pred) +
  geom_line(aes(shrub, mean)) +
  geom_ribbon(aes(shrub,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  geom_rug(data = shrub_scaled, aes(x = Transitionalwoodland.shrub)) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Transitionalwoodland.shrub[], na.rm = T),
                                  max(env_vars_scaled$Transitionalwoodland.shrub[], na.rm = T),
                                  length.out = 10),
                     labels = round(seq(min(env_vars_scaled$Transitionalwoodland.shrub[], na.rm = T),
                                        max(env_vars_scaled$Transitionalwoodland.shrub[], na.rm = T),
                                        length.out = 10), 0)) +
  labs(x = "Percentage of shrubs", y = "Effect") +
  theme_bw()

#### Eastness ####

# eastness_scaled <- extract(eastness, sett_subset)
# 
# eastness.pred <- predict(
#   m4,
#   n.samples = 1000,
#   newdata = data.frame(eastness = seq(min(eastness[], na.rm = T),
#                                       max(eastness[], na.rm = T),
#                                       length.out = 1000)),
#   formula = ~ Eff.eastness_eval(eastness),
#   exclude = c(
#     "Eff.elevation",
#     # "Eff.eastness",
#     "Eff.northness",
#     "Eff.slope",
#     "Eff.crops",
#     "Eff.tcd",
#     "Eff.swf",
#     "Eff.pasture",
#     "Eff.shrub",
#     "Eff.peatbogsandMoors",
#     "Eff.grasslands",
#     "Eff.artificial",
#     "Eff.smooth"
#   ))
# 
# eval.eastness <- ggplot(eastness.pred) +
#   geom_line(aes(eastness, mean)) +
#   geom_ribbon(aes(eastness,
#                   ymin = mean - 1 * sd,
#                   ymax = mean + 1 * sd),
#               alpha = 0.2) +
#   geom_rug(data = eastness.pred, aes(x = eastness)) + 
#   scale_x_continuous(breaks = seq(min(eastness[], na.rm = T),
#                                   max(eastness[], na.rm = T),
#                                   length.out = 10),
#                      labels = round(seq(min(eastness[], na.rm = T),
#                                         max(eastness[], na.rm = T),
#                                         length.out = 10), 2)) +
#   labs(x = "Eastness", y = "Effect") +
#   theme_bw()


### plot ####

multiplot(
  eval.elev,
  eval.slope,
  # eval.eastness,
  # eval.northness,
  # eval.tcd,
  # eval.swf,
  # eval.artificial, 
  eval.grasslands,
  eval.pasture,
  eval.crops,
  eval.peatbogsmoors,
  eval.shrub,
  cols = 3
)

beepr::beep(sound = 3)
# ggsave("setts_covars.png")


