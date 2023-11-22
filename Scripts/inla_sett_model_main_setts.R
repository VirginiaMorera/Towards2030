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

sett_subset <- sett_all%>% 
  filter(DATE_OF_FIELD_VISIT >= 2015) %>% 
  filter(MAIN_SETT == "Yes")

setts <- as_Spatial(sett_subset)

env_vars <- stack("Data/all_covars_1km.grd")
env_vars_scaled <- raster::scale(env_vars)

# corine <- stack("Data/Covars/corine_5km.grd")
# copernicus <- stack("Data/Covars/env_vars_5km.grd")
# 
# env_vars <- stack(corine, copernicus)
# env_vars_scaled <- scale(env_vars)

plot(env_vars_scaled)

## load mesh boundaries and samplers ####
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
ireland_outline <- as_Spatial(ireland_outline_sf)

### three meshes so we can choose which one ####
# mesh1 <- readRDS("Data/Inla/mesh.RDS")
mesh2 <- readRDS("Data/Inla/small_mesh.RDS")
# mesh3 <- readRDS("Data/Inla/Dambly_mesh3.RDS")

inner_boundary <- readRDS("Data/Inla/inner_boundary.RDS")

samplers <- readRDS("Data/Inla/samplers.RDS")
samplers %<>% st_transform(crs = projKM)
samplers <- as_Spatial(samplers)

## create different integration points dfs ####
df <- pixels(mesh2, mask = inner_boundary, nx = 72*10, ny = 81*10)
# df <- pixels(mesh2, mask = inner_boundary)
ipoints <- ipoints(inner_boundary, mesh2)

## make sure all projections match because inla is a lil bitch ####
setts@proj4string <- mesh2$crs
inner_boundary@proj4string <- mesh2$crs
samplers@proj4string <- mesh2$crs
ireland_outline@proj4string <- mesh2$crs
ipoints@proj4string <- mesh2$crs


## plot all data ####
ggplot() + 
  gg(ireland_outline) + 
  gg(samplers, fill = "green", alpha = 0.5) + 
  gg(setts, alpha = 0.5) + 
  coord_equal() + 
  theme_bw() + 
  labs(x = "", y = "")


# Prepare covars ####

elevation <- as(env_vars_scaled$elevation, "SpatialPixelsDataFrame")
elevation@proj4string <- mesh2$crs

slope <- as(env_vars_scaled$slope, "SpatialPixelsDataFrame")
slope@proj4string <- mesh2$crs

northness <- cos(raster::terrain(env_vars$elevation, opt = "aspect", units = "rad"))
northness <- as(northness, "SpatialPixelsDataFrame")
northness@proj4string <- mesh2$crs

eastness <- sin(raster::terrain(env_vars$elevation, opt = "aspect", units = "rad"))
eastness <- as(eastness, "SpatialPixelsDataFrame")
eastness@proj4string <- mesh2$crs

hfi <- as(env_vars_scaled$human_footprint_index, "SpatialPixelsDataFrame")
hfi@proj4string <- mesh2$crs

# forest_distances <- as(env_vars_scaled$forest_distances, "SpatialPixelsDataFrame")
# forest_distances@proj4string <- mesh2$crs

tcd <- as(env_vars_scaled$tree_cover_density, "SpatialPixelsDataFrame")
tcd@proj4string <- mesh2$crs

swf <- as(env_vars_scaled$small_woody_features, "SpatialPixelsDataFrame")
swf@proj4string <- mesh2$crs

artificial <- as(env_vars_scaled$Artificialsurfaces, "SpatialPixelsDataFrame")
artificial@proj4string <- mesh2$crs

crops <- as(env_vars_scaled$Plantedvegetationandcrops, "SpatialPixelsDataFrame")
crops@proj4string <- mesh2$crs

pastures <- as(env_vars_scaled$Pastures, "SpatialPixelsDataFrame")
pastures@proj4string <- mesh2$crs

broadleaf <- as(env_vars_scaled$Broad.leavedforest, "SpatialPixelsDataFrame")
broadleaf@proj4string <- mesh2$crs

coniferous <- as(env_vars_scaled$Coniferousforest, "SpatialPixelsDataFrame")
coniferous@proj4string <- mesh2$crs

mixed <- as(env_vars_scaled$Mixedforest, "SpatialPixelsDataFrame")
mixed@proj4string <- mesh2$crs

grassland <- as(env_vars_scaled$Naturalgrasslands, "SpatialPixelsDataFrame")
grassland@proj4string <- mesh2$crs

moors <- as(env_vars_scaled$Moorsandheathland, "SpatialPixelsDataFrame")
moors@proj4string <- mesh2$crs

shrub <- as(env_vars_scaled$Transitionalwoodland.shrub, "SpatialPixelsDataFrame")
shrub@proj4string <- mesh2$crs

open <- as(env_vars_scaled$Openspaceswithlittleornovegetation, "SpatialPixelsDataFrame")
open@proj4string <- mesh2$crs

water <- as(env_vars_scaled$Inlandwaters, "SpatialPixelsDataFrame")
water@proj4string <- mesh2$crs

peatbogs <- as(env_vars_scaled$Peatbogs, "SpatialPixelsDataFrame")
peatbogs@proj4string <- mesh2$crs


## Prepare 1D meshes for covars ####

### 1d mesh for elevation ####

# mesh1D_elev <- inla.mesh.1d(seq(min(elevation$elevation)-1, max(elevation$elevation)+1, length.out = 10), 
#                             degree = 2) 
# 
# matern1D_elev <- inla.spde2.pcmatern(mesh1D_elev,
#                                      prior.range = c(2.37, 0.1), # 1 third range mesh
#                                      prior.sigma = c(0.001, 0.1))

### 1d mesh for slope ####

mesh1D_slope <- inla.mesh.1d(seq(min(slope$slope)-1, max(slope$slope)+1, 
                                 length.out = 10), 
                             degree = 2) 

matern1D_slope <- inla.spde2.pcmatern(mesh1D_slope,
                                      prior.range = c(3.3, 0.01), # 1 third range mesh
                                      prior.sigma = c(0.1, 0.01))

### 1d mesh for aspect ####

# mesh1D_aspect <- inla.mesh.1d(seq(min(aspect$layer)-1, 
#                                   max(aspect$layer)+1, 
#                                   length.out = 10), 
#                               degree = 2) 
# 
# matern1D_aspect <- inla.spde2.pcmatern(mesh1D_aspect,
#                                        prior.range = c(0.1, 0.01), # 1 third range mesh
#                                        prior.sigma = c(0.001, 0.01))

### 1d mesh for northness ####

# mesh1D_northness <- inla.mesh.1d(seq(min(northness$layer)-1, 
#                                   max(northness$layer)+1, 
#                                   length.out = 10), 
#                               degree = 2) 
# 
# matern1D_northness <- inla.spde2.pcmatern(mesh1D_northness,
#                                        prior.range = c(0.6, 0.01), # 1 third range mesh
#                                        prior.sigma = c(0.01, 0.01))

### 1d mesh for eastness ####

mesh1D_eastness <- inla.mesh.1d(seq(min(eastness$layer)-1, 
                                    max(eastness$layer)+1, 
                                    length.out = 10), 
                                degree = 2) 

matern1D_eastness <- inla.spde2.pcmatern(mesh1D_eastness,
                                         prior.range = c(0.66, 0.1), # 1 third range mesh
                                         prior.sigma = c(0.01, 0.1))

### 1d mesh for crops ####

mesh1D_crops <- inla.mesh.1d(seq(min(crops$Plantedvegetationandcrops)-1, 
                                 max(crops$Plantedvegetationandcrops)+1, 
                                 length.out = 10), 
                             degree = 2) 

matern1D_crops <- inla.spde2.pcmatern(mesh1D_crops,
                                      prior.range = c(2.25, 0.1), # 1 third range mesh
                                      prior.sigma = c(0.01, 0.1))


### 1d mesh for tree cover density ####

mesh1D_tcd <- inla.mesh.1d(seq(min(tcd$tree_cover_density)-1, 
                               max(tcd$tree_cover_density)+1, 
                               length.out = 10), 
                           degree = 2) 

matern1D_tcd <- inla.spde2.pcmatern(mesh1D_tcd,
                                    prior.range = c(1, 0.01), # 1 third range mesh
                                    prior.sigma = c(0.01, 0.01))

### 1d mesh for small woody features ####

mesh1D_swf <- inla.mesh.1d(seq(min(swf$small_woody_features)-1, 
                               max(swf$small_woody_features)+1, 
                               length.out = 10), 
                           degree = 2) 

matern1D_swf <- inla.spde2.pcmatern(mesh1D_swf,
                                    prior.range = c(5, 0.1), # 1 third range mesh
                                    prior.sigma = c(0.01, 0.1))


### 1d mesh for pastures ####

mesh1D_pastures <- inla.mesh.1d(seq(min(pastures$Pastures)-1, 
                                    max(pastures$Pastures)+1, 
                                    length.out = 10), 
                                degree = 2) 

matern1D_pastures <- inla.spde2.pcmatern(mesh1D_pastures,
                                         prior.range = c(3, 0.01), # force a longer range, and same as grasslands
                                         prior.sigma = c(0.01, 0.01))

### 1d mesh for artificial surfaces ####

mesh1D_artificial <- inla.mesh.1d(seq(min(artificial$Artificialsurfaces)-1, 
                                      max(artificial$Artificialsurfaces)+1, 
                                      length.out = 10), 
                                  degree = 2) 

matern1D_artificial <- inla.spde2.pcmatern(mesh1D_artificial,
                                           prior.range = c(0.5, 0.01), # 1 third range mesh
                                           prior.sigma = c(0.1, 0.01))

### 1d mesh for grasslands ####

mesh1D_grassland <- inla.mesh.1d(seq(min(grassland$Naturalgrasslands)-1, 
                                     max(grassland$Naturalgrasslands)+1, 
                                     length.out = 10), 
                                 degree = 2) 

matern1D_grassland <- inla.spde2.pcmatern(mesh1D_grassland,
                                          prior.range = c(1, 0.1), # force a longer range together with pastures
                                          prior.sigma = c(0.01, 0.1))

### 1d mesh for moors ####

mesh1D_moors <- inla.mesh.1d(seq(min(moors$Moorsandheathland)-1, 
                                 max(moors$Moorsandheathland)+1, 
                                 length.out = 10), 
                             degree = 2) 

matern1D_moors <- inla.spde2.pcmatern(mesh1D_grassland,
                                      prior.range = c(2, 0.01), # 1 third range mesh
                                      prior.sigma = c(0.001, 0.01))

### 1d mesh for shrub ####

mesh1D_shrub <- inla.mesh.1d(seq(min(shrub$Transitionalwoodland.shrub)-1, 
                                 max(shrub$Transitionalwoodland.shrub)+1, 
                                 length.out = 10), 
                             degree = 2) 

matern1D_shrub <- inla.spde2.pcmatern(mesh1D_shrub,
                                      prior.range = c(1, 0.01), # 1 third range mesh
                                      prior.sigma = c(0.001, 0.01))

### 1d mesh for peatbogs ####

mesh1D_peatbogs <- inla.mesh.1d(seq(min(peatbogs$Peatbogs)-1, 
                                    max(peatbogs$Peatbogs)+1, 
                                    length.out = 10), 
                                degree = 2) 

matern1D_peatbogs <- inla.spde2.pcmatern(mesh1D_peatbogs,
                                         prior.range = c(1.6, 0.01), # 1 third range mesh
                                         prior.sigma = c(0.01, 0.01))


# M4 non-linear covar effects + spde ####

## Set up spde ####

matern2D <- inla.spde2.pcmatern(mesh2,
                                prior.range = c(90, 0.1),  #1/3 y coordinate
                                prior.sigma = c(0.001, 0.1))


## Formula ####

nonlinear_SPDE <- coordinates ~  Intercept(1)  +
  Eff.elevation(elevation, model = "linear") +
  Eff.slope(slope, model = matern1D_slope) +
  Eff.eastness(eastness, model = "linear") +
  Eff.northness(northness, model = "linear") +
  Eff.tcd(tcd, model = "linear") +
  Eff.swf(swf, model = matern1D_swf) +
  Eff.crops(crops, model = "linear") +
  Eff.pasture(pastures, model = matern1D_pastures) +
  Eff.artificial(artificial, model = matern1D_artificial) +
  Eff.grasslands(grassland, model = "linear") +
  Eff.moors(moors, model = "linear") +
  Eff.shrub(shrub, model = "linear") +
  Eff.peatbog(peatbogs, model = "linear") +
  Eff.smooth(coordinates, model = matern2D) + 
  NULL

## Run model ####
m4 <- lgcp(nonlinear_SPDE,
           setts,
           domain = list(coordinates = mesh2),
           samplers = samplers)

# saveRDS(m4, file = "Outputs/main_sett_model_1km_23/final_main_sett_model.RDS")
m4 <- readRDS("Outputs/main_sett_model_1km/final_main_sett_model.RDS")

summary(m4)

## Predict linear scale ####

### predict ####

lp4 <- predict(m4, df, ~ list(
  elevation = Eff.elevation,
  slope = Eff.slope,
  northness = Eff.northness,
  eastness = Eff.eastness,
  crops = Eff.crops,
  tcd = Eff.tcd,
  swf = Eff.swf,
  pastures = Eff.pasture,
  artificial = Eff.artificial,
  grassland = Eff.grasslands,
  moors = Eff.moors,
  shrub = Eff.shrub,
  peatbogs = Eff.peatbog,
  spfield = Eff.smooth,
  all = Intercept + 
    Eff.elevation +
    Eff.slope +
    Eff.northness +
    Eff.eastness +
    Eff.crops +
    Eff.tcd +
    Eff.swf +
    Eff.pasture +
    Eff.artificial +
    Eff.grasslands +
    Eff.moors +
    Eff.shrub +
    Eff.peatbog +
    Eff.smooth))

# saveRDS(lp4, file = "Outputs/main_sett_model_1km_23/m4_lp.RDS")
lp4 <- readRDS("Outputs/main_sett_model_1km/m4_lp.RDS")

### plot #### 

p.lp4.elev <- ggplot() +
  gg(lp4$elevation, mask = ireland_outline) +
  ggtitle("Elevation") +
  coord_equal() +
  theme_bw() +
  # scale_fill_carto_c(type = "quantitative", palette = "Sunset", direction = -1) + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp4.slope <- ggplot() +
  gg(lp4$slope, mask = ireland_outline) +
  ggtitle("Slope") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp4.northness <- ggplot() +
  gg(lp4$northness, mask = ireland_outline) +
  ggtitle("Northness") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp4.eastness <- ggplot() +
  gg(lp4$eastness, mask = ireland_outline) +
  ggtitle("Eastness") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.crops <- ggplot() +
  gg(lp4$crops, mask = ireland_outline) +
  ggtitle("Planted vegetations and crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.tcd <- ggplot() +
  gg(lp4$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp4.swf <- ggplot() +
  gg(lp4$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.pastures <- ggplot() +
  gg(lp4$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.artificial <- ggplot() +
  gg(lp4$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.grassland <- ggplot() +
  gg(lp4$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp4.moors <- ggplot() +
  gg(lp4$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp4.shrub <- ggplot() +
  gg(lp4$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.peatbogs <- ggplot() +
  gg(lp4$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.smooth <- ggplot() +
  gg(lp4$spfield, mask = ireland_outline) +
  labs(title = "Spatial field", x = "", y = "") + 
  # ggtitle("Spatial field") +
  coord_equal() + 
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_gradientn(colours=brewer.pal(7,"RdBu")) + 
  # theme(legend.position = "bottom") + 
  NULL

p.lp4.all <- ggplot() + 
  gg(lp4$all, mask = ireland_outline) + 
  ggtitle("all covariates effect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

multiplot(
  p.lp4.elev,
  p.lp4.slope, 
  p.lp4.northness,
  p.lp4.eastness,
  p.lp4.crops,
  p.lp4.tcd,
  p.lp4.swf,
  p.lp4.pastures,
  p.lp4.artificial,
  p.lp4.grassland,
  p.lp4.moors,
  p.lp4.shrub,
  p.lp4.peatbogs,
  cols = 4) 

multiplot(p.lp4.smooth, 
          p.lp4.all, cols = 2)

## Predict response scale ####

### predict ####

rp4 <- predict(m4, df, ~ list(
  elevation = exp(Eff.elevation),
  slope = exp(Eff.slope),
  northness = exp(Eff.northness),
  eastness = exp(Eff.eastness),
  crops = exp(Eff.crops),
  tcd = exp(Eff.tcd),
  swf = exp(Eff.swf),
  pastures = exp(Eff.pasture),
  artificial = exp(Eff.artificial),
  grassland = exp(Eff.grasslands),
  moors = exp(Eff.moors),
  shrub = exp(Eff.shrub),
  peatbogs = exp(Eff.peatbog),
  spfield = exp(Eff.smooth),
  all = exp(Intercept + 
              Eff.elevation +
              Eff.slope +
              Eff.northness +
              Eff.eastness + 
              Eff.crops +
              Eff.tcd +
              Eff.swf +
              Eff.pasture + 
              Eff.artificial +
              Eff.grasslands +
              Eff.moors +
              Eff.shrub +
              Eff.peatbog +
              Eff.smooth)))

# saveRDS(rp4, file = "Outputs/main_sett_model_1km_23/m4_rp.RDS")
rp4 <- readRDS("Outputs/main_sett_model_1km_23/m4_rp.RDS")

### plot #### 

p.rp4.elev <- ggplot() +
  gg(rp4$elevation, mask = ireland_outline) +
  ggtitle("Elevation") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp4.slope <- ggplot() +
  gg(rp4$slope, mask = ireland_outline) +
  ggtitle("Slope") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp4.northness <- ggplot() +
  gg(rp4$northness, mask = ireland_outline) +
  ggtitle("Northness") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp4.eastness <- ggplot() +
  gg(rp4$eastness, mask = ireland_outline) +
  ggtitle("Eastness") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp4.crops <- ggplot() +
  gg(rp4$crops, mask = ireland_outline) +
  ggtitle("crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp4.tcd <- ggplot() +
  gg(rp4$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp4.swf <- ggplot() +
  gg(rp4$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp4.pastures <- ggplot() +
  gg(rp4$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp4.artificial <- ggplot() +
  gg(rp4$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp4.grassland <- ggplot() +
  gg(rp4$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp4.moors <- ggplot() +
  gg(rp4$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp4.shrub <- ggplot() +
  gg(rp4$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp4.peatbogs <- ggplot() +
  gg(rp4$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp4.smooth <- ggplot() +
  gg(rp4$spfield, mask = ireland_outline) +
  ggtitle("Spatial field") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

(p.rp4.all <- ggplot() + 
    gg(rp4$all, mask = ireland_outline) + 
    labs(title = "Predicted sett abundance per square km", x = "", y = "") + 
    coord_equal() + 
    theme_bw() + 
    scale_fill_viridis_c(option = "A") +
    # theme(legend.position = "bottom") + 
    NULL)

multiplot(
  p.rp4.elev,
  p.rp4.slope, 
  # p.rp4.northness,
  p.rp4.eastness,
  p.rp4.crops,
  # p.rp4.tcd,
  p.rp4.swf,
  p.rp4.pastures, 
  p.rp4.artificial,
  p.rp4.grassland,
  # p.rp4.moors,
  p.rp4.shrub,
  p.rp4.peatbogs,
  cols = 4)

multiplot(p.rp4.all, 
          p.lp4.smooth, 
          cols = 2)

pred_respRD <- raster(rp4$all['mean'])

pred_resp <- mask(pred_respRD, ireland_outline)
plot(pred_resp)
writeRaster(pred_resp, 
            filename = "badger_sett_pprediction.grd",
            overwrite = T)


## Predict total abundance ####

(Abun.m4 <- predict(
  m4,
  ipoints,
  ~ sum(weight * exp(Intercept + 
                       Eff.elevation + 
                       Eff.eastness +
                       Eff.northness +
                       Eff.slope +
                       Eff.crops +
                       Eff.tcd +
                       Eff.swf +
                       Eff.shrub + 
                       Eff.moors +
                       Eff.peatbog + 
                       Eff.artificial +
                       Eff.grasslands +
                       Eff.pasture))))

## Spde and cor covariance plots ####

spde.range <- spde.posterior(m4, "Eff.smooth", what = "range")
spde.logvar <- spde.posterior(m4, "Eff.smooth", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)

multiplot(range.plot, var.plot)

corplot <- plot(spde.posterior(m4, "Eff.smooth", what = "matern.correlation"))
covplot <- plot(spde.posterior(m4, "Eff.smooth", what = "matern.covariance"))

multiplot(covplot, corplot)

## Evaluate effects ####

### predict ####

elev.pred <- predict(
  m4,
  data = data.frame(elevation = seq(min(elevation$elevation), 
                                    max(elevation$elevation), 
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
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

pasture.pred <- predict(
  m4,
  data = data.frame(pastures = seq(min(pastures$Pastures), 
                                   max(pastures$Pastures), 
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
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))


peatbogs.pred <- predict(
  m4,
  data = data.frame(peatbogs = seq(min(peatbogs$Peatbogs), 
                                   max(peatbogs$Peatbogs), 
                                   length.out = 1000)),
  formula = ~ Eff.peatbog_eval(peatbogs), 
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
    "Eff.moors",
    # "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

moors.pred <- predict(
  m4,
  data = data.frame(moors = seq(min(moors$Moorsandheathland),
                                max(moors$Moorsandheathland),
                                length.out = 1000)),
  formula = ~ Eff.moors_eval(moors),
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
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial",
    "Eff.smooth"
  ))

grassland.pred <- predict(
  m4,
  data = data.frame(grassland = seq(min(grassland$Naturalgrasslands),
                                    max(grassland$Naturalgrasslands),
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
    "Eff.moors",
    "Eff.peatbog",
    # "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

artificial.pred <- predict(
  m4,
  data = data.frame(artificial = seq(min(artificial$Artificialsurfaces), 
                                     max(artificial$Artificialsurfaces), 
                                     length.out = 1000)),
  formula = ~ Eff.artificial_eval(artificial), 
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
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    # "Eff.artificial", 
    "Eff.smooth"
  ))

crops.pred <- predict(
  m4,
  data = data.frame(crops = seq(min(crops$Plantedvegetationandcrops), 
                                max(crops$Plantedvegetationandcrops), 
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
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

swf.pred <- predict(
  m4,
  data = data.frame(swf = seq(min(swf$small_woody_features), 
                              max(swf$small_woody_features), 
                              length.out = 1000)),
  formula = ~ Eff.swf_eval(swf), 
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    # "Eff.swf",
    "Eff.pasture",
    "Eff.shrub", 
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

tcd.pred <- predict(
  m4,
  data = data.frame(tcd = seq(min(tcd$tree_cover_density),
                              max(tcd$tree_cover_density),
                              length.out = 1000)),
  formula = ~ Eff.tcd_eval(tcd),
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    # "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub",
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial",
    "Eff.smooth"
  ))

northness.pred <- predict(
  m4,
  data = data.frame(northness = seq(min(northness$layer),
                                    max(northness$layer),
                                    length.out = 1000)),
  formula = ~ Eff.northness_eval(northness),
  exclude = c(
    "Eff.elevation",
    "Eff.eastness",
    # "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub",
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial",
    "Eff.smooth"
  ))

eastness.pred <- predict(
  m4,
  data = data.frame(eastness = seq(min(eastness$layer), 
                                   max(eastness$layer), 
                                   length.out = 1000)),
  formula = ~ Eff.eastness_eval(eastness), 
  exclude = c(
    "Eff.elevation",
    # "Eff.eastness",
    "Eff.northness",
    "Eff.slope",
    "Eff.crops",
    "Eff.tcd",
    "Eff.swf",
    "Eff.pasture",
    "Eff.shrub", 
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

slope.pred <- predict(
  m4,
  data = data.frame(slope = seq(min(slope$slope), 
                                max(slope$slope), 
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
    "Eff.moors",
    "Eff.peatbog",
    "Eff.grasslands",
    "Eff.artificial", 
    "Eff.smooth"
  ))

### plot ####

eval.elev <- ggplot(elev.pred) +
  geom_line(aes(elevation, mean)) +
  geom_ribbon(aes(elevation,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Elevation", y = "Effect") + 
  geom_ribbon(aes(elevation,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$elevation[], na.rm = T), 
                                  max(env_vars_scaled$elevation[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$elevation[], na.rm = T), 
                                        max(env_vars$elevation[], na.rm = T), 
                                        length.out = 10), 0))

eval.pasture <- ggplot(pasture.pred) +
  geom_line(aes(pastures, mean)) +
  geom_ribbon(aes(pastures,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Percentage of pastures", y = "Effect") + 
  geom_ribbon(aes(pastures,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Pastures[], na.rm = T), 
                                  max(env_vars_scaled$Pastures[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Pastures[], na.rm = T), 
                                             max(env_vars$Pastures[], na.rm = T), 
                                             length.out = 10)), 0))

eval.peatbogs <- ggplot(peatbogs.pred) +
  geom_line(aes(peatbogs, mean)) +
  geom_ribbon(aes(peatbogs,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Percentage of peatbogs", y = "Effect") + 
  geom_ribbon(aes(peatbogs,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Peatbogs[], na.rm = T), 
                                  max(env_vars_scaled$Peatbogs[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Peatbogs[], na.rm = T), 
                                             max(env_vars$Peatbogs[], na.rm = T), 
                                             length.out = 10)), 0))


eval.moors <- ggplot(moors.pred) +
  geom_line(aes(moors, mean)) +
  geom_ribbon(aes(moors,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Percentage of moors", y = "Effect") + 
  geom_ribbon(aes(moors,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Moorsandheathland[], na.rm = T), 
                                  max(env_vars_scaled$Moorsandheathland[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Moorsandheathland[], na.rm = T), 
                                             max(env_vars$Moorsandheathland[], na.rm = T), 
                                             length.out = 10)), 0))


eval.grasslands <- ggplot(grassland.pred) +
  geom_line(aes(grassland, mean)) +
  geom_ribbon(aes(grassland,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Percentage of grasslands", y = "Effect") + 
  geom_ribbon(aes(grassland,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Naturalgrasslands[], na.rm = T), 
                                  max(env_vars_scaled$Naturalgrasslands[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Naturalgrasslands[], na.rm = T), 
                                             max(env_vars$Naturalgrasslands[], na.rm = T), 
                                             length.out = 10)), 0))


eval.artificial <- ggplot(artificial.pred) +
  geom_line(aes(artificial, mean)) +
  geom_ribbon(aes(artificial,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Percentage of artificial surfaces", y = "Effect") + 
  geom_ribbon(aes(artificial,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Artificialsurfaces[], na.rm = T), 
                                  max(env_vars_scaled$Artificialsurfaces[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Artificialsurfaces[], na.rm = T), 
                                             max(env_vars$Artificialsurfaces[], na.rm = T), 
                                             length.out = 10)), 0))


eval.crops <- ggplot(crops.pred) +
  geom_line(aes(crops, mean)) +
  geom_ribbon(aes(crops,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Percentage of crops", y = "Effect") + 
  geom_ribbon(aes(crops,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$Plantedvegetationandcrops[], na.rm = T), 
                                  max(env_vars_scaled$Plantedvegetationandcrops[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(100*(seq(min(env_vars$Plantedvegetationandcrops[], na.rm = T), 
                                             max(env_vars$Plantedvegetationandcrops[], na.rm = T), 
                                             length.out = 10)), 0))


eval.swf <- ggplot(swf.pred) +
  geom_line(aes(swf, mean)) +
  geom_ribbon(aes(swf,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Percentage of small woody features", y = "Effect") + 
  geom_ribbon(aes(swf,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$small_woody_features[], na.rm = T), 
                                  max(env_vars_scaled$small_woody_features[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$small_woody_features[], na.rm = T), 
                                        max(env_vars$small_woody_features[], na.rm = T), 
                                        length.out = 10), 0))


eval.tcd <- ggplot(tcd.pred) +
  geom_line(aes(tcd, mean)) +
  geom_ribbon(aes(tcd,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Tree cover density", y = "Effect") + 
  geom_ribbon(aes(tcd,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) +
  scale_x_continuous(breaks = seq(min(env_vars_scaled$tree_cover_density[], na.rm = T), 
                                  max(env_vars_scaled$tree_cover_density[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$tree_cover_density[], na.rm = T), 
                                        max(env_vars$tree_cover_density[], na.rm = T), 
                                        length.out = 10), 0))

eval.northness <- ggplot(northness.pred) +
  geom_line(aes(northness, mean)) +
  geom_ribbon(aes(northness,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Northness", y = "Effect") + 
  geom_ribbon(aes(northness,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(northness@data, na.rm = T), 
                                  max(northness@data, na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(northness@data, na.rm = T), 
                                        max(northness@data, na.rm = T), 
                                        length.out = 10), 2))

eval.eastness <- ggplot(eastness.pred) +
  geom_line(aes(eastness, mean)) +
  geom_ribbon(aes(eastness,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Eastness", y = "Effect") + 
  geom_ribbon(aes(eastness,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(eastness@data, na.rm = T), 
                                  max(eastness@data, na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(eastness@data, na.rm = T), 
                                        max(eastness@data, na.rm = T), 
                                        length.out = 10), 2))


eval.slope <- ggplot(slope.pred) +
  geom_line(aes(slope, mean)) +
  geom_ribbon(aes(slope,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  labs(x = "Slope", y = "Effect") + 
  geom_ribbon(aes(slope,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2) + 
  scale_x_continuous(breaks = seq(min(env_vars_scaled$slope[], na.rm = T), 
                                  max(env_vars_scaled$slope[], na.rm = T), 
                                  length.out = 10), 
                     labels = round(seq(min(env_vars$slope[], na.rm = T), 
                                        max(env_vars$slope[], na.rm = T), 
                                        length.out = 10), 0))
multiplot(
  eval.elev,
  eval.pasture,
  eval.peatbogs,
  eval.moors,
  eval.grasslands,
  eval.artificial, 
  eval.crops,
  eval.swf,
  eval.tcd,
  eval.eastness,
  eval.northness,
  eval.slope,
  cols = 2
)

#~~~~~~~~~~~~~~~####
# BACKUP MODELS ####
#~~~~~~~~~~~~~~~####

# M1: Very simple linear non-spatial model ####

## Formula ####
linear_nonSPDE <- coordinates ~  Intercept  +
  Eff.elevation(elevation, model = "linear") +
  Eff.aspect(aspect, model = "linear") + 
  # Eff.slope(slope, model = "linear") +
  Eff.crops(crops, model = "linear") +
  Eff.tcd(tcd, model = "linear") +
  Eff.swf(swf, model = "linear") + 
  Eff.pastures(pastures, model = "linear") +
  Eff.artificial(artificial, model = "linear") +
  Eff.grassland(grassland, model = "linear") +
  Eff.moors(moors, model = "linear") +
  Eff.shrub(shrub, model = "linear") +
  # Eff.open(open, model = "linear") +
  # Eff.water(water, model = "linear") +
  Eff.peatbogs(peatbogs, model = "linear") +
  NULL
# land use seems to whack the model out of balance, so we're not using it for now. We can add the continuous layers if necessary

## Run model ####
m1 <- lgcp(linear_nonSPDE,
           setts,
           domain = list(coordinates = mesh1),
           samplers = samplers)

summary(m1)

## Predict linear scale ####

### predict ####

lp1 <- predict(m1, df, ~ list(
  elevation = Eff.elevation,
  # slope = Eff.slope,
  aspect = Eff.aspect,
  crops = Eff.crops,
  # broadlead = Eff.broadleaf, 
  # coniferous = Eff.coniferous,
  # mixed = Eff.mixed,
  tcd = Eff.tcd, 
  swf = Eff.swf, 
  pastures = Eff.pastures,
  artificial = Eff.artificial, 
  grassland = Eff.grassland, 
  moors = Eff.moors, 
  shrub = Eff.shrub, 
  # open = Eff.open, 
  # water = Eff.water, 
  peatbogs = Eff.peatbogs,
  all = Eff.elevation +
    Eff.aspect + 
    Eff.crops + 
    # Eff.broadleaf +
    # Eff.coniferous +
    # Eff.mixed +
    Eff.tcd +
    Eff.swf + 
    Eff.pastures + 
    Eff.artificial + 
    Eff.grassland + 
    Eff.moors + 
    Eff.shrub + 
    # Eff.open +
    # Eff.water + 
    Eff.peatbogs))

### plot #### 

(p.lp1.elev <- ggplot() +
  gg(lp1$elevation, mask = ireland_outline) +
  ggtitle("Elevation") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL)

# p.lp1.slope <- ggplot() +
#   gg(lp1$slope, mask = ireland_outline) +
#   ggtitle("Slope") +
#   coord_equal() + 
#   theme_bw() + 
#   # theme(legend.position = "bottom") + 
#   NULL

(p.lp1.aspect <- ggplot() +
    gg(lp1$aspect, mask = ireland_outline) +
    ggtitle("Aspect") +
    coord_equal() + 
    theme_bw() + 
    scale_fill_viridis_c(option = "A") +
    # theme(legend.position = "bottom") + 
    NULL)

p.lp1.crops <- ggplot() +
  gg(lp1$crops, mask = ireland_outline) +
  ggtitle("crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.tcd <- ggplot() +
  gg(lp1$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.swf <- ggplot() +
  gg(lp1$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.pastures <- ggplot() +
  gg(lp1$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.artificial <- ggplot() +
  gg(lp1$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.grassland <- ggplot() +
  gg(lp1$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.moors <- ggplot() +
  gg(lp1$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.shrub <- ggplot() +
  gg(lp1$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.peatbogs <- ggplot() +
  gg(lp1$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp1.all <- ggplot() + 
  gg(lp1$all, mask = ireland_outline) + 
  ggtitle("all covariates effect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL


multiplot(p.lp1.elev, 
          p.lp1.aspect,
          p.lp1.crops,
          p.lp1.tcd,
          p.lp1.swf,
          p.lp1.pastures, 
          p.lp1.artificial,
          p.lp1.grassland,
          p.lp1.moors,
          p.lp1.shrub,
          p.lp1.peatbogs,
          p.lp1.all, cols = 4)


## Predict in response scale ####

### predict ####

rp1 <- predict(m1, df, ~ list(
  elevation = exp(Eff.elevation),
  # slope = exp(Eff.slope),
  aspect = exp(Eff.aspect),
  crops = exp(Eff.crops),
  tcd = exp(Eff.tcd), 
  swf = exp(Eff.swf),
  pastures = exp(Eff.pastures),
  artificial = exp(Eff.artificial), 
  grassland = exp(Eff.grassland), 
  moors = exp(Eff.moors), 
  shrub = exp(Eff.shrub), 
  peatbogs = exp(Eff.peatbogs),
  all = exp(Eff.elevation +
              Eff.aspect + 
              Eff.crops + 
              Eff.tcd +
              Eff.swf +
              Eff.pastures + 
              Eff.artificial + 
              Eff.grassland + 
              Eff.moors + 
              Eff.shrub + 
              Eff.peatbogs)))

### plot #### 

(p.rp1.elev <- ggplot() +
   gg(rp1$elevation, mask = ireland_outline) +
   ggtitle("Elevation") +
   coord_equal() + 
   theme_bw() + 
   scale_fill_viridis_c(option = "A") +
   # theme(legend.position = "bottom") + 
   NULL)

# p.rp1.slope <- ggplot() +
#   gg(rp1$slope, mask = ireland_outline) +
#   ggtitle("Slope") +
#   coord_equal() + 
#   theme_bw() + 
#   # theme(legend.position = "bottom") + 
#   NULL

p.rp1.aspect <- ggplot() +
  gg(rp1$aspect, mask = ireland_outline) +
  ggtitle("Aspect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.crops <- ggplot() +
  gg(rp1$crops, mask = ireland_outline) +
  ggtitle("crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.tcd <- ggplot() +
  gg(rp1$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.swf <- ggplot() +
  gg(rp1$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.pastures <- ggplot() +
  gg(rp1$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.artificial <- ggplot() +
  gg(rp1$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.grassland <- ggplot() +
  gg(rp1$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.moors <- ggplot() +
  gg(rp1$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.shrub <- ggplot() +
  gg(rp1$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.peatbogs <- ggplot() +
  gg(rp1$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp1.all <- ggplot() + 
  gg(rp1$all, mask = ireland_outline) + 
  ggtitle("all covariates effect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

multiplot(p.rp1.elev, 
          p.rp1.aspect,
          p.rp1.crops,
          p.rp1.tcd,
          p.rp1.swf,
          p.rp1.pastures, 
          p.rp1.artificial,
          p.rp1.grassland,
          p.rp1.moors,
          p.rp1.shrub,
          p.rp1.peatbogs,
          p.rp1.all, cols = 4)


## Predict total abundance ####
Abun.m1 <- predict(
  m1,
  ipoints,
  ~ sum(weight * exp(Eff.elevation +
                       Eff.crops +
                       Eff.aspect + 
                       Eff.tcd +
                       Eff.swf +
                       Eff.pastures + 
                       Eff.artificial + 
                       Eff.grassland + 
                       Eff.moors + 
                       Eff.shrub + 
                       Eff.peatbogs)))

Abun.m1


# M2: Non-linear covar effects but not spatial ####
  

## Formula ####
nonlinear_nonSPDE <- coordinates ~  Intercept  +
  # Eff.elevation(elevation, model = "linear") +
  Eff.slope(slope, model = matern1D_slope) +
  Eff.aspect(aspect, model = "linear") + 
  Eff.tcd(tcd, model = "linear") + 
  Eff.swf(swf, model = "linear") + 
  Eff.crops(crops, model = "linear") + 
  Eff.pasture(pastures, model = matern1D_pastures) + 
  Eff.artificial(artificial, model = "linear") + 
  Eff.grasslands(grassland, model = "linear") + 
  Eff.moors(moors, model = "linear") + 
  Eff.shrub(shrub, model = "linear") + 
  Eff.peatbog(peatbogs, model = "linear") +
  NULL

## Run model ####
m2 <- lgcp(nonlinear_nonSPDE,
           setts,
           domain = list(coordinates = mesh1),
           samplers = samplers)

summary(m2)

## Predict linear scale ####

### predict ####

lp2 <- predict(m2, df, ~ list(
  # elevation = Eff.elevation,
  slope = Eff.slope,
  aspect = Eff.aspect,
  crops = Eff.crops,
  # broadlead = Eff.broadleaf, 
  # coniferous = Eff.coniferous,
  # mixed = Eff.mixed,
  tcd = Eff.tcd, 
  swf = Eff.swf, 
  pastures = Eff.pasture,
  artificial = Eff.artificial, 
  grassland = Eff.grasslands, 
  moors = Eff.moors, 
  shrub = Eff.shrub, 
  # open = Eff.open, 
  # water = Eff.water, 
  peatbogs = Eff.peatbog,
  all = Eff.slope +
    Eff.aspect + 
    Eff.crops + 
    # Eff.broadleaf +
    # Eff.coniferous +
    # Eff.mixed +
    Eff.tcd +
    Eff.swf + 
    Eff.pasture + 
    Eff.artificial + 
    Eff.grasslands + 
    Eff.moors + 
    Eff.shrub + 
    # Eff.open +
    # Eff.water + 
    Eff.peatbog))

### plot #### 

# p.lp2.elev <- ggplot() +
#    gg(lp2$elevation, mask = ireland_outline) +
#    ggtitle("Elevation") +
#    coord_equal() + 
#    theme_bw() + 
#    scale_fill_viridis_c(option = "A") +
#    # theme(legend.position = "bottom") + 
   NULL

p.lp2.slope <- ggplot() +
  gg(lp2$slope, mask = ireland_outline) +
  ggtitle("Slope") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp2.aspect <- ggplot() +
    gg(lp2$aspect, mask = ireland_outline) +
    ggtitle("Aspect") +
    coord_equal() + 
    theme_bw() + 
    scale_fill_viridis_c(option = "A") +
    # theme(legend.position = "bottom") + 
    NULL

p.lp2.crops <- ggplot() +
  gg(lp2$crops, mask = ireland_outline) +
  ggtitle("crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.tcd <- ggplot() +
  gg(lp2$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.swf <- ggplot() +
  gg(lp2$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.pastures <- ggplot() +
  gg(lp2$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.artificial <- ggplot() +
  gg(lp2$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.grassland <- ggplot() +
  gg(lp2$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.moors <- ggplot() +
  gg(lp2$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.shrub <- ggplot() +
  gg(lp2$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.peatbogs <- ggplot() +
  gg(lp2$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp2.all <- ggplot() + 
  gg(lp2$all, mask = ireland_outline) + 
  ggtitle("all covariates effect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL


multiplot(p.lp2.slope, 
          p.lp2.aspect,
          p.lp2.crops,
          p.lp2.tcd,
          p.lp2.swf,
          p.lp2.pastures, 
          p.lp2.artificial,
          p.lp2.grassland,
          p.lp2.moors,
          p.lp2.shrub,
          p.lp2.peatbogs,
          p.lp2.all, cols = 4)


## Predict in response scale ####

### predict ####

rp2 <- predict(m2, df, ~ list(
  # elevation = exp(Eff.elevation),
  slope = exp(Eff.slope),
  aspect = exp(Eff.aspect),
  crops = exp(Eff.crops),
  tcd = exp(Eff.tcd), 
  swf = exp(Eff.swf),
  pastures = exp(Eff.pasture),
  artificial = exp(Eff.artificial), 
  grassland = exp(Eff.grasslands), 
  moors = exp(Eff.moors), 
  shrub = exp(Eff.shrub), 
  peatbogs = exp(Eff.peatbog),
  all = exp(Eff.slope +
              Eff.aspect + 
              Eff.crops + 
              Eff.tcd +
              Eff.swf +
              Eff.pasture + 
              Eff.artificial + 
              # Eff.grasslands + 
              Eff.moors + 
              Eff.shrub + 
              Eff.peatbog)))


### plot ####

# p.rp2.elev <- ggplot() +
#   gg(rp2$elevation, mask = ireland_outline) +
#   ggtitle("Elevation") +
#   coord_equal() + 
#   theme_bw() + 
#   scale_fill_viridis_c(option = "A") +
#   # theme(legend.position = "bottom") + 
#   NULL

p.rp2.slope <- ggplot() +
  gg(rp2$slope, mask = ireland_outline) +
  ggtitle("Slope") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp2.aspect <- ggplot() +
  gg(rp2$aspect, mask = ireland_outline) +
  ggtitle("Aspect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.crops <- ggplot() +
  gg(rp2$crops, mask = ireland_outline) +
  ggtitle("crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.tcd <- ggplot() +
  gg(rp2$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.swf <- ggplot() +
  gg(rp2$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.pastures <- ggplot() +
  gg(rp2$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.artificial <- ggplot() +
  gg(rp2$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.grassland <- ggplot() +
  gg(rp2$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.moors <- ggplot() +
  gg(rp2$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.shrub <- ggplot() +
  gg(rp2$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.peatbogs <- ggplot() +
  gg(rp2$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp2.all <- ggplot() + 
  gg(rp2$all, mask = ireland_outline) + 
  ggtitle("all covariates effect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

multiplot(p.rp2.slope, 
          p.rp2.aspect,
          p.rp2.crops,
          p.rp2.tcd,
          p.rp2.swf,
          p.rp2.pastures, 
          p.rp2.artificial,
          p.rp2.grassland,
          p.rp2.moors,
          p.rp2.shrub,
          p.rp2.peatbogs,
          p.rp2.all, cols = 4)

## Predict total abundance ####

(Abun.m2 <- predict(
  m2,
  ipoints,
  ~ sum(weight * exp(Eff.aspect +
                       # Eff.slope +
                       Eff.crops +
                       Eff.tcd +
                       Eff.swf +
                       # Eff.pasture +
                       Eff.artificial +
                       Eff.grasslands +
                       Eff.moors +
                       Eff.shrub +
                       Eff.peatbog))))


## Evaluate effects ####

### predict ####
pastures.pred <- predict(
  m2,
  data = data.frame(pastures = seq(min(pastures$Pastures), 
                                    max(pastures$Pastures), 
                                    length.out = 1000)),
  formula = ~ Eff.pasture_eval(pastures), 
  exclude = c("Eff.slope", 
              "Eff.aspect", 
              "Eff.crops", 
              "Eff.tcd", 
              "Eff.swf", 
              "Eff.artificial", 
              "Eff.grasslands", 
              "Eff.moors", 
              "Eff.shrub", 
              "Eff.peatbog"))


tcd.pred <- predict(
  m2,
  data = data.frame(tcd = seq(min(tcd$tree_cover_density), 
                              max(tcd$tree_cover_density), 
                              length.out = 1000)),
  formula = ~ Eff.tcd_eval(tcd), 
  exclude = c("Eff.elevation", 
              "Eff.aspect", 
              "Eff.crops", 
              "Eff.grasslands", 
              "Eff.swf", 
              "Eff.artificial", 
              "Eff.pasture", 
              "Eff.moors", 
              "Eff.shrub", 
              "Eff.peatbog"))

slope.pred <- predict(
  m2,
  data = data.frame(slope = seq(min(slope$slope), 
                                max(slope$slope), 
                                length.out = 1000)),
  formula = ~ Eff.slope_eval(slope), 
  exclude = c("Eff.grasslands", 
              "Eff.aspect", 
              "Eff.crops", 
              "Eff.tcd", 
              "Eff.swf", 
              "Eff.artificial", 
              "Eff.pasture", 
              "Eff.moors", 
              "Eff.shrub", 
              "Eff.peatbog"))

### plot ####

(eval.pasture <- ggplot(pastures.pred) +
  geom_line(aes(pastures, mean)) +
  geom_ribbon(aes(pastures,
                  ymin = q0.025,
                  ymax = q0.975),
              alpha = 0.2) +
  geom_ribbon(aes(pastures,
                  ymin = mean - 1 * sd,
                  ymax = mean + 1 * sd),
              alpha = 0.2))

(eval.tcd <- ggplot(tcd.pred) +
    geom_line(aes(tcd, mean)) +
    geom_ribbon(aes(tcd,
                    ymin = q0.025,
                    ymax = q0.975),
                alpha = 0.2) +
    geom_ribbon(aes(tcd,
                    ymin = mean - 1 * sd,
                    ymax = mean + 1 * sd),
                alpha = 0.2))

(eval.slope <- ggplot(slope.pred) +
    geom_line(aes(slope, mean)) +
    geom_ribbon(aes(slope,
                    ymin = q0.025,
                    ymax = q0.975),
                alpha = 0.2) +
    geom_ribbon(aes(slope,
                    ymin = mean - 1 * sd,
                    ymax = mean + 1 * sd),
                alpha = 0.2))

# M3 linear covar effects + spde ####

## Set up spde ####


matern2D <- inla.spde2.pcmatern(mesh2,
                                prior.range = c(90, 0.1),  #1/3 y coordinate
                                prior.sigma = c(0.001, 0.01))


## Formula ####

linear_SPDE <- coordinates ~  Intercept  +
  Eff.elevation(elevation, model = "linear") +
  Eff.slope(slope, model = "linear") +
  Eff.eastness(eastness, model = "linear") +
  Eff.northness(northness, model = "linear") +
  Eff.tcd(tcd, model = "linear") +
  Eff.swf(swf, model = "linear") +
  Eff.crops(crops, model = "linear") +
  Eff.pasture(pastures, model = "linear") +
  Eff.artificial(artificial, model = "linear") +
  Eff.grasslands(grassland, model = "linear") +
  Eff.moors(moors, model = "linear") +
  Eff.shrub(shrub, model = "linear") +
  Eff.peatbog(peatbogs, model = "linear") +
  Eff.smooth(coordinates, model = matern2D) + 
  NULL

## Run model ####
m3 <- lgcp(linear_SPDE,
           setts,
           domain = list(coordinates = mesh2),
           samplers = samplers)

summary(m3)
saveRDS(m3, file = "Outputs/main_sett_model_1km/m3_model.RDS")
m3 <- readRDS("Outputs/main_sett_model_1km/m3_model.RDS")
## Predict linear scale ####

### predict ####

lp3 <- predict(m3, df, ~ list(
  elevation = Eff.elevation, 
  slope = Eff.slope,
  eastness = Eff.eastness,
  northness = Eff.northness,
  tcd = Eff.tcd,
  swf = Eff.swf,
  crops = Eff.crops,
  pastures = Eff.pasture,
  artificial = Eff.artificial,
  grassland = Eff.grasslands,
  moors = Eff.moors,
  shrub = Eff.shrub,
  peatbogs = Eff.peatbog,
  spfield = Eff.smooth,
  all = Intercept + 
    Eff.elevation + 
    Eff.slope +
    Eff.eastness + 
    Eff.northness +
    Eff.tcd +
    Eff.swf +
    Eff.crops +
    Eff.pasture +
    Eff.artificial +
    Eff.grasslands +
    Eff.moors +
    Eff.shrub +
    Eff.peatbog +
    Eff.smooth))

saveRDS(lp3, file = "Outputs/main_sett_model_1km/m3_lp3.RDS")
lp3 <- readRDS("Outputs/main_sett_model_1km/m3_lp3.RDS")

### plot #### 

p.lp3.elev <- ggplot() +
   gg(lp3$elevation, mask = ireland_outline) +
   ggtitle("Elevation") +
   coord_equal() +
   theme_bw() +
   scale_fill_viridis_c(option = "A") +
   # theme(legend.position = "bottom") +
   NULL

p.lp3.slope <- ggplot() +
  gg(lp3$slope, mask = ireland_outline) +
  ggtitle("Slope") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.lp3.aspect <- ggplot() +
  gg(lp3$aspect, mask = ireland_outline) +
  ggtitle("Aspect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.crops <- ggplot() +
  gg(lp3$crops, mask = ireland_outline) +
  ggtitle("crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.tcd <- ggplot() +
  gg(lp3$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.swf <- ggplot() +
  gg(lp3$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.pastures <- ggplot() +
  gg(lp3$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.artificial <- ggplot() +
  gg(lp3$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.grassland <- ggplot() +
  gg(lp3$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.moors <- ggplot() +
  gg(lp3$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.shrub <- ggplot() +
  gg(lp3$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.peatbogs <- ggplot() +
  gg(lp3$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.smooth <- ggplot() +
  gg(lp3$spfield, mask = ireland_outline) +
  ggtitle("Spatial field") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.lp3.all <- ggplot() + 
  gg(lp3$all, mask = ireland_outline) + 
  ggtitle("all covariates effect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL


multiplot(p.lp3.slope, 
          p.lp3.aspect,
          p.lp3.crops,
          p.lp3.tcd,
          p.lp3.swf,
          p.lp3.pastures,
          p.lp3.artificial,
          p.lp3.grassland,
          p.lp3.moors,
          p.lp3.shrub,
          p.lp3.peatbogs,
          cols = 4) 

multiplot(p.lp3.smooth, 
          p.lp3.all, cols = 2)

## Predict response scale ####

### predict ####

rp3 <- predict(m3, df, ~ list(
  elevation = exp(Eff.elevation), 
  slope = exp(Eff.slope),
  eastness = exp(Eff.eastness),
  northness = exp(Eff.northness),
  tcd = exp(Eff.tcd),
  swf = exp(Eff.swf),
  crops = exp(Eff.crops),
  pastures = exp(Eff.pasture),
  artificial = exp(Eff.artificial),
  grassland = exp(Eff.grasslands),
  moors = exp(Eff.moors),
  shrub = exp(Eff.shrub),
  peatbogs = exp(Eff.peatbog),
  spfield = exp(Eff.smooth),
  all = exp(Intercept + 
              Eff.elevation + 
              Eff.slope +
              Eff.eastness + 
              Eff.northness +
              Eff.tcd +
              Eff.swf +
              Eff.crops +
              Eff.pasture +
              Eff.artificial +
              Eff.grasslands +
              Eff.moors +
              Eff.shrub +
              Eff.peatbog +
              Eff.smooth)))

saveRDS(rp3, file = "Outputs/main_sett_model_1km/m3_rp3.RDS")
rp3 <- readRDS("Outputs/main_sett_model_1km/m3_rp3.RDS")

### plot #### 

# (p.lp1.elev <- ggplot() +
#    gg(lp1$elevation, mask = ireland_outline) +
#    ggtitle("Elevation") +
#    coord_equal() + 
#    theme_bw() + 
#    scale_fill_viridis_c(option = "A") +
#    # theme(legend.position = "bottom") + 
#    NULL)

p.rp3.slope <- ggplot() +
  gg(rp3$slope, mask = ireland_outline) +
  ggtitle("Slope") +
  coord_equal() +
  theme_bw() +
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") +
  NULL

p.rp3.aspect <- ggplot() +
    gg(rp3$aspect, mask = ireland_outline) +
    ggtitle("Aspect") +
    coord_equal() + 
    theme_bw() + 
    scale_fill_viridis_c(option = "A") +
    # theme(legend.position = "bottom") + 
    NULL

p.rp3.crops <- ggplot() +
  gg(rp3$crops, mask = ireland_outline) +
  ggtitle("crops") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.tcd <- ggplot() +
  gg(rp3$tcd, mask = ireland_outline) +
  ggtitle("Tree cover density") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.swf <- ggplot() +
  gg(rp3$swf, mask = ireland_outline) +
  ggtitle("Small woody features") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.pastures <- ggplot() +
  gg(rp3$pastures, mask = ireland_outline) +
  ggtitle("Pastures") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.artificial <- ggplot() +
  gg(rp3$artificial, mask = ireland_outline) +
  ggtitle("Artificial surfaces") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.grassland <- ggplot() +
  gg(rp3$grassland, mask = ireland_outline) +
  ggtitle("Grasslands") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.moors <- ggplot() +
  gg(rp3$moors, mask = ireland_outline) +
  ggtitle("Moors and heathland") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.shrub <- ggplot() +
  gg(rp3$shrub, mask = ireland_outline) +
  ggtitle("Transitional forest and shrub") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.peatbogs <- ggplot() +
  gg(rp3$peatbogs, mask = ireland_outline) +
  ggtitle("Peat bogs") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

p.rp3.smooth <- ggplot() +
  gg(rp3$spfield, mask = ireland_outline) +
  ggtitle("Spatial field") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  # theme(legend.position = "bottom") + 
  NULL

(p.rp3.all <- ggplot() + 
  gg(rp3$all, mask = ireland_outline) + 
  ggtitle("all covariates effect") +
  coord_equal() + 
  theme_bw() + 
  scale_fill_viridis_c(option = "A", trans = "log") +
  # theme(legend.position = "bottom") + 
  NULL)


multiplot(p.rp3.slope, 
          p.rp3.aspect,
          p.rp3.crops,
          p.rp3.tcd,
          p.rp3.swf,
          p.rp3.pastures, 
          p.rp3.artificial,
          p.rp3.grassland,
          p.rp3.moors,
          p.rp3.shrub,
          p.rp3.peatbogs,
          cols = 4)

multiplot(p.rp3.smooth, 
          p.rp3.all, 
          cols = 2)

## Predict total abundance ####

(Abun.m3 <- predict(
  m3,
  ipoints,
  ~ sum(weight * exp(Intercept + 
                       Eff.elevation + 
                       Eff.slope +
                       Eff.eastness + 
                       Eff.northness +
                       Eff.tcd +
                       Eff.swf +
                       Eff.crops +
                       Eff.pasture +
                       Eff.artificial +
                       Eff.grasslands +
                       Eff.moors +
                       Eff.shrub +
                       Eff.peatbog +
                       Eff.smooth))))


spde.range <- spde.posterior(m3, "Eff.smooth", what = "range")
spde.logvar <- spde.posterior(m3, "Eff.smooth", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)

multiplot(range.plot, var.plot)
 
corplot <- plot(spde.posterior(m3, "Eff.smooth", what = "matern.correlation"))
covplot <- plot(spde.posterior(m3, "Eff.smooth", what = "matern.covariance"))

multiplot(covplot, corplot)

