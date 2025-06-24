rm(list = ls())
source("Scripts/setup.R")

# load data ####
env_vars <- stack("Data/Covars/all_covars1km.grd")
env_vars_km <- projectRaster(env_vars, crs = projKM)

ireland <- st_read("Data/Other/ireland_ITM.shp")

ireland_outline <- ireland %>% 
  summarise(geometry = st_union(geometry),
            AREA = sum(AREA)) %>% 
  st_transform(crs = projKM)

# saveRDS(ireland_outline, "Data/ireland_outline_km.RDS")

# create empty 5km raster to resample ####

largerRas <- raster(ext = extent(env_vars_km), crs = projKM, resolution = c(5,5))

# Resample rasters that are factors ####

##  Land use ####

# 1 km
landUse <- env_vars$landCover
landUse_km <- projectRaster(landUse, crs = projKM, method = "ngb")

landCover <- as(landUse_km, "SpatialPixelsDataFrame")
landCover@data$landCover <- recode(landCover@data$landCover, 
                                   '1' = "Builtup", '2' = "Saltwater_related", 
                                   '3' = "No_vegetation", '4' = "Freshwater_related", 
                                   '5' = "Other_vegetation", '6' = "Agricultural", 
                                   '7' = "Pasture", '8' = "Grassland", '9' = "Transitional", 
                                   '10' = "Coniferous_forest", '11' = "Mixed_forest", 
                                   '12' = "Broadleaf_forest")

table(landCover@data$landCover)

saveRDS(landCover, file = "Data/landCover_1km.RDS")

# 5 km

landUse_5km <- resample(landUse_km, largerRas, method = "ngb")
landCover_5km <- as(landUse_5km, "SpatialPixelsDataFrame")
landCover_5km@data$landCover <- recode(landCover_5km@data$landCover, 
                                   '1' = "Builtup", '2' = "Saltwater_related", 
                                   '3' = "No_vegetation", '4' = "Freshwater_related", 
                                   '5' = "Other_vegetation", '6' = "Agricultural", 
                                   '7' = "Pasture", '8' = "Grassland", '9' = "Transitional", 
                                   '10' = "Coniferous_forest", '11' = "Mixed_forest", 
                                   '12' = "Broadleaf_forest")


table(landCover_5km@data$landCover)

saveRDS(landCover_5km, file = "Data/landCover_5km.RDS")

##  Dominant leaf type ####

# 1 km
dlt <- env_vars$dominant_leaf_type

dlt_km <- projectRaster(dlt, crs = projKM, method = "ngb")

dlt_pix <- as(dlt_km, "SpatialPixelsDataFrame")

dlt_pix@data$dominant_leaf_type <- recode(dlt_pix@data$dominant_leaf_type, 
                                          '1' = "Broadleaf", '2' = "Coniferous")

table(dlt_pix@data$dominant_leaf_type)

saveRDS(dlt_pix, file = "Data/dlt_1km.RDS")

# 5 km

dlt_5km <- resample(dlt_km, largerRas, method = "ngb")
dlt_pix_5km <- as(dlt_5km, "SpatialPixelsDataFrame")
dlt_pix_5km@data$dominant_leaf_type <- recode(dlt_pix_5km@data$dominant_leaf_type, 
                                              '1' = "Broadleaf", '2' = "Coniferous")


table(dlt_pix_5km@data$dominant_leaf_type)

saveRDS(dlt_pix_5km, file = "Data/dlt_5km.RDS")

##  Forest type ####

# 1 km
ft <- env_vars$forest_type

ft_km <- projectRaster(ft, crs = projKM, method = "ngb")

ft_pix <- as(ft_km, "SpatialPixelsDataFrame")

ft_pix@data$forest_type <- recode(ft_pix@data$forest_type, 
                                          '0' = "Non-forest",  '1' = "Broadleaf", '2' = "Coniferous")

table(ft_pix@data$forest_type, useNA = "always")

saveRDS(ft_pix, file = "Data/ft_1km.RDS")

# 5 km

ft_5km <- resample(ft_km, largerRas, method = "ngb")
ft_pix_5km <- as(ft_5km, "SpatialPixelsDataFrame")
ft_pix_5km@data$forest_type <- recode(ft_pix_5km@data$forest_type, 
                                      '0' = "Non-forest",  '1' = "Broadleaf", '2' = "Coniferous")

table(ft_pix_5km@data$forest_type, useNA = "always")

saveRDS(ft_pix_5km, file = "Data/ft_5km.RDS")

# Calculate forest distance ####

dist_to_fEdge <- (-1)*env_vars_km$dist_to_fEdge

dist_to_forest <- env_vars_km$dist_to_forest

forest_distances <- sum(dist_to_forest, dist_to_fEdge)

plot(forest_distances)

env_vars_sel <- dropLayer(env_vars_km, c("dist_to_forest", "dist_to_fEdge", 
                                         "landCover", "dominant_leaf_type", "forest_type"))

env_vars_sel$forest_distances <- forest_distances

# Save modified stacks ####

## save as 1km ####

writeRaster(env_vars_sel, filename = "Data/env_vars_1km.grd", format = "raster")

## resample to 5km and save ####
env_vars_sel_5km <- resample(env_vars_sel, largerRas, method = "bilinear")

writeRaster(env_vars_sel_5km, filename = "Data/env_vars_5km.grd", format = "raster")


# Corine continuous layers ####

corine <- stack("Data/Covars/corine_ireland_1km.grd")
env_vars_km <- stack("Data/Covars/env_vars_1km.grd")
corineKM <- projectRaster(corine, crs = projKM)
corineKM <- resample(corineKM, env_vars_km)
all_covars_1km <- stack(env_vars_km, corineKM)
all_covars_1km <- subset(all_covars_1km, subset = c(2, 3, 6, 7, 15:17, 21:23, 26))
all_covars_1km$northness <- cos(raster::terrain(all_covars_1km$elevation, opt = "aspect", units = "rad"))
all_covars_1km$eastness <- sin(raster::terrain(all_covars_1km$elevation, opt = "aspect", units = "rad"))

writeRaster(all_covars_1km, filename = "Data/all_covars_1km.grd", format = "raster", 
            overwrite = T)

plot(corineKM)
corine_5k <- resample(corineKM, largerRas, method = "bilinear")
plot(corine_5k)
writeRaster(corine_5k, filename = "Data/corine_5km.grd", format = "raster", overwrite = T)

# Smooth covariates to avoid issues with mesh resolution


ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")

par(mfrow = c(1,3))
r <- env_vars$tree_cover_density
r2 <- focal(r, w= matrix(1,3,3), mean, na.rm = T)
r3 <- focal(r, w= matrix(1,9,9), mean, na.rm = T)
plot(r)
plot(mesh2, add = T)
plot(r2)
plot(mesh2, add = T)
plot(r3)
plot(mesh2, add = T)

env_vars_smooth3 <- stack()
env_vars_smooth5 <- stack()

for (i in 1:nlayers(all_covars_1km)) {
  # i = 1
  r <- all_covars_1km[[i]]
  r2 <- focal(r, w= matrix(1,3,3), mean, na.rm = T)
  r3 <- focal(r, w= matrix(1,5,5), mean, na.rm = T)
  r2 <- crop(r2, ireland_outline_sf)
  r3 <- crop(r3, ireland_outline_sf)
  env_vars_smooth3 <- stack(env_vars_smooth3, r2)
  env_vars_smooth5 <- stack(env_vars_smooth5, r3)
}

names(env_vars_smooth3) <- names(all_covars_1km)
names(env_vars_smooth5) <- names(all_covars_1km)

env_vars <- crop(all_covars_1km, ireland_outline_sf)
plot(env_vars$tree_cover_density)
plot(env_vars_smooth3$tree_cover_density)
plot(env_vars_smooth5$tree_cover_density)

writeRaster(env_vars, filename = "Data/all_covars_1km.grd", 
            format = "raster", overwrite = T)
writeRaster(env_vars_smooth3, filename = "Data/all_covars_1km_smooth3.grd", 
            format = "raster", overwrite = T)
writeRaster(env_vars_smooth5, filename = "Data/all_covars_1km_smooth5.grd", 
            format = "raster", overwrite = T)


## sett model output as covar in the badger model
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS")
rp4 <- readRDS("Outputs/sett_model/response_predictor.RDS")

inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]


r <- stars::st_rasterize(y %>% dplyr::select(median, geometry))
plot(r)
r <- rast(r)


env_vars <- terra::rast("Data/Covars/final_covars_terra.grd")

r <- resample(r, env_vars)

env_vars$setts <- r

plot(env_vars)

writeRaster(env_vars, file = "Data/Covars/final_covars_terra_with_setts.grd", overwrite = T)

saveRDS(env_vars, file = "env_vars_with_setts.RDS")
