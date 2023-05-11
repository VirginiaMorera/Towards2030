rm(list = ls())
source("Scripts/setup.R")

#### load data 1km res
env_vars <- stack("Data/Covars/all_covars1km.grd")
env_vars_km <- projectRaster(env_vars, crs = projKM)

ireland <- st_read("Data/Other/ireland_ITM.shp")

ireland_outline <- ireland %>% 
  summarise(geometry = st_union(geometry),
            AREA = sum(AREA)) %>% 
  st_transform(crs = projKM)

# saveRDS(ireland_outline, "Data/ireland_outline_km.RDS")

#### create 5km raster to resample

largerRas <- raster(ext = extent(env_vars_km), crs = projKM, resolution = c(5,5))

#### rasters that are factors

##  Land use

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

##  dominant leaf type

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

##  forest type

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

#### calculate forest distance

dist_to_fEdge <- (-1)*env_vars_km$dist_to_fEdge

dist_to_forest <- env_vars_km$dist_to_forest

forest_distances <- sum(dist_to_forest, dist_to_fEdge)

plot(forest_distances)

env_vars_sel <- dropLayer(env_vars_km, c("dist_to_forest", "dist_to_fEdge", 
                                         "landCover", "dominant_leaf_type", "forest_type"))

env_vars_sel$forest_distances <- forest_distances

#### save as 1km

saveRDS(env_vars_sel, file = "Data/env_vars_1km.RDS")

#### resample to 5km and save
env_vars_sel_5km <- resample(env_vars_sel, largerRas, method = "bilinear")

saveRDS(env_vars_sel_5km, file = "Data/env_vars_5km.RDS")
