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
# for some reason if I load them all as a stack it throws an error related to the srs when projecting, so doing them in a loop, projecting to km, and then stacking

lyrs <- list.files(path = "Data/Covars/Corine", pattern='\\.gri$', full.names = T)
nms <- gsub(".grd", "", list.files(path = "Data/Covars/Corine", pattern='\\.grd$'))

corine <- stack()

for(i in seq_along(lyrs)) {
  par(mfrow = c(1,2))
  x <- raster(lyrs[i])
  plot(x)
  x@crs <- CRS("EPSG:2157")
  x2 <- projectRaster(from = x, crs = projKM)
  plot(x2)
  par(mfrow = c(1,1))
  names(x2) <- nms[i]
  corine <- stack(corine, x2)
}

plot(corine)
res(corine)

writeRaster(corine, filename = "Data/corine_5km.grd", format = "raster")
