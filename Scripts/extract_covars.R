rm(list = ls())
source("Scripts/setup.R")

env_vars <- stack("Data/all_covars1km.grd")
ireland <- st_read("Data/ireland_ITM.shp")

landUse <- env_vars$landCover
landUseITM <- projectRaster(landUse, crs = crs(ireland), method = "ngb")
env_vars_ITM <- projectRaster(env_vars, crs = crs(ireland))
env_vars_ITM$landCover <- landUseITM
table(env_vars_ITM$landCover[])

landCover <- as(env_vars_ITM$landCover, "SpatialPixelsDataFrame")
landCover@data$landCover <- recode(landCover@data$landCover, 
                                   '1' = "Builtup", '2' = "Saltwater_related", 
                                   '3' = "No_vegetation", '4' = "Freshwater_related", 
                                   '5' = "Other_vegetation", '6' = "Agricultural", 
                                   '7' = "Pasture", '8' = "Grassland", '9' = "Transitional", 
                                   '10' = "Coniferous_forest", '11' = "Mixed_forest", 
                                   '12' = "Broadleaf_forest")

table(landCover@data$landCover)
saveRDS(landCover, file = "Data/landCover.RDS")
saveRDS(env_vars_ITM,file = "Data/env_vars.RDS")
