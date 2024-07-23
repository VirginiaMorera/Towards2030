rm(list = ls())
source("Scripts/setup.R")


ireland <- st_read("Data/Other/ireland_ITM.shp")

covars <- stack("Data/Covars/all_covars_1km.grd")
env_vars <- stack("Data/Covars/env_vars_1km.grd")

covars2 <- projectRaster(from = covars, to = env_vars)

new_covars <- stack(covars2, env_vars)

new_covars <- raster::subset(new_covars, subset = c(1,2,3,4,7,8,9,10,11,17,18,21,22,23,27))

new_covars <- projectRaster(new_covars, crs = projKM)

terra_covars <- terra::rast(new_covars)
plot(terra_covars)
names(terra_covars)
names(terra_covars)[1:4] <- c("tree_cover_density", "small_woody_features", "elevation", "slope")

