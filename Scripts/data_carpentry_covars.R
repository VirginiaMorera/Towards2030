rm(list = ls())
source("Scripts/setup.R")

#### 1. Load data ####

sett_all <- readRDS("Data/sett_all.RDS")
ireland <- st_read("Data/Other/ireland_ITM.shp")


# ugly data carpentry to put covars together 
covars <- stack("Data/Covars/all_covars_1km.grd")
env_vars <- stack("Data/Covars/env_vars_1km.grd")

covars@crs
covars_sub <- stack("Covars/covar_subset_ITM.grd")
covars_sub@crs


covars_irenet <- projectRaster(covars, to = covars_sub)
covars_irenet@crs

corine_irenet <- projectRaster(covars$landCover, to = covars_sub, method = "ngb")

plot(corine_irenet)
table(corine_irenet[])

covars_irenet$landCover <- corine_irenet
plot(covars_irenet)


crops <- stack("Covars/ireland_crops_1k.grd")

stack(covars_irenet, crops)


img <- list.files(path = "Covars/Corine", pattern = ".grd", full.names = T)
names <- gsub(".grd", "", list.files(path = "Covars/Corine", pattern = ".grd"))
corine_percentages <- stack(img)
names(corine_percentages) <- names
plot(corine_percentages)
corine_percentages@crs <- corine_irenet@crs


all_covars <- list_to_stack(raster_list = list(covars_irenet, crops, corine_percentages), new_res = res(corine_percentages), dest_crs = covars_irenet@crs)

plot(all_covars, maxnl=32)


writeRaster(all_covars, filename = "Data/Covars/all_covars_ITM_5k.grd")
all_covars <- stack("Data/Covars/all_covars_ITM_5k.grd")
plot(all_covars)

dist_to_fEdge <- (-1)*all_covars$dist_to_fEdge
dist_to_fEdge[is.na(dist_to_fEdge)] <- 0

dist_to_forest <- all_covars$dist_to_forest
dist_to_forest[is.na(dist_to_forest)] <- 0

forest_distances <- sum(dist_to_forest, dist_to_fEdge)

forest_distances <- mask(forest_distances, ireland)
plot(forest_distances)

new_covars <- raster::subset(all_covars, subset = c(9,10,13,14,15))
new_covars <- stack(new_covars, forest_distances)
names(new_covars)[6] <- "Forest_distance"
plot(new_covars)

new_covars <- projectRaster(new_covars, crs = projKM)
env_vars <- stack("Data/all_covars_1km_smooth5.grd")


env_vars_list <- as.list(env_vars)
new_covars_list <- as.list(new_covars)
all_covars_list <- c(env_vars_list, new_covars_list)

all_covars_stack <- list_to_stack(all_covars_list, new_res = res(env_vars), 
                                  dest_crs = crs(env_vars))

all_covars_stack <- trim(all_covars_stack)

writeRaster(all_covars_stack, filename = "Data/Covars/final_covars.grd")
