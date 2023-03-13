rm(list = ls())
source("Scripts/setup.R")

#### 1. Load data ####

sett_all <- readRDS("Data/sett_all.RDS")
ireland <- st_read("Data/ireland_ITM.shp")

covars <- stack("Covars/all_covars.grd")
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


writeRaster(all_covars, filename = "Covars/all_covars_ITM_5k.grd")
