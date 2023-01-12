setwd("C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers")

`%!in%` = Negate(`%in%`)

library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(raster)
library(ggfortify)

sett_all <- readRDS("sett_all.RDS")
class(sett_all)

ireland <- st_read("data/ireland_ITM.shp") %>% 
  st_transform(crs = st_crs(sett_all)) %>% 
  st_union()

env_vars <- stack("C:/Users/morer/Dropbox/Virginia_post_UB/SMARTDEER_isdm/large_env_data/all_covars1km.grd")
env_vars$aspect <- terrain(env_vars$elevation, opt = "aspect", unit = "degrees")

ggplot(sett_all) + 
  geom_sf() + 
  geom_sf(data = ireland, fill = NA)


sett_buffer <- sett_all %>% 
  st_buffer(dist = 500) %>% 
  st_union()

ovrlap <- st_difference(ireland, sett_buffer) %>% 
  filter(NAME_TAG %!in% c("Antrim", "Armagh", "Down", "Fermanagh", "Londonderry", "Tyrone")) 
  

ggplot(ovrlap) + 
  geom_sf() + 
  theme_bw()

pseudoabsences <- ovrlap %>% 
  st_sample(size = 47554*5)

pseudoabsences_sf <- st_sf(
  Presence = 0, 
  geom = pseudoabsences, 
  crs = st_crs(ireland)) %>% 
  st_transform(crs = env_vars@crs)

plot(env_vars$dist_to_forest)
plot(pseudoabsences_sf, add = T)

pseudoabs_values <- raster::extract(env_vars, pseudoabsences_sf)

pseudoabs_values <- as.data.frame(pseudoabs_values, col.names = names(env_vars))

pseudoabs_values <- pseudoabs_values %>% 
  mutate(Presence = "No", 
         X_COORDINATE = st_coordinates(pseudoabsences)[,1], 
         Y_COORDINATE = st_coordinates(pseudoabsences)[,2])

write.csv(pseudoabs_values, file = "pseudoabsences.csv", row.names = F)

sett_all_ITM <- sett_all %>% 
  st_transform(crs = env_vars@crs)


presence_values <- raster::extract(env_vars, sett_all_ITM)
presence_values <- as.data.frame(presence_values, colnames = names(env_vars))

sett_final <- bind_cols(sett_all_ITM, presence_values) %>% 
  st_set_geometry(NULL) %>% 
  mutate(Presence = "Yes")

write.csv(sett_final, file = "presences.csv", row.names = F)


### pca elev-slope
pres <- read.csv("presences.csv")
pseud <- read.csv("pseudoabsences.csv")

all_data <- bind_rows(pres, pseud) 

vars <- c("elevation", "slope", "Presence")
for_pca <- all_data %>% 
  drop_na(any_of(vars))


#pca
pca_res <- prcomp(for_pca[c(41:42)], scale. = TRUE)


autoplot(pca_res, 
         colour = 'elevation',
         size = 0.3,
         alpha = 0.5,
         # shape = FALSE, 
         label = FALSE, 
         loadings = TRUE, 
         loadings.label = TRUE) + 
  theme_bw()



final_df <- for_pca %>% 
  mutate(PC1 = pca_res$x[,1],
         PC2 = pca_res$x[,2]) 


## calculat enew distance
final_df <- final_df %>% 
  mutate(dist_to_fEdge = if_else(dist_to_fEdge == 0, NA_real_, dist_to_fEdge), 
         dist_to_fEdge = -1*dist_to_fEdge,
         newDistance = if_else(is.na(dist_to_fEdge), dist_to_forest, dist_to_fEdge))
  

summary(final_df$newDistance)

hist(final_df$newDistance, breaks = 100)



### create dataset for prediction

newdata <- rasterToPoints(env_vars, spatial = TRUE)
newdata$x_coord <- newdata@coords[,1]
newdata$y_coord <- newdata@coords[,2]

plot(newdata, pch = ".")

newdata_sf <- st_as_sf(newdata, crs = crs(env_vars)) %>% 
  st_transform(crs = st_crs(ireland))


ggplot(newdata_sf) + 
  geom_sf(aes(col = small_woody_features), size = 0.1) + 
  theme_bw()  
  

write.csv(newdata_for_pred, file = "newdata_for_pred.csv", row.names = F)
saveRDS(newdata, file = "newdataSpPointsDf.RDS")


newdata_sf <- newdata_sf %>% 
  mutate(predNumber = spatial_data$predNumber, 
         x_coord = st_coordinates(.)[,1],
         y_coord = st_coordinates(.)[,2]) %>% 
  st_set_geometry(NULL)

write.csv(newdata_sf, file = "spatial_data_correctedCoords.csv", row.names = F)

library(sf)
library(ggplot2)
library(viridis)
library(tidyverse)

spatial_data <- read.csv("spatial_data_correctedCoords.csv")
ireland <- st_read("ireland_ITM.shp")

spatial_data <- spatial_data %>% 
  st_as_sf(coords = c("x_coord", "y_coord")) %>% 
  st_set_crs(st_crs(ireland)) 

ggplot(spatial_data) + 
  geom_sf(aes(col = predNumber), size = 0.5, shape = 15) + 
  geom_sf(data = ireland, fill = NA) + 
  scale_color_viridis(option = "D", trans = "log") + 
  theme_bw()
