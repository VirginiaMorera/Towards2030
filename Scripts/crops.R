setwd("C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers")

library(raster)
library(rasterVis)
crops5k <- raster("ireland_crops_5k.grd")
plot(crops5k)

crops1k <- raster("ireland_crops_1k.grd")
crops1k <- trim(crops1k)
plot(crops1k)
table(crops1k[])

r <- ratify(crops1k)
rat <- levels(r)[[1]]
rat$Crop <- c("Artificial land", "Wheat", "Barley", "Maize", 
              "Potatoes", "Sunflower", "Rape and turnip rape", 
              "Dry pulses, vegetables and flowers", "Bare arable land", 
              "Woodland and Shrubland", "Grassland (permament and temporary)")
levels(r) <- rat
levelplot(r)


crops1k_sum <- crops1k

crops1k_sum[crops1k_sum %in% c(211, 213, 216)] <- 210
crops1k_sum[crops1k_sum %in% c(231, 232)] <- 230

r2 <- ratify(crops1k_sum)
rat2 <- levels(r2)[[1]]
rat2$Crop <- c("Artificial land", "Cereal", 
               "Potatoes", "Industrial crops", 
               "Dry pulses, vegetables and flowers", "Bare arable land", 
               "Woodland and Shrubland", "Grassland (permament and temporary)")
levels(r2) <- rat2
levelplot(r2)
