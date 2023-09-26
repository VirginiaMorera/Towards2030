library(tidyverse)
library(sf)
library(lubridate)
library(viridis)
library(raster)
library(png)
library(grid)
library(tidylog)
library(magrittr)
library(readxl)
library(purrr)
library(inlabru)
library(INLA)
library(RColorBrewer)
library(rcartocolor)
library(scales)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

`%!in%` <- Negate(`%in%`)


list_to_stack <- function(raster_list, new_res, dest_crs = CRS("+proj=longlat"), turn_0_to_NA = FALSE) {
  require(raster)
  if(turn_0_to_NA == TRUE) {
    for(i in seq_along(raster_list)) {
      raster_list[[i]][raster_list[[i]] == 0] <- NA
      raster_list[[i]] <- trim(raster_list[[i]])}
  }
  
  # reproject rasters to dest_crs
  raster_list_proj <- lapply(raster_list, projectRaster, crs = dest_crs)
  
  # get the minimum common extent that encompasses all rasters in list 
  ext_list <- lapply(X = raster_list_proj, function(x) {as.matrix(x@extent)})
  matrix_extent <- matrix(unlist(ext_list), ncol=length(ext_list))
  rownames(matrix_extent)<-c("xmin", "ymin", "xmax", "ymax")
  best_extent <- extent(min(matrix_extent[1,]), max(matrix_extent[3,]), 
                        min(matrix_extent[2,]), max(matrix_extent[4,]))
  
  # resample all rasters in list to best extent, new crs and new resolution
  res_list2 <- lapply(raster_list_proj, resample, 
                      raster(ext = best_extent, crs = dest_crs, resolution = new_res))
  
  # turn to stack
  res_stack <- stack(res_list2)
  
  return(res_stack)
}

projKM <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=0.99982 +x_0=600000 +y_0=750000
           +ellps=GRS80 +units=km +no_defs"

