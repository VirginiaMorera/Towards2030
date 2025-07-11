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
library(ggthemes)
library(fmesher)
library(patternogram)
library(usdm)
library(patchwork)
library(tidyterra)
library(mgcv)
library(mgcViz)
library(rgl)
library(gstat)


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

scale01 <- function(x){(x-min(x))/(max(x)-min(x))}

sf.kde.new <- function (x, y = NULL, bw = NULL, ref = NULL, res = NULL, standardize = FALSE, 
                        scale.factor = 10000, mask = FALSE) 
{
  if (missing(x)) 
    stop("x argument must be provided")
  if (!inherits(x, c("sf", "sfc"))) 
    stop(deparse(substitute(x)), " must be a sf, or sfc object")
  if (unique(as.character(sf::st_geometry_type(x))) != "POINT") 
    stop(deparse(substitute(x)), " must be single-part POINT geometry")
  if (is.null(scale.factor)) 
    scale.factor = 1
  ref.flag = inherits(ref, "SpatRaster")
  if (is.null(res) && ref.flag == TRUE) {
    res <- terra::res(ref)
  }
  if (is.null(res) && ref.flag == FALSE) {
    res <- 100
    message("resoultion not defined, defaulting to 100")
  }
  if (is.null(ref)) {
    ref <- terra::rast(terra::ext(x), resolution = res, crs = terra::crs(x))
  }
  else if (inherits(ref, "numeric")) {
    if (length(ref) != 4) 
      stop("Need xmin, xmax, ymin, ymax bounding coordinates")
    ref <- terra::rast(terra::ext(ref), resolution = res, 
                       crs = terra::crs(x))
  }
  else {
    if (!inherits(ref, "SpatRaster")) 
      stop(deparse(substitute(ref)), " must be a terra SpatRast object")
    if (terra::res(ref)[1] != res[1]) 
      message("reference raster defined, res argument is being ignored")
  }
  fhat <- function(x, y, h, w, n = 25, lims = c(range(x), range(y))) {
    nx <- length(x)
    if (length(y) != nx) 
      stop("data vectors must be the same length")
    if (length(w) != nx & length(w) != 1) 
      stop("weight vectors must be 1 or length of data")
    if (missing(h)) {
      h <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
    }
    else {
      h <- rep(h, length.out = 2L)
    }
    if (any(h <= 0)) 
      stop("bandwidths must be strictly positive")
    if (missing(w)) {
      w <- numeric(nx) + 1
    }
    gx <- seq(lims[1], lims[2], length = n[1])
    gy <- seq(lims[3], lims[4], length = n[2])
    h <- h/4
    ax <- outer(gx, x, "-")/h[1]
    ay <- outer(gy, y, "-")/h[2]
    z <- (matrix(rep(w, n[1]), nrow = n[1], ncol = nx, byrow = TRUE) * 
            matrix(stats::dnorm(ax), n[1], nx)) %*% t(matrix(stats::dnorm(ay), 
                                                             n[2], nx))/(sum(w) * h[1] * h[2])
    return(list(x = gx, y = gy, z = z))
  }
  if (is.null(bw)) {
    bwf <- function(x) {
      r <- stats::quantile(x, c(0.25, 0.75))
      h <- (r[2] - r[1])/1.34
      4 * 1.06 * min(sqrt(stats::var(x)), h) * length(x)^(-1/5)
    }
    bw <- c(bwf(sf::st_coordinates(x)[, 1]), bwf(sf::st_coordinates(x)[, 
                                                                       2]))
    message("Using ", round(bw[1], 3), ", ", round(bw[2], 
                                                   3), " for bandwidth", "\n")
  }
  else {
    bw <- c(bw, bw)
  }
  n <- c(terra::nrow(ref), terra::ncol(ref))
  if (!is.null(y)) {
    message("\n", "calculating weighted kde", "\n")
    k <- fhat(sf::st_coordinates(x)[, 1], sf::st_coordinates(x)[, 
                                                                2], w = y, h = bw, n = n, lims = as.vector(terra::ext(ref)))
  }
  else {
    message("\n", "calculating unweighted kde", "\n")
    k <- MASS::kde2d(sf::st_coordinates(x)[, 1], sf::st_coordinates(x)[, 
                                                                       2], h = bw, n = n, lims = as.vector(terra::ext(ref)))
  }
  k$z <- k$z * scale.factor
  if (standardize == TRUE) {
    k$z <- (k$z - min(k$z))/(max(k$z) - min(k$z))
  }
  pts <- data.frame(expand.grid(x = k$x, y = k$y), z = round(as.vector(array(k$z, 
                                                                             length(k$z))) * scale.factor, 10))
  kde.est <- terra::rast(pts, type = "xyz", extent = terra::ext(ref))
  if (mask == TRUE) {
    kde.est <- terra::mask(kde.est, ref)
  }
  terra::crs(kde.est) <- terra::crs(x)
  return(kde.est)
}

