rm(list = ls())
source("Scripts/setup.R")

#### Preparation ####

### Load map of ireland to use as boundary, and point data to check they're inside the boundary
ireland <- st_read("Data/ireland_ITM.shp")
sett_all <- readRDS("Data/sett_all.RDS")
mesh <- readRDS("Data/mesh.RDS")
inner_boundary <- readRDS("Data/inner_boundary.RDS")
samplers <- readRDS("Data/samplers.RDS")
landCover <- readRDS("Data/landCover.RDS")
env_vars <- readRDS("Data/env_vars.RDS")

landCover@data$landCover <- as.factor(landCover@data$landCover)
setts <- as_Spatial(sett_all)
samplers <- as_Spatial(samplers)
# Construct latent model components
matern <- inla.spde2.pcmatern(mesh,
                              prior.sigma = c(0.5, 0.05),
                              prior.range = c(1000, 0.05)
)


df <- pixels(mesh, mask = inner_boundary)

ggplot() +
  gg(landCover) +
  gg(mesh) +
  gg(inner_boundary) + 
  gg(samplers) +
  # gg(setts, color = "white", size = 0.1, alpha = 0.5) +
  coord_equal()


landCover@proj4string <- mesh$crs
setts@proj4string <- mesh$crs
inner_boundary@proj4string <- mesh$crs
samplers@proj4string <- mesh$crs

# model formula
comp2 <- coordinates ~ - Intercept  +landUse(landCover, model = "factor_full") +
  mySmooth(coordinates, model = matern)

m1 <- lgcp(comp2, 
           setts, 
           domain = list(coordinates = mesh),
           samplers = samplers)

summary(m1)

int2 <- predict(m1, 
                df, 
                # mesh = mesh, mask = inner_boundary,
                formula = ~ landUse,
                # predictor = TRUE,
                # fun = 'exp'
                n.samples = 1000)

ggplot() +
  gg(int2, aes(fill = mean)) +
  # gg(boundary, alpha = 0, lwd = 2) +
  # gg(nests) +
  coord_equal()

lp2 <- predict(m1, df, ~ list(
  smooth_landUse = mySmooth + landUse,
  smooth = mySmooth,
  landUse = landUse
))

lprange <- range(lp2$smooth_landUse$median, lp2$smooth$median, lp2$landUse$median)
csc <- scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), limits = lprange)

plot.lp2 <- ggplot() +
  gg(lp2$smooth_landUse) +
  csc +
  theme(legend.position = "bottom") +
  gg(inner_boundary, alpha = 0) +
  ggtitle("mySmooth + landUse") +
  coord_equal()

plot.lp2.spde <- ggplot() +
  gg(lp2$smooth) +
  csc +
  theme(legend.position = "bottom") +
  gg(inner_boundary, alpha = 0) +
  ggtitle("mySmooth") +
  coord_equal()

plot.lp2.veg <- ggplot() +
  gg(lp2$landUse) +
  csc +
  theme(legend.position = "bottom") +
  gg(inner_boundary, alpha = 0) +
  ggtitle("landUse") +
  coord_equal()

multiplot(plot.lp2, plot.lp2.spde, plot.lp2.veg, cols = 3)

flist <- vector("list", NROW(m1$summary.random$landUse))
for (i in seq_along(flist)) flist[[i]] <- plot(m1, "landUse", index = i)
multiplot(plotlist = flist, cols = 3)

spde.range <- spde.posterior(m1, "mySmooth", what = "range")
spde.logvar <- spde.posterior(m1, "mySmooth", what = "log.variance")
range.plot <- plot(spde.range)
var.plot <- plot(spde.logvar)

multiplot(range.plot, var.plot)

corplot <- plot(spde.posterior(m1, "mySmooth", what = "matern.correlation"))
covplot <- plot(spde.posterior(m1, "mySmooth", what = "matern.covariance"))
multiplot(covplot, corplot)

elev <- as(env_vars$elevation, "SpatialPixelsDataFrame")


f.elev <- function(xy) {
  # turn coordinates into SpatialPoints object:
  # with the appropriate coordinate reference system (CRS)
  spp <- SpatialPoints(data.frame(x = xy[, 1], y = xy[, 2]),
                       proj4string = fm_CRS(elev)
  )
  # Extract elevation values at spp coords, from our elev SpatialGridDataFrame
  v <- over(spp, elev)
  if (any(is.na(v$elevation))) {
    v$elevation <- bru_fill_missing(elev, spp, v$elevation)
  }
  return(v$elevation)
}

matern <- inla.spde2.pcmatern(mesh,
                              prior.sigma = c(0.1, 0.01),
                              prior.range = c(5, 0.01)
)

ecomp <- coordinates ~ elev(f.elev(coordinates(.data.)), model = "linear") +
  mySmooth(coordinates, model = matern) + Intercept(1)

efit <- lgcp(ecomp, setts, 
             domain = list(coordinates = mesh),
             samplers = samplers)

summary(efit)

e.lp <- predict(
  efit, df,
  ~ list(
    smooth_elev = mySmooth + elev,
    elev = elev,
    smooth = mySmooth
  )
)

lprange <- range(e.lp$smooth_elev$mean, e.lp$elev$mean, e.lp$smooth$mean)

library(RColorBrewer)
csc <- scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), limits = lprange)

plot.e.lp <- ggplot() +
  gg(e.lp$smooth_elev, mask = inner_boundary) +
  csc +
  theme(legend.position = "bottom") +
  gg(inner_boundary, alpha = 0) +
  ggtitle("SPDE + elevation") +
  coord_equal()

plot.e.lp.spde <- ggplot() +
  gg(e.lp$smooth, mask = inner_boundary) +
  csc +
  theme(legend.position = "bottom") +
  gg(inner_boundary, alpha = 0) +
  ggtitle("SPDE") +
  coord_equal()

plot.e.lp.elev <- ggplot() +
  gg(e.lp$elev, mask = inner_boundary) +
  csc +
  theme(legend.position = "bottom") +
  gg(inner_boundary, alpha = 0) +
  ggtitle("elevation") +
  coord_equal()

multiplot(plot.e.lp,
          plot.e.lp.spde,
          plot.e.lp.elev,
          cols = 3
)
