#### house keeping ####
rm(list = ls())
source("Scripts/setup.R")

bru_options_set(bru_verbose = TRUE,
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.inla = list(int.strategy="eb"))

#### Preparation of data ####

## load sett data and covariates

sett_all <- readRDS("Data/sett_all.RDS")
sett_all %<>% st_transform(crs = projKM)
setts <- as_Spatial(sett_all)


env_vars <- readRDS("Data/env_vars_1km.RDS")
env_vars_scaled <- raster::scale(env_vars)
land_use <- readRDS("Data/landCover_1km.RDS")
land_use@data$landCover <- as.factor(land_use@data$landCover)



## load mesh boundaries and samplers
ireland_outline <- readRDS("Data/Inla/ireland_outline_km.RDS")
ireland_outline <- as_Spatial(ireland_outline)

# three meshes so we can choose which one
mesh1 <- readRDS("Data/Inla/mesh.RDS")
mesh2 <- readRDS("Data/Inla/small_mesh.RDS")
mesh3 <- readRDS("Data/Inla/Dambly_mesh3.RDS")

inner_boundary <- readRDS("Data/Inla/inner_boundary.RDS")

samplers <- readRDS("Data/Inla/samplers.RDS")
samplers %<>% st_transform(crs = projKM)
samplers <- as_Spatial(samplers)

## create different integration points dfs
df <- pixels(mesh1, mask = inner_boundary)
ipoints <- ipoints(inner_boundary, mesh1)

## make sure all projections match because inla is a lil bitch
setts@proj4string <- mesh1$crs
inner_boundary@proj4string <- mesh1$crs
samplers@proj4string <- mesh1$crs
ireland_outline@proj4string <- mesh1$crs
ipoints@proj4string <- mesh1$crs


#### Model components setup ####

## spatial random effect
matern <- inla.spde2.pcmatern(mesh1,
                              alpha = 3/2,
                              prior.range = c(15, 0.5), 
                              prior.sigma = c(0.1, 0.05)
)


## 1d mesh for elevation
elevation <- as(env_vars_scaled$elevation, "SpatialPixelsDataFrame")
elevation@proj4string <- mesh1$crs

ggplot() +
  gg(elevation) +
  gg(mesh1) +
  gg(samplers, colour = "white") +
  coord_equal()

mesh1D_elev <- inla.mesh.1d(seq(min(elevation$elevation)-1, max(elevation$elevation)+1, length.out = 10), 
                            degree = 2) 

ggplot() +
  gg(mesh1D_elev, shape = "|", size = 5) 


matern1D_elev <- inla.spde2.pcmatern(mesh1D_elev,
                                     prior.range = c(2, 0.01),
                                     prior.sigma = c(0.1, 0.01)
)

## 1d mesh for slope
slope <- as(env_vars_scaled$slope, "SpatialPixelsDataFrame")
slope@proj4string <- mesh1$crs

ggplot() +
  gg(slope) +
  gg(mesh1) +
  gg(samplers, colour = "white") +
  coord_equal()

mesh1D_slope <- inla.mesh.1d(seq(min(slope$slope)-1, max(slope$slope)+1, length.out = 10), 
                            degree = 2) 

ggplot() +
  gg(mesh1D_slope, shape = "|", size = 5) 


matern1D_slope <- inla.spde2.pcmatern(mesh1D_slope,
                                     prior.range = c(3, 0.01),
                                     prior.sigma = c(0.1, 0.01)
)


## 1d mesh for hfi
hfi <- as(env_vars_scaled$human_footprint_index, "SpatialPixelsDataFrame")
hfi@proj4string <- mesh1$crs

ggplot() +
  gg(hfi) +
  gg(mesh1) +
  gg(samplers, colour = "white") +
  coord_equal()

mesh1D_hfi <- inla.mesh.1d(seq(min(hfi$human_footprint_index)-1, max(hfi$human_footprint_index)+1, length.out = 10), 
                             degree = 2) 

ggplot() +
  gg(mesh1D_hfi, shape = "|", size = 5) 


matern1D_hfi <- inla.spde2.pcmatern(mesh1D_hfi,
                                      prior.range = c(3, 0.01),
                                      prior.sigma = c(0.1, 0.01)
)

#### Model formulas ####

# land use seems to whack the model out of balance, so we're not using it for now. We can add the continuous layers if necessary
linear_nonSPDE <- coordinates ~  Intercept  +
  # Eff.landUse(land_use, model = "factor_full") +
  Eff.elevation(elevation, model = "linear") +
  Eff.slope(slope, model = "linear") +
  Eff.hfi(hfi, model = "linear") +
  NULL

linear_SPDE <- 
  linear_nonSPDE <- coordinates ~  Intercept  +
  # Eff.landUse(land_use, model = "factor_full") +
  Eff.elevation(elevation, model = "linear") +
  # Eff.slope(slope, model = "linear") +
  Eff.hfi(hfi, model = "linear") +
  mySmooth(coordinates, model = matern) +
  NULL



#### Running models ####

m1 <- lgcp(linear_nonSPDE,
           setts,
           domain = list(coordinates = mesh1),
           samplers = samplers)

summary(m1)

m2 <- lgcp(linear_SPDE,
           setts,
           domain = list(coordinates = mesh1),
           samplers = samplers)

summary(m2)

#### Predict in linear scale ####

### m1

## predict

lp1 <- predict(m1, df, ~ list(
  # landUse = Eff.landUse,
  elevation = Eff.elevation,
  slope = Eff.slope, 
  hfi = Eff.hfi,
  all = Eff.elevation + Eff.slope + Eff.hfi
))

## plot 

p.lp1.elev <- ggplot() +
  gg(lp1$elevation) +
  theme(legend.position = "bottom") +
  ggtitle("Elevation") +
  coord_equal()

p.lp1.slope <- ggplot() +
  gg(lp1$slope) +
  theme(legend.position = "bottom") +
  ggtitle("Slope") +
  coord_equal()

p.lp1.hfi <- ggplot() +
  gg(lp1$hfi) +
  theme(legend.position = "bottom") +
  ggtitle("Human Footprint Index") +
  coord_equal()

p.lp1.all <- ggplot() + 
  gg(lp1$all) + 
  theme(legend.position = "bottom") +
  ggtitle("all covariates effect") +
  coord_equal()

multiplot(p.lp1.elev, p.lp1.slope, p.lp1.hfi, p.lp1.all, cols = 2)

### m2

## predict

lp2 <- predict(m2, df, ~ list(
  # landUse = Eff.landUse,
  elevation = Eff.elevation,
  slope = Eff.slope, 
  hfi = Eff.hfi,
  smooth = mySmooth,
  all = Eff.elevation + Eff.slope + Eff.hfi + mySmooth
))


## plot 

p.lp2.elev <- ggplot() +
  gg(lp2$elevation, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Elevation") +
  coord_equal()

p.lp2.slope <- ggplot() +
  gg(lp2$slope, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Slope") +
  coord_equal()

p.lp2.hfi <- ggplot() +
  gg(lp2$hfi, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Human Footprint Index") +
  coord_equal()

p.lp2.all <- ggplot() + 
  gg(lp2$all, mask = ireland_outline) + 
  theme(legend.position = "bottom") +
  ggtitle("all covariates effect + smooth") +
  coord_equal()

p.lp2.smooth <- ggplot() + 
  gg(lp2$smooth, mask = ireland_outline) + 
  theme(legend.position = "bottom") +
  ggtitle("Smooth") +
  coord_equal()

multiplot(p.lp2.elev, p.lp2.slope, p.lp2.hfi, p.lp2.all, 
          p.lp2.smooth, cols = 3)

#### Predict in response scale ####

## predict

rp1 <- predict(m1, df, ~ list(
  # landUse = Eff.landUse,
  elevation = exp(Eff.elevation),
  slope = exp(Eff.slope), 
  hfi = exp(Eff.hfi),
  all = exp(Eff.elevation + Eff.slope + Eff.hfi)
))

## plot 

p.rp1.elev <- ggplot() +
  gg(rp1$elevation, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Elevation") +
  coord_equal()

p.rp1.slope <- ggplot() +
  gg(rp1$slope, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Slope") +
  coord_equal()

p.rp1.hfi <- ggplot() +
  gg(rp1$hfi, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Human Footprint Index") +
  coord_equal()

p.rp1.all <- ggplot() + 
  gg(rp1$all, mask = ireland_outline) + 
  theme(legend.position = "bottom") +
  ggtitle("all covariates effect") +
  coord_equal()

multiplot(p.rp1.elev, p.rp1.slope, p.rp1.hfi, p.rp1.all, cols = 2)

# predict total abundance 
Abun.m1 <- predict(
  m1,
  ipoints,
  ~ sum(weight * exp(Eff.elevation + Eff.slope + Eff.hfi))
)

Abun.m1

### m2

## predict

rp2 <- predict(m2, df, ~ list(
  # landUse = Eff.landUse,
  elevation = exp(Eff.elevation),
  slope = exp(Eff.slope), 
  hfi = exp(Eff.hfi),
  smooth = exp(mySmooth),
  all = exp(Eff.elevation + Eff.slope + Eff.hfi + mySmooth)
))


## plot 

p.rp2.elev <- ggplot() +
  gg(rp2$elevation, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Elevation") +
  coord_equal()

p.rp2.slope <- ggplot() +
  gg(rp2$slope, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Slope") +
  coord_equal()

p.rp2.hfi <- ggplot() +
  gg(rp2$hfi, mask = ireland_outline) +
  theme(legend.position = "bottom") +
  ggtitle("Human Footprint Index") +
  coord_equal()

p.rp2.all <- ggplot() + 
  gg(rp2$all, mask = ireland_outline) + 
  theme(legend.position = "bottom") +
  ggtitle("all covariates effect + smooth") +
  coord_equal()

p.rp2.smooth <- ggplot() + 
  gg(rp2$smooth, mask = ireland_outline) + 
  theme(legend.position = "bottom") +
  ggtitle("Smooth") +
  coord_equal()

multiplot(p.rp2.elev, p.rp2.slope, p.rp2.hfi, p.rp2.all, 
          p.rp2.smooth, cols = 3)

#### evaluate effects ####

elev.pred <- predict(
  m1,
  data = data.frame(elevation = seq(min(elevation$elevation), max(elevation$elevation), length.out = 1000)),
  formula = ~ Eff.elevation_eval(elevation), 
  exclude = c("Eff.slope", "Eff.hfi")
  )


ggplot(elev.pred) +
  geom_line(aes(elevation, mean)) +
  geom_ribbon(
    aes(elevation,
        ymin = q0.025,
        ymax = q0.975
    ),
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(elevation,
        ymin = mean - 1 * sd,
        ymax = mean + 1 * sd
    ),
    alpha = 0.2
  )

hfi.pred <- predict(
  m1,
  data = data.frame(hfi = seq(min(hfi$human_footprint_index), max(hfi$human_footprint_index), length.out = 1000)),
  formula = ~ Eff.hfi_eval(hfi), 
  exclude = c("Eff.slope", "Eff.elevation")
)


ggplot(hfi.pred) +
  geom_line(aes(hfi, mean)) +
  geom_ribbon(
    aes(hfi,
        ymin = q0.025,
        ymax = q0.975
    ),
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(hfi,
        ymin = mean - 1 * sd,
        ymax = mean + 1 * sd
    ),
    alpha = 0.2
  )

slope.pred <- predict(
  m1,
  data = data.frame(slope = seq(min(slope$slope), max(slope$slope), length.out = 1000)),
  formula = ~ Eff.slope_eval(slope), 
  exclude = c("Eff.hfi", "Eff.elevation")
)


ggplot(slope.pred) +
  geom_line(aes(slope, mean)) +
  geom_ribbon(
    aes(slope,
        ymin = q0.025,
        ymax = q0.975
    ),
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(slope,
        ymin = mean - 1 * sd,
        ymax = mean + 1 * sd
    ),
    alpha = 0.2
  )





# # model formula
# 
# m_landuse <- lgcp(comp_landuse,
#                   setts,
#                   domain = list(coordinates = mesh1),
#                   samplers = samplers)
# 
# summary(m_landuse)
# 
# 
# 
# 
# (lprange <- range(lp2$smooth_landUse$median, lp2$smooth$median, lp2$landUse$median))
# csc <- scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), limits = lprange)
# 
# 
# flist <- vector("list", NROW(m_landuse$summary.random$landUse))
# for (i in seq_along(flist)) flist[[i]] <- plot(m_landuse, "landUse", index = i)
# multiplot(plotlist = flist, cols = 3)
# 
# 
# 
# 
# 
# 
# 
# comp_elev <- coordinates ~ elevation(main = elev, model = matern1D) +
#   mySmooth(coordinates, model = matern) 
# 
# 
# fit_elev <- lgcp(comp_elev, setts, 
#                  domain = list(coordinates = mesh1),
#                  samplers = samplers)
# 
# summary(fit_elev)
# 
# 
# e.lp <- predict(
#   fit_elev, df,
#   ~ list(
#     all = mySmooth + elevation,
#     smooth = mySmooth,
#     elev = elevation
#   )
# )
# 
# (lprange <- range(e.lp$all$mean, e.lp$elev$mean, e.lp$smooth$mean))
# 
# csc <- scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), limits = lprange)
# 
# plot.e.lp <- ggplot() +
#   gg(e.lp$all, mask = ireland_outline) +
#   csc +
#   theme(legend.position = "bottom") +
#   gg(inner_boundary, alpha = 0) +
#   ggtitle("SPDE + all covars") +
#   coord_equal()
# 
# plot.e.lp.spde <- ggplot() +
#   gg(e.lp$smooth, mask = ireland_outline) +
#   csc +
#   theme(legend.position = "bottom") +
#   gg(inner_boundary, alpha = 0) +
#   ggtitle("SPDE") +
#   coord_equal()
# 
# plot.e.lp.elev <- ggplot() +
#   gg(e.lp$elev, mask = ireland_outline) +
#   csc +
#   theme(legend.position = "bottom") +
#   gg(inner_boundary, alpha = 0) +
#   ggtitle("elevation") +
#   coord_equal()
# 
# 
# multiplot(plot.e.lp,
#           plot.e.lp.spde,
#           plot.e.lp.elev,
#           cols = 3
# )
# 
# spde.range <- spde.posterior(fit_elev, "mySmooth", what = "range")
# spde.logvar <- spde.posterior(fit_elev, "mySmooth", what = "log.variance")
# range.plot <- plot(spde.range)
# var.plot <- plot(spde.logvar)
# 
# multiplot(range.plot, var.plot)
# 
# corplot <- plot(spde.posterior(fit_elev, "mySmooth", what = "matern.correlation"))
# covplot <- plot(spde.posterior(fit_elev, "mySmooth", what = "matern.covariance"))
# multiplot(covplot, corplot)
# 
# Lambda <- predict(
#   efit, ipoints,
#   ~ sum(weight * exp(mySmooth + elevation + Intercept))
# )
# 
# Lambda
# 
# elev.pred <- predict(
#   fit_elev,
#   data = data.frame(elev = seq(40, 600, length.out = 100)),
#   formula = ~ elevation_eval(elev)
# )
# 
# ggplot(elev.pred) +
#   geom_line(aes(elev, mean)) +
#   geom_ribbon(
#     aes(elev,
#         ymin = q0.025,
#         ymax = q0.975
#     ),
#     alpha = 0.2
#   ) +
#   geom_ribbon(
#     aes(elev,
#         ymin = mean - 1 * sd,
#         ymax = mean + 1 * sd
#     ),
#     alpha = 0.2
#   )
# 
# 
# tcd <- as(env_vars$tree_cover_density, "SpatialPixelsDataFrame")
# tcd@proj4string <- mesh1$crs
# 
# ggplot() +
#   gg(tcd) +
#   gg(mesh1) +
#   # gg(inner_boundary) +
#   gg(samplers) +
#   gg(setts, color = "white", size = 0.1, alpha = 0.5) +
#   coord_equal()
# 
# 
# 
# # outside "mesh" of 10% of the range of the data
# # outside mesh 5 knots each side
# # inside mesh 100 knots
# 
# 
# mesh1D_tcd <- inla.mesh.1d(unique(c(seq(min(tcd$tree_cover_density)-0.4*diff(range(tcd$tree_cover_density)), min(tcd$tree_cover_density), length.out = 2), 
#                                     seq(min(tcd$tree_cover_density), max(tcd$tree_cover_density), length.out = 10),
#                                     seq(max(tcd$tree_cover_density), max(tcd$tree_cover_density)+0.4*diff(range(tcd$tree_cover_density)), length.out = 2))), 
#                            degree = 2) 
# 
# ggplot() +
#   gg(mesh1D, shape = "|", size = 5) 
# 
# 
# matern1D_tcd <- inla.spde2.pcmatern(mesh1D_tcd,
#                                 prior.range = c(40, 0.5),
#                                 prior.sigma = c(1, 0.5)
# )
# 
# matern <- inla.spde2.pcmatern(mesh1,
#                               alpha = 3/2,
#                               prior.range = c(500, 0.05), 
#                               prior.sigma = c(0.1, 0.01)
# )
# 
# 
# comp_tcd <- coordinates ~ tree_cover(main = tcd, model = matern1D_tcd) +
#   elevation(main = elev, model = matern1D_elev) + 
#   mySmooth(coordinates, model = matern) 
# 
# 
# fit_tcd <- lgcp(comp_tcd, setts, 
#                  domain = list(coordinates = mesh1),
#                  samplers = samplers)
# 
# summary(fit_tcd)
# 
# tcd.lp <- predict(
#   fit_tcd, df,
#   ~ list(
#     all = mySmooth + tree_cover + elevation,
#     smooth = mySmooth,
#     tcd = tree_cover,
#     elev = elevation
#   )
# )
# 
# (lprange <- range(tcd.lp$all$mean, tcd.lp$tcd$mean, tcd.lp$smooth$mean, tcd.lp$elev$mean))
# 
# csc <- scale_fill_gradientn(colours = brewer.pal(9, "YlOrRd"), limits = c(lprange[1], 5000))
# 
# plot.tcd.lp <- ggplot() +
#   gg(tcd.lp$all, mask = ireland_outline) +
#   csc +
#   theme(legend.position = "bottom") +
#   gg(inner_boundary, alpha = 0) +
#   ggtitle("SPDE + covars") +
#   coord_equal()
# 
# plot.tcd.lp.spde <- ggplot() +
#   gg(tcd.lp$smooth, mask = ireland_outline) +
#   csc +
#   theme(legend.position = "bottom") +
#   gg(inner_boundary, alpha = 0) +
#   ggtitle("SPDE") +
#   coord_equal()
# 
# plot.tcd.lp.tcd <- ggplot() +
#   gg(tcd.lp$tcd, mask = ireland_outline) +
#   csc +
#   theme(legend.position = "bottom") +
#   gg(inner_boundary, alpha = 0) +
#   ggtitle("tree cover density") +
#   coord_equal()
# 
# plot.tcd.lp.elev <- ggplot() +
#   gg(tcd.lp$elev, mask = ireland_outline) +
#   csc +
#   theme(legend.position = "bottom") +
#   gg(inner_boundary, alpha = 0) +
#   ggtitle("elevation") +
#   coord_equal()
# 
# 
# multiplot(plot.tcd.lp,
#           plot.tcd.lp.spde,
#           plot.tcd.lp.elev,
#           plot.tcd.lp.tcd,
#           cols = 4
# )
# 
# 
# spde.range <- spde.posterior(fit_tcd, "mySmooth", what = "range")
# spde.logvar <- spde.posterior(fit_tcd, "mySmooth", what = "log.variance")
# range.plot <- plot(spde.range)
# var.plot <- plot(spde.logvar)
# 
# multiplot(range.plot, var.plot)
# 
# corplot <- plot(spde.posterior(fit_tcd, "mySmooth", what = "matern.correlation"))
# covplot <- plot(spde.posterior(fit_tcd, "mySmooth", what = "matern.covariance"))
# multiplot(covplot, corplot)
# 
# Lambda <- predict(
#   efit, ipoints,
#   ~ sum(weight * exp(mySmooth + elevation + Intercept))
# )
# 
# Lambda
# 
# elev.pred <- predict(
#   fit_tcd,
#   data = data.frame(tcd = seq(0, 100, length.out = 100)),
#   formula = ~ tree_cover_eval(tcd)
# )
# 
# ggplot(elev.pred) +
#   geom_line(aes(tcd, mean)) +
#   geom_ribbon(
#     aes(tcd,
#         ymin = q0.025,
#         ymax = q0.975
#     ),
#     alpha = 0.2
#   ) +
#   geom_ribbon(
#     aes(tcd,
#         ymin = mean - 1 * sd,
#         ymax = mean + 1 * sd
#     ),
#     alpha = 0.2
#   )
# 
