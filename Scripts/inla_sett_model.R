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
                              prior.range = c(500, 0.05), 
                              prior.sigma = c(0.1, 0.05)
)


## 1d mesh for elevation
elev <- as(env_vars_scaled$elevation, "SpatialPixelsDataFrame")
elev@proj4string <- mesh1$crs

ggplot() +
  gg(elev) +
  gg(mesh1) +
  gg(samplers, colour = "white") +
  coord_equal()

mesh1D_elev <- inla.mesh.1d(seq(min(elev$elevation)-1, max(elev$elevation)+1, length.out = 10), 
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

linear_nonSPDE <- coordinates ~ - Intercept  +
  landUseEff(land_use, model = "factor_full") +
  elevationEff(elev, model = "linear") + 
  slopeEff(slope, model = "linear") + 
  hfiEff(hfi, model = "linear") + 
  NULL


#### Running models ####

m1 <- lgcp(linear_nonSPDE,
           setts,
           domain = list(coordinates = mesh1),
           samplers = samplers)

summary(m1)


#### Predict in linear scale

## spatial prediction

lp1 <- predict(m1, df, ~ list(
  landUse = landUseEff,
  elevation = elevationEff,
  slope = slopeEff, 
  hfi = hfiEff
))


p.lp1.landuse <- ggplot() +
  gg(lp1$landUse) +
  theme(legend.position = "bottom") +
  ggtitle("Land Use") +
  coord_equal()

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

multiplot(p.lp1.landuse, p.lp1.elev, p.lp1.slope, p.lp1.hfi, cols = 2)

## predict effect

# factor

flist <- vector("list", nrow(m1$summary.random$landUseEff))
for (i in seq_along(flist)) flist[[i]] <- plot(m1, "landUseEff", index = i)
multiplot(plotlist = flist, cols = 3)

# continuous 

elev.pred <- predict(
  m1,
  data = data.frame(ele = seq(min(elev$elevation), max(elev$elevation), length.out = 1000)),
  formula = ~ elevationEff_eval(ele) # this doesn't fucking work but it should
)

ggplot(elev.pred) +
  geom_line(aes(elev, mean)) +
  geom_ribbon(
    aes(elev,
        ymin = q0.025,
        ymax = q0.975
    ),
    alpha = 0.2
  ) +
  geom_ribbon(
    aes(elev,
        ymin = mean - 1 * sd,
        ymax = mean + 1 * sd
    ),
    alpha = 0.2
  )



# predict total abundance 
Lambda2 <- predict(
  m1,
  ipoints,
  ~ sum(weight * exp(landUseEff + elevationEff + slopeEff + hfiEff))
)

Lambda2


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
