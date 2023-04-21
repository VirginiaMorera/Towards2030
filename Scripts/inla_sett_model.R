rm(list = ls())
source("Scripts/setup.R")

#### Preparation ####

### Load map of ireland to use as boundary, and point data to check they're inside the boundary
ireland <- st_read("Data/ireland_ITM.shp")
sett_all <- readRDS("Data/sett_all.RDS")
mesh <- readRDS("Data/mesh.RDS")
inner_boundary <- readRDS("Data/inner_boundary.RDS")
samplers <- readRDS("Data/samplers.RDS")
env_vars <- stack("Data/all_covars1km.grd")

env_vars$aspect <- terrain(env_vars$elevation, opt = "aspect", unit = "degrees")
env_vars <- as(env_vars, "SpatialPixelsDataFrame")



setts <- SpatialPoints(coords = st_coordinates(sett_all), proj4string = CRS("EPSG:2157"))
samplers <- as_Spatial(samplers)

# Construct latent model components
matern <- inla.spde2.pcmatern(mesh,
                              prior.sigma = c(2, 0.01),
                              prior.range = c(5000, 0.01)
)

inner_boundary@proj4string <- mesh$crs

df <- pixels(mesh, mask = inner_boundary)

# model formula
comp2 <- coordinates ~ landUse(map = env_vars$landCover, model = "factor_full") - Intercept  +
  mySmooth(coordinates, model = matern)

m1 <- lgcp(comp2, 
           setts, 
           domain = list(coordinates = mesh),
           samplers = samplers)

summary(m1)

int2 <- predict(m1, 
                df, 
                # mesh = mesh, mask = inner_boundary, 
                formula = ~ exp(mySmooth + landUse),
                # predictor = TRUE,
                # fun = 'exp'
                n.samples = 1000)

ggplot() +
  gg(int2, aes(fill = q0.025)) +
  gg(inner_boundary, alpha = 0, lwd = 2) +
  # gg(nests) +
  coord_equal()
