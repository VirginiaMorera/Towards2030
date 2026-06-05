# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

bru_options_set(bru_verbose = TRUE,
                control.inla = list(int.strategy = "auto"))

# Load datasets ####
sett_all <- readRDS("Data/sett_all_2025.RDS")
badgers_all <- readRDS("Data/badgers_all_2025.RDS")

badgers_all_settID <- badgers_all %>% 
  select(BADGER_ID, SETT_ID, CAPTURE_BLOCK_EVENT, YEAR) 

# Combine badgers per sett ####
badgers_sum <- badgers_all_settID %>% 
  group_by(SETT_ID, YEAR, CAPTURE_BLOCK_EVENT) %>% 
  tally()
  
badgers_year_sum <- badgers_sum %>% 
  group_by(SETT_ID, YEAR) %>% 
  summarise(mean_group_size = mean(n), 
            sd_group_size = sd(n))

badgers_final_sum <- badgers_sum %>% 
  group_by(SETT_ID) %>% 
  summarise(mean_group_size = round(mean(n), 0), 
            sd_group_size = sd(n))


for_marked <- sett_all %>% 
  select(SETT_ID) %>% 
  left_join(badgers_final_sum) %>% 
  mutate(GROUP_SIZE = replace_na(mean_group_size, 0)) %>% 
  select(SETT_ID, GROUP_SIZE) %>% 
  st_transform(st_crs(ireland_counties))

## load mesh boundaries and samplers ####
ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 

carlow <- ireland_counties %>% 
  filter(NAME_TAG == "Carlow") %>% 
  st_as_sf()

ggplot() + 
  geom_sf(data = carlow) + 
  geom_sf(data = for_marked) + 
  coord_sf(xlim = st_bbox(carlow)[c(1,3)], st_bbox(carlow)[c(2,4)]) + 
  theme_bw()

ca_data <- for_marked[carlow, , op = st_within]
library(GeoThinneR)
coords <- st_coordinates(ca_data)
thinning <- distance_thinning(thin_dist = 10, 
                              coordinates = coords, 
                              trials = 1000)

ca_data_thinned <- ca_data %>% 
  mutate(thin = thinning[[1]]) %>% 
  filter(thin == TRUE) %>% 
  select(-thin) %>% 
  st_transform(projKM)

ggplot() + 
  geom_sf(data = carlow) + 
  geom_sf(data = ca_data_thinned, aes(col = GROUP_SIZE)) + 
  theme_bw()


badger_data <- list()
badger_data$carlow <- carlow
badger_data$data <- ca_data_thinned
saveRDS(badger_data, file = "badger_data.RDS")

inner_boundary <- st_buffer(st_simplify(carlow, dTolerance = 1, TRUE),  1)
outer_boundary <- st_buffer(inner_boundary, 15)

mesh <- fm_mesh_2d_inla(
  boundary = list(inner_boundary, outer_boundary),
  max.edge = c(2, 7),  # this controls the size of the triangles
  cutoff = 1, 
  crs = st_crs(kk_data)) 

ggplot() + 
  gg(mesh) + 
  geom_sf(data = carlow, fill = NA, col = "red") +
  geom_sf(data = ca_data_thinned) + 
  theme_bw()

# Load objects needed for model
env_vars <- terra::rast("Data/Covars/final_covars_terra.grd")
env_vars$forest_distances <- env_vars$forest_distances/1000

env_vars$PeatbogsandMoors <- sum(env_vars$Peatbogs, env_vars$Moorsandheathland)
env_vars$GrasslandPastures <- sum(env_vars$Naturalgrasslands, env_vars$Pastures)

env_vars_scaled <- terra::scale(env_vars) 

env_vars_ca <- trim(crop(env_vars_scaled, carlow))
plot(env_vars_ca)
saveRDS(env_vars_ca, file = "env_vars_ca.RDS")

elevation <- env_vars_scaled$elevation
slope <- env_vars_scaled$slope
forestDist <- env_vars_scaled$forest_distances
topo_wetness <- env_vars_scaled$topographic_wetness_index

## load mesh boundaries and samplers ####

ips <- fm_int_mesh_2d(domain = mesh, samplers = inner_boundary)

matern_p <- inla.spde2.pcmatern(mesh,
                                prior.range = c(15, 0.1),
                                prior.sigma = c(1, 0.1))

matern_m <- inla.spde2.pcmatern(mesh,
                                prior.range = c(15, 0.1),
                                prior.sigma = c(1, 0.1))


cmp <- ~ -1 +
  point_field(geometry, model = matern_p) +
  mark_field(geometry, model = matern_m) +
  scale(1) + inter_point(1) + inter_mark(1) +
  Eff.elev_point(elevation, model = "linear") + 
  Eff.slope_point(slope, model = "linear") + 
  Eff.elev_mark(elevation, model = "linear") + 
  Eff.slope_mark(slope, model = "linear") 
  
lik1 <- bru_obs(formula = geometry ~ -1 + inter_point + point_field + 
                  Eff.elev_point + Eff.slope_point, 
                family = "cp",
                data = ca_data_thinned,
                domain =  list(geometry = mesh))

lik2 <- bru_obs(formula = GROUP_SIZE ~ -1 + inter_mark + point_field*scale + mark_field + 
                  Eff.elev_mark + Eff.slope_mark, 
                family = "poisson",
                data = ca_data_thinned,
                domain =  list(geometry = mesh))

fit <- bru(cmp, lik1, lik2, 
           options = list(control.inla = list(int.strategy = "eb")))
summary(fit)

newdf <- fm_pixels(mesh,
                   dims = c(100, 100),
                   mask = carlow,
                   format = "sf")

pred_setts <- predict(
  fit, 
  newdf, 
  formula = ~ exp(inter_point +  point_field + 
                    Eff.elev_point + Eff.slope_point))

pred_badgers <- predict(
  fit, 
  newdf, 
  formula = ~ exp(inter_mark +  point_field*scale + 
                    Eff.elev_mark + Eff.slope_mark))


ggplot() + 
  gg(data = pred_setts, aes(fill = q0.5), geom = "tile") +
  # geom_sf(data = nests, col = "red", size = 0.5) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c(option = "B") + 
  theme_bw() + 
  
ggplot() + 
  gg(data = pred_badgers, aes(fill = q0.5), geom = "tile") +
  # geom_sf(data = nests, aes(col = mark), size = 1) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c(option = "B") + 
  theme_bw() 
