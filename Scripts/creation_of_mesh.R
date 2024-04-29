rm(list = ls())
source("Scripts/setup.R")

#### Preparation ####

### Load map of ireland to use as boundary, and point data to check they're inside the boundary
ireland <- st_read("Data/Other/ireland_ITM.shp")
sett_all <- readRDS("Data/sett_all_2023.RDS")
ireland %<>% st_transform(crs = projKM)
sett_all %<>% st_transform(crs = projKM)

### create boundary simplifying map of ireland
boundary <- ireland %>% 
  mutate(Country = "Ireland") %>% 
  group_by(Country) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% 
  st_simplify(dTolerance = 20) %>% # simplify border otherwise triangles in the edge will be too small  
  st_buffer(dist = 20) # buffer to avoid complicated coastline

### Buffer the inner boundary by 200 km more to create outer boundary

boundary2 <- boundary %>% 
  st_buffer(dist = 200)

### Plot to check what we've got

ggplot(boundary) +
  geom_sf(data = boundary2, col = "red", fill = NA) +
  geom_sf(fill =NA) + 
  geom_sf(data = ireland, fill = NA, col = "darkgray") + 
  geom_sf(data = sett_all, size = 0.5, alpha = 0.5) +
  theme_bw()          

#### Creation of mesh ####

### Transform to sp objects

boundary_sp <- as_Spatial(boundary)
boundary2_sp <- as_Spatial(boundary2)
sett_all_sp <- sett_all %>% 
  filter(!st_is_empty(geometry)) %>% 
  as_Spatial()
crs(boundary_sp)
crs(boundary2_sp)
crs(sett_all_sp)

### Create boundary objects

boundary_in <- inla.sp2segment(boundary_sp)
boundary_out <- inla.sp2segment(boundary2_sp)
boundary_in$crs
boundary_out$crs

### Create mesh

# Cutoff of 10 km (segments can't be smaller than 10 km, that's why we needed the simplification)
# The edge of the triangles in the inner mesh is 20km
# The edge of the triangles in the outer mesh is 50km 

# parameters from deer model (ish) i.e. very large mesh 
mesh_20k <- inla.mesh.2d(boundary = list(boundary_in, boundary_out), 
                         max.edge = c(20, 100), 
                         cutoff = 20, crs = boundary_out$crs)

# parameters at same resolution as smoothed covars
mesh_5k <- inla.mesh.2d(boundary = list(boundary_in, boundary_out), 
                         max.edge = c(5, 100), 
                         cutoff = 5, crs = boundary_out$crs)

# parameters at smaller resolution than smoothed covars
mesh_2k <- inla.mesh.2d(boundary = list(boundary_in, boundary_out), 
                        max.edge = c(2, 100), 
                        cutoff = 2, crs = boundary_out$crs)

mesh_1k <- inla.mesh.2d(boundary = list(boundary_in, boundary_out), 
                        max.edge = c(1, 100), 
                        cutoff = 1, crs = boundary_out$crs)
#### Plot mesh ####
par(mfrow = c(2,2))
plot(mesh_20k)
plot(boundary_sp, border = "green", add = T)
plot(boundary2_sp, border = "orange", add = T)

plot(mesh_5k)
plot(mesh_2k)
plot(mesh_1k)
par(mfrow = c(1,1))

# saveRDS(poly.barrier, file = "data/barrier.RDS")
saveRDS(mesh_20k, file = "Data/Inla/mesh_20k.RDS")
saveRDS(mesh_5k, file = "Data/Inla/mesh_5k.RDS")
saveRDS(mesh_2k, file = "Data/Inla/mesh_2k.RDS")
saveRDS(mesh_1k, file = "Data/Inla/mesh_1k.RDS")
saveRDS(boundary2_sp, file = "Data/outer_boundary.RDS")
saveRDS(boundary_sp, file = "Data/inner_boundary.RDS")


