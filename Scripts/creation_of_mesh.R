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

meshes = list()
meshes[[1]] =  fm_mesh_2d_inla(boundary = list(boundary, boundary2),
                               max.edge = c(20,80), cutoff = 20, crs = projKM)
# meshes[[1]] <- mesh_20k

ggplot() + gg(meshes[[1]]) + coord_equal()

n_meshes = 4

for(i in 2:n_meshes) {
  meshes[[i]] = fmesher:::fm_subdivide(meshes[[i-1]])
  
  sapply(meshes, function(x)x$n) %>% plot()
  
} 

saveRDS(meshes, file = "Data/Inla/meshes.RDS")

p1 <- ggplot() + gg(meshes[[1]]) + coord_equal()
p2 <- ggplot() + gg(meshes[[2]]) + coord_equal()
p3 <- ggplot() + gg(meshes[[3]]) + coord_equal()
p4 <- ggplot() + gg(meshes[[4]]) + coord_equal()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

# samplers <- readRDS("Data/Inla/samplers.RDS")
samplers <- readRDS("Data/Inla/weightedSampler.RDS")
# samplers <- readRDS("Data/Inla/samplers.RDS")
samplers %<>% st_transform(crs = projKM)

samplers <- samplers %>% 
  filter(WEIGHT >0) %>% 
  group_by(WEIGHT) %>% 
  summarise() %>% 
  rename(weight = WEIGHT)

ggplot(samplers) + 
  geom_sf(aes(fill = weight, col = weight)) + 
  theme_bw()


meshes <- readRDS("Data/Inla/meshes.RDS")

int_points4 = fm_int(meshes[[4]], samplers = samplers)

saveRDS(int_points4, file = "Data/Inla/int_points4_weighted.RDS")

ggplot(int_points4) + 
  geom_sf(aes(col = weight)) + 
  theme_bw()
