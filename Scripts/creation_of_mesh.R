rm(list = ls())
source("Scripts/setup.R")

#### Preparation ####

### Load map of ireland to use as boundary, and point data to check they're inside the boundary
ireland <- st_read("Data/ireland_ITM.shp")
sett_all <- readRDS("Data/sett_all.RDS")
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

# parameters from deer model (ish)
mesh <- inla.mesh.2d(boundary = list(boundary_in, boundary_out), 
                     max.edge = c(20, 100), 
                     cutoff = 20, crs = boundary_out$crs)
mesh$crs

# parameters from Dambly et al 2023

mesh <- inla.mesh.2d(boundary = list(boundary_in, boundary_out), 
                     max.edge = c(45, 150), 
                     cutoff = 9, crs = boundary_out$crs)
mesh$crs


#### Plot mesh ####

plot(mesh)
# plot(all_data_sp, add = T, col = "red")
plot(boundary_sp, border = "green", add = T)
plot(boundary2_sp, border = "orange", add = T)

## Creation of mask

# This will allow us to hide the predictions on the outer boundary from our results (probably there's a better way but just in case)
# This code is taken from Haakon Bakka's barrier model from the website https://haakonbakka.bitbucket.io/btopic107.html#4_barrier_models and it runs all at once

### Code to create the mask

tl = length(mesh$graph$tv[,1]) # Number of triangles of the mesh
posTri = matrix(0, tl, 2) # matrix containing the coordinates of each triangle's 
# we fill it with the vertex "coordinates here
for (t in 1:tl) {
  # Take the vertex of triangles
  temp = mesh$loc[mesh$graph$tv[t, ], ]
  # Compute barycenter which sintetize the triangle
  posTri[t,] = colMeans(temp)[c(1,2)]
}
#transform it in Spatial Points
posTri = SpatialPoints(posTri)
posTri@proj4string <- CRS("NA")
boundary_sp@proj4string <- CRS("NA")
normal = over(boundary_sp, posTri, returnList = T) # this are the polygons contained in the inner boundary, the "good" ones
normal = unlist(normal)
barrier.triangles = setdiff(1:tl, normal) # this are the polygons of the outer boundary, the ones we want to hide
# build a polygon that contains all polygons that are outer boundary and not inner. 
poly.barrier = inla.barrier.polygon(mesh, barrier.triangles)

### Plot the mask to check everything is ok
plot(mesh)
plot(poly.barrier, col = "gray", add = T)


# saveRDS(poly.barrier, file = "data/barrier.RDS")
saveRDS(mesh, file = "Data/mesh.RDS")
saveRDS(mesh, file = "Data/small_mesh.RDS")
saveRDS(mesh, file = "Data/Dambly_mesh3.RDS")
saveRDS(boundary2_sp, file = "Data/outer_boundary.RDS")
saveRDS(boundary_sp, file = "Data/inner_boundary.RDS")
saveRDS(poly.barrier, file = "Data/barrier.RDS")

