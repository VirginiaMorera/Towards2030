# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load ireland maps ####
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS") %>% 
  st_transform(crs = projKM) 

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 


# load sett data ####
sett_all <- readRDS("Data/sett_all_inside_effort.RDS") %>% 
  st_transform(crs = projKM) 

sett_subset <- sett_all %>% 
  mutate(year = lubridate::year(DATE_OF_FIELD_VISIT)) %>% 
  filter(year > 2018) %>% 
  filter(ACTIVITY > 1) %>% 
  filter(MAIN_SETT == "Yes") %>%
  filter(CAPTURE_BLOCK_ID %!in% c("NOT ASSIGN", "NOT ASSIGNE")) %>% 
  select(SETT_ID, geometry)


# load badger data ####
badgers_all <- readRDS("Data/badgers_jittered_filtered.RDS") %>% 
  st_transform(crs = projKM) 


# load effort data ####
effort <- readRDS("Data/Inla/weightedSampler.RDS") %>% 
  st_transform(crs = projKM) 


# Table 1: sett model covariates #### 
sett_sum <- m4$summary.hyperpar %>% 
  mutate(variable = rownames(.)) %>% 
  separate(variable, into = c(NA, NA, NA, "variable")) %>%
  # filter(variable != "smooth") %>%
  mutate(parameter = c(rep(c("range", "sd"), 6), "range")) %>% 
  select(variable, parameter, median = `0.5quant`, lower = `0.025quant`, upper = `0.975quant`)

scaling_parameters_clean <- scaling_parameters %>% 
  filter(names %in% c("elevation", "slope", "GrasslandPastures", "forest_distances", 
                      "topographic_wetness_index", "human_footprint_index")) %>% 
  mutate(names = recode(names, "GrasslandPastures" = "grassPast", 
                        "forest_distances" = "forestdist", 
                        "topographic_wetness_index" = "topo", 
                        "human_footprint_index" = "hfi")) %>% 
  rename(variable = names)


sett_sum_descaled <- sett_sum %>% 
  left_join(scaling_parameters_clean)


sett_sum_descaled <- sett_sum_descaled %>% 
  rowwise %>% 
  mutate(median_s = (median*sds) + means, 
         lower_s = (lower*sds) + means, 
         higher_s = (upper*sds) + means)

write.csv(sett_sum_descaled, file = "Outputs/sett_model/sett_sum.csv")

# Fig 1: data maps ####
pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_1s.pdf", width = 12, height = 10)
ggplot() + 
  geom_sf(data = ireland_counties, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = sett_subset, alpha = 0.5) + 
  theme_bw() +
  
ggplot() +   
  geom_sf(data = ireland_counties, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = badgers_all, alpha = 0.5, shape = 1, size = 1, stroke = 0.1) + 
  theme_bw() +

ggplot() + 
  geom_sf(data = effort, aes(fill = NDAYS, col = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "darkgray", fill = NA) + 
  scale_fill_viridis_c(na.value = NA, name = "N. days") + 
  scale_colour_viridis_c(na.value = NA, name = "N. days") + 
  theme_bw() +
  
plot_layout(nrow = 2) + 
plot_annotation(tag_levels = 'a')

dev.off()

# Fig. 2: covariate effects ####
# For this to work the code in the sett model script that obtains the non spatial evaluation of each covariate needs to have been run 
pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_2.pdf", 
    width = 8, height = 8)
multiplot(
  eval.elev,
  eval.grasslandsPastures,
  eval.hfi,
  
  eval.slope,
  eval.forestdist,
  eval.topo,
  cols = 2 
)
dev.off()


# Fig. 3: sett distribution map ####
rp4 <- readRDS("Outputs/sett_model/response_predictor.RDS")
inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_3.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = y, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "a)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D", 
                       breaks = c(0.005, 0.010, 0.015, 0.020, 0.025)) +
  NULL + 
  
  ggplot() + 
  gg(data = y, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "95% CI width", 
       title = "b)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D") +
  NULL

dev.off()


# Fig S1. Spatial covariate effects ####
lp4 <- readRDS("Outputs/sett_model/response_predictor.RDS")
inside = sapply(st_intersects(lp4$elevation, ireland_outline_sf), function(x){length(x)==0})

elev_df <- lp4$elevation[!inside,] 
elev_df <- elev_df %>% 
  mutate(Variable = "Elevation")

slope_df <- lp4$slope[!inside,] 
slope_df <- slope_df %>% 
  mutate(Variable = "Slope")

grass_df <- lp4$grassland[!inside,] 
grass_df <- grass_df %>% 
  mutate(Variable = "Grasslands and pastures")

topo_df <- lp4$topoWetness[!inside,]
topo_df <- topo_df %>%
  mutate(Variable = "Topographic wetness index")

hfi_df <- lp4$hfi[!inside,]
hfi_df <- hfi_df %>%
  mutate(Variable = "Human footprint index")

forestdist_df <- lp4$forestDistance[!inside,]
forestdist <- forestdist_df %>%
  mutate(Variable = "Distance to forest edge")

limit <- max(abs(c(elev_df$q0.5, slope_df$q0.5, grass_df$q0.5, hfi_df$q0.5,
                   forestdist_df$q0.5, topo_df$q0.5))) * c(-1, 1)

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S1.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1, limits = limit) + 
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1, limits = limit) + 
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1, limits = limit) + 
  labs(title = "Pasture and grasslands effect", x = "", y = "", fill = "Median") +
  
ggplot() +
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1, limits = limit) +
  labs(title = "Topographic wetness index effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1, limits = limit) +
  labs(title = "Human footprint index effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = forestdist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = 1, limits = limit) +
  labs(title = "Distance to forest edge", x = "", y = "", fill = "Mean") +
  
plot_layout(ncol = 3)

dev.off()

# Fig S2: spatial field ####
lp4 <- readRDS("Outputs/sett_model/linear_predictor.RDS")

inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S2.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", title = "Spatial random field") + 
  
  ggplot() + 
  gg(data = spb, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_viridis_c() +
  labs(x = "", y = "", fill = "95% CI", title = "Uncertainty of the spatial random field") 

dev.off()

# Table 2: badger model covariates #### 
badger_sum <- m4$summary.hyperpar %>% 
  mutate(variable = rownames(.)) %>% 
  separate(variable, into = c(NA, NA, NA, "variable")) %>% 
  # filter(variable != "smooth") %>%
  mutate(parameter = c(rep(c("range", "sd"), 6), "range")) %>% 
  select(variable, parameter, median = `0.5quant`, lower = `0.025quant`, upper = `0.975quant`)

scaling_parameters_clean <- scaling_parameters %>% 
  filter(names %in% c("elevation", "slope", "forest_distances", 
                      "topographic_wetness_index", "human_footprint_index", "setts")) %>% 
  mutate(names = recode(names,  
                        "forest_distances" = "forestdist", 
                        "topographic_wetness_index" = "topo", 
                        "human_footprint_index" = "hfi", 
                        "setts" = "sett")) %>% 
  rename(variable = names)


badger_sum_descaled <- badger_sum %>% 
  left_join(scaling_parameters_clean)


badger_sum_descaled <- badger_sum_descaled %>% 
  rowwise %>% 
  mutate(median_s = median*sds + means, 
         lower_s = lower*sds + means, 
         higher_s = upper*sds + means)

write.csv(badger_sum_descaled, file = "Outputs/badgers_all_model/badgers_sum.csv")

# Fig. 4: covariate effects badgers####
# For this to work the code in the sett model script that obtains the non spatial evaluation of each covariate needs to have been run 
pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_4.pdf", 
    width = 8, height = 8)
multiplot(
  eval.elev,
  eval.grasslandsPastures,
  eval.hfi,
  eval.sett, 
  eval.slope,
  eval.forestDist,
  eval.topo,
  cols = 2 
)

dev.off()

# Fig. 5 badger distribution map ####
rp4 <- readRDS("Outputs/badgers_all_model/response_predictor.RDS")
inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_5.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = y, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "a)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D")+
                       # breaks = c(0.005, 0.010, 0.015, 0.020, 0.025)) +

ggplot() + 
  gg(data = y, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "95% CI width", 
       title = "b)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D",
                       breaks = c(0.02, 0.04, 0.06, 0.08, 0.10, 0.12)) +
  NULL

dev.off()

# Fig S3. Spatial covariate effects badgers ####
lp4 <- readRDS("Outputs/badgers_all_model/linear_predictor.RDS")
inside = sapply(st_intersects(lp4$elevation, ireland_outline_sf), function(x){length(x)==0})

elev_df <- lp4$elevation[!inside,] 
elev_df <- elev_df %>% 
  mutate(Variable = "Elevation")

slope_df <- lp4$slope[!inside,] 
slope_df <- slope_df %>% 
  mutate(Variable = "Slope")

grass_df <- lp4$grassland[!inside,] 
grass_df <- grass_df %>% 
  mutate(Variable = "Grasslands and pastures")

hfi_df <- lp4$hfi[!inside,]
hfi_df <- hfi_df %>% 
  mutate(Variable = "Human footprint index")

fordist_df <- lp4$forestDistance[!inside,]
fordist_df <- fordist_df %>%
  mutate(Variable = "Distance to forest edge")

topo_df <- lp4$topoWetness[!inside,]
topo_df <- topo_df %>% 
  mutate(Variable = "Topographic wetness index")

setts_df <- lp4$setts[!inside,]
setts_df <- setts_df %>% 
  mutate(Variable = "Sett relative density")

limit <- max(abs(c(elev_df$q0.5, slope_df$q0.5, grass_df$q0.5, hfi_df$q0.5,
                   fordist_df$q0.5, topo_df$q0.5, setts_df$q0.5))) * c(-1, 1)

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S3.pdf", 
    width = 12, height = 8)
ggplot() +
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Elevation", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Slope", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Grasslands and pastures", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Human footprint index", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = fordist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Distance to the forest edge", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = topo_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Topographic wetness index", x = "", y = "", fill = "Mean") +
  
plot_layout(ncol = 3)
dev.off()


# Fig. S4. Spatial effect of sett diribution ####
limit <- max(abs(setts_df$q0.5)) * c(-1, 1)

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S4.pdf", 
    width = 10, height = 8)

ggplot() +
  gg(data = setts_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Sett relative density", x = "", y = "", fill = "Mean") 

dev.off()

# Fig S5: spatial field ####
lp4 <- readRDS("Outputs/badgers_all_model/linear_predictor.RDS")

inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S5.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", title = "Spatial random field") + 
  
ggplot() + 
  gg(data = spb, aes(fill = q0.975 - q0.025), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_viridis_c() +
  labs(x = "", y = "", fill = "95% CI", title = "Uncertainty of the spatial random field") 

dev.off()

# Fig. 6 GAM output ####

m1 <- readRDS("Outputs/gam_model.RDS")

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_6.pdf", 
    width = 10, height = 10)
vis.gam(m1, view = c("B_DENSITY", "S_DENSITY"), theta = 40, n.grid = 50, 
        type = "response", color = "terrain", xlab = "Badger density", 
        ylab = "Sett density", zlab = "Effect")

dev.off()

# Fig. 7 Group size ####

sett_pred <- readRDS("Outputs/sett_model/response_predictor.RDS")

inside = sapply(st_intersects(sett_pred$all, ireland_outline_sf), function(x){length(x)==0})
sett_pred <- sett_pred$all[!inside,]

sett_pred2 <- sett_pred %>% 
  mutate(x = st_coordinates(.)[,1], 
         y = st_coordinates(.)[,2]) %>% 
  select(x, y, z = q0.5) %>% 
  st_drop_geometry()

sett_rast <- rast(sett_pred2, type="xyz", crs = crs(sett_pred))

badger_pred <- readRDS("Outputs/badgers_all_model/response_predictor.RDS")

inside = sapply(st_intersects(badger_pred$all, ireland_outline_sf), function(x){length(x)==0})
badger_pred <- badger_pred$all[!inside,]

badger_pred2 <- badger_pred %>% 
  mutate(x = st_coordinates(.)[,1], 
         y = st_coordinates(.)[,2]) %>% 
  select(x, y, z = q0.5) %>% 
  st_drop_geometry()

badger_rast <- rast(badger_pred2, type="xyz", crs = crs(sett_pred))

group_size <- badger_rast/sett_rast

group_size_df <- as.data.frame(group_size, xy = T)

group_size_sf <- st_as_sf(group_size_df, coords = c("x", "y"), 
                          crs = st_crs(ireland_counties))

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_7.pdf", 
    width = 8, height = 8)
ggplot() + 
  gg(data = group_size_sf, aes(fill = z), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") +
  labs(x = "", y = "", fill = "Badgers/Setts") +  
  scale_fill_viridis_c(option = "D") + 
  theme_bw()
dev.off()
