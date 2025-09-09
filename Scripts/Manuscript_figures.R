# house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load ireland maps ####
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS") %>% 
  st_transform(crs = projKM) 

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 


# load sett data ####
sett_all <- readRDS("Data/sett_2025_inside_effort.RDS") %>% 
  st_transform(crs = projKM) 

sett_subset <- sett_all %>% 
  filter(MAIN_SETT == "Yes") %>%
  select(SETT_ID, geometry)


# load badger data ####
badgers_all <- readRDS("Data/badgers_jittered_filtered_2025.RDS") %>% 
  st_transform(crs = projKM) 


# load effort data ####
effort <- readRDS("Data/Inla/weightedSampler.RDS") %>% 
  st_transform(crs = projKM) 


# Table 1: sett model covariates #### 
sett_sum <- m4$summary.hyperpar %>% 
  mutate(variable = rownames(.)) %>% 
  separate(variable, into = c(NA, NA, NA, "variable")) %>%
  # filter(variable != "smooth") %>%
  mutate(parameter = c(rep(c("range", "sd"), 5), "range")) %>% 
  select(variable, parameter, median = `0.5quant`, lower = `0.025quant`, 
         upper = `0.975quant`)

scaling_parameters_clean <- scaling_parameters %>% 
  filter(names %in% c("elevation", "slope", "GrasslandPastures", 
                      "forest_distances", 
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

#elevation prior
3*scaling_parameters[3,3] + scaling_parameters[3,2]

#slope prior
2*scaling_parameters[4,3] + scaling_parameters[4,2]

#grassland prior
0.1*scaling_parameters[17,3] + scaling_parameters[17,2]

# distance to forest edge prior
4*scaling_parameters[15,3] + scaling_parameters[15,2]

# hfi prior
2*scaling_parameters[14,3] + scaling_parameters[14,2]

# Fig 1: data maps ####
pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_1.pdf", width = 12, height = 10)
ggplot() + 
  geom_sf(data = ireland_counties, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = sett_subset, alpha = 0.5, shape = 1, size = 1, stroke = 0.1) + 
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
  # eval.topo,
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
  scale_fill_viridis_c(option = "D") +
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
lp4 <- readRDS("Outputs/sett_model/linear_predictor.RDS")
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

forestdist_df <- lp4$forestDistance[!inside,]
forestdist <- forestdist_df %>%
  mutate(Variable = "Distance to forest edge")

limit <- max(abs(c(elev_df$q0.5, slope_df$q0.5, grass_df$q0.5, hfi_df$q0.5,
                   forestdist_df$q0.5))) * c(-1, 1)

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S1.pdf", 
    width = 12, height = 8)

ggplot() + 
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) + 
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) + 
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) + 
  labs(title = "Pasture and grasslands effect", x = "", y = "", fill = "Median") +
  
ggplot() +
  gg(data = hfi_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) +
  labs(title = "Human footprint index effect", x = "", y = "", fill = "Mean") +
  
ggplot() +
  gg(data = forestdist_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', direction = -1, limits = limit) +
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

# badger_sum <- m4$summary.hyperpar %>% 

badger_sum <- readRDS("Outputs/badgers_all_model/m4_summary.RDS") %>% 
  mutate(variable = rownames(.)) %>% 
  separate(variable, into = c(NA, NA, NA, "variable")) %>% 
  # filter(variable != "smooth") %>%
  mutate(parameter = c(rep(c("range", "sd"), 8), "range")) %>% 
  select(variable, parameter, median = `0.5quant`, lower = `0.025quant`, upper = `0.975quant`)

scaling_parameters_clean <- scaling_parameters %>% 
  filter(names %in% c("elevation", "slope", "forest_distances", "GrasslandPastures",
                      "human_footprint_index", "setts", "cull_hist")) %>% 
  mutate(names = recode(names,  
                        "forest_distances" = "forestdist", 
                        "GrasslandPastures" = "grassPast",
                        "human_footprint_index" = "hfi", 
                        "cull_hist" = "cull",
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

#elevation prior
4*scaling_parameters[3,3] + scaling_parameters[3,2]

#slope prior
2*scaling_parameters[4,3] + scaling_parameters[4,2]

#grassland prior
1*scaling_parameters[18,3] + scaling_parameters[18,2]

# distance to forest edge prior
2*scaling_parameters[15,3] + scaling_parameters[15,2]

# hfi prior
4*scaling_parameters[14,3] + scaling_parameters[14,2]

# sett density prior 
2*scaling_parameters[16,3] + scaling_parameters[16,2]

# cullhist prior
3*scaling_parameters[19,3] + scaling_parameters[19,2]

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
  # eval.topo,
  eval.cull,
  cols = 2 
)

dev.off()

# Fig. 5 badger distribution map ####
rp4 <- readRDS("Outputs/badgers_all_model/response_predictor.RDS")
inside = sapply(st_intersects(rp4$all, ireland_outline_sf), function(x){length(x)==0})
y <- rp4$all[!inside,]

y <- y %>% 
  mutate(IQ = q0.975 - q0.025) 

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
  gg(data = y, aes(fill = IQ), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA, col = "white") + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "95% CI width", 
       title = "b)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "D", trans = "log",
                       breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3)) +
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

cull_df <- lp4$cull[!inside,]
cull_df <- cull_df %>% 
  mutate(Variable = "Culling history 2014 - 2018")

setts_df <- lp4$setts[!inside,]
setts_df <- setts_df %>% 
  mutate(Variable = "Sett relative density")

limit <- max(abs(c(elev_df$q0.5, slope_df$q0.5, grass_df$q0.5, hfi_df$q0.5,
                   fordist_df$q0.5, setts_df$q0.5, cull_df$q0.5))) * c(-1, 1)

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
  gg(data = setts_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Sett relative density", x = "", y = "", fill = "Mean") +

ggplot() +
  gg(data = cull_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = badgers_all, alpha = 0.5, size = 0.1) +
  theme_bw() +
  scale_fill_distiller(palette = 'RdBu', limit = limit) +
  labs(title = "Culling history 2014 - 2018", x = "", y = "", fill = "Mean") +
  
plot_layout(ncol = 3)
dev.off()



# Fig S4: spatial field ####
lp4 <- readRDS("Outputs/badgers_all_model/linear_predictor.RDS")

inside = sapply(st_intersects(lp4$spfield_big, ireland_outline_sf), function(x){length(x)==0})
spb <- lp4$spfield_big[!inside,]

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S4.pdf", 
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

# Fig. S7 Other gam outputs ####

m1 <- readRDS("Outputs/gam_model.RDS")

badgers_clean <- readRDS("Data/badgers_for_gam.RDS")
badgers_clean <- badgers_clean %>% drop_na()

resid_sp <- badgers_clean  %>% 
  add_partial_residuals(m1) %>% 
  select(x, y, resid = `s(B_DENSITY,S_DENSITY)`) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(2157) %>% 
  st_transform(st_crs(ireland_counties))

sm_spatial <- smooth_estimates(m1)  %>% 
  add_confint()  %>% 
  filter(.smooth == "s(x,y)") %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(2157) %>% 
  st_transform(st_crs(ireland_counties))

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_S7.pdf", 
    width = 15, height = 8)
ggplot() + 
  geom_sf(data = sm_spatial, aes(col = .estimate), shape = 15, size = 2.5) + 
  geom_sf(data = ireland_counties, fill = NA) + 
  geom_sf(data = resid_sp, aes(size = abs(resid)), alpha = 0.1) +
  scale_color_distiller(palette = 'RdBu') +
  scale_size(range = c(0, 2)) + 
  theme_bw() + 

draw(m1, select = "s(MONTH)") &
  theme_bw() 
dev.off()

# Fig. 6 GAM output ####

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_6a.pdf", 
    width = 15, height = 8)
draw(m1, select = "s(B_DENSITY,S_DENSITY)") + 
  # coord_equal() + 
  theme_bw() +
  labs(title = "a)", x = "Badger density", y = "Sett density") 
dev.off()


resid <- badgers_clean  %>% 
  add_partial_residuals(m1) %>% 
  select(GROUP_SIZE, resid = `s(GROUP_SIZE)`)

sm <- smooth_estimates(m1)  %>% 
  add_confint()  %>% 
  filter(.smooth == "s(GROUP_SIZE)")

pdf(file = "C:/Users/morer/Dropbox/Virginia_post_UB/05_Badgers/Draft/MS_Figures/Fig_6b.pdf", 
    width = 12, height = 6)
ggplot(sm) +
  geom_rug(aes(x = GROUP_SIZE),
           sides = "b") +
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = GROUP_SIZE),
              alpha = 0.2) +
  geom_line(aes(x = GROUP_SIZE, y = .estimate), lwd = 1) +
  # geom_point(data = resid,
  #   aes(x = GROUP_SIZE, y = resid,
  #     colour = GROUP_SIZE), # <-- map fac to colour aesthetic
  #     cex = 1.5)
  labs(y = "Partial effect", x = "Group size", title = "b)") + 
  theme_bw()
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

summary(sett)
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



