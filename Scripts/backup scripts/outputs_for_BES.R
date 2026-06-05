rm(list = ls())
source("Scripts/setup.R")

# Setts ####

# load
sett_all <- readRDS("Data/sett_all_2023.RDS")

ireland <- st_read("Data/Other/ireland_ITM.shp")

samplers <- readRDS("Data/Inla/samplers.RDS")

ggplot(sett_all %>% filter(YEAR > 2000)) + 
  geom_bar(aes(x = YEAR, fill = MAIN_SETT), position = "stack") + 
  theme_classic() + 
  scale_fill_tableau() + 
  labs(x = "Year", y = "No. of setts", fill = "Main sett")


sett_all %<>% 
  filter(YEAR > 2015) %>% 
  mutate(BADGERS_CAPTURED = if_else(NO_BADGERS_CAPTURED == 0, "No", "Yes"))  
  
table(sett_all$ACTIVITY, useNA = "ifany")
table(sett_all$NO_BADGERS_CAPTURED, useNA = "ifany")
table(sett_all$NO_BADGERS_CAPTURED, sett_all$ACTIVITY, useNA = "ifany")
table(sett_all$BADGERS_CAPTURED, useNA = "ifany")


sett_all %>% 
  distinct(SETT_ID, .keep_all = T) %>% 
  filter(YEAR > 2021) %>% 
  # mutate(BADGERS_CAPTURED = if_else(NO_BADGERS_CAPTURED == 0, "No", "Yes")) %>% 
  ggplot + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = samplers, fill = "darkgray") + 
  geom_sf(data = sett_all %>% filter(MAIN_SETT == "No"), 
          alpha = 1, size = 1, col = "#4E79A7") + 
  geom_sf(data = sett_all %>% filter(MAIN_SETT == "Yes"), 
          alpha = 0.75, size = 1, col = "#F28E2B") + 
  scale_color_tableau() + 
  labs(x = "Longitude", y = "Latitude", col = "Main sett") + 
  theme_classic() + 
  guides(col = guide_legend(override.aes = list(size=1.5, alpha = 1)))


# Badgers ####

badgers_all <- readRDS("Data/badgers_setts.RDS") %>% 
  mutate(YEAR = as.numeric(levels(YEAR))[YEAR])

ggplot(badgers_all  %>% filter(!is.na(YEAR))) + 
  geom_bar(aes(x = YEAR, fill = PROGRAMME), position = "stack") + 
  theme_classic() + 
  scale_fill_tableau() + 
  labs(x = "Year", y = "No. of badgers", fill = "Programme")

badgers_all %>% 
  filter(YEAR > 2015) %>% 
  ggplot + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = badgers_all %>% filter(PROGRAMME == "Culling"), 
          alpha = 1, size = 1, col = "#4E79A7") + 
  geom_sf(data = badgers_all %>% filter(PROGRAMME == "Vaccination"), 
          alpha = 0.75, size = 1, col = "#F28E2B") + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic() 


badgers_jit <- readRDS("Data/badgers_jittered.RDS")

badgers_jit %>% 
  mutate(YEAR = as.numeric(levels(YEAR))[YEAR]) %>% 
  filter(YEAR > 2015) %>% 
  ggplot + 
  geom_sf(data = ireland, col = "darkgray", fill = "lightgray") + 
  geom_sf(data = badgers_jit %>% filter(PROGRAMME == "Culling"), 
          alpha = 1, size = 1, col = "#4E79A7") + 
  geom_sf(data = badgers_jit %>% filter(PROGRAMME == "Vaccination"), 
          alpha = 0.75, size = 1, col = "#F28E2B") + 
  labs(x = "Longitude", y = "Latitude") + 
  theme_classic() 


# Env data ####

env_vars <- stack("Data/all_covars_1km.grd")
env_vars <- raster::scale(env_vars)

rdf <- as.data.frame(env_vars, xy=TRUE) #Convert raster to data.frame
names(rdf) <- c("x", "y", "Tree_cover_density", "Small_woody_features", "Elevation", 
                "Slope", "Artificial_surfaces", "Crops", "Pastures", "Grasslands", 
                "Moors", "Shrub", "Peatbogs", "Northness", "Eastness")

rdf_long <- pivot_longer(rdf, cols = Tree_cover_density:Eastness, names_to = "Variable", values_to = "Value") %>% 
  filter(!is.na(Value))

ggplot(data = rdf_long) +
  geom_tile(aes(x = x, y = y, fill = Value)) +
  scale_fill_viridis_c() +
  theme_void() +
  # theme(legend.position = "bottom") + 
  facet_wrap(~Variable, nrow = 3) + 
  coord_equal()


## Model outputs

badgers <- raster("Outputs/badger_abundance_pprediction.grd")
setts <- raster("Outputs/sett_pprediction.grd")
sett_spde <- raster("Outputs/sett_spde.grd")
badgers_spde <- raster("Outputs/badgers_spde.grd")
sett_sd <- raster("Outputs/sett_sd.grd")
badgers_sd <- raster("Outputs/badger_sd.grd")

setts_df <- as.data.frame(setts, xy = TRUE)

ggplot(data = setts_df %>% filter(!is.na(mean))) +
  geom_tile(aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c() +
  theme_classic() +
  coord_equal() + 
  labs(x = "Longitude", y = "Latitude", fill = "No. of setts", title = bquote('Setts per '~km^2))
ggsave(file = "sett_model.png", scale = 2)

setts_sd_df <- as.data.frame(sett_sd, xy = TRUE)

ggplot(data = setts_sd_df %>% filter(!is.na(sd))) +
  geom_tile(aes(x = x, y = y, fill = sd)) +
  scale_fill_viridis_c() +
  theme_classic() +
  coord_equal() + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = bquote('Setts per '~km^2))
ggsave(file = "sett_sd.png", scale = 2)

sett_spde_df <- as.data.frame(sett_spde, xy = T)
ggplot(data = sett_spde_df %>% filter(!is.na(mean))) +
  geom_tile(aes(x = x, y = y, fill = mean)) +
  # scale_fill_viridis_c() +
  scale_fill_distiller(palette="RdBu", direction=-1, type = "div") + 
  theme_classic() +
  coord_equal() + 
  labs(x = "Longitude", y = "Latitude", fill = "", title = "Spatial random field")

ggsave(file = "sett_spde.png", scale = 2)

badgers_df <- as.data.frame(badgers, xy = TRUE)
ggplot(data = badgers_df %>% filter(!is.na(mean))) +
  geom_tile(aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c() +
  theme_classic() +
  coord_equal() + 
  labs(x = "Longitude", y = "Latitude", fill = "No. of badgers", title = bquote('Badgers per '~km^2))
ggsave(file = "Badger_model.png", scale = 2)

badgers_spde_df <- as.data.frame(badgers_spde, xy = T)
ggplot(data = badgers_spde_df %>% filter(!is.na(mean))) +
  geom_tile(aes(x = x, y = y, fill = mean)) +
  # scale_fill_viridis_c() +
  scale_fill_distiller(palette="RdBu", direction=-1, type = "div") + 
  theme_classic() +
  coord_equal() + 
  labs(x = "Longitude", y = "Latitude", fill = "", title = "Spatial random field")

ggsave(file = "badgers_spde.png", scale = 2)

badgers_sd_df <- as.data.frame(badgers_sd, xy = T)
ggplot(data = badgers_sd_df %>% filter(!is.na(sd))) +
  geom_tile(aes(x = x, y = y, fill = sd)) +
  scale_fill_viridis_c() +
  theme_classic() +
  coord_equal() + 
  labs(x = "Longitude", y = "Latitude", fill = "SD", title = bquote('Badgers per '~km^2))

ggsave(file = "badgers_sd.png", scale = 2)

rs_stack <- stack(badgers, setts)

rs_diff <- rs_stack[[2]]/rs_stack[[1]]

rs_df <- as.data.frame(rs_diff, xy=TRUE) #Convert raster to data.frame

ggplot(data = rs_df %>% filter(!is.na(layer))) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(limits = c(0.001, 30),
                       trans = "log10",
                       breaks=trans_breaks('log10', function(x) 10^x),
                       labels = function(x)round(x,2)) +
  theme_classic() +
  coord_equal() + 
  labs(x = "Longitude", y = "Latitude", fill = "Ratio", title = "Badger to main sett ratio")

ggsave(file = "ratio.png", scale = 2)
