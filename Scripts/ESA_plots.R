# To be used with the model scripts

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 

## Effort ####

effort <- readRDS("Data/Inla/weightedSampler.RDS")
cul_effort <- readRDS("Data/Inla/cul_effort.RDS")
vac_effort <- readRDS("Data/Inla/vacc_effort.RDS")


png("Outputs/ESA_Figures/two_efforts.jpg", width = 7.46*2, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = cul_effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  labs(title = "Culling programme effort", 
       col = "Number of days", fill = "Number of days", x = "", y = "") + 
  theme_bw() +

ggplot() + 
  geom_sf(data = vac_effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  labs(title = "Vaccination programme effort", 
       col = "Number of days", fill = "Number of days", x = "", y = "") + 
  theme_bw() +

plot_layout(nrow = 1)
dev.off()

png("Outputs/ESA_Figures/final_effort.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  scale_fill_viridis_c(na.value = NA) + 
  scale_color_viridis_c(na.value = NA) + 
  labs(title = "Total effort in number of days", 
       col = "Number of days", fill = "Number of days", x = "", y = "") + 
  theme_bw()
dev.off()

png("Outputs/ESA_Figures/effort_trans.jpg", width = 6, height = 4, res = 600, units = "in")
ggplot(effort) + 
  geom_point(aes(x = NDAYS, y = WEIGHT, col = NDAYS)) + 
  scale_color_viridis() + 
  labs(x = "Number of days", y = "Log of number of days", 
       col = "Number of days", title = "Transformation of effort") + 
  theme_bw()
dev.off()

## datasets ####

### setts ####
sett_all <- readRDS("Data/sett_all_inside_effort.RDS") %>% 
  st_transform(crs = projKM) 

sett_subset <- sett_all %>% 
  mutate(year = lubridate::year(DATE_OF_FIELD_VISIT)) %>% 
  filter(year > 2018) %>% 
  filter(MAIN_SETT == "Yes") %>%
  filter(CAPTURE_BLOCK_ID %!in% c("NOT ASSIGN", "NOT ASSIGNE")) %>% 
  select(SETT_ID, geometry)


png("Outputs/ESA_Figures/Effort_and_setts.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = effort %>% filter(NDAYS > 0), 
          aes(col = NDAYS, fill = NDAYS)) + 
  geom_sf(data = sett_subset, size = 1, shape = 21, 
          col = "black", fill = "orange") + 
  scale_fill_viridis_c(na.value = NA, trans = "log", 
                       breaks = c(10, 30, 75, 200, 400),
                       labels = round(c(10, 30, 75, 200, 400), 1)) + 
  scale_color_viridis_c(na.value = NA, trans = "log", 
                        breaks = c(10, 30, 75, 200, 400),
                        labels = round(c(10, 30, 75, 200, 400), 1)) +
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  labs(title = "Main setts", fill = "Number of days", color = "Number of days") + 
  theme_bw()
dev.off()

### badgers ####

badgers <- readRDS(file = "Data/badgers_jittered_filtered.RDS") %>% 
  mutate(YEAR = year(DATE_CAUGHT)) %>% 
  filter(YEAR > 2018)

png("Outputs/ESA_Figures/badgers_programme.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  geom_sf(data = badgers, shape = 21, col = "black", size = 1,
          aes(fill = PROGRAMME)) + 
  labs(title = "Badgers captured", fill = "Programme") + 
  theme_bw()
dev.off()

png("Outputs/ESA_Figures/badgers_culling.jpg", width = 7.46, height = 7.79, res = 600, units = "in")
ggplot() + 
  geom_sf(data = ireland_counties, col = "black", fill = NA) + 
  geom_sf(data = badgers %>% filter(PROGRAMME == "Culling"), shape = 21, col = "black", size = 1,
          aes(fill = PROGRAMME)) + 
  labs(title = "Badgers captured", fill = "Programme") + 
  theme_bw()
dev.off()

## Covariates

## Sett predictions ####

png("Outputs/ESA_Figures/sett_prediction.jpg", width = 7.46*2, height = 7.79, 
    res = 600, units = "in")
ggplot() + 
  gg(data = x, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) + 
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  labs(x = "", y = "", fill = "Median", 
       title = "Main sett distribution (linear scale)") +  
  theme_bw() + 
  scale_fill_viridis_c(option = "A") +
  NULL + 
  
ggplot() + 
  gg(data = spb, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_counties, fill = NA) +
  # geom_sf(data = sett_subset, alpha = 0.5, size = 1) +
  theme_bw() + 
  # scale_fill_viridis_c(option = "A") +
  scale_fill_distiller(palette = 'RdBu') + 
  labs(x = "", y = "", fill = "Median", title = "Spatial random field")   
dev.off()


## Sett covariate effect ####
png("Outputs/ESA_Figures/sett_covars_effect.jpg", height = 10, width = 8.5, res = 600, units = "in")
ggplot() + 
  gg(data = elev_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Elevation effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = slope_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Slope effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = swf_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Small woody features effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = grass_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Pasture and grasslands effect", x = "", y = "", fill = "Median") +
  
ggplot() + 
  gg(data = shrub_df, aes(fill = q0.5), geom = "tile") +
  geom_sf(data = ireland_outline_sf, fill = NA) +
  theme_bw() + 
  scale_fill_distiller(palette = 'RdBu', direction = 1) + 
  labs(title = "Transitional woodland and shrub", x = "", y = "", fill = "Median") +

plot_layout(ncol = 2)
dev.off()

## evaluate effects ####
png("Outputs/ESA_Figures/sett_covars_eval.jpg", height = 9, width = 11, res = 600, units = "in")
multiplot(
  eval.elev,
  eval.swf,
  eval.shrub, 
  eval.slope,
  eval.grasslandsPastures,
  cols = 2
)
dev.off()
