# 0. house keeping ####
rm(list = ls())
source("Scripts/setup.R")

ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS") %>% 
  st_transform(crs = projKM)
# 1. Clean badger dataset ####

# badgers_all <- readRDS("Data/badgers_jittered_filtered_2025.RDS") 

badgers_all <- readRDS("Data/badgers_setts_2025.RDS")

badgers_clean <- badgers_all %>% 
  filter(
    # PROGRAMME == "Vaccination", 
    !is.na(WEIGHT),
    WEIGHT < 20,
    WEIGHT > 4,
    AGE %!in% c("Cub", "Juvenile"),
    !is.na(SEX), 
    !is.na(MONTH), 
    YEAR %in% c(2019, 2020, 2021, 2022, 2023, 2024, 2025))



ggplot(badgers_clean) + 
  geom_boxplot(aes(x = MONTH, y = WEIGHT, fill = PROGRAMME)) + 
  scale_x_discrete(breaks=seq(1, 12, 1), 
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", 
                              "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "Month of capture", y = "Weight (kg)", fill = "Programme") + 
  facet_wrap(~SEX) + 
  ggtitle("Weight by month")  +
  theme_bw()


ggplot(badgers_clean)  + 
  geom_histogram(aes(x = WEIGHT, col = SEX, fill = SEX), alpha = 0.5, binwidth = 1) + 
  theme_bw() + 
  facet_wrap(~SEX) +
  NULL

badgers_clean %>%
  group_by(SEX) %>%
  summarise(mean = mean(WEIGHT, na.rm = T), 
            sd = sd(WEIGHT, na.rm = T)) 

badgers_clean <- badgers_clean %>% 
  select(SETT_ID, BADGER_ID, AGE, WEIGHT, SEX, DATE, MONTH, PROGRAMME) 


# 2. Extract model predictions ####

sett_pred <- readRDS("Outputs/sett_model/response_predictor.RDS")

inside = sapply(st_intersects(sett_pred$all, ireland_outline_sf), function(x){length(x)==0})
sett_pred <- sett_pred$all[!inside,]

sett_pred2 <- sett_pred %>% 
  mutate(x = st_coordinates(.)[,1], 
         y = st_coordinates(.)[,2]) %>% 
  select(x, y, z = q0.5) %>% 
  st_drop_geometry()
  
sett_rast <- rast(sett_pred2, type="xyz", crs = crs(sett_pred))

plot(sett_rast)

locs_sett <- terra::extract(sett_rast, badgers_clean)


badger_pred <- readRDS("Outputs/badgers_all_model/response_predictor.RDS")

inside = sapply(st_intersects(badger_pred$all, ireland_outline_sf), function(x){length(x)==0})
badger_pred <- badger_pred$all[!inside,]

badger_pred2 <- badger_pred %>% 
  mutate(x = st_coordinates(.)[,1], 
         y = st_coordinates(.)[,2]) %>% 
  select(x, y, z = q0.5) %>% 
  st_drop_geometry()

badger_rast <- rast(badger_pred2, type="xyz", crs = crs(sett_pred))

plot(badger_rast)

locs_badger <- extract(badger_rast, badgers_clean)

group_rast <- badger_rast/sett_rast

locs_group <- extract(group_rast, badgers_clean)

badgers_clean <- badgers_clean %>% 
  mutate(B_DENSITY = locs_badger$z,
         S_DENSITY = locs_sett$z,
         GROUP_SIZE = locs_group$z,
         x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         SEX = as.factor(SEX)) %>%
  select(WEIGHT, MONTH, B_DENSITY, S_DENSITY, GROUP_SIZE, x, y, SEX, PROGRAMME) %>%
  mutate(MONTH = as.numeric(MONTH), 
         PROGRAMME = as.factor(PROGRAMME)) %>% 
  st_drop_geometry() %>%
  as.data.frame()

str(badgers_clean)


names(badgers_clean)

badgers_clean %>% 
  group_by(SEX, PROGRAMME) %>% 
  summarise(mean = mean(WEIGHT), 
            sd = sd(WEIGHT))

# saveRDS(badgers_clean, file = "Data/badgers_for_gam.RDS")

# 3. GAM ####

# badgers_clean_1 <- readRDS("Data/badgers_for_gam.RDS")

badgers_clean <- badgers_clean %>% drop_na()

m1 <- gam(formula = WEIGHT ~ s(MONTH, bs = "cc", k = 12) + 
            s(B_DENSITY, S_DENSITY, k = 15) +
            s(GROUP_SIZE, k = 9) +
            s(x, y, k = 29) +
            # B_DENSITY + S_DENSITY + 
            # PROGRAMME +
            SEX, 
          data = badgers_clean, 
          select = TRUE,
          reml = T,
          # family = "gaussian")
          family = Gamma(link=log))

# saveRDS(m1, file = "Outputs/gam_model.RDS")
summary(m1)


## evaluate model ####
residuals_m1 <- residuals(m1)
spatial_data <- data.frame(x = badgers_clean$x, y = badgers_clean$y, residuals = residuals_m1)
spatial_data <- spatial_data %>% 
  st_as_sf(coords = c("x", "y"))
variogram_obj <- variogram(residuals ~ 1, data = spatial_data)
plot(variogram_obj, main = "Empirical Variogram of GAM Residuals")

appraise(m1)
gam.check(m1)

## Visualise ####
draw(m1)
draw(parametric_effects(m1))


# add partial residuals to data
badgers_clean <- badgers_clean  %>% 
  add_partial_residuals(m1)

sm <- smooth_estimates(m1)  %>% 
  add_confint()


sm_spatial <- sm %>% 
  filter(.smooth == "s(x,y)") 


ggplot() + 
  geom_tile(data = sm_spatial, 
            aes(x = x, y = y, fill = .estimate)) + 
  scale_fill_distiller(palette = 'RdBu') +
  geom_point(data = badgers_clean, aes(x = x, y = y), alpha = 0.1)



sm_spatial <- sm_spatial %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(2157) %>% 
  st_transform(st_crs(ireland_counties))

resid_sp <- badgers_clean %>% 
  select(x, y, resid = `s(B_DENSITY,S_DENSITY)`) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(2157) %>% 
  st_transform(st_crs(ireland_counties))

ggplot() + 
  geom_sf(data = sm_spatial, aes(col = .estimate), shape = 15, size = 2.5) + 
  geom_sf(data = ireland_counties, fill = NA) + 
  geom_sf(data = resid_sp, aes(size = abs(resid)), alpha = 0.1) +
  scale_color_distiller(palette = 'RdBu') +
  scale_size(range = c(0, 2)) + 
  theme_bw()

draw(m1, select = "s(B_DENSITY,S_DENSITY)") + 
  theme_bw() + coord_equal()
