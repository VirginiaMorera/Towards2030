# 0. house keeping ####
rm(list = ls())
source("Scripts/setup.R")

ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS") %>% 
  st_transform(crs = projKM)
# 1. Clean badger dataset ####

badgers_all <- readRDS("Data/badgers_jittered_filtered_2025.RDS") 
# sett_all <- readRDS("Data/sett_all_2025.RDS") %>% 
#   select(SETT_ID)
# 
# badgers_all <- left_join(badgers_all, sett_all) %>% 
#   st_as_sf(sf_column_name = "geometry")

badgers_clean <- badgers_all %>% 
  filter(
    # PROGRAMME == "Vaccination", 
    !is.na(WEIGHT),
    WEIGHT < 20,
    # WEIGHT > 4,
    AGE %!in% c("Cub", "Juvenile"),
    !is.na(SEX), 
    !is.na(MONTH))

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

badgers_clean <- badgers_clean %>% 
  select(SETT_ID, BADGER_ID, AGE, WEIGHT, SEX, DATE, MONTH) 


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

cull <- rast("Data/Covars/culling_history.grd")
locs_culling <- extract(cull, badgers_clean)


badgers_clean <- badgers_clean %>% 
  mutate(B_DENSITY = locs_badger$z,
         S_DENSITY = locs_sett$z,
         CULL_HISTORY = locs_culling$z,
         x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2],
         SEX = as.factor(SEX)) %>%
  select(WEIGHT, MONTH, B_DENSITY, S_DENSITY, CULL_HISTORY, x, y, SEX) %>%
  mutate(MONTH = as.numeric(MONTH)) %>% 
  st_drop_geometry() %>%
  as.data.frame()

str(badgers_clean)


names(badgers_clean)

badgers_clean %>% 
  group_by(SEX) %>% 
  summarise(mean = mean(WEIGHT), 
            sd = sd(WEIGHT))

saveRDS(badgers_clean, file = "Data/badgers_for_gam.RDS")



# 3. GAM ####

badgers_clean <- readRDS("Data/badgers_for_gam.RDS")

badgers_clean <- badgers_clean %>% drop_na()

m1 <- gam(formula = WEIGHT ~ s(MONTH, bs = "cc", k = 12) + 
            s(B_DENSITY, S_DENSITY, k = 12) + 
            # s(CULL_HISTORY, k = 5) +
            s(x, y, k = 29) +
            SEX, 
          data = badgers_clean, 
          select = TRUE,
          reml = T,
          # family = "gaussian")
          family = Gamma(link=log))

# saveRDS(m1, file = "Outputs/gam_model.RDS")
summary(m1)
m1Viz <- getViz(m1, nsim = 100)

# Diagnose 

check(m1Viz,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 50))


residuals_m1 <- residuals(m1)
spatial_data <- data.frame(x = badgers_clean$x, y = badgers_clean$y, residuals = residuals_m1)
spatial_data <- spatial_data %>% 
  st_as_sf(coords = c("x", "y"))
variogram_obj <- variogram(residuals ~ 1, data = spatial_data)
plot(variogram_obj, main = "Empirical Variogram of GAM Residuals")

# extra checking from mgcViz 
ck1 <- check1D(m1Viz, "B_DENSITY")
ck2 <- check1D(m1Viz, "S_DENSITY")
ck3 <- check1D(m1Viz, "x")
ck4 <- check1D(m1Viz, "y")
ck5 <- check1D(m1Viz, "SEX")
ck6 <- check1D(m1Viz, "MONTH")

gridPrint(ck1 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2) + 
            labs(x = "Badger density"), 
          ck2 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2) + 
            labs(x = "Sett density"), 
          ck3 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2) + 
            labs(x = "Easting"), 
          ck4 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2) + 
            labs(x = "Northing"), ncol = 2)

# variance of the residuals higher at low x values, but otherwise not too bad 

gridPrint(ck3 + l_gridCheck1D(gridFun = sd, showReps = TRUE) + l_rug(alpha = 0.2), 
          ck4 + l_gridCheck1D(gridFun = sd, showReps = TRUE) + l_rug(alpha = 0.2), ncol = 2)
# weirdness at low latitudes, probably because not too much data in the southernmost latitudes (kerry-ish?) 

gridPrint(ck5 + l_points(), 
          ck6 + l_points(), ncol = 2)
# no concerns here other than very few data points in july august



ck7 <- check2D(m1Viz, x1 = "B_DENSITY", x2 = "S_DENSITY", type = "tnormal")
ck8 <- check2D(m1Viz, x1 = "x", x2 = "y", type = "tnormal")

gridPrint(ck7 + l_gridCheck2D(gridFun = mean), 
          ck8 + l_gridCheck2D(gridFun = mean), ncol = 2)
# looking at it in 2d no obvious pattern in residuals. Good! 


# Plot

print(plot(m1Viz, allTerms = T), pages = 1) # Calls print.plotGam()  

plot(sm(m1Viz, 1) ) + 
  l_fitLine(colour = "red") + 
  # l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + 
  theme_bw()

plot(sm(m1Viz, 2)) + 
  l_fitRaster() + 
  l_fitContour() + 
  l_points()

plot(sm(m1Viz, 3)) + 
  l_fitRaster() + 
  l_fitContour() + 
  l_points() 
  
plot(m1Viz, select = 4) + 
  theme_bw()

vis.gam(m1, view = c("B_DENSITY", "S_DENSITY"), theta = 30, n.grid = 50, 
        type = "response", color = "terrain", xlab = "Badger density", 
        ylab = "Sett density", zlab = "Effect")