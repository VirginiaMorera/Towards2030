# 0. house keeping ####
rm(list = ls())
source("Scripts/setup.R")

# load ireland maps 
ireland_outline_sf <- readRDS("Data/Inla/ireland_outline_km.RDS") %>% 
  st_transform(crs = projKM) 

ireland_counties <- read_sf("Data/Other/Ireland_ITM.shp") %>% 
  st_transform(crs = projKM) 


# 1. Clean badger dataset ####

badgers_all <- readRDS("Data/badgers_jittered_filtered.RDS") %>% 
  st_transform(crs = projKM) 


badgers_clean <- badgers_all %>% 
  select(SETT_ID, BADGER_ID, AGE, WEIGHT, SEX, DATE_CAUGHT, PROGRAMME, geometry) %>% 
  filter(PROGRAMME == "Vaccination", 
         AGE == "Adult", 
         !is.na(WEIGHT), 
         WEIGHT < 20, 
         !is.na(SEX)) %>% 
  mutate(MONTH = month(DATE_CAUGHT))

ggplot(badgers_clean) + 
  geom_boxplot(aes(x = factor(MONTH), y = WEIGHT, fill = factor(MONTH))) + 
  theme_bw() + 
  scale_x_discrete(breaks=seq(1, 12, 1), 
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", 
                              "Sep", "Oct", "Nov", "Dec")) +
  scale_fill_discrete(breaks=seq(1, 12, 1), 
                      labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                 "Jul", "Aug", 
                                 "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "Month of capture", y = "Weight (kg)", fill = "Month of capture") + 
  facet_wrap(~SEX) + 
  ggtitle("Weight by month")

ggplot(badgers_clean)  + 
  geom_histogram(aes(x = WEIGHT, col = SEX, fill = SEX), alpha = 0.5, binwidth = 1) + 
  theme_bw() + 
  facet_wrap(~SEX) +
  NULL

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

locs_sett <- extract(sett_rast, badgers_clean)


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


badgers_clean <- badgers_clean %>% 
  mutate(B_DENSITY = locs_badger$z, 
         S_DENSITY = locs_sett$z, 
         x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) 


names(badgers_clean)


saveRDS(badgers_clean, file = "Data/badgers_for_gam.RDS")

# 3. GAM ####

badgers_clean <- readRDS("Data/badgers_for_gam.RDS")

badgers_clean <- badgers_clean %>% drop_na()

library(mgcv)
library(mgcViz)
library(rgl)

m1 <- gam(formula = WEIGHT ~ s(MONTH, bs = "cc", k = 12) + 
            # s(B_DENSITY, k = 50) + s(S_DENSITY, k = 50) +
            s(B_DENSITY, S_DENSITY, k = 10) + 
            s(x, y, k = 29) +
            SEX, 
          data = badgers_clean, 
          select = TRUE,
          reml = T,
          # family = "gaussian")
          family = Gamma(link=log))

summary(m1)
m1Viz <- getViz(m1, nsim = 100)

# Diagnose 

check(m1Viz,
      a.qq = list(method = "tnorm", 
                  a.cipoly = list(fill = "light blue")), 
      a.respoi = list(size = 0.5), 
      a.hist = list(bins = 50))


library(gstat)

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

gridPrint(ck1 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2), 
          ck2 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2), ncol = 2)
#variance of the residuals higher at extreme values of density for both setts and badgers

gridPrint(ck1 + l_gridCheck1D(gridFun = sd, showReps = TRUE) + l_rug(alpha = 0.2), 
          ck2 + l_gridCheck1D(gridFun = sd, showReps = TRUE) + l_rug(alpha = 0.2), ncol = 2)
# higher variance of residuals coincides with less data points, not much to do about it 

gridPrint(ck3 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2), 
          ck4 + l_dens(type = "cond", alpha = 0.8) + l_rug(alpha = 0.2), ncol = 2)
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



vis.gam(m1, view = c("B_DENSITY", "S_DENSITY"), theta = 40)




