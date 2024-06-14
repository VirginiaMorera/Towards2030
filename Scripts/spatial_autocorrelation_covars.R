rm(list = ls())
source("Scripts/setup.R")

# load environmental data
# env_vars <- stack("Data/Covars/all_covars_1km.grd")
env_vars <- stack("Data/all_covars_1km_smooth5.grd")

# select variables we're actually using in the model
env_vars_sub <- subset(env_vars, 
                       subset = c("elevation", "slope", "eastness", 
                                  "northness", "tree_cover_density", 
                                  "small_woody_features", 
                                  "Plantedvegetationandcrops", 
                                  "Pastures", "Artificialsurfaces", 
                                  "Naturalgrasslands", "Moorsandheathland", 
                                  "Transitionalwoodland.shrub", "Peatbogs"))
x11()
par(mfrow = c(4,4))

for(i in 1:nlayers(env_vars_sub)) {
  n.values <- getValues(env_vars[[i]])
  range.n.values <- range(n.values, na.rm = T)
  h<-hist(n.values, breaks = seq(range.n.values[1], range.n.values[2], length = 40), 
          col=rev(topo.colors(40)), xlab = names(env_vars)[i],
          main="",
          xlim = range.n.values, 
          #ylim=c(0,0.001599), 
          freq=FALSE) 
  xfit<-seq(range.n.values[1], range.n.values[2], length=100) 
  yfit<-dnorm(xfit,mean=mean(n.values),sd=sd(n.values)) 
  lines(xfit, yfit, col="black", lwd=2)
  # boxplot(n.values, horizontal=TRUE,  outline=TRUE,  axes=FALSE,at=0.05,
  #         ylim=range.n.values, col = "white", add = TRUE, boxwex = 0.1)
  
}
par(mfrow = c(1,1))

x11()
par(mfrow = c(4,4))

for(i in 1:nlayers(env_vars_sub)) {
  n.variogram <- Variogram(env_vars[[i]], cutoff = 50, lag = 1)
  plot(n.variogram, main = names(env_vars)[i], col = "blue")
}
par(mfrow = c(1,1))



pr <- patternogram(terra::rast(env_vars$eastness), cutoff = 250, cloud = F)
plot(pr)
