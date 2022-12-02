library(tidyverse)
library(metafor)

setwd("H:/Shared drives/MC - Chapter one manuscript/microcystin_cycle")
litdat = read.csv("inverseVariance_poolFlux.csv")
# https://www.metafor-project.org/doku.php/tips:weights_in_rma.mv_models

#Regress mean vs. SD for all recorded values in the data set
sd_regression = lm(litdat$sd ~ litdat$mean)

#Estimate SD using the regression from above for missing SD
litdatSD = litdat %>%
  mutate(sd = case_when(is.na(.$sd) ~ (mean*sd_regression$coefficients[2] - sd_regression$coefficients[1]), 
                        TRUE ~ sd)) %>%
  mutate(n = as.numeric(n)) %>%
  #Replace missing n values with 1
  mutate(n = case_when(is.na(.$n) ~ 1, TRUE ~ n))


# Aquatic Consumer Pools and Fluxes =================================================
# invertebrate pools
# vertebrate pools
# biotransformation rate

# SEDIMENT ==========================================================================
# Bulk Sediment
sedimentPool = litdatSD %>% filter(type == "pool" & fluxPool == "bulkSediment")
sedimentPool.res.ee <- rma(mean, sd, data=sedimentPool, method="REML") 

# Sediment Sorption Capacity
sorptionPool = litdatSD %>% filter(type == "pool" & fluxPool == "sorptionCapacity")
sorptionPool.res.ee <- rma(mean, sd, data=sorptionPool, method="REML") 

# Biofilm 
biofilmPool = litdatSD %>% filter(type == "pool" & group == "biofilm")
biofilmPool.res.ee <- rma(mean, sd, data=biofilmPool, method="REML") 

#Sediment Biodegradation
sedbiodeg = litdatSD %>% filter(group == 'bulkSediment' & fluxPool == 'biodegradation')
sedbiodeg.res.ee <- rma(mean, sd, data=sedbiodeg, method="REML") 

#Burial
burial = litdatSD %>% filter(fluxPool == 'burial')

#Diffusion
diffusion = litdatSD %>% filter(fluxPool == 'diffusion')

# Macrophyte Pools and Fluxes =======================================================
# macrophyte pool
macPool = litdatSD %>% filter(type == "pool" & fluxPool == "macrophyteTissue")
macPool.res.ee <- rma(mean, sd, data = macPool, method="REML") 

# epiphyte pool 
epiPool = litdatSD %>% filter(type == "pool" & fluxPool == "epiphyte")
epiPool.res.ee <- rma(mean, sd, data = epiPool, method="REML") 

# macrophyte uptake rate
macUptake = litdatSD %>% filter(type == "flux" & majorGroup =="macrophyte" & fluxPool == "uptake")
macUptake.res.ee <- rma(mean, sd, data = macUptake, method="REML") 

# macrophyte biotransformation rate
macDetox = litdatSD %>% filter(type == "flux" & majorGroup =="macrophyte" & fluxPool == "biotransformation")
macDetox.res.ee <- rma(mean, sd, data = macDetox, method="REML") 

# Consumer Pools ============================================================
zoopPool = litdatSD %>% filter(type == "pool" & fluxPool == "zooplankton")
zoopPool.res.ee <- rma(mean, sd, data = zoopPool, method="REML") 

invertPool = litdatSD %>% filter(type == "pool" & fluxPool == "macroinvert")
invertPool.res.ee <- rma(mean, sd, data = invertPool, method="REML") 

fishPool = litdatSD %>% filter(type == "pool" & fluxPool == "fish")
fishPool.res.ee <- rma(mean, sd, data = fishPool, method="REML") 

# Misc Pools and Rates ============================================================
aerosolPool = litdatSD %>% filter(type == "pool" & fluxPool == "aerosol")
aerosolPool.res.ee <- rma(mean, sd, data = aerosolPool, method="REML") 

waterbiodeg = litdatSD %>% filter(group == 'waterColumn' & fluxPool == 'biodegradation')
waterbiodeg.res.ee <- rma(mean, sd, data=waterbiodeg, method="REML")

#=================================================
#=================================================
# SEDIMENT PANEL
windows(height = 6, width = 4.5)
par(mfrow=c(2,1), mai = c(0.15,1.7,0.1,0.1), omi = c(0.25,0.05,0.05,0.05))

plot(c(log10(sedimentPool.res.ee$beta), 
       log10(sorptionPool.res.ee$beta), 
       log10(biofilmPool.res.ee$beta),
       log10(macPool.res.ee$beta), 
       log10(epiPool.res.ee$beta),
       log10(zoopPool.res.ee$beta),
       log10(invertPool.res.ee$beta),
       log10(fishPool.res.ee$beta)),
     c(1,2,3,4,5,6,7,8), pch = 15, cex = 1.5, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
     xlim = c(-3.5,2.5), ylim = c(0.5,8.5),
     col = c(rgb(108,88,76, max = 255, alpha = 180), 
             rgb(108,88,76, max = 255, alpha = 180), 
             rgb(108,88,76, max = 255, alpha = 180),
             rgb(1, 102, 94, max = 255, alpha = 180), 
             rgb(1, 102, 94, max = 255, alpha = 180),
             rgb(54, 144, 192, max = 255, alpha = 180),
             rgb(54, 144, 192, max = 255, alpha = 180),
             rgb(54, 144, 192, max = 255, alpha = 180)))
axis(side = 1, at = c(log10(0.001), log10(0.002), log10(0.003), log10(0.004), log10(0.005),
                      log10(0.006), log10(0.007), log10(0.008), log10(0.009),
                      log10(0.01), log10(0.02), log10(0.03), log10(0.04), log10(0.05),
                      log10(0.06), log10(0.07), log10(0.08), log10(0.09),
                      log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5),
                      log10(0.6), log10(0.7), log10(0.8), log10(0.9),
                      log10(1), log10(2), log10(3), log10(4), log10(5),
                      log10(6), log10(7), log10(8), log10(9),
                      log10(10), log10(20), log10(30), log10(40), log10(50),
                      log10(60), log10(70), log10(80), log10(90), log10(100),log10(200)),
     labels = c("","","","","","","","","",
                "","","","","","","","","",
                "","","","","","","","","",
                "","","","","","","","","",
                "","","","","","","","","","",""), cex.axis = 0.8)
axis(side = 2, at = c(1,2,3,4,5,6,7,8), las = 2, cex.axis = 0.8,
     labels = c("Sediment Pool", "Sediment Sorption", "Biofilm Pool", "Macrophyte Pool",
                "Epiphyte Pool", "Zooplankton", "Macroinvertebrates", "Fish"))
mtext(side = 2, line = 7.5, "POOLS (µg g-1 d.w.)", font = 2)
lines(c(log10(0.0015), log10(0.0015)), c(-1,9), lty=3)
text(log10(0.0005),3, "Confidence Interval <0", srt = 90, cex = 0.8, font = 3)

arrows(c(log10(0.001), #sediment pool, lower CI < 0
         log10(0.001), #sorption capacity, lower CI < 0
         log10(0.001), #biofilm pool, lower CI < 0
         log10(0.001), #macrophyte pool, lower CI < 0
         log10(epiPool.res.ee$ci.lb),
         log10(zoopPool.res.ee$ci.lb),
         log10(invertPool.res.ee$ci.lb),
         log10(0.001)), #fish pool, lower CI < 0
       c(1,2,3,4,5,6,7,8),
       x1 = c(log10(sedimentPool.res.ee$ci.ub),
              log10(sorptionPool.res.ee$ci.ub), 
              log10(biofilmPool.res.ee$ci.ub),
              log10(macPool.res.ee$ci.ub), 
              log10(epiPool.res.ee$ci.ub),
              log10(zoopPool.res.ee$ci.ub),
              log10(invertPool.res.ee$ci.ub),
              log10(fishPool.res.ee$ci.ub)),
       y1 = c(1,2,3,4,5,6,7,8),
       col = c(rgb(108,88,76, max = 255), 
               rgb(108,88,76, max = 255), 
               rgb(108,88,76, max = 255),
               rgb(1, 102, 94, max = 255), 
               rgb(1, 102, 94, max = 255),
               rgb(54, 144, 192, max = 255),
               rgb(54, 144, 192, max = 255),
               rgb(54, 144, 192, max = 255)),
       angle = 0, length = 0, lwd = 3)
text(c(log10(sedimentPool.res.ee$beta), 
       log10(sorptionPool.res.ee$beta), 
       log10(biofilmPool.res.ee$beta),
       log10(macPool.res.ee$beta), 
       log10(epiPool.res.ee$beta),
       log10(zoopPool.res.ee$beta),
       log10(invertPool.res.ee$beta),
       log10(fishPool.res.ee$beta)) + 0.1,
     c(1,2,3,4,5,6,7,8)+0.5,
     c("n=12", "n=3", "n=3", 
       "n=6", "n=4*", 
       "n=2*", "n=10*", "n=25"), cex = 0.8)

# Fluxes
plot(c(log10(waterbiodeg.res.ee$beta),
          log10(sedbiodeg.res.ee$beta),
          log10(burial$mean), 
          log10(diffusion$mean),
          log10(macUptake.res.ee$beta),
          log10(macDetox.res.ee$beta)),
     c(1,2,3,4,5,6), pch = 15, cex = 1.5, xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", xlim = c(-3.5,2.5), ylim = c(0.5,6.5),
     col = c(rgb(90, 90, 90, max = 255, alpha = 150), 
             rgb(214, 159, 126, max = 255, alpha = 150), 
             rgb(214, 159, 126, max = 255, alpha = 150),
             rgb(214, 159, 126, max = 255, alpha = 150),
             rgb(90, 180, 172, max = 255, alpha = 180),
             rgb(90, 180, 172, max = 255, alpha = 180))) 
axis(side = 1, at = c(log10(0.001), log10(0.002), log10(0.003), log10(0.004), log10(0.005),
                      log10(0.006), log10(0.007), log10(0.008), log10(0.009),
                      log10(0.01), log10(0.02), log10(0.03), log10(0.04), log10(0.05),
                      log10(0.06), log10(0.07), log10(0.08), log10(0.09),
                      log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5),
                      log10(0.6), log10(0.7), log10(0.8), log10(0.9),
                      log10(1), log10(2), log10(3), log10(4), log10(5),
                      log10(6), log10(7), log10(8), log10(9),
                      log10(10), log10(20), log10(30), log10(40), log10(50),
                      log10(60), log10(70), log10(80), log10(90), log10(100),log10(200)),
     labels = c("0.001","","","","","","","","",
                "0.01","","","","","","","","",
                "0.1","","","","","","","","",
                "1","","","","","","","","",
                "10","","","","","","","","","100",""), cex.axis = 0.8)

arrows(c(log10(waterbiodeg.res.ee$ci.lb),
         log10(sedbiodeg.res.ee$ci.lb), 
         log10(0.001), #burial, lower CI<0
         log10(diffusion$mean-diffusion$sd), 
         log10(0.001),
         log10(0.001)), #macrophyte uptake, lower CI<0
       c(1,2,3,4,5,6),
       x1 = c(log10(waterbiodeg.res.ee$ci.ub),
              log10(sedbiodeg.res.ee$ci.ub), 
              log10(burial$mean + burial$sd), 
              log10(diffusion$mean+diffusion$sd), 
              log10(macUptake.res.ee$ci.ub),
              log10(macDetox.res.ee$ci.ub)),
       y1 = c(1,2,3,4,5,6),
       col = c(rgb(90, 90, 90, max = 255, alpha = 150), 
               rgb(214, 159, 126, max = 255, alpha = 150), 
               rgb(214, 159, 126, max = 255, alpha = 150),
               rgb(214, 159, 126, max = 255, alpha = 150),
               rgb(90, 180, 172, max = 255, alpha = 180),
               rgb(90, 180, 172, max = 255, alpha = 180)),
       angle = 0, length = 0, lwd = 3)

text(c(log10(waterbiodeg.res.ee$beta),
       log10(sedbiodeg.res.ee$beta),
       log10(burial$mean), 
       log10(diffusion$mean),
       log10(macUptake.res.ee$beta),
       log10(macDetox.res.ee$beta)) + 0.1,
     c(1,2,3,4,5,6)+0.5,
     c("n=8*", "n=8*", "n=1", 
       "n=1*", "n=1", "n=1"), cex = 0.8)
axis(side = 2, at = c(1,2,3,4,5,6), las = 2, cex.axis = 0.8,
     labels = c("Water Biodegradation\n(t0.5, days)", "Sed Biodegradation\n(t0.5, days)",
                "Sediment Burial\n(µg m-2 d-1)", "Sediment Diffusion\n(µg m-2 d-1)",
                "Macrophyte Uptake\n(µg g-1 d.w. d-1)", 
                "Macrophyte Depuration\n(µg g-1 d.w. d-1)"))
mtext(side = 2, line = 7.5, "FLUXES", font = 2)
lines(c(log10(0.0015), log10(0.0015)), c(-1,9), lty=3)
text(log10(0.0005),2.3, "Confidence Interval <0", srt = 90, cex = 0.8, font = 3)
