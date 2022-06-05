library(tidyverse)
pools_rates = read.csv("mc_pools_fluxes.csv")
inverts = pools_rates %>% 
  filter(majorGroup == "invertebrate") %>%
  filter(!tissue=="reproductive", 
         !tissue=="gland",
         !tissue=="hepatopancreas",
         !tissue=="intestine")
abiotic = pools_rates %>% 
  filter(tissue=='abiotic') %>%
  filter(!(is.na(position)))
verts = pools_rates %>% filter(majorGroup=="vertebrate")
  
# setting up the plot ============================
windows(height = 9, width = 6.5)
par(omi = c(0.1,0.1,0.1,0.1), mai = c(0.75,1.25,0,0))

#Vertebrates
plot(verts$position ~ log10(verts$begin), 
     yaxt = 'n', xaxt = 'n', xlab = "", ylab = "",
     xlim = c(log10(0.00007),log10(2000)), ylim = c(0,104),
     pch = 15, cex = 0.65, col = verts$tissueColor, bty = 'n')

polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(0,0,13,13), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(14,14,18,18), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(19,19,34,34), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(35,35,40,40), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(41,41,55,55), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(56,56,70,70), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(71,71,75,75), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(76,76,80,80), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(81,81,84,84), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(85,85,91,91), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(92,92,95,95), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(96,96,100,100), col = "gray90", border = NA)
polygon(c(log10(0.00007),log10(2000), log10(2000), log10(0.00007)),
        c(101,101,104,104), col = "gray90", border = NA)

points(verts$position ~ log10(verts$begin), 
       pch = 19, cex = 0.65, col = verts$tissueColor)
points(verts$position ~ log10(verts$end), 
       pch = 19, cex = 0.65, col = verts$tissueColor)
arrows(y0 = verts$position, x0 = log10(verts$begin), 
       y1 = verts$position, x1 = log10(verts$end),
       angle = 0, length = 0, lwd = 3,
       col = verts$tissueColor)

# All Invertebrates ============================
points(inverts$position ~ log10(inverts$begin), 
       pch = 19, cex = 0.65, col = inverts$tissueColor)
points(inverts$position ~ log10(inverts$end), 
       pch = 19, cex = 0.65, col = inverts$tissueColor)
arrows(y0 = inverts$position, x0 = log10(inverts$begin), 
       y1 = inverts$position, x1 = log10(inverts$end),
       angle = 0, length = 0, lwd = 3,
       col = inverts$tissueColor)

# Abiotic========================================
points(abiotic$position ~ log10(abiotic$begin), 
     yaxt = 'n', xaxt = 'n', xlab = "", ylab = "",
     pch = 19, cex = 0.65, col = abiotic$tissueColor)
points(abiotic$position ~ log10(abiotic$begin), 
       pch = 19, cex = 0.65, col = abiotic$tissueColor)
points(abiotic$position ~ log10(abiotic$end), 
       pch = 19, cex = 0.65, col = abiotic$tissueColor)
arrows(y0 = abiotic$position, x0 = log10(abiotic$begin), 
       y1 = abiotic$position, x1 = log10(abiotic$end),
       angle = 0, length = 0, lwd = 3,
       col = abiotic$tissueColor)
axis(side = 1, 
     at = c(log10(0.0001), log10(0.0002), log10(0.0003), log10(0.0004), 
            log10(0.0005), log10(0.0006), log10(0.0007), log10(0.0008), 
            log10(0.0009), log10(0.001), log10(0.002), log10(0.003), 
            log10(0.004), log10(0.005), log10(0.006), log10(0.007), 
            log10(0.008), log10(0.009), log10(0.01), log10(0.02), 
            log10(0.03), log10(0.04), log10(0.05), log10(0.06), log10(0.07),
            log10(0.08), log10(0.09), log10(0.1), log10(0.2), log10(0.3),
            log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8),
            log10(0.9), log10(1), log10(2), log10(3), log10(4), log10(5),
            log10(6), log10(7), log10(8), log10(9), log10(10), log10(20),
            log10(30), log10(40), log10(50), log10(60), log10(70), 
            log10(80), log10(90), log10(100), log10(200), log10(300), 
            log10(400), log10(500), log10(600), log10(700), log10(800),
            log10(900), log10(1000), log10(2000)),
     labels = c("0.0001","","","","","","","","",
                "0.001","","","","","","","","",
                "0.01","","","","","","","","",
                "0.1","","","","","","","","",
                "1","","","","","","","","",
                "10","","","","","","","","",
                "100","","","","","","","","",
                "1000",""), las = 2)

