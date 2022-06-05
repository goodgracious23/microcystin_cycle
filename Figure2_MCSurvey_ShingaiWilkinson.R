setwd("G:/Shared drives/MC - Chapter one manuscript")
detect = read.csv("WaterColumnDetection_Surveys.csv")

pdf(file = "Figure2_18Dec2021.pdf",
    width = 6.5, height = 3,
    useDingbats = FALSE)

# windows(height = 3, width = 6.5)
par(mfrow = c(1,2), omi = c(0.1,0.3,0.1,0.1), mai = c(0.8,0.5,0.1,0.1))
plot(detect$PercentDetection, ylim = c(0,100), 
     xlab = "", ylab = "",
     pch = 19, col = detect$DetectColors, cex = 1.25, las = 2)
mtext(side = 2, line = 2.5, "% Detected")
mtext(side = 1, line = 2.5, "Rank")
legend("bottomright", legend = c("% of waterbodies", "% of samples"),
       col = c("skyblue4", "aquamarine3"), pch = 15, pt.cex = 2,
       inset = 0.0, cex = 0.9, bty = "n")

plot(log10(detect$WaterbodiesSampled), detect$PercentDetection,
     ylim = c(0,100), xlab = "", ylab = "", las = 2,
     pch = 19, col = detect$DetectColors, cex = 1.25, xaxt = "n")
axis(side = 1, at = c(log10(10),log10(20),log10(30),log10(40),log10(50),
                      log10(60),log10(70),log10(80),log10(90),
                      log10(100),log10(200),log10(300),log10(400),log10(500),
                      log10(600),log10(700),log10(800),log10(900),
                      log10(1000)),
     labels = c("10","","","","","","","","","100",
                "","","","","","","","","1000"), las = 2)
mtext(side = 1, line = 2.5, "Waterbodies in Survey")
surveylm = lm(detect$PercentDetection ~ log10(detect$WaterbodiesSampled))
summary(surveylm)
abline(surveylm, lty = 3)

dev.off()