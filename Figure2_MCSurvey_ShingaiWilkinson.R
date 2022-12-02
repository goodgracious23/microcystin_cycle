library(tidyverse)
WaterColumnDetection_Surveys <- read_csv("H:/Shared drives/MC - Chapter one manuscript/microcystin_cycle/WaterColumnDetection_Surveys.csv")
detect = WaterColumnDetection_Surveys

pdf(file = "Figure2_23Nov2022.pdf",
    width = 6.5, height = 2.5,
    useDingbats = FALSE)

windows(height = 2.5, width = 6.5)
par(mfrow = c(1,3), omi = c(0.1,0.25,0.1,0.1), mai = c(0.6,0.3,0.1,0.1))
plot(detect$PercentDetection, ylim = c(0,100), 
     xlab = "", ylab = "",
     pch = 19, col = detect$DetectColors, cex = 1.25, las = 2)
mtext(side = 2, line = 2.5, "% Detected")
mtext(side = 1, line = 3, "Survey Rank", cex = 0.9)
legend("bottomright", legend = c("% of waterbodies", "% of samples"),
       col = c("skyblue4", "aquamarine3"), pch = 15, pt.cex = 2,
       inset = 0.0, cex = 1, bty = "n")

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
mtext(side = 1, line = 3.75, "Waterbodies in \neach Survey", cex = 0.9)
surveylm = lm(detect$PercentDetection ~ log10(detect$WaterbodiesSampled))
summary(surveylm)
abline(surveylm, lty = 3)

survey = detect %>% filter(!SurveyStyle=="mix")
boxplot(survey$PercentDetection~survey$SurveyStyle, 
        xlab = "", ylab = "", ylim = c(0,100), las = 2, names = FALSE)
mtext(side = 1, line = 1.2, "Repeated                  ", cex = 0.8)
mtext(side = 1, line = 1.2, "                 Snapshot", cex = 0.8)
mtext(side = 1, line = 3.5, "Survey Design", cex = 0.9)
summary(aov(survey$PercentDetection~survey$SurveyStyle))

boxplot(survey$WaterbodiesSampled~survey$SurveyStyle, 
        xlab = "", ylab = "",  las = 2, names = FALSE)
summary(aov(survey$WaterbodiesSampled~survey$SurveyStyle))
# dev.off()
