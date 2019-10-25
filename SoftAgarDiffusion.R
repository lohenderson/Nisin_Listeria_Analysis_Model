library(dplyr)
library(agricolae)

sad <- read.csv("~/Desktop/Nisin_FollowUp/SoftAgarDiffusion2.csv")
#sad$pH <- as.factor(sad$pH)
sad_summary <- sad %>%
  group_by(pH) %>%
  summarize(mean_mm = mean(mm),
            sd_mm = sd(mm))
aov_sad <- aov(mm ~ pH, data = sad)
summary(aov_sad)

a <- HSD.test(aov_sad, "pH", group = TRUE)
print(a) 