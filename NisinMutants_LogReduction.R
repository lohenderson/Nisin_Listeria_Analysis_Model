library(dplyr)
library(agricolae)
library(emmeans)
library(lmerTest)

nm <- read.csv("~/Desktop/NisinMutants_LogReduction.csv")
nm$pH <- as.factor(nm$pH)
nm_summary <- nm %>%
  group_by(Strain, pH) %>%
  summarize(Count = mean(Reduction),
            sd_Reduction = sd(Reduction))
m1<-lm(Reduction~Strain*pH, data=nm)
aov_nm <- aov(m1, data = nm)
summary(aov_nm)


a <- HSD.test(aov_nm, "Strain", group = TRUE)
print(a)

em <- emmeans(m1, pairwise~ pH | Strain)
em