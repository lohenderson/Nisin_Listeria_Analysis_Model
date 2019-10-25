nisin <- read.csv("Cheese_Nisin_Masterfile.csv", colClasses = c("numeric",
                                                                          "factor",
                                                                          "numeric",
                                                                          "numeric",
                                                                          "numeric",
                                                                          "factor",
                                                                          "numeric",
                                                                          "factor",
                                                                          "numeric",
                                                                          "numeric",
                                                                          "factor",
                                                                          "factor",
                                                                          "factor"))
library(dplyr)
library(tidyr)
library(lmerTest)
library(lsmeans)
library(ggplot2)
library(gridExtra)
source("load_data.R")

##Temp Model##

nisin %>%
  filter(ph==6.5) %>%
  mutate(log_count=log10(count),
         log_milk_apc=log10(milk_apc+1),
         log_inoculum=log10(inoculum),
         temperature=as.factor(temperature),
         day=as.factor(day),
         ph=relevel(as.factor(ph),ref="6.5")) -> nisin_temperature


lmer(log_count ~ temperature*nisin + nisin*day + nisin*strain +
       log_inoculum +
       milk_age +
       log_milk_apc +
       #(1|milk_batch) +
       #(1|cheese_make:milk_batch),
       (1|cheese_plate:cheese_make:milk_batch) +
       (1|rep),
     data=nisin_temperature) -> m_temperature

# Full plot for temperature model
lsm_temperature <- summary(lsmeans(m_temperature,~nisin+temperature+day))
lsm_temperature %>%
  data.frame() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=lsmean,color=nisin)) +
  geom_point(position=position_dodge(1)) +
  labs(y="Least Square Means", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_errorbar(width=1,
                aes(ymin=lower.CL,ymax=upper.CL),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  ylim(5,11) +
  facet_grid(~temperature)

#Raw data of full plot above
nisin_temperature %>%
  filter(!is.na(log_count)) %>%
  group_by(day, temperature, nisin) %>%
  summarize(mean_log_count=mean(log_count),
            se_log_count=sd(log_count)/sqrt(n())) %>%
  ungroup() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=mean_log_count,color=nisin)) +
  labs(y="Average Log Count", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_point(position=position_dodge(1)) +
  geom_errorbar(width=1,
                aes(ymin=mean_log_count-2*se_log_count,
                    ymax=mean_log_count+2*se_log_count),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1),aes(group=nisin)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  ylim(3,11) +
  facet_grid(~temperature)

# Full temperature model (same as above) separated by strain
lsm_temperature_w_strain <- summary(lsmeans(m_temperature~nisin+temperature+day+strain))
lsm_temperature_w_strain %>%
  data.frame() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=lsmean,color=nisin)) +
  labs(y="Least Square Means", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_point(position=position_dodge(1)) +
  geom_errorbar(width=1,
                aes(ymin=lower.CL,ymax=upper.CL),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1),aes(group=nisin)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  ylim(3,11) +
  facet_grid(strain~temperature)

# Raw data of full temperature plot (separated by strain)
nisin_temperature %>%
  filter(!is.na(log_count)) %>%
  group_by(day, temperature, nisin, strain) %>%
  summarize(mean_log_count=mean(log_count),
            se_log_count=sd(log_count)/sqrt(n())) %>%
  ungroup() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=mean_log_count,color=nisin)) +
  labs(y="Average Log Count", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_point(position=position_dodge(1)) +
  geom_errorbar(width=1,
                aes(ymin=mean_log_count-2*se_log_count,
                    ymax=mean_log_count+2*se_log_count),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1),aes(group=nisin)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  ylim(3,11) +
  facet_grid(strain~temperature)


##pH Model##

nisin %>%
  filter(temperature==6) %>%
  mutate(log_count=log10(count),
         log_milk_apc=log10(milk_apc+1),
         log_inoculum=log10(inoculum),
         temperature=as.factor(temperature),
         day=as.factor(day),
         ph=relevel(as.factor(ph),ref="6.5")) -> nisin_ph


lmer(log_count ~ ph*nisin + nisin*day + nisin*strain +
       log_inoculum +
       milk_age +
       log_milk_apc +
       #(1|milk_batch) +
       #(1|cheese_make:milk_batch) +
       (1|cheese_plate:cheese_make:milk_batch) +
       (1|rep),
     data=nisin_ph) -> m_ph

# Full plot for pH model
lsm_ph <- summary(lsmeans(m_ph,~nisin+ph+day))
lsm_ph$ph_f<-factor(lsm_ph$ph, levels = c("5.5", "6", "6.5"))
lsm_ph %>%
  data.frame() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=lsmean,color=nisin)) +
  geom_point(position=position_dodge(1)) +
  labs(y="Least Square Means", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_errorbar(width=1,
                aes(ymin=lower.CL,ymax=upper.CL),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  facet_grid(~ph_f)

# Raw data of full plot above
lsm_ph <- summary(lsmeans(m_ph,~nisin+ph+day))
lsm_ph$ph_f<-factor(lsm_ph$ph, levels = c("5.5", "6", "6.5"))
lsm_ph %>%
  filter(!is.na(log_count)) %>%
  group_by(day, ph_f, nisin) %>%
  summarize(mean_log_count=mean(log_count),
            se_log_count=sd(log_count)/sqrt(n())) %>%
  ungroup() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=lsmean,color=nisin)) +
  geom_point(position=position_dodge(1)) +
  labs(y="Average Log Count", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_errorbar(width=1,
                aes(ymin=lower.CL,ymax=upper.CL),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  ylim(3,11) +
  facet_grid(~ph_f)

# Full ph model (same as above) separated by strain
lsm_ph_w_strain <- summary(lsmeans(m_ph,~nisin+ph+day+strain))
lsm_ph_w_strain$ph_f<-factor(lsm_ph_w_strain$ph, levels = c("5.5", "6", "6.5"))
lsm_ph_w_strain %>%
  data.frame() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=lsmean,color=nisin)) +
  geom_point(position=position_dodge(1)) +
  labs(y="Least Square Means", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_errorbar(width=1,
                aes(ymin=lower.CL,ymax=upper.CL),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1),aes(group=nisin)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  ylim(3,11) +
  facet_grid(strain~ph_f)

# Raw data of full pH plot (separated by strain)
nisin_ph$ph_f<-factor(nisin_ph$ph, levels = c("5.5", "6", "6.5"))
nisin_ph %>%
  filter(!is.na(log_count)) %>%
  group_by(day,ph_f,nisin,strain) %>%
  summarize(mean_log_count=mean(log_count),
            se_log_count=sd(log_count)/sqrt(n())) %>%
  ungroup() %>%
  mutate(day=as.numeric(as.character(day))) %>%
  ggplot(aes(x=day,y=mean_log_count,color=nisin)) +
  labs(y="Average Log Count", x="Day", color = "Nisin Added") +
  scale_color_manual(labels = c("No", "Yes"), values = c("red", "blue")) +
  geom_point(position=position_dodge(1)) +
  geom_errorbar(width=1,
                aes(ymin=mean_log_count-2*se_log_count,
                    ymax=mean_log_count+2*se_log_count),
                position=position_dodge(1)) +
  geom_line(position=position_dodge(1),aes(group=nisin)) +
  scale_x_continuous(breaks=c(1,7,14)) +
  ylim(3,11) +
  facet_grid(strain~ph_f)