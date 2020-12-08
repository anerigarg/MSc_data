# Ch. 2 Figure Selection August 2020


# packages and pallettes --------------------------------------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(PNWColors)

# df files ----------------------------------------------------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)



# Overall Recruitment -----------------------------------------------------


# Col plot - density ~ treatment*complexity -------------------------------

ARD_3_treatcomp_sumcol <- ARD_3_with_0 %>% 
  # mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))
  # mutate(fish.size = 3)

ggplot(data = ARD_3_treatcomp_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2)+
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("Overall mean fish recruit density (3cm and smaller)") +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )


# col plot - density rate -------------------------------------------------

#import rate csv (made this in R file "change in density - code and plots for collab meeting july)
ARD_3_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_3/(final - initial) over time/ARD_3_rate.csv")

#join ARD_3_rate to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_3_rate <- full_join(ARD_3_rate, ARD_lookup)
hist(ARD_3_rate$rate) #normalish lol (not rly...)

#make treatment levels in order from control to 100% and order of dates chronological
ARD_3_rate_sum <- ARD_3_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
  mutate(rate.se = rate.sd/sqrt(96))


#plot it (rate)
ggplot(data = ARD_3_rate_sum) +
  geom_col(aes(x = treatment,
               y = rate.mean,
               fill = treatment),
           alpha = 0.7) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) +
  ggtitle("mean recruitment rate (final density - initial density / total time)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )
