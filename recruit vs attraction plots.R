#attraction vs retention plots

# packages and palletes ----------------------------------------------------------------

# install.packages("inferno")
# install.packages("ggpmisc")
# install.packages("purrr")

library(readr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(PNWColors)
library(ggpmisc)
library(purrr)
library(broom)

install.packages("tidyverse")

pal <- pnw_palette(6, name = "Starfish", type = "discrete")
pal1 <- pnw_palette(3, name = "Winter", type = "continuous")
pal2 <-pnw_palette(6, name = "Cascades", type = "continuous")
pal3 <-pnw_palette(3, name = "Lake", type = "continuous")


# df files ----------------------------------------------------------------


ARD_2_5_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_2.5/ARD_2.5_with_0.csv")
ARD_2_5_with_0 <- ARD_2_5_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_2_5to5_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_2.5-5/ARD_2.5to5_with_0.csv")
ARD_2_5to5_with_0 <- ARD_2_5to5_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>% 
mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                          labels = c("control", "0%", "30%", "50%", "70%", "100%")))

# line plot - all treat comp, all time points, fish 2.5cm and smaller --------


ARD_2_5_with_0_sum <- ARD_2_5_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1608))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_2_5_with_0,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_line(data = ARD_2_5_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_2_5_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_2_5_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3) +
  geom_smooth(data = ARD_2_5_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment) +
  ylim(0,10)+
  xlim(0,9) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time for fish 2.5cm and smaller") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  


# line plot, all treat comp, just damselfish under 2.5---------------------


ARD_2_5_with_0_dam <- ARD_2_5_with_0 %>% 
  # filter(visit == c(1,2,3,4,5,6,7,8,9)) %>% 
  filter(Genus == c("Microspathadon", "Stegastes"))

ARD_2_5_with_0_sum_dam <- ARD_2_5_with_0_dam %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(231))

#adjusted xlim and ylim for visits
ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_2_5_with_0_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_2_5_with_0_sum_dam,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_2_5_with_0_sum_dam,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_2_5_with_0_sum_dam,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3) +
  geom_smooth(data = ARD_2_5_with_0_sum_dam,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment) +
  ylim(0,8)+
  # xlim(0,9)+
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time for damselfish 2.5cm and smaller (all time points)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  




# line plot, all treat comp, damselfish 2.5- 5 ----------------------------


ARD_2_5to5_with_0_dam <- ARD_2_5to5_with_0 %>% 
  # filter(visit == c(1,2,3,4,5,6,7,8,9)) %>% 
  filter(Genus == c("Microspathadon", "Stegastes"))

ARD_2_5to5_with_0_dam_sum <- ARD_2_5to5_with_0_dam %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(271))

#adjusted xlim and ylim for visits
ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_2_5to5_with_0_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_2_5to5_with_0_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_2_5to5_with_0_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_2_5to5_with_0_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3) +
  geom_smooth(data = ARD_2_5to5_with_0_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment) +
  # ylim(0,8)+
  xlim(9,18)+
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time for damselfish 2.5-5cm") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  

#interesting, it looks like for damselfish bw 2-5-5cm in visits 9-18, you see similar trends for low quality
#habitats in low complexity and "high" quality habitats in hich complexity.Interesting, so could you almost
#"compensate" for low quality depending on the surrounding environment? ie the type of quality may not matter as much
# in high complexit environmnets cause there's alread so much variation?
# vs in low complexity environments, even though all artificial attract, may not actually retain fishes...
#HUH...HUH HUH HUH


# damselfish under 2.5 first 9  -------------------------------------------

#filter was being funny in R, just sorted by visit and kept only the first 9 visits. still have to filter by damselfish

ARD_2_5_with_0_first9 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_2.5/ARD_2.5_with_0_first9.csv")
View(ARD_2_5_with_0_first9)
ARD_2_5_with_0_first9 <- ARD_2_5_with_0_first9 %>% 
mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                          labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_2_5_with_0_first9_dam <- ARD_2_5_with_0_first9 %>% 
  filter(Genus == c("Microspathadon", "Stegastes"))

ARD_2_5_with_0_first9_dam_sum <- ARD_2_5_with_0_first9_dam %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(113))

#adjusted xlim and ylim for visits
ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_2_5_with_0_first9_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_2_5_with_0_first9_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_2_5_with_0_first9_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_2_5_with_0_first9_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3) +
  geom_smooth(data = ARD_2_5_with_0_first9_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment) +
  # ylim(0,8)+
  # xlim(0,9)+
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time for damselfish 2.5cm and smaller first 9 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  


# damselfish 2.5 and smaller, last 9 visits...don't actually need this... ---------------------------------

ARD_2_5_with_0_last9 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_2.5/ARD_2.5_with_0_last9.csv")
View(ARD_2_5_with_0_last9)
ARD_2_5_with_0_last9 <- ARD_2_5_with_0_last9 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_2_5_with_0_last9_dam <- ARD_2_5_with_0_last9 %>% 
  filter(Genus == c("Microspathadon", "Stegastes"))

ARD_2_5_with_0_last9_dam_sum <- ARD_2_5_with_0_last9_dam %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(113))

#adjusted xlim and ylim for visits
ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_2_5_with_0_last9_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_2_5_with_0_last9_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_2_5_with_0_last9_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_2_5_with_0_last9_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3) +
  geom_smooth(data = ARD_2_5_with_0_last9_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment) +
  # ylim(0,8)+
  # xlim(0,9)+
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time for damselfish 2.5cm and smaller last 9 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  


# damselfish between 2.5-5cm, last 9 visits -------------------------------

ARD_2_5to5_with_0_last9 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_2.5-5/ARD_2.5to5_with_0_last9.csv")

ARD_2_5to5_with_0_last9 <- ARD_2_5to5_with_0_last9 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_2_5to5_with_0_last9_dam <- ARD_2_5to5_with_0_last9 %>% 
  filter(Genus == c("Microspathadon", "Stegastes"))

ARD_2_5to5_with_0_last9_dam_sum <- ARD_2_5to5_with_0_last9_dam %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(137))

#adjusted xlim and ylim for visits
ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_2_5to5_with_0_last9_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_2_5to5_with_0_last9_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_2_5to5_with_0_last9_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_2_5to5_with_0_last9_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3) +
  geom_smooth(data = ARD_2_5to5_with_0_last9_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment) +
  # ylim(0,8)+
  # xlim(0,9)+
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time for damselfish 2.5cm-5cm last 9 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  





# OLD CODE # -----------------------------------------------------------------------


# all fish 3cm and under, first 9 visits ------------------------------------------------


ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_3_with_0_first9 <- ARD_3_with_0 %>% 
  filter(visit %in% (1:10))

ARD_3_with_0_first9_sum <- ARD_3_with_0_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(939))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_with_0_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_with_0_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_with_0_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_with_0_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_with_0_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )


# test all fish 3cm and under new 0 ---------------------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>%  
 mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

View(ARD_3_with_0)

ARD_3_first9 <- ARD_3_with_0 %>% 
  filter(visit %in% (1:10))

ARD_3_first9_sum <- ARD_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data =ARD_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (3cm and smaller) over time -  first 10 visits")
  # theme_classic() +
  # theme(
  #   axis.title = element_text(size = 16),
  #   axis.text = element_text(size = 16),
  #   strip.text = element_text(size = 16),
  #   legend.position = "none",
  # )


# all fish 6cm and under last 10 visits -----------------------------------

ARD_6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_6/ARD_6_with_0.csv")

ARD_6_with_0 <- ARD_6_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_6_with_0_last9 <- ARD_6_with_0 %>% 
  filter(visit %in% (10:18))

ARD_6_with_0_last9_sum <- ARD_6_with_0_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(732))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_6_with_0_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_6_with_0_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_6_with_0_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_6_with_0_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_smooth(data = ARD_6_with_0_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,20) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (6cm and smaller) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  

# all fish, 4-6cm last 10 visits ------------------------------------------

`%notin%` = negate(`%in%`)

ARD_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0.csv")

ARD_4to6_with_0 <- ARD_4to6_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_4to6_with_0_last9 <- ARD_4to6_with_0 %>% 
  filter(visit %in% (10:18))

ARD_4to6_with_0_last9_sum <- ARD_4to6_with_0_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(755))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_with_0_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_4to6_with_0_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_4to6_with_0_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_with_0_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_smooth(data = ARD_4to6_with_0_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (4-6cm) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )


# all fish, all visits, 3cm and under and 6cm and under -------------------

ARD_6_with_0_sum <- ARD_6_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1553))

ARD_3_with_0_sum <- ARD_3_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1597))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_6_with_0,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.2) +
  geom_jitter(data = ARD_3_with_0,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_6_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_line(data = ARD_3_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_6_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_6_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_point(data = ARD_3_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_6_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_smooth(data = ARD_3_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              alpha = 0.5, size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE) +
  # scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,20) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm and 6cm) all visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    # legend.position = "none",
  )


# damselfish --------------------------------------------------------------


# damselfish, 3cm and under, first 9 visits --------------------------------------------------------------

#1) filter for just the ones with damselfish
ARD_3_first9_onlydam <- ARD_3_with_0 %>% 
  select(-c(density, biomass)) %>% 
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>% 
  filter(visit %in% (1:10)) %>% 
  mutate(damselfish.score = 1)

#2) filter for the ones that aren't damselfish, and make their density value = 0
ARD_3_first9_nodam <- ARD_3_with_0 %>% 
  select(-c(density, biomass)) %>% 
  filter(Genus %notin% c("Microspathodon", "Stegastes")) %>% 
  filter(visit %in% (1:10)) %>% 
  mutate(damselfish.score = 0)

#3) bind them into 1 df and re-calculate density based on new df
ARD_3_first9_dam <- rbind(ARD_3_first9_onlydam , ARD_3_first9_nodam) %>% 
  group_by(plot_grid_survey) %>%
  mutate(density = sum(damselfish.score)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

write.csv(ARD_3_first9_dam, "ARD_3_first9_dam.csv")

ARD_3_first9_dam_sum <- ARD_3_first9_dam %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(939))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_first9_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_first9_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_first9_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_first9_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_first9_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              formula = y ~ x + I(x^2),
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  # ylim(0,1.5) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  

# new test ----------------------------------------------------------------


#1) filter for just the ones with damselfish
ARD_3_first9_onlydam <- ARD %>%  
  filter(TL <= 3) %>% 
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>%
  filter(visit %in% (1:10)) %>% 
  mutate(damselfish.score = 1)

#2) filter for the ones that aren't damselfish, and make their density value = 0
ARD_3_first9_nodam <- ARD %>% 
  filter(TL <= 3) %>%
  filter(Genus %notin% c("Microspathodon", "Stegastes")) %>% 
  filter(visit %in% (1:10)) %>% 
  mutate(damselfish.score = 0)

#3) bind them into 1 df and re-calculate density based on new df
ARD_3_first9_dam <- rbind(ARD_3_first9_onlydam , ARD_3_first9_nodam) %>% 
  group_by(plot_grid_survey) %>%
  mutate(density = sum(damselfish.score)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

write.csv(ARD_3_first9_dam, "ARD_3_first9_dam.csv")

ARD_3_first9_dam_sum <- ARD_3_first9_dam %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(939))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_first9_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_first9_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_first9_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_first9_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_first9_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              formula = y ~ x + I(x^2),
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,1.5) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  

# damselfish, 6cm and under, last 9 visits --------------------------------


#1) filter for just the ones with damselfish
ARD_6_last9_onlydam <- ARD_6_with_0 %>% 
  select(-c(density, biomass)) %>% 
  filter(Genus %in% c("Microspathadon", "Stegastes")) %>% 
  filter(visit %in% (10:18)) %>% 
  mutate(damselfish.score = 1)

#2) filter for the ones that aren't damselfish, and make their density value = 0
ARD_6_last9_nodam <- ARD_6_with_0 %>% 
  select(-c(density, biomass)) %>% 
  filter(Genus %notin% c("Microspathadon", "Stegastes")) %>% 
  filter(visit %in% (10:18)) %>% 
  mutate(damselfish.score = 0)

#3) bind them into 1 df and re-calculate density based on new df
ARD_6_last9_dam <- rbind(ARD_3_last9_onlydam , ARD_3_last9_nodam) %>% 
  group_by(plot_grid_survey) %>%
  mutate(density = sum(damselfish.score)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

ARD_6_last9_dam <- ARD_6_last9_dam %>% 
  group_by(plot_grid_survey) %>%
  mutate(density = sum(damselfish.score)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

write.csv(ARD_6_last9_dam, "ARD_6_last9_dam.csv")

ARD_6_last9_dam_sum <- ARD_6_last9_dam %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(732))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_6_last9_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_6_last9_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_6_last9_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_6_last9_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_smooth(data = ARD_6_last9_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              formula = y ~ x + I(x^2),
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density (6cm and smaller) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  


# damselfish, 4-6cm, last 10 visits ---------------------------------------


#1) filter for just the ones with damselfish
ARD_4to6_last9_onlydam <- ARD_4to6_with_0 %>% 
  filter(Genus %in% c("Microspathadon", "Stegastes")) %>% 
  filter(visit %in% (10:18))

#2) filter for the ones that aren't damselfish, and make their density value = 0
ARD_4to6_last9_nodam <- ARD_4to6_with_0 %>% 
  select(-c(density, biomass)) %>% 
  filter(Genus %notin% c("Microspathadon", "Stegastes")) %>% 
  filter(visit %in% (10:18)) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0)

ARD_4to6_last9_dam <- rbind(ARD_4to6_last9_onlydam , ARD_4to6_last9_nodam)

ARD_4to6_last9_dam_sum <- ARD_4to6_last9_dam %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(732))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_last9_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3) +
  geom_line(data = ARD_4to6_last9_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_4to6_last9_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_last9_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_smooth(data = ARD_4to6_last9_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # formula = y ~ x + I(x^2),
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,8) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density (4-6cm) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )  


# damselfish, all visits, 3cm and 6cm and under ---------------------------

#1) filter for just the ones with damselfish
ARD_3_onlydam <- ARD_3_with_0 %>% 
  filter(Genus %in% c("Microspathadon", "Stegastes"))

#2) filter for the ones that aren't damselfish, and make their density value = 0
ARD_3_nodam <- ARD_3_with_0 %>% 
  select(-c(density, biomass)) %>% 
  filter(Genus %notin% c("Microspathadon", "Stegastes")) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0)

#3) bind them into 1 df
ARD_3_dam <- rbind(ARD_3_onlydam , ARD_3_nodam)

#now same for ARD_6

#1) filter for just the ones with damselfish
ARD_6_onlydam <- ARD_6_with_0 %>% 
  filter(Genus %in% c("Microspathadon", "Stegastes"))

#2) filter for the ones that aren't damselfish, and make their density value = 0
ARD_6_nodam <- ARD_6_with_0 %>% 
  select(-c(density, biomass)) %>% 
  filter(Genus %notin% c("Microspathadon", "Stegastes")) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0)

#3) bind them into 1 df
ARD_6_dam <- rbind(ARD_6_onlydam , ARD_6_nodam)


#summary dfs
ARD_3_dam_sum <- ARD_3_dam %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1597))

ARD_6_dam_sum <- ARD_6_dam %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1553))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_6_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.2) +
  geom_jitter(data = ARD_3_dam,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_6_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_line(data = ARD_3_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_6_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_6_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_point(data = ARD_3_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_6_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_smooth(data = ARD_3_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              alpha = 0.5, size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE) +
  scale_colour_manual(values = pal) +
  # scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,15) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density over time (3cm and 6cm) all visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )


# NEW CODE ----------------------------------------------------------------


# all fish 3cm and under, first 9 visits ----------------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

View(ARD_3_with_0)

ARD_3_first9 <- ARD_3_with_0 %>% 
  filter(visit %in% (1:10))

ARD_3_first9_sum <- ARD_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data =ARD_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
  axis.title = element_text(size = 14),
  axis.text = element_text(size = 12),
  strip.text = element_text(size = 14),
  legend.position = "none",
)


# experimenting with R2 ---------------------------------------------------

# df <- data.frame(x = c(1:100))
# df$y <- 20 * c(0, 1) + 3 * df$x + rnorm(100, sd = 40)
# df$group <- factor(rep(c("A", "B"), 50))
# my.formula <- y ~ x

#1) make plot that has everything but the geom_smooth line, called p
p <- ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data =ARD_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_first_9_df,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = linear.formula,
              se = FALSE ) +
  stat_poly_eq(formula = linear.formula,
               aes(label = paste(..eq.label..,
                                 ..rr.label..,
                                 sep = "~~~")),
               parse = TRUE) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )
p

ARD_3_first_9_df <- ARD_3_first9 %>% 
  select(visit, density, complexity, treatment) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density))

linear.formula <- y ~ x
quadratic.formula <- y ~ x + I(x^2)

p1 <- p + 
geom_smooth(data = ARD_3_first_9_df,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 2,
            method = lm,
            formula = linear.formula,
            se = FALSE ) +
  facet_grid(complexity~treatment) +
  stat_poly_eq(formula = linear.formula,
             aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
             parse = TRUE) +
  geom_point()

p1
#this didn't seem to work. going to try doing the models first then plotting em to ggplot

fit1 <- lm(density.mean ~ visit, data = ARD_3_first_9_df)
summary(fit1)
plot(density.mean ~ visit, data = ARD_3_first_9_df)
abline(fit1)

#nope, but have to divide by groups, the previous gave value for all treat_comp grouped

#) trial 1: (doesn't give r.squared tho)
ARD_3_first9$treatment_complexity <-paste(ARD_3_first9$treatment, "-",ARD_3_first9$complexity)

models <- ARD_3_first9 %>% 
  group_by(treatment_complexity) %>% 
  summarise(mod = list(lm(density ~ visit)))

models$mod[[2]]$coefficients

#) trial 2: using package purrr
ARD_3_first9_linear_models <- ARD_3_first9 %>% 
  split(.$treatment_complexity) %>% 
  map(~lm(density~visit, formula = x ~ y, data = .)) %>% 
  map(summary) %>% 
  map_dbl("r.squared")

view(ARD_3_first9_linear_models)
#this may show R2 values but note sure...

#)trial 3: using lmer and broom

ARD_3_first9_lm <- ARD_3_first9 %>% 
  nest(-treatment_complexity) %>% 
  mutate(fit = map(data, ~lm (density ~ visit, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(treatment_complexity, r.squared, p.value)

#try and also extract slope and intercept and coefficients
ARD_3_first9_lmtest <- ARD_3_first9 %>% 
  nest(-treatment_complexity) %>% 
  mutate(fit = map(data, ~lm (density ~ visit,data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(treatment_complexity, estimate, r.squared, p.value) %>% 
  mutate(estimate = exp(estimate))

# code for R2 values for lm and qm ----------------------------------------



#ok yup works on last 9 visits

ARD_6_last9$treatment_complexity <-paste(ARD_6_last9$treatment, "-",ARD_6_last9$complexity)

ARD_6_last9_lm <- ARD_6_last9 %>% 
  nest(-treatment_complexity) %>% 
  mutate(fit = map(data, ~lm (density ~ visit, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(treatment_complexity, r.squared, p.value) %>% 
  mutate(R2 = r.squared*100) 

#both method 2 and 3 work for getting R2, but method 3 has easier to see out-put.
#now to try with dif formula... ie. quadratic

ARD_6_last9q <- ARD_6_last9 %>% 
  mutate(visit2 = visit^2)

ARD_6_last9_qm <- ARD_6_last9q %>% 
  nest(-treatment_complexity) %>% 
  mutate(fit = map(data, ~lm (density ~ visit + visit2, data = .)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(treatment_complexity, r.squared, p.value) %>% 
  mutate(R2 = r.squared*100) 

write.csv(ARD_6_last9, "ARD_6_last9.csv")


# lm_eqn <- function(ARD_3_first_9_df){
#   m <- lm(density.mean ~ visit, ARD_3_first_9_df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 2), 
#                         b = format(coef(m)[2], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));                 
}

# lm_eqn <- function(df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 2), 
#                         b = format(coef(m)[2], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));                 
# }

p2 <- ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data =ARD_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = linear.formula,
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

p2

p3 <- p2 +geom_text(x = 2, y = 8, label = lm_eqn(ARD_3_first_9_df), parse = TRUE)
p3
#I think it's not faceting, try new one: 
# https://stackoverflow.com/questions/19699858/ggplot-adding-regression-line-equation-and-r2-with-facet


# Lm_coefficient_extraction_test ------------------------------------------

#used code from Coleen, in R file: Lm_coefficient extraction script

#)1 make a df with the variables I need

ARD_3_lm_data <- ARD_3_first9 %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_lm_data$treatment_complexity <-paste(ARD_3_lm_data$treatment, "-",ARD_3_lm_data$complexity)

#2) calculate slopes

ARD_3_lm_slopes <- ARD_3_first9 %>% 
  split(.$treatment_complexity) %>% 
  map(~ lm(density ~ visit, data = .)) %>% 
  map_df(broom::tidy, .id = "treatment_complexity") %>% 
  filter(term == "Recruit Density") %>% 
  select("treatment_complexity", "estimate") %>% 
  rename(Slope = estimate)
  

# all fish, 3cm last 10 visits ------------------------------------------------------

ARD_3_last9 <- ARD_3_with_0 %>% 
  filter(visit %in% (10:18))

ARD_3_last9_sum <- ARD_3_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data =ARD_3_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_3_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (3cm and smaller) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

# all fish 6cm and under, last 9 visits -----------------------------------

ARD_6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_6/ARD_6_with_0.csv")
ARD_6_with_0 <- ARD_6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(ARD_6_with_0)

ARD_6_last9 <- ARD_6_with_0 %>% 
  filter(visit %in% (10:18))

ARD_6_last9_sum <- ARD_6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,20) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (6cm and smaller) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# all fish, 4-6cm, last 9 visits ------------------------------------------


ARD_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0.csv")
ARD_4to6_with_0 <- ARD_4to6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

View(ARD_4to6_with_0)

ARD_4to6_last9 <- ARD_4to6_with_0 %>% 
  filter(visit %in% (10:18))

ARD_4to6_last9_sum <- ARD_4to6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))

ggpot(data = ARD_4to6_with_0)+
  
  


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_4to6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = ARD_4to6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (4-6cm) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# all fish, 3cm and 6cm and under, all visits -----------------------------

ARD_6_with_0_sum <- ARD_6_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ARD_3_with_0_sum <- ARD_3_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ggplot() +
  facet_grid(complexity~treatment) +
  # geom_jitter(data = ARD_6_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1) +
  # geom_jitter(data = ARD_3_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1,
  #             shape = 2,
  #             size = 1) +
  geom_line(data = ARD_6_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_line(data = ARD_3_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.3) +
  geom_errorbar(data = ARD_6_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = ARD_3_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_6_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = ARD_3_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3,
             shape = 17) +
  geom_smooth(data = ARD_6_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_line(data = ARD_3_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            stat = "smooth",
            method = "lm",
            alpha = 0.65,
            size = 2) +
  formula = y ~x + I(x^2)) +
  # geom_smooth(data = ARD_3_with_0_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             alpha = 0.5, size = 2,
  #             method = lm,
  #             formula = y ~ x + I(x^2),
  #             se = FALSE) +
  scale_x_continuous(breaks = c(3, 6, 9, 12,15,18)) +
  ylim(0,20) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm and 6cm) all visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# all fish, 3cm, all visits -----------------------------------------------

ARD_3_with_0_sum <- ARD_3_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_with_0,
            aes(x = visit,
                y = density,
                colour = treatment),
            alpha = 0.2,
            shape = 2,
            size = 1) +
  geom_line(data = ARD_3_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3,
             shape = 17) +
  geom_smooth(data = ARD_3_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  scale_x_continuous(breaks = c(6, 12, 18)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm) all visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

# all fish 3cm and 6cm and under - no bhw ---------------------------------

ARD_6_with_0_nobhw <- ARD_6_with_0 %>%
  filter(common_name != "Bluehead Wrasse")
  
ARD_6_with_0_sum_nobhw <- ARD_6_with_0_nobhw %>%
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1127))

ARD_3_with_0_nobhw <- ARD_3_with_0 %>%
  filter(common_name != "Bluehead Wrasse")

ARD_3_with_0_sum_nobhw <- ARD_3_with_0_nobhw %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1066))

ggplot() +
  facet_grid(complexity~treatment) +
  # geom_jitter(data = ARD_6_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1) +
  # geom_jitter(data = ARD_3_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1,
  #             shape = 2,
#             size = 1) +
geom_line(data = ARD_6_with_0_sum_nobhw,
          aes(x = visit,
              y = density.mean,
              colour = treatment,
              group = treatment),
          size = 1.2,
          alpha = 0.4) +
  geom_line(data = ARD_3_with_0_sum_nobhw,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.3) +
  geom_errorbar(data = ARD_6_with_0_sum_nobhw,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = ARD_3_with_0_sum_nobhw,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_6_with_0_sum_nobhw,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = ARD_3_with_0_sum_nobhw,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3,
             shape = 17) +
  geom_smooth(data = ARD_6_with_0_sum_nobhw,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_line(data = ARD_3_with_0_sum_nobhw, 
            aes(x = visit, 
                y = density.mean,
                colour = treatment,
                group = treatment),
            stat = "smooth",
            method = "lm",
            alpha = 0.65,
            size = 2,
            formula = y ~ x + I(x^2),
            se = FALSE ) +
  # geom_smooth(data = ARD_3_with_0_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             alpha = 0.5, size = 2,
  #             method = lm,
  #             formula = y ~ x + I(x^2),
  #             se = FALSE) +
  scale_x_continuous(breaks = c(3, 6, 9, 12,15,18)) +
  ylim(0,20) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm and 6cm) all visits - no bhw") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# all fish, 3cm first 9 and 4-6cm last nine (same graph) ----------------------

ARD_3_first9 <- ARD_3_with_0 %>% 
  filter(visit %in% (1:10))

ARD_3_first9_sum <- ARD_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927))

ARD_4to6_last9 <- ARD_4to6_with_0 %>% 
  filter(visit %in% (10:18))

ARD_4to6_last9_sum <- ARD_4to6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))

ARD_all_visits_sum <- rbind(ARD_3_first9_sum, ARD_4to6_last9_sum)
ARD_all_visits <- rbind(ARD_3_first9,ARD_4to6_last9)

ggplot() +
  facet_grid(complexity~treatment) +
geom_jitter(data = ARD_all_visits,
            aes(x = visit,
                y = density,
                colour = treatment),
            alpha = 0.1) +
geom_line(data = ARD_all_visits_sum,
          aes(x = visit,
              y = density.mean,
              colour = treatment,
              group = treatment),
          size = 1.2,
          alpha = 0.4) +
  geom_errorbar(data = ARD_all_visits_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_point(data = ARD_all_visits_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  # geom_smooth(data = ARD_all_visits_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             size = 2,
  #             method = lm,
  #             # formula = y ~ x + I(x^2),
  #             se = FALSE ) +
  geom_vline(xintercept = 10, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  scale_x_continuous(breaks = c(3, 6, 9, 12,15,18)) +
  ylim(0,10) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm first 9, 4-6 last 9)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# all fish 3cm fisrt 9, 6cm last 9 (same grapht)----------------------------------------

RD_3_first9 <- ARD_3_with_0 %>% 
  filter(visit %in% (1:10))

ARD_3_first9_sum <- ARD_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927)) %>% 
  mutate(fish.size = 3)

ARD_6_last9 <- ARD_6_with_0 %>% 
  filter(visit %in% (10:18))

ARD_6_last9_sum <- ARD_6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754)) %>% 
  mutate(fish.size = 6)

ARD_all_visits_sum2 <- rbind(ARD_3_first9_sum, ARD_6_last9_sum)
ARD_all_visits2 <- rbind(ARD_3_first9,ARD_6_last9)

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_all_visits2,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_line(data = ARD_all_visits_sum2,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_errorbar(data = ARD_all_visits_sum2,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_point(data = ARD_all_visits_sum2,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  # geom_smooth(data = ARD_all_visits_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             size = 2,
  #             method = lm,
  #             # formula = y ~ x + I(x^2),
  #             se = FALSE ) +
  geom_vline(xintercept = 10, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  scale_x_continuous(breaks = c(3, 6, 9, 12,15,18)) +
  ylim(0,15) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm first 9, 6cm last 9)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


#try and plot separately to get separate smooth lines:
ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_jitter(data = ARD_6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_line(data = ARD_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_line(data = ARD_6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_errorbar(data = ARD_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = ARD_6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_point(data = ARD_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = ARD_6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_smooth(data = ARD_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_smooth(data = ARD_6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_vline(xintercept = 10, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  scale_x_continuous(breaks = c(3, 6, 9, 12,15,18)) +
  ylim(0,15) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm first 9, 6cm last 9)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


#try and extrat intercept and slope:

ARD_3_stats <- ARD_3_first9_sum %>% 
  group_by(treatment, complexity) %>% 
  do({
    mod = lm(density.mean ~ visit, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })

ARD_3_first9_sumstats <- ARD_3_first9 %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))


# DAMSELFISH --------------------------------------------------------------


# damselfish 3cm and under first 9 visits ---------------------------------

dam_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_3_with_0.csv")
dam_3_with_0 <- dam_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(ARD_3_with_0)

dam_3_first9 <- dam_3_with_0 %>% 
  filter(visit %in% (1:10))

dam_3_first9_sum <- dam_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = dam_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = dam_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = dam_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = dam_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = dam_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,0.5) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# damselfish 6cm and under last 9 visits ----------------------------------


dam_6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_6/dam_6_with_0.csv")
dam_6_with_0 <- dam_6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(ARD_6_with_0)

dam_6_last9 <- dam_6_with_0 %>% 
  filter(visit %in% (10:18))

dam_6_last9_sum <- dam_6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = dam_6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = dam_6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = dam_6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = dam_6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = dam_6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  scale_colour_manual(values = pal) +
  ylim(0,4) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density (6cm and smaller) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# damselfish 4-6 last 9 ---------------------------------------------------

dam_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_4to6/dam_4to6_with_0.csv")
dam_4to6_with_0 <- dam_4to6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

View(dam_4to6_with_0)

dam_4to6_last9 <- dam_4to6_with_0 %>% 
  filter(visit %in% (10:18))

dam_4to6_last9_sum <- dam_4to6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = dam_4to6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = dam_4to6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = dam_4to6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = dam_4to6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = dam_4to6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  scale_colour_manual(values = pal) +
  ylim(0,2) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density (4-6cm) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# damselfish, 3 and 6cm, all visits ---------------------------------------

dam_6_with_0_sum <- dam_6_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

dam_3_with_0_sum <- dam_3_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ggplot() +
  facet_grid(complexity~treatment) +
  # geom_jitter(data = ARD_6_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1) +
  # geom_jitter(data = ARD_3_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1,
  #             shape = 2,
#             size = 1) +
geom_line(data = dam_6_with_0_sum,
          aes(x = visit,
              y = density.mean,
              colour = treatment,
              group = treatment),
          size = 1.2,
          alpha = 0.4) +
  geom_line(data = dam_3_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.3) +
  geom_errorbar(data = dam_6_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = dam_3_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = dam_6_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = dam_3_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3,
             shape = 17) +
  geom_smooth(data = dam_6_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_line(data = dam_3_with_0_sum, 
            aes(x = visit, 
                y = density.mean,
                colour = treatment,
                group = treatment),
            stat = "smooth",
            method = "lm",
            alpha = 0.65,
            size = 2) +
            # formula = y ~x + I(x^2)) +
  # geom_smooth(data = ARD_3_with_0_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             alpha = 0.5, size = 2,
  #             method = lm,
  #             formula = y ~ x + I(x^2),
  #             se = FALSE) +
  scale_x_continuous(breaks = c(6, 12,18)) +
  scale_colour_manual(values = pal) +
  ylim(0,4) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean damselfish recruit density over time (3cm and 6cm) all visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# PARROTFISH --------------------------------------------------------------


# par_3 first 9 visits -------------------------------------------------------------------

par_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/parrotfish/par_3/par_3_with_0.csv")
par_3_with_0 <- par_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(ARD_3_with_0)

par_3_first9 <- par_3_with_0 %>% 
  filter(visit %in% (1:10))

par_3_first9_sum <- par_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = par_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = par_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = par_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = par_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = par_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_colour_viridis(discrete = TRUE) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,3) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean parrotfish recruit density (3cm and smaller) over time -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )



# par_6 last 9 visits -----------------------------------------------------

par_6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/parrotfish/par_6/par_6_with_0.csv")
par_6_with_0 <- par_6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(ARD_6_with_0)

par_6_last9 <- par_6_with_0 %>% 
  filter(visit %in% (10:18))

par_6_last9_sum <- par_6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = par_6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = par_6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = par_6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = par_6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = par_6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  scale_colour_viridis(discrete = TRUE) +
  ylim(0,8) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean parrotfish recruit density (6cm and smaller) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# par 4-6 last 9 visits ---------------------------------------------------

par_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/parrotfish/par_4to6/par_4to6_with_0.csv")
par_4to6_with_0 <- par_4to6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

View(par_4to6_with_0)

par_4to6_last9 <- par_4to6_with_0 %>% 
  filter(visit %in% (10:18))

par_4to6_last9_sum <- par_4to6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = par_4to6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = par_4to6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = par_4to6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = par_4to6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = par_4to6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  scale_colour_viridis(discrete = TRUE) +
  ylim(0,3) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean parrotfish recruit density (4-6cm) over time -  last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# par 3 and 6 all visits --------------------------------------------------

par_6_with_0_sum <- par_6_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

par_3_with_0_sum <- par_3_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ggplot() +
  facet_grid(complexity~treatment) +
  # geom_jitter(data = ARD_6_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1) +
  # geom_jitter(data = ARD_3_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1,
  #             shape = 2,
#             size = 1) +
geom_line(data = par_6_with_0_sum,
          aes(x = visit,
              y = density.mean,
              colour = treatment,
              group = treatment),
          size = 1.2,
          alpha = 0.4) +
  geom_line(data = par_3_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.3) +
  geom_errorbar(data = par_6_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = par_3_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = par_6_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = par_3_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3,
             shape = 17) +
  geom_smooth(data = par_6_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_line(data = par_3_with_0_sum, 
            aes(x = visit, 
                y = density.mean,
                colour = treatment,
                group = treatment),
            stat = "smooth",
            method = "lm",
            alpha = 0.65,
            size = 2) +
            # formula = y ~x + I(x^2)) +
  # geom_smooth(data = ARD_3_with_0_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             alpha = 0.5, size = 2,
  #             method = lm,
  #             formula = y ~ x + I(x^2),
  #             se = FALSE) +
  scale_x_continuous(breaks = c(6, 12,18)) +
  scale_colour_viridis(discrete = TRUE) +
  # ylim(0,5) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean parrotfish recruit density over time (3cm and 6cm) all visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )



# GOBIES AND BLENNIES -----------------------------------------------------


# gob_3 -------------------------------------------------------------------

gob_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_3/gob_3_with_0.csv")
gob_3_with_0 <- gob_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(ARD_3_with_0)

gob_3_first9 <- gob_3_with_0 %>% 
  filter(visit %in% (1:10))

gob_3_first9_sum <- gob_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = gob_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = gob_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = gob_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = gob_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = gob_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_colour_manual(values = pal2) +
  scale_x_continuous(breaks = c(2,4,6,8,10)) +
  ylim(0,6) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (3cm and smaller) over time - gobies and blennies -  first 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# gob_6 -------------------------------------------------------------------

gob_6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_6/gob_6_with_0.csv")
gob_6_with_0 <- gob_6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(ARD_6_with_0)

gob_6_last9 <- gob_6_with_0 %>% 
  filter(visit %in% (10:18))

gob_6_last9_sum <- gob_6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = gob_6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = gob_6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = gob_6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = gob_6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = gob_6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  scale_colour_manual(values = pal2) +
  ylim(0,6) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (6cm and smaller) over time - gobies and blennies - last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# gob_4to6 ----------------------------------------------------------------

gob_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_4to6/gob_4to6_with_0.csv")
gob_4to6_with_0 <- gob_4to6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

# View(par_4to6_with_0)

gob_4to6_last9 <- gob_4to6_with_0 %>% 
  filter(visit %in% (10:18))

gob_4to6_last9_sum <- gob_4to6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754))


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = gob_4to6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.3,
              shape = 2,
              size = 1) +
  geom_line(data = gob_4to6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = gob_4to6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = gob_4to6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5,
             shape = 17) +
  geom_smooth(data = gob_4to6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  facet_grid(complexity~treatment) +
  scale_x_continuous(breaks = c(10, 12, 14, 16, 18)) +
  scale_colour_manual(values = pal2) +
  ylim(0,1) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density (4-6cm) over time - gobies and blennies - last 10 visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# gob 3 and 6 all visits --------------------------------------------------

gob_6_with_0_sum <- gob_6_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

gob_3_with_0_sum <- gob_3_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ggplot() +
  facet_grid(complexity~treatment) +
  # geom_jitter(data = ARD_6_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1) +
  # geom_jitter(data = ARD_3_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.1,
  #             shape = 2,
#             size = 1) +
geom_line(data = gob_6_with_0_sum,
          aes(x = visit,
              y = density.mean,
              colour = treatment,
              group = treatment),
          size = 1.2,
          alpha = 0.4) +
  geom_line(data = gob_3_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.3) +
  geom_errorbar(data = gob_6_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = gob_3_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = gob_6_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = gob_3_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3,
             shape = 17) +
  geom_smooth(data = gob_6_with_0_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_line(data = gob_3_with_0_sum, 
            aes(x = visit, 
                y = density.mean,
                colour = treatment,
                group = treatment),
            stat = "smooth",
            method = "lm",
            alpha = 0.65,
            size = 2) +
            # formula = y ~x + I(x^2)) +
  # geom_smooth(data = ARD_3_with_0_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             alpha = 0.5, size = 2,
  #             method = lm,
  #             formula = y ~ x + I(x^2),
  #             se = FALSE) +
  scale_x_continuous(breaks = c(6, 12,18)) +
  scale_colour_manual(values = pal2) +
  ylim(0,6) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm and 6cm)- gobies and blennies - all visits") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# gob 3 first 9, gob 6 last 9 (same graph) --------------------------------

gob_3_first9_sum <- gob_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927)) %>% 
  mutate(fish.size = 3)

gob_6_last9_sum <- gob_6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754)) %>% 
  mutate(fish.size = 6)


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = gob_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_jitter(data = gob_6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_line(data = gob_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_line(data = gob_6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_errorbar(data = gob_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = gob_6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_point(data = gob_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = gob_6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_smooth(data = gob_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_smooth(data = gob_6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_vline(xintercept = 10, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  scale_x_continuous(breaks = c(3, 6, 9, 12,15,18)) +
  scale_colour_manual(values = pal2) +
  ylim(0,6) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean crypto recruit density over time (3cm first 9, 6cm last 9)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )



# HERBIVORES --------------------------------------------------------------

herb_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_3_with_0.csv")
herb_3_with_0 <- herb_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

herb_6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_6/herb_6_with_0.csv")
herb_6_with_0 <- herb_6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)


# herb 3 first 9, herb_6 last 9 (same graph) ------------------------------

herb_3_first9 <- herb_3_with_0 %>% 
  filter(visit %in% (1:10))

herb_3_first9_sum <- herb_3_first9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(927)) %>% 
  mutate(fish.size = 3)

herb_6_last9 <- herb_6_with_0 %>% 
  filter(visit %in% (10:18))

herb_6_last9_sum <- herb_6_last9 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(754)) %>% 
  mutate(fish.size = 6)


ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = herb_3_first9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_jitter(data = herb_6_last9,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_line(data = herb_3_first9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_line(data = herb_6_last9_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.2,
            alpha = 0.4) +
  geom_errorbar(data = herb_3_first9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_errorbar(data = herb_6_last9_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.4,
                size = 0.5) +
  geom_point(data = herb_3_first9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_point(data = herb_6_last9_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.4) +
  geom_smooth(data = herb_3_first9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_smooth(data = herb_6_last9_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  geom_vline(xintercept = 10, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  scale_x_continuous(breaks = c(3, 6, 9, 12,15,18)) +
  scale_colour_viridis(discrete = TRUE) +
  ylim(0,9) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean herbivore recruit density over time (3cm first 9, 6cm last 9)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

