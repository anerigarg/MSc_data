# Carrying Capacity and Maximum density


# packages and pallettes --------------------------------------------------

library(readr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(viridis)
library(fishualize)
library(PNWColors)


# dfs ---------------------------------------------------------------------

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)


# top density -------------------------------------------------------------


ARD_top_dens <- ARD_3_with_0_new_2 %>% 
  group_by(treatment, complexity, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1539))
  # arrange(desc(density.mean),by_group = TRUE)

write.xlsx(ARD_top_dens, "ARD_top_dens.xlsx")

# sorted by complexity, treatment, density.mean
# kept only the top density --> plot to see how it looks

ARD_top1_dens <- read_csv("Artificial coral experiment/data/Art Recruit data/carrying capacity/top_density/ARD_3/ARD_top1_dens.csv")
ARD_top1_dens <- ARD_top1_dens %>%
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = ARD_top1_dens) +
  geom_point(aes(x = days_since_outplanting,
                  y = density.mean,
                 colour = treatment),
             size = 2) +
  facet_grid(~ complexity) + 
  theme_classic()


ggplot(data = ARD_top1_dens) +
    geom_errorbar(aes(x = treatment, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se), 
                colour = "gray44",
                position = position_dodge(0.2),
                width = 0.2,
                size = 0.3)+
  geom_point(aes(x = treatment,
                 y = density.mean,
                 colour = treatment),
             size = 3) +
  facet_grid(~ complexity) +
  labs(x = expression(Treatment),
       y = expression(Max~Density~(fish~m^{2})),
       colour = "Treatment") + 
  theme_classic() +
  theme(
    legend.position = "none")


# mean top 3 density ------------------------------------------------------

  
