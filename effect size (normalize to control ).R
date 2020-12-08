# Effect size (normalize to control) 


# packages and pallettes --------------------------------------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(viridis)
library(PNWColors)

pal2 <-pnw_palette(6, name = "Cascades", type = "continuous")
pal <- pnw_palette(6, name = "Starfish", type = "discrete")


# dfs ---------------------------------------------------------------------

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_3_with_0_new_2  <- ARD_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_dr_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_3/daily rate over time/ARD_3_dr_rate.csv")
ARD_3_dr_rate <- ARD_3_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))
  # distinct(plot_grid_visit, .keep_all = TRUE)

# relative recruitment rate - ARD 3------------------------------------

ARD_3_rate_no_control <- ARD_3_dr_rate %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(dr.mean = mean(dr_density), dr.sd = sd(dr_density)) %>% 
  mutate(dr.se = dr.sd/sqrt(1360)) 

ARD_3_rate_no_control$days_comp <- paste(ARD_3_rate_no_control$days_since_outplanting, "-", ARD_3_rate_no_control$complexity)

ARD_3_rate_control <- ARD_3_dr_rate %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(dr.control = mean(dr_density), dr.sd = sd(dr_density)) %>% 
  mutate(dr.se = dr.sd/sqrt(272))

ARD_3_rate_control$days_comp <- paste(ARD_3_rate_control$days_since_outplanting, "-", ARD_3_rate_control$complexity)

#export to make a lookup table
write.csv(ARD_3_rate_control, "ARD_3_rate_control.csv")

#copy and paste density.control and days_comp to a new df and call ARD_3_control_lookup
ARD_3_rate_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/rate/ARD_3_rate_control_lookup.csv")

ARD_3_rate_joined <- full_join(ARD_3_rate_no_control, ARD_3_rate_control_lookup)

ARD_3_rate_effect <- ARD_3_rate_joined %>% 
  mutate(effect.size = dr.mean - dr.control)

write.csv(ARD_3_rate_effect, "ARD_3_rate_effect.csv")

ARD_3_rate_effect <- ARD_3_rate_effect %>% 
  mutate(treatment = factor(treatment, levels = c( "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = ARD_3_rate_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  # geom_smooth(aes(x = days_since_outplanting,
  #                 y = effect.size,
  #                 colour = treatment),
  #             size = 2,
  #             method = "lm",
  #             formulat = y ~ splines ::bs(x,3),
  #             # family = "binomial",
  #             # formula = y ~ x + I(x^2),
  #             se = FALSE) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              alpha = 0.5,
              size = 2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Recruitment~Rate~(fish~m^{2}~day^{1})),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  ylim(-1.5, 1) +
  ggtitle("Effect size (Relative Recruitment Rate over Time (2nd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )


#colum plot
ARD_3_rate_effect_col <- ARD_3_rate_effect %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(effect.mean = mean(effect.size), effect.sd = sd(effect.size)) %>%
  mutate(effect.se = effect.sd/sqrt(170))


ggplot(data = ARD_3_rate_effect_col) +
  geom_col(aes(x = treatment,
               y = effect.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = effect.mean - effect.se,
                    ymax = effect.mean + effect.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Relative~Recruitment~rate~(fish~m^{2}~day^{1}))) +
  ggtitle("Relative Daily Recruitment Rate")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

# relative density over time: ----------------------------------------------



# ARD 3  ---------------------------------------------------------

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_3_with_0_new_2 <- ARD_3_with_0_new_2 %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_grouped_no_control <- ARD_3_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

ARD_3_grouped_no_control$days_comp <- paste(ARD_3_grouped_no_control$days_since_outplanting, "-", ARD_3_grouped_no_control$complexity)

ARD_3_control <- ARD_3_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

ARD_3_control$days_comp <- paste(ARD_3_control$days_since_outplanting, "-", ARD_3_control$complexity)

# ARD_3_control_lookup <- ARD_3_control %>% 
#   select(density.control, days_comp)
#this is also saving treatment and days_since_outplanting. so export and save just these two cols, then impor the lookup

write.csv(ARD_3_control, "ARD_3_control.csv")

ARD_3_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_control_lookup.csv")

ARD_3_joined <- full_join(ARD_3_grouped_no_control, ARD_3_control_lookup) %>% 
  mutate(effect.size = density.mean - density.control)

ARD_3_effect <- ARD_3_joined %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = ARD_3_effect) +
  # facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  # geom_smooth(aes(x = days_since_outplanting,
  #                 y = effect.size,
  #                 colour = treatment),
  #             size = 2,
  #             method = "lm",
  #             formulat = y ~ splines ::bs(x,3),
  #             # family = "binomial",
  #             # formula = y ~ x + I(x^2),
  #             se = FALSE) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # geom_vline(xintercept = 25, 
  #            linetype = "dotted", 
  #            colour = "grey65", 
  #            size = 1) +
  ylim(-3, 5) +
  # xlim(0,50) +
  ggtitle("Relative Density over time (2nd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# ARD 3 test - test w other spp as NA  ---------------------------------------------------------

ARD_3_with_0_new_2test <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2test.csv")
ARD_3_with_0_new_2test<- ARD_3_with_0_new_2test %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_grouped_no_control_test <- ARD_3_with_0_new_2test %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

ARD_3_grouped_no_control_test$days_comp <- paste(ARD_3_grouped_no_control_test$days_since_outplanting, "-", ARD_3_grouped_no_control_test$complexity)

ARD_3_control_test <- ARD_3_with_0_new_2test %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

ARD_3_control_test$days_comp <- paste(ARD_3_control_test$days_since_outplanting, "-", ARD_3_control_test$complexity)

write.csv(ARD_3_control_test, "ARD_3_control_test.csv")
#make lookup with just density.control and days_comp

ARD_3_control_test_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_control_test_lookup.csv")

ARD_3_joined_test <- full_join(ARD_3_grouped_no_control_test, ARD_3_control_test_lookup) %>% 
  mutate(effect.size = density.mean - density.control)

ARD_3_effect_test<- ARD_3_joined_test %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = ARD_3_effect_test) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  # geom_smooth(aes(x = days_since_outplanting,
  #                 y = effect.size,
  #                 colour = treatment),
  #             size = 2,
  #             method = "lm",
  #             formulat = y ~ splines ::bs(x,3),
  #             # family = "binomial",
  #             # formula = y ~ x + I(x^2),
  #             se = FALSE) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # geom_vline(xintercept = 25, 
  #            linetype = "dotted", 
  #            colour = "grey65", 
  #            size = 1) +
  ylim(-3, 5) +
  # xlim(0,50) +
  ggtitle("Relative Density over time (2nd order poly) n2") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )



# ARD 4 to 6  ---------------------------------------------------------

ARD_4to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0_new_2.csv")
ARD_4to6_with_0_new_2 <- ARD_4to6_with_0_new_2 %>% 
mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                          labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_4to6_no_control <- ARD_4to6_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(5004)) 

ARD_4to6_no_control$days_comp <- paste(ARD_4to6_no_control$days_since_outplanting, "-", ARD_4to6_no_control$complexity)

ARD_4to6_control <- ARD_4to6_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(862))

ARD_4to6_control$days_comp <- paste(ARD_4to6_control$days_since_outplanting, "-", ARD_4to6_control$complexity)

#export to make a lookup table
write.csv(ARD_4to6_control, "ARD_4to6_control.csv")

#copy and paste density.control and days_comp to a new df and call ARD_3_control_lookup
ARD_4to6_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_4to6/all fish/density/ARD_4to6_control_lookup.csv")

ARD_4to6_joined <- full_join(ARD_4to6_no_control, ARD_4to6_control_lookup)

ARD_4to6_effect <- ARD_4to6_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

ARD_4to6_effect1 <- ARD_4to6_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = ARD_4to6_effect1) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,4,raw = TRUE),
              size = 2) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  ylim(-5, 5) +
  # xlim(0,50) +
  ggtitle("Relative Density over time 4cm to 6cm (3rd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

#colum plot
ARD_4to6_effect_col <- ARD_4to6_effect1 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(effect.mean = mean(effect.size), effect.sd = sd(effect.size)) %>%
  mutate(effect.se = effect.sd/sqrt(170))


ggplot(data = ARD_4to6_effect_col) +
  geom_col(aes(x = treatment,
               y = effect.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = effect.mean - effect.se,
                    ymax = effect.mean + effect.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Relative~Recruitment~rate~(fish~m^{2}~day^{1}))) +
  ggtitle("Relative Daily Recruitment Rate")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# herb 3-------------------------------------------

herb_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_3_with_0_new_2.csv")
herb_3_with_0_new_2 <- herb_3_with_0_new_2 %>% 
mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                          labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

herb_3_no_control <- herb_3_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

herb_3_no_control$days_comp <- paste(herb_3_no_control$days_since_outplanting, "-", herb_3_no_control$complexity)

herb_3_control <- herb_3_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

herb_3_control$days_comp <- paste(herb_3_control$days_since_outplanting, "-", herb_3_control$complexity)

#export to make a lookup table
write.csv(herb_3_control, "herb_3_control.csv")

#copy and paste density.control and days_comp to a new df and call herb_3_control_lookup
herb_3_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/herb/herb_3_control_lookup.csv")

herb_3_joined <- full_join(herb_3_no_control, herb_3_control_lookup)

herb_3_effect <- herb_3_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

herb_3_effect <- herb_3_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = herb_3_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_viridis(discrete = TRUE) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # geom_vline(xintercept = 25, 
  #            linetype = "dotted", 
  #            colour = "grey65", 
  #            size = 1) +
  # ylim(-5, 10) +
  # xlim(0,50) +
  ggtitle("Relative Density over time herbivores 3cm (3rd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )


# herb 4 to 6   ---------------------------------------------------------

herb_4to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_4to6/herb_4to6_with_0_new_2.csv")
herb_4to6_with_0_new_2 <- herb_4to6_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

herb_4to6_no_control <- herb_4to6_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

herb_4to6_no_control$days_comp <- paste(herb_4to6_no_control$days_since_outplanting, "-", herb_4to6_no_control$complexity)

herb_4to6_control <- herb_4to6_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

herb_4to6_control$days_comp <- paste(herb_4to6_control$days_since_outplanting, "-", herb_4to6_control$complexity)

#export to make a lookup table
write.csv(herb_4to6_control, "herb_4to6_control.csv")

#copy and paste density.control and days_comp to a new df and call ARD_3_control_lookup
herb_4to6_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/herb/herb_4to6_control_lookup.csv")

herb_4to6_joined <- full_join(herb_4to6_no_control, herb_4to6_control_lookup)

herb_4to6_effect <- herb_4to6_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

herb_4to6_effect <- herb_4to6_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = herb_4to6_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_viridis(discrete = TRUE) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # ylim(-5, 10) +
  # xlim(0,50) +
  ggtitle("Relative Density over time herbivores 4cm to 6cm (3rd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )


# gob 2.5 no masked goby-----------------------------------------------------------------

gob_2.5_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_2.5/gob_2.5_with_0_new_2.csv")
gob_2.5_with_0_new_2 <- gob_2.5_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

gob_2.5_no_control <- gob_2.5_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

gob_2.5_no_control$days_comp <- paste(gob_2.5_no_control$days_since_outplanting, "-", gob_2.5_no_control$complexity)

gob_2.5_control <- gob_2.5_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

gob_2.5_control$days_comp <- paste(gob_2.5_control$days_since_outplanting, "-", gob_2.5_control$complexity)

#export to make a lookup table
write.csv(gob_2.5_control, "gob_2.5_control.csv")

#copy and paste density.control and days_comp to a new df and call gob_3_control_lookup
gob_2.5_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/gob/gob_2.5_control_lookup.csv")

gob_2.5_joined <- full_join(gob_2.5_no_control, gob_2.5_control_lookup)

gob_2.5_effect <- gob_2.5_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

gob_2.5_effect <- gob_2.5_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = gob_2.5_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_manual(values = pal2) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # geom_vline(xintercept = 25, 
  #            linetype = "dotted", 
  #            colour = "grey65", 
  #            size = 1) +
  # ylim(-0.5, 0.75) +
  # xlim(0,50) +
  ggtitle("Relative Density over time Gobies and Blennies 2.5cm (3rd order poly) no masked goby") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )


# gob 3-------------------------------------------

gob_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_3/gob_3_with_0_new_2.csv")
gob_3_with_0_new_2 <- gob_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

gob_3_no_control <- gob_3_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

gob_3_no_control$days_comp <- paste(gob_3_no_control$days_since_outplanting, "-", gob_3_no_control$complexity)

gob_3_control <- gob_3_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

gob_3_control$days_comp <- paste(gob_3_control$days_since_outplanting, "-", gob_3_control$complexity)

#export to make a lookup table
write.csv(gob_3_control, "gob_3_control.csv")

#copy and paste density.control and days_comp to a new df and call gob_3_control_lookup
gob_3_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/gob/gob_3_control_lookup.csv")

gob_3_joined <- full_join(gob_3_no_control, gob_3_control_lookup)

gob_3_effect <- gob_3_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

gob_3_effect <- gob_3_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = gob_3_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_manual(values = pal2) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # geom_vline(xintercept = 25, 
  #            linetype = "dotted", 
  #            colour = "grey65", 
  #            size = 1) +
  # ylim(-5, 10) +
  # xlim(0,50) +
  ggtitle("Relative Density over time Gobies and Blennies 3cm (3rd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# gob 3 to 6   ---------------------------------------------------------

gob_3to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_3to6/gob_3to6_with_0_new_2.csv")
gob_3to6_with_0_new_2 <- gob_3to6_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

gob_3to6_no_control <- gob_3to6_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

gob_3to6_no_control$days_comp <- paste(gob_3to6_no_control$days_since_outplanting, "-", gob_3to6_no_control$complexity)

gob_3to6_control <- gob_3to6_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

gob_3to6_control$days_comp <- paste(gob_3to6_control$days_since_outplanting, "-", gob_3to6_control$complexity)

#export to make a lookup table
write.csv(gob_3to6_control, "gob_3to6_control.csv")

#copy and paste density.control and days_comp to a new df and call ARD_3_control_lookup
gob_3to6_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/gob/gob_3to6_control_lookup.csv")


gob_3to6_joined <- full_join(gob_3to6_no_control, gob_3to6_control_lookup)

gob_3to6_effect <- gob_3to6_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

gob_3to6_effect <- gob_3to6_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = gob_3to6_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_manual(values = pal2) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # ylim(-0.5, 0.5) +
  # xlim(0,50) +
  ggtitle("Relative Density over time gobies and blennies 3cm to 6cm (3rd order poly) no masked goby") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# gob 4 to 6   ---------------------------------------------------------

gob_4to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_4to6/gob_4to6_with_0_new_2.csv")
gob_4to6_with_0_new_2 <- gob_4to6_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

gob_4to6_no_control <- gob_4to6_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

gob_4to6_no_control$days_comp <- paste(gob_4to6_no_control$days_since_outplanting, "-", gob_4to6_no_control$complexity)

gob_4to6_control <- gob_4to6_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

gob_4to6_control$days_comp <- paste(gob_4to6_control$days_since_outplanting, "-", gob_4to6_control$complexity)

#export to make a lookup table
write.csv(gob_4to6_control, "gob_4to6_control.csv")

#copy and paste density.control and days_comp to a new df and call ARD_3_control_lookup
gob_4to6_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/gob/gob_4to6_control_lookup.csv")


gob_4to6_joined <- full_join(gob_4to6_no_control, gob_4to6_control_lookup)

gob_4to6_effect <- gob_4to6_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

gob_4to6_effect <- gob_4to6_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = gob_4to6_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_manual(values = pal2) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  ylim(-0.5, 0.5) +
  # xlim(0,50) +
  ggtitle("Relative Density over time gobies and blennies 4cm to 6cm (3rd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# dam 3-------------------------------------------

dam_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_3/dam_3_with_0_new_2.csv")
dam_3_with_0_new_2 <- dam_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

dam_3_no_control <- dam_3_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

dam_3_no_control$days_comp <- paste(dam_3_no_control$days_since_outplanting, "-", dam_3_no_control$complexity)

dam_3_control <- dam_3_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

dam_3_control$days_comp <- paste(dam_3_control$days_since_outplanting, "-", dam_3_control$complexity)

#export to make a lookup table
write.csv(dam_3_control, "dam_3_control.csv")

#copy and paste density.control and days_comp to a new df and call gob_3_control_lookup
dam_3_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/dam/dam_3_control_lookup.csv")

dam_3_joined <- full_join(dam_3_no_control, dam_3_control_lookup)

dam_3_effect <- dam_3_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

dam_3_effect <- dam_3_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = dam_3_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_manual(values = pal) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # geom_vline(xintercept = 25, 
  #            linetype = "dotted", 
  #            colour = "grey65", 
  #            size = 1) +
  # ylim(-5, 10) +
  # xlim(0,50) +
  ggtitle("Relative Density over time damselfish 3cm (3rd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )


# dam 4 to 6 --------------------------------------------------------------

dam_4to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_4to6/dam_4to6_with_0_new_2.csv")
dam_4to6_with_0_new_2 <- dam_4to6_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

dam_4to6_no_control <- dam_4to6_with_0_new_2 %>% 
  filter(treatment != "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1292)) 

dam_4to6_no_control$days_comp <- paste(dam_4to6_no_control$days_since_outplanting, "-", dam_4to6_no_control$complexity)

dam_4to6_control <- dam_4to6_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.control = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(247))

dam_4to6_control$days_comp <- paste(dam_4to6_control$days_since_outplanting, "-", dam_4to6_control$complexity)

#export to make a lookup table
write.csv(dam_4to6_control, "dam_4to6_control.csv")

#copy and paste density.control and days_comp to a new df and call ARD_3_control_lookup
dam_4to6_control_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/dam/dam_4to6_control_lookup.csv")

dam_4to6_joined <- full_join(dam_4to6_no_control, dam_4to6_control_lookup)

dam_4to6_effect <- dam_4to6_joined %>% 
  mutate(effect.size = density.mean - density.control)

# write.csv(ARD_3_effect, "ARD_3_effect.csv")
# ARD_3_effect <- read_csv("Artificial coral experiment/data/Art Recruit data/effect size calcuations/ARD_3/ARD_3_effect.csv")

dam_4to6_effect <- dam_4to6_effect %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

ggplot(data = dam_4to6_effect) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = effect.size,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = effect.size,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  # scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  scale_colour_manual(values = pal) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  # ylim(-0.5, 0.5) +
  # xlim(0,50) +
  ggtitle("Relative Density over time damselfish 4cm to 6cm (3rd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )
