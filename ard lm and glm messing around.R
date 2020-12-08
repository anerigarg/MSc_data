# start some glm modeling - ch 2


# packages ----------------------------------------------------------------

library(tidyverse)
library(broom)

# density ~ treatment -----------------------------------------------------

M1 <- glm(density ~ treatment, family = "poisson", data = ARD_3_with_0)
summary(M1)

#I shoud make a col called treatment.m to use in modeling that shows treatment as a continuous var, maybe make -1 control? 
# AIC = inf because it is calculated using negative lof-likelihood (so log probability of oberved values in model) for non-integer values
# the negative log-likelihood is inf... so need to make 

ARD_3_with_0_new_2 <- ARD_3_with_0_new_2 %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High")))

M2 <- glm(density ~ treatment*complexity, family = "poisson", data = ARD_3_with_0_new_2)
=summary(M1)

#over time? repeated measure anova? but not normally distributed... look at zuur for time series

ARD_3_abun_treatcomp <- ARD_3_with_0_new_2 %>%
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(treatment, complexity) %>% 
  summarize(abundance = sum(presence))

ggplot(data = ARD_3_abun_treatcomp,
       aes(x = treatment,
           y = abundance)) +
  geom_point() +
  facet_grid(~complexity)

ARD_3_dens_treatcomp <- ARD_3_with_0_new_2 %>%
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(treatment, complexity) %>% 
  summarize(density = sum(presence)/0.79)

ggplot(data = ARD_3_dens_treatcomp,
       aes(x = treatment,
           y = density)) +
  geom_point() +
  facet_grid(~complexity)

M1 <-glm(density ~ treatment * complexity, data = ARD_3_with_0_new_2, family = "")
summary(M1)
