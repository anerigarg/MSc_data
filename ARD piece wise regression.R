# Piecewise Regression


# data files --------------------------------------------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

#This one has days since outplanting as a col: 
ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_3_with_0_new_2  <- ARD_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0.csv")
ARD_4to6_with_0 <- ARD_4to6_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)


# packages and pallettes --------------------------------------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(PNWColors)
library(ggpmisc)
library(purrr)
library(broom)
library(segmented)
library(gapminder)
library(patchwork)

# install.packages("tidyverse")
# install.packages("viridis")
# install.packages("PNWColors")
# install.packages("ggpmisc")
# install.packages("purrr")
# install.packages("segmented")
install.packages("gapminder")
install.packages("patchwork")

pal <- pnw_palette(6, name = "Starfish", type = "discrete")
pal1 <- pnw_palette(3, name = "Winter", type = "continuous")
pal2 <-pnw_palette(6, name = "Cascades", type = "continuous")
pal3 <-pnw_palette(3, name = "Lake", type = "continuous")


# initial plots (ARD3 and ARD4to6 same graph, all time points) ------------

ARD_3_with_0_sum <- ARD_3_with_0 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ARD_4to6_with_0_sum <- ARD_4to6_with_0 %>% 
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
            size = 1.5,
            alpha = 0.5) +
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
             alpha = 0.7,
             shape = 17) +
  # geom_smooth(data = ARD_3_with_0,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             size = 2,
  #             # method = lm,
  #             # formula = y ~ x + I(x^2),
  #             se = FALSE ) +
   scale_x_continuous(breaks = c(3,6, 9,12, 15,18)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
    ylim(0,21) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (3cm)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )



ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_with_0,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              size = 1) +
  geom_line(data = ARD_4to6_with_0_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_4to6_with_0_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_with_0_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  # geom_smooth(data = ARD_4to6_with_0,
  #             aes(x = visit,
#                 y = density.mean,
#                 colour = treatment,
#                 group = treatment),
#             size = 2,
#             # method = lm,
#             # formula = y ~ x + I(x^2),
#             se = FALSE ) +
scale_x_continuous(breaks = c(3,6, 9,12, 15,18)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,11) +
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time (4-6 cm)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

#It is WAY too messy when plotted together


# Segmented package (youtube tutprial) (test) -------------------------------------------------------


# https://www.youtube.com/watch?v=onfXC1qe7LI&ab_channel=ShokoufehMirzaei

# estimates linear and GLM with one or more segmented relatinships in linear predictor
# provides estimates of the slopes and breakpoints (and Standard errors)

# iterative procedure, needs starting values only for the breakpoint paramteters

#basic glm: (linear)
ARD_3_0_L <- ARD_3_with_0 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low")

glm_ARD3_0_L <- glm(density ~ visit, data = ARD_3_0_L, family = "poisson")
plot(glm_ARD3_0_L)
summary(glm_ARD3_0_L)


#quadratic glm
ARD_3_0_L_quad <- ARD_3_0_L %>% 
  mutate(visit2 = visit^2)

glm_quad_ARD3_0_L <- glm(density ~ visit + visit2, data = ARD_3_0_L_quad, family = "poisson")
summary(glm_quad_ARD3_0_L)


#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k) xk)

ARD3_0_L_pwr <- ARD_3_0_L %>% 
  group_by(visit) %>% 
  summarize(density.mean = mean(density))
  
attach(ARD3_0_L_pwr)
ARD3_0_L_pwr
dummyknot = rep(0,length(visit))

dummyknot[visit > 9] = 1

ARD3_0_L_pwr$Xdif = visit - 9
ARD3_0_L_pwr$DN = dummyknot
ARD3_0_L_pwr$X = ARD3_0_L_pwr$Xdif * ARD3_0_L_pwr$DN
ARD3_0_L_pwr

ARD3_0_L_regout <- lm(density.mean ~ visit + X, data = ARD3_0_L_pwr)
summary(ARD3_0_L_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

# Y = 4.1372 + 0.2214visit + -0.2388 (visit-9) xk
#can replace visit by which visit
#may be more useful to do days since outplanting as a predictor variable...

#make a lookup table with date and day since out-planting for each plot?


# segmented - muggeo paper ------------------------------------------------
ARD_3_0_L_sum
fit.glm <- glm(density ~ days_since_outplanting, data = ARD_3_0_L)
fit.seg <- segmented(fit.glm, seg.Z = days_since_outplanting, psi = list(x = 4,9,23, 30, 43))
#getting error message saying wrong number f terms in seg.Z or psi...
#have tried playing around with the values in psi, added a list to include many possible breakpoints even...


# multiple linear regression analysis -------------------------------------

ARD_3_0_L_left <- ARD_3_0_L %>% 
  filter(days_since_outplanting <= 23)

ARD_3_0_L_left_lm <- lm(density ~ days_since_outplanting, data = ARD_3_0_L_left)
summary(ARD_3_0_L_left_lm)

# non-significant p value
# 

# plots with days since outplant (staggered june 4 & 5) ------------------------------------------

ARD_3_with_0_new <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new.csv")
ARD_3_with_0_new<- ARD_3_with_0_new %>%  
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_with_0_new_sum <- ARD_3_with_0_new %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_with_0_new,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_with_0_new_sum,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_with_0_new_sum,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_with_0_new_sum,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_with_0_new_sum,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,11) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time - staggered start (3cm)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# plots with days since outplant (all starting june 5) USE THIS ONE--------------------

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_3_with_0_new_2<- ARD_3_with_0_new_2 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>%  
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_with_0_new__2sum <- ARD_3_with_0_new_2 %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

write.csv(ARD_3_with_0_new__2sum, "ARD_3_with_0_new__2sum.csv")

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_with_0_new_2,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_with_0_new__2sum,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_with_0_new__2sum,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_with_0_new__2sum,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_with_0_new__2sum,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = "glm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,11) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time - june 5 start (3cm)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

#try plotting median to see how it looks --> essentially the same pattern as mean but more dimunited
ARD_3_with_0_new__2sum_median <- ARD_3_with_0_new_2 %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.median = median(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1587))

# write.csv(ARD_3_with_0_new__2sum, "ARD_3_with_0_new__2sum.csv")

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_with_0_new_2,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_with_0_new__2sum_median,
            aes(x = days_since_outplanting,
                y = density.median,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_with_0_new__2sum_median,
                aes(x = days_since_outplanting, 
                    ymin = density.median - density.se, 
                    ymax = density.median + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_with_0_new__2sum_median,
             aes(x = days_since_outplanting, 
                 y = density.median,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_with_0_new__2sum_median,
              aes(x = days_since_outplanting,
                  y = density.median,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = "glm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,11) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("median fish recruit density over time - june 5 start (3cm)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

# manual 1 break-point in R (youtube) --> with days since outplant (test) ----------------


# https://www.youtube.com/watch?v=onfXC1qe7LI&ab_channel=ShokoufehMirzaei

# estimates linear and GLM with one or more segmented relatinships in linear predictor
# provides estimates of the slopes and breakpoints (and Standard errors)

# iterative procedure, needs starting values only for the breakpoint paramteters

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_3_with_0_new_2<- ARD_3_with_0_new_2 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>%  
  distinct(plot_grid_visit, .keep_all = TRUE)

#basic glm: (linear)
ARD_3_0_L <- ARD_3_with_0_new_2 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low")

#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k) xk)

ARD3_0_L_pwr <- ARD_3_0_L %>% 
  group_by(days_since_outplanting) %>% 
  summarize(density.mean = mean(density))

attach(ARD3_0_L_pwr)
ARD3_0_L_pwr
dummyknot = rep(0,length(days_since_outplanting))

dummyknot[days_since_outplanting > 25] = 1

ARD3_0_L_pwr$Xdif = days_since_outplanting - 25
ARD3_0_L_pwr$DN = dummyknot
ARD3_0_L_pwr$X = ARD3_0_L_pwr$Xdif * ARD3_0_L_pwr$DN
ARD3_0_L_pwr

ARD3_0_L_regout <- lm(density.mean ~ days_since_outplanting + X, data = ARD3_0_L_pwr)
summary(ARD3_0_L_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

# density = 2.8595 + 0.08105 * days_since_outplanting - 0.1285 (days_since_outplanting - 25) * xk
# so if days since outplanting is over 25, xk is 1
# if days since outplanting is less than 25 xk is 0
# except R square value is tiny --> maybe I should change the breakpoint? 

#try and plot?



ARD_3_0_L_sum <- ARD_3_with_0_new__2sum %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low")

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_0_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_0_L_sum,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_0_L_sum,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_0_L_sum,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_0_L_sum,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c( "firebrick")) +
  ylim(0,11) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time - 0% Low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

  

# segmented package (test) ------------------------------------------------------

# https://stackoverflow.com/questions/8758646/piecewise-regression-with-r-plotting-the-segments

# example: 
library(segmented)
set.seed(12)
xx <- 1:100
zz <- runif(100)
yy <- 2 + 1.5*pmax(xx - 35, 0) - 1.5*pmax(xx - 70, 0) + 15*pmax(zz - .5, 0) + 
  rnorm(100,0,2)
dati <- data.frame(x = xx, y = yy, z = zz)
out.lm <- lm(y ~ x, data = dati)
o <- segmented(out.lm, seg.Z = ~x, psi = list(x = c(30,60)),
               control = seg.control(display = FALSE)
)
dat2 = data.frame(x = xx, y = broken.line(o)$fit)

library(ggplot2)
ggplot(dati, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'blue')

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_3_with_0_new_2  <- ARD_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3_0_L <- ARD_3_with_0_new_2 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low") %>% 
  select(treatment, complexity, days_since_outplanting, density)

ARD_3_0_L_lm <- lm(density ~ days_since_outplanting, data = ARD_3_0_L)
ARD_3_0_L_seg <- segmented(ARD_3_0_L_lm, seg.Z = ~ days_since_outplanting, psi = list(days_since_outplanting = c(10,30)),
                           control = seg.control(display = FALSE))
#getting error message: no breakpoint estimated

#sept 16: one person said to try forcing the command to repeat istelf until it decides there's no error in the model. 

lm.model <- lm(density ~ days_since_outplanting, data = ARD_3_0_L)
if.false <- F
while(if.false == F){
  tryCatch({
    s <- segmented(lm.model, seg.Z =~ydata, psi = NA)
    if.false <- T
  }, error = function(e){
  }, finally = {})
}


#try with 0 high
ARD_3_0_H <- ARD_3_with_0_new_2 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "High") %>% 
  select(treatment, complexity, days_since_outplanting, density)

ARD_3_0_H_lm <- lm(density ~ days_since_outplanting, data = ARD_3_0_H)
ARD_3_0_H_seg <- segmented(ARD_3_0_H_lm, seg.Z = ~ days_since_outplanting, psi = list(days_since_outplanting = c(15,35)),
                           control = seg.control(display = FALSE))
#no psi code
ARD_3_0_H_seg <- segmented(ARD_3_0_H_lm, seg.Z = ~ days_since_outplanting, control = seg.control(display = FALSE))
ggplot(ARD_3_0_H_lm, aes(x = days_since_outplanting, y = density)) +
  geom_point() +
  # geom_smooth()
  geom_line(data = ARD_3_0_H_seg, color = 'blue')
#oh this looks wrong lol. i think i need to do it with summed data?

#maybe I should try with the sum data?

ARD_3_0_L <- ARD_3_with_0_new_2 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low") %>% 
  select(treatment, complexity, days_since_outplanting, density)

ARD_3_0_L_sum <- ARD_3_0_L %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(133))

ARD_3_0_L_lm <- lm(density.mean ~ days_since_outplanting, data = ARD_3_0_L_sum)
ARD_3_0_L_seg <- segmented(ARD_3_0_L_lm, seg.Z = ~ days_since_outplanting, #psi = list(days_since_outplanting = c(20,50)),
                           control = seg.control(display = FALSE))

#try taking out the psi piece of the code since that's the part where you visually set the break point
ARD_3_0_L_seg <- segmented(ARD_3_0_L_lm, seg.Z = ~ days_since_outplanting, control = seg.control(display = FALSE))
ggplot(ARD_3_0_L_lm, aes(x = days_since_outplanting, y = density.mean)) +
  geom_point() +
  geom_line(data = ARD_3_0_L_seg, color = 'blue')


# strucchange package -----------------------------------------------------

install.packages("strucchange")
library(strucchange)

ARD_3_0_L <- ARD_3_with_0_new_2 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low") %>% 
  select(treatment, complexity, days_since_outplanting, density)

breakpoints(density ~ days_since_outplanting, data = ARD_3_0_L(days_since_outplanting, density), h = 3)




# Manually plot first peak for each treat*comp ----------------------------

# ARD_3 -------------------------------------------------------------------


# Control High ------------------------------------------------------------

ARD_3_C_H <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "control")

ARD_3_C_H_sum <- ARD_3_C_H %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(120))

#plot two linear slopes:
ARD_3_C_H_sum_left <- ARD_3_C_H %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(73))

ARD_3_C_H_sum_right <- ARD_3_C_H %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(54))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_C_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_C_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_C_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_C_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_C_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_C_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_C_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
geom_smooth(data = ARD_3_C_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 2,
            method = "lm",
            # family = "binomial",
            # formula = y ~ x + I(x^2),
            se = TRUE ) +
geom_smooth(data = ARD_3_C_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 2,
            method = "lm",
            # family = "binomial",
            # formula = y ~ x + I(x^2),
            se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("Control High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank()
  )

#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k)) xk)

ARD3_C_H_pwr <- ARD_3_C_H %>% 
  group_by(days_since_outplanting) %>% 
  summarize(density.mean = mean(density))

attach(ARD3_C_H_pwr)
ARD3_C_H_pwr
dummyknot = rep(0,length(days_since_outplanting))

dummyknot[days_since_outplanting > 23] = 1

ARD3_C_H_pwr$Xdif = days_since_outplanting - 23
ARD3_C_H_pwr$DN = dummyknot
ARD3_C_H_pwr$X = ARD3_C_H_pwr$Xdif * ARD3_C_H_pwr$DN
ARD3_C_H_pwr

ARD3_C_H_regout <- lm(density.mean ~ days_since_outplanting + X, data = ARD3_C_H_pwr)
summary(ARD3_C_H_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

#check which values apply where

# density = 2.8595 + 0.08105 * days_since_outplanting - 0.1285 (days_since_outplanting - 25) * xk
# so if days since outplanting is over 25, xk is 1
# if days since outplanting is less than 25 xk is 0
# except R square value is tiny --> maybe I should change the breakpoint? 


# 0% High -----------------------------------------------------------------

ARD_3_0_H <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "0%")

ARD_3_0_H_sum <- ARD_3_0_H %>%
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(density.se = density.sd/sqrt(126))

#plot two linear slopes:
ARD_3_0_H_sum_left <- ARD_3_0_H %>% 
  filter(days_since_outplanting <= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(94))

ARD_3_0_H_sum_right <- ARD_3_0_H %>% 
  filter(days_since_outplanting >= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(40))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_0_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_0_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_0_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_0_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_0_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_0_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_0_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_0_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_0_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 30, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("0% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k)) xk)

# ARD3_C_H_pwr <- ARD_3_C_H %>% 
#   group_by(days_since_outplanting) %>% 
#   summarize(density.mean = mean(density))
# 
# attach(ARD3_C_H_pwr)
# ARD3_C_H_pwr
# dummyknot = rep(0,length(days_since_outplanting))
# 
# dummyknot[days_since_outplanting > 23] = 1
# 
# ARD3_C_H_pwr$Xdif = days_since_outplanting - 23
# ARD3_C_H_pwr$DN = dummyknot
# ARD3_C_H_pwr$X = ARD3_C_H_pwr$Xdif * ARD3_C_H_pwr$DN
# ARD3_C_H_pwr
# 
# ARD3_C_H_regout <- lm(density.mean ~ days_since_outplanting + X, data = ARD3_C_H_pwr)
# summary(ARD3_C_H_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

#check which values apply where

# density = 2.8595 + 0.08105 * days_since_outplanting - 0.1285 (days_since_outplanting - 25) * xk
# so if days since outplanting is over 25, xk is 1
# if days since outplanting is less than 25 xk is 0
# except R square value is tiny --> maybe I should change the breakpoint? 


# 30% High ----------------------------------------------------------------

ARD_3_30_H <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "30%")

ARD_3_30_H_sum <- ARD_3_30_H %>%
  select(treatment, complexity, days_since_outplanting, density) %>%
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(density.se = density.sd/sqrt(121))

#plot two linear slopes:
ARD_3_30_H_sum_left <- ARD_3_30_H %>% 
  filter(days_since_outplanting <= 5) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(30))

ARD_3_30_H_sum_right <- ARD_3_30_H %>% 
  filter(days_since_outplanting >= 5) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(98))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_30_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_30_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_30_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_30_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_30_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_30_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_30_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_30_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_30_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 5, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("30% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k)) xk)

# ARD3_C_H_pwr <- ARD_3_C_H %>% 
#   group_by(days_since_outplanting) %>% 
#   summarize(density.mean = mean(density))
# 
# attach(ARD3_C_H_pwr)
# ARD3_C_H_pwr
# dummyknot = rep(0,length(days_since_outplanting))
# 
# dummyknot[days_since_outplanting > 23] = 1
# 
# ARD3_C_H_pwr$Xdif = days_since_outplanting - 23
# ARD3_C_H_pwr$DN = dummyknot
# ARD3_C_H_pwr$X = ARD3_C_H_pwr$Xdif * ARD3_C_H_pwr$DN
# ARD3_C_H_pwr
# 
# ARD3_C_H_regout <- lm(density.mean ~ days_since_outplanting + X, data = ARD3_C_H_pwr)
# summary(ARD3_C_H_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

#check which values apply where

# density = 2.8595 + 0.08105 * days_since_outplanting - 0.1285 (days_since_outplanting - 25) * xk
# so if days since outplanting is over 25, xk is 1
# if days since outplanting is less than 25 xk is 0
# except R square value is tiny --> maybe I should change the breakpoint? 


# 50% High ----------------------------------------------------------------

ARD_3_50_H <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "50%")

ARD_3_50_H_sum <- ARD_3_50_H %>%
  select(treatment, complexity, days_since_outplanting, density) %>%
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(density.se = density.sd/sqrt(128))

#plot two linear slopes:
ARD_3_50_H_sum_left <- ARD_3_50_H %>% 
  filter(days_since_outplanting <= 3) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(24))

ARD_3_50_H_sum_right <- ARD_3_50_H %>% 
  filter(days_since_outplanting >= 3) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(112))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_50_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_50_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_50_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_50_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_50_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_50_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_50_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_50_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_50_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 3, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("50% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k)) xk)

# ARD3_C_H_pwr <- ARD_3_C_H %>% 
#   group_by(days_since_outplanting) %>% 
#   summarize(density.mean = mean(density))
# 
# attach(ARD3_C_H_pwr)
# ARD3_C_H_pwr
# dummyknot = rep(0,length(days_since_outplanting))
# 
# dummyknot[days_since_outplanting > 23] = 1
# 
# ARD3_C_H_pwr$Xdif = days_since_outplanting - 23
# ARD3_C_H_pwr$DN = dummyknot
# ARD3_C_H_pwr$X = ARD3_C_H_pwr$Xdif * ARD3_C_H_pwr$DN
# ARD3_C_H_pwr
# 
# ARD3_C_H_regout <- lm(density.mean ~ days_since_outplanting + X, data = ARD3_C_H_pwr)
# summary(ARD3_C_H_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

#check which values apply where

# density = 2.8595 + 0.08105 * days_since_outplanting - 0.1285 (days_since_outplanting - 25) * xk
# so if days since outplanting is over 25, xk is 1
# if days since outplanting is less than 25 xk is 0
# except R square value is tiny --> maybe I should change the breakpoint? 


# 70% High ----------------------------------------------------------------

ARD_3_70_H <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "70%")

ARD_3_70_H_sum <- ARD_3_70_H %>%
  select(treatment, complexity, days_since_outplanting, density) %>%
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(density.se = density.sd/sqrt(121))

#plot two linear slopes:
ARD_3_70_H_sum_left <- ARD_3_70_H %>% 
  filter(days_since_outplanting <= 4) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(26))

ARD_3_70_H_sum_right <- ARD_3_70_H %>% 
  filter(days_since_outplanting >= 4) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(99))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_70_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_70_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_70_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_70_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_70_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_70_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_70_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_70_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_70_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 4, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c( "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("70% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k)) xk)

# ARD3_C_H_pwr <- ARD_3_C_H %>% 
#   group_by(days_since_outplanting) %>% 
#   summarize(density.mean = mean(density))
# 
# attach(ARD3_C_H_pwr)
# ARD3_C_H_pwr
# dummyknot = rep(0,length(days_since_outplanting))
# 
# dummyknot[days_since_outplanting > 23] = 1
# 
# ARD3_C_H_pwr$Xdif = days_since_outplanting - 23
# ARD3_C_H_pwr$DN = dummyknot
# ARD3_C_H_pwr$X = ARD3_C_H_pwr$Xdif * ARD3_C_H_pwr$DN
# ARD3_C_H_pwr
# 
# ARD3_C_H_regout <- lm(density.mean ~ days_since_outplanting + X, data = ARD3_C_H_pwr)
# summary(ARD3_C_H_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

#check which values apply where

# density = 2.8595 + 0.08105 * days_since_outplanting - 0.1285 (days_since_outplanting - 25) * xk
# so if days since outplanting is over 25, xk is 1
# if days since outplanting is less than 25 xk is 0
# except R square value is tiny --> maybe I should change the breakpoint? 



# 100% High ---------------------------------------------------------------

ARD_3_100_H <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "100%")

ARD_3_100_H_sum <- ARD_3_100_H %>%
  select(treatment, complexity, days_since_outplanting, density) %>%
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(density.se = density.sd/sqrt(124))

#plot two linear slopes:
ARD_3_100_H_sum_left <- ARD_3_100_H %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(76))

ARD_3_100_H_sum_right <- ARD_3_100_H %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(48))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_100_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_100_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_100_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_100_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_100_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_100_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_100_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_100_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_100_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("100% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

#iterative piecewise regression:

# Y = b0 + b1visit + b2 (x-x(k)) xk)

# ARD3_C_H_pwr <- ARD_3_C_H %>% 
#   group_by(days_since_outplanting) %>% 
#   summarize(density.mean = mean(density))
# 
# attach(ARD3_C_H_pwr)
# ARD3_C_H_pwr
# dummyknot = rep(0,length(days_since_outplanting))
# 
# dummyknot[days_since_outplanting > 23] = 1
# 
# ARD3_C_H_pwr$Xdif = days_since_outplanting - 23
# ARD3_C_H_pwr$DN = dummyknot
# ARD3_C_H_pwr$X = ARD3_C_H_pwr$Xdif * ARD3_C_H_pwr$DN
# ARD3_C_H_pwr
# 
# ARD3_C_H_regout <- lm(density.mean ~ days_since_outplanting + X, data = ARD3_C_H_pwr)
# summary(ARD3_C_H_regout)

# Y = b0 + b1visit + b2 (visit-9) xk

#check which values apply where

# density = 2.8595 + 0.08105 * days_since_outplanting - 0.1285 (days_since_outplanting - 25) * xk
# so if days since outplanting is over 25, xk is 1
# if days since outplanting is less than 25 xk is 0
# except R square value is tiny --> maybe I should change the breakpoint? 



# Control Low -------------------------------------------------------------

ARD_3_C_L <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "control")

ARD_3_C_L_sum <- ARD_3_C_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(127))

#plot two linear slopes:
ARD_3_C_L_sum_left <- ARD_3_C_L %>% 
  filter(days_since_outplanting <= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(68))

ARD_3_C_L_sum_right <- ARD_3_C_L %>% 
  filter(days_since_outplanting >= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(67))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_C_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_C_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_C_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_C_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_C_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_C_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_C_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_C_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_C_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 13, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("Control Low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none"
    # axis.title.x = element_blank()
  )


# 0% Low ------------------------------------------------------------------

ARD_3_0_L <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "0%")

ARD_3_0_L_sum <- ARD_3_0_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(133))

#plot two linear slopes:
ARD_3_0_L_sum_left <- ARD_3_0_L %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(86))

ARD_3_0_L_sum_right <- ARD_3_0_L %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(55))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_0_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_0_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_0_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_0_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_0_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_0_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_0_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_0_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_0_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("0% Low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )


# 30% low -----------------------------------------------------------------

ARD_3_30_L <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "30%")

ARD_3_30_L_sum <- ARD_3_30_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(136))

#plot two linear slopes:
ARD_3_30_L_sum_left <- ARD_3_30_L %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(88))

ARD_3_30_L_sum_right <- ARD_3_30_L %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(56))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_30_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_30_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_30_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_30_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_30_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_30_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_30_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_30_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_30_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("30% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )


# 50% low -----------------------------------------------------------------

ARD_3_50_L <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "50%")

ARD_3_50_L_sum <- ARD_3_50_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(134))

#plot two linear slopes:
ARD_3_50_L_sum_left <- ARD_3_50_L %>% 
  filter(days_since_outplanting <= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(104))

ARD_3_50_L_sum_right <- ARD_3_50_L %>% 
  filter(days_since_outplanting >= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(38))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_50_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_50_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_50_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_50_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_50_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_50_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_50_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_50_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_50_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 30, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("50% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )



# 70% low -----------------------------------------------------------------

ARD_3_70_L <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "70%")

ARD_3_70_L_sum <- ARD_3_70_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(134))

#plot two linear slopes:
ARD_3_70_L_sum_left <- ARD_3_70_L %>% 
  filter(days_since_outplanting <= 7) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(46))

ARD_3_70_L_sum_right <- ARD_3_70_L %>% 
  filter(days_since_outplanting >= 7) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(94))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_70_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_70_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_70_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_70_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_70_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_70_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_70_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_70_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_70_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 7, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("70% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )


# 100% low ----------------------------------------------------------------

ARD_3_100_L <- ARD_3_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "100%")

ARD_3_100_L_sum <- ARD_3_100_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(135))

#plot two linear slopes:
ARD_3_100_L_sum_left <- ARD_3_100_L %>% 
  filter(days_since_outplanting <= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(71))

ARD_3_100_L_sum_right <- ARD_3_100_L %>% 
  filter(days_since_outplanting >= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(72))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_100_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_3_100_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_3_100_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_100_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_3_100_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_100_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_3_100_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_100_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_3_100_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 13, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("100% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )


# ARD 4-6 -----------------------------------------------------------------

ARD_4to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0_new_2.csv")

#full plot

ARD_4to6_with_0_new_2<- ARD_4to6_with_0_new_2 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>%  
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_4to6_with_0_new__2sum <- ARD_4to6_with_0_new_2 %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1539))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_with_0_new_2,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.4,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_with_0_new__2sum,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_with_0_new__2sum,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_with_0_new__2sum,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_with_0_new__2sum,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = "glm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,11) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time - june 5 start (4-6cm)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )

# ARD_3 and ARD4to6 together ----------------------------------------------

ggplot() +
  facet_grid(complexity~treatment) +
  # geom_jitter(data = ARD_3_with_0_new_2,
  #             aes(x = days_since_outplanting,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.2,
  #             shape = 2,
  #             size = 1) +
  geom_line(data = ARD_3_with_0_new__2sum,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_with_0_new__2sum,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_with_0_new__2sum,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_3_with_0_new__2sum,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = "glm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = FALSE) +
  # geom_jitter(data = ARD_4to6_with_0_new_2,
  #             aes(x = days_since_outplanting,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.4,
  #             # shape = 2,
  #             size = 1) +
  geom_line(data = ARD_4to6_with_0_new__2sum,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_4to6_with_0_new__2sum,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_with_0_new__2sum,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
             # shape = 17) +
  geom_line(data = ARD_4to6_with_0_new__2sum,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
            stat = "smooth",
              size = 2,
              alpha = 0.5,
              # linetype = 5,
              method = "loess",
              # # family = "binomial",
              # # formula = y ~ x + I(x^2),
              se = FALSE ) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  # ylim(0,11) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time - june 5 start (3cm and 4-6cm)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
  )


# ARD 4to6 ----------------------------------------------------------------



# Control High ------------------------------------------------------------

ARD_4to6_C_H <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "control")

ARD_4to6_C_H_sum <- ARD_4to6_C_H %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(120))

#plot two linear slopes:
ARD_4to6_C_H_sum_left <- ARD_4to6_C_H %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(73))

ARD_4to6_C_H_sum_right <- ARD_4to6_C_H %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(54))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_C_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_C_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_C_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_C_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_C_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_C_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_C_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_C_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_C_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("Control High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank()
  )


# 0% High -----------------------------------------------------------------

ARD_4to6_0_H <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "0%")

# ARD_4to6_0_H_sum <- ARD_4to6_0_H %>% 
#   select(treatment, complexity, days_since_outplanting, density) %>% 
#   group_by(days_since_outplanting, treatment, complexity) %>%
#   summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
#   mutate(density.se = density.sd/sqrt(120))

#plot two linear slopes:
ARD_4to6_0_H_sum_left <- ARD_4to6_0_H %>% 
  filter(days_since_outplanting <= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(94))

ARD_4to6_0_H_sum_right <- ARD_4to6_0_H %>% 
  filter(days_since_outplanting >= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(40))

ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_0_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_0_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_0_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_0_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_0_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_0_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_0_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_0_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_0_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 30, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("0% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )


# 30% High ----------------------------------------------------------------

ARD_4to6_30_H <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "30%")

# ARD_4to6_0_H_sum <- ARD_4to6_0_H %>% 
#   select(treatment, complexity, days_since_outplanting, density) %>% 
#   group_by(days_since_outplanting, treatment, complexity) %>%
#   summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
#   mutate(density.se = density.sd/sqrt(120))

#plot two linear slopes:
ARD_4to6_30_H_sum_left <- ARD_4to6_30_H %>% 
  filter(days_since_outplanting <= 5) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(30))

ARD_4to6_30_H_sum_right <- ARD_4to6_30_H %>% 
  filter(days_since_outplanting >= 5) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(98))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_30_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_30_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_30_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_30_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_30_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_30_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_30_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_30_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_30_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 5, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("30% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# 50% High ----------------------------------------------------------------

ARD_4to6_50_H <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "50%")

# ARD_4to6_0_H_sum <- ARD_4to6_0_H %>% 
#   select(treatment, complexity, days_since_outplanting, density) %>% 
#   group_by(days_since_outplanting, treatment, complexity) %>%
#   summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
#   mutate(density.se = density.sd/sqrt(120))

#plot two linear slopes:
ARD_4to6_50_H_sum_left <- ARD_4to6_50_H %>% 
  filter(days_since_outplanting <= 3) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(24))

ARD_4to6_50_H_sum_right <- ARD_4to6_50_H %>% 
  filter(days_since_outplanting >= 3) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(112))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_50_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_50_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_50_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_50_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_50_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_50_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_50_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_50_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_50_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 3, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("50% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# 70% High ----------------------------------------------------------------

ARD_4to6_70_H <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "70%")

# ARD_4to6_0_H_sum <- ARD_4to6_0_H %>% 
#   select(treatment, complexity, days_since_outplanting, density) %>% 
#   group_by(days_since_outplanting, treatment, complexity) %>%
#   summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
#   mutate(density.se = density.sd/sqrt(120))

#plot two linear slopes:
ARD_4to6_70_H_sum_left <- ARD_4to6_70_H %>% 
  filter(days_since_outplanting <= 4) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(26))

ARD_4to6_70_H_sum_right <- ARD_4to6_70_H %>% 
  filter(days_since_outplanting >= 4) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(99))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_70_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_70_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_70_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_70_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_70_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_70_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_70_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_70_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_70_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 4, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c( "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("70% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# 100% High ---------------------------------------------------------------

ARD_4to6_100_H <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "100%")

ARD_4to6_100_H_sum <- ARD_4to6_100_H %>%
  select(treatment, complexity, days_since_outplanting, density) %>%
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(density.se = density.sd/sqrt(124))

#plot two linear slopes:
ARD_4to6_100_H_sum_left <- ARD_4to6_100_H %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(76))

ARD_4to6_100_H_sum_right <- ARD_4to6_100_H %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(48))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_100_H,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_100_H_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_100_H_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_100_H_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_100_H_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_100_H_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_100_H_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_100_H_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_100_H_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("100% High") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# Control Low -------------------------------------------------------------

ARD_4to6_C_L <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "control")

ARD_4to6_C_L_sum <- ARD_4to6_C_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(127))

#plot two linear slopes:
ARD_4to6_C_L_sum_left <- ARD_4to6_C_L %>% 
  filter(days_since_outplanting <= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(68))

ARD_4to6_C_L_sum_right <- ARD_4to6_C_L %>% 
  filter(days_since_outplanting >= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(67))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_C_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_C_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_C_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_C_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_C_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_C_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_C_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_C_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_C_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 13, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("Control Low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none"
    # axis.title.x = element_blank()
  )

# 0% Low ------------------------------------------------------------------

ARD_4to6_0_L <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "0%")

ARD_4to6_0_L_sum <- ARD_4to6_0_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(133))

#plot two linear slopes:
ARD_4to6_0_L_sum_left <- ARD_4to6_0_L %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(86))

ARD_4to6_0_L_sum_right <- ARD_4to6_0_L %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(55))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_0_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_0_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_0_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_0_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_0_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_0_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_0_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_0_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_0_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("0% Low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )

# 30% low -----------------------------------------------------------------

ARD_4to6_30_L <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "30%")

ARD_4to6_30_L_sum <- ARD_4to6_30_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(136))

#plot two linear slopes:
ARD_4to6_30_L_sum_left <- ARD_4to6_30_L %>% 
  filter(days_since_outplanting <= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(88))

ARD_4to6_30_L_sum_right <- ARD_4to6_30_L %>% 
  filter(days_since_outplanting >= 23) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(56))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_30_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_30_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_30_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_30_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_30_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_30_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_30_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_30_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_30_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 23, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("30% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )

# 50% low -----------------------------------------------------------------

ARD_4to6_50_L <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "50%")

ARD_4to6_50_L_sum <- ARD_4to6_50_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(134))

#plot two linear slopes:
ARD_4to6_50_L_sum_left <- ARD_4to6_50_L %>% 
  filter(days_since_outplanting <= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(104))

ARD_4to6_50_L_sum_right <- ARD_4to6_50_L %>% 
  filter(days_since_outplanting >= 30) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(38))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_50_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_50_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_50_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_50_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_50_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_50_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_50_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_50_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_50_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 30, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("goldenrod1", "mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("50% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )

# 70% low -----------------------------------------------------------------

ARD_4to6_70_L <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "70%")

ARD_4to6_70_L_sum <- ARD_4to6_70_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(134))

#plot two linear slopes:
ARD_4to6_70_L_sum_left <- ARD_4to6_70_L %>% 
  filter(days_since_outplanting <= 7) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(46))

ARD_4to6_70_L_sum_right <- ARD_4to6_70_L %>% 
  filter(days_since_outplanting >= 7) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(94))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_70_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_70_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_70_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_70_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_70_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_70_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_70_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_70_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_70_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 7, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("mediumseagreen", "royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("70% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )

# 100% low ----------------------------------------------------------------

ARD_4to6_100_L <- ARD_4to6_with_0_new_2 %>% 
  filter(complexity == "Low") %>% 
  filter(treatment == "100%")

ARD_4to6_100_L_sum <- ARD_4to6_100_L %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(135))

#plot two linear slopes:
ARD_4to6_100_L_sum_left <- ARD_4to6_100_L %>% 
  filter(days_since_outplanting <= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(71))

ARD_4to6_100_L_sum_right <- ARD_4to6_100_L %>% 
  filter(days_since_outplanting >= 13) %>% 
  select(treatment, complexity, days_since_outplanting, density) %>% 
  group_by(days_since_outplanting, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(72))


ggplot() +
  # facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_4to6_100_L,
              aes(x = days_since_outplanting,
                  y = density,
                  colour = treatment),
              alpha = 0.2,
              shape = 2,
              size = 1) +
  geom_line(data = ARD_4to6_100_L_sum_left,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_line(data = ARD_4to6_100_L_sum_right,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.5) +
  geom_errorbar(data = ARD_4to6_100_L_sum_left,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_errorbar(data = ARD_4to6_100_L_sum_right,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_4to6_100_L_sum_left,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_point(data = ARD_4to6_100_L_sum_right,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7,
             shape = 17) +
  geom_smooth(data = ARD_4to6_100_L_sum_left,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_smooth(data = ARD_4to6_100_L_sum_right,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = "lm",
              # family = "binomial",
              # formula = y ~ x + I(x^2),
              se = TRUE ) +
  geom_vline(xintercept = 13, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60)) +
  scale_colour_manual(values = c("royalblue")) +
  ylim(0,10) +
  # xlim(0,20) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("100% low") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank()
  )
