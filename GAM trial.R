# GAM trial

# http://environmentalcomputing.net/intro-to-gams/


# notes on GAMs -----------------------------------------------------------

# GAMs are wiggly GLM's, so an extension of the GLM with a smoothing function
# it adds a smoothing term to the linear model --> can also add a quadratic term as predictor?


# packages ----------------------------------------------------------------

library(mgcv)
library(tidyverse)
library(ggplot2)
library(readr)

install.packages("nlstools")
library(nlstools)

# df ----------------------------------------------------------------------

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

hist(ARD_3_with_0_new_2$density)

ARD_3_with_0_new_2 <- ARD_3_with_0_new_2 %>% 
  mutate(log.density = log(density))

hist(ARD_3_with_0_new_2$log.density) #still quite skewed

ARD_3_with_0_new_2 <- ARD_3_with_0_new_2 %>% 
  mutate(sqrt.density = sqrt(density))

hist(ARD_3_with_0_new_2$sqrt.density) #still quite skewed, but the lesat skewed of the log

ARD_3_with_0_new_2 <- ARD_3_with_0_new_2 %>% 
  mutate(logcube.density = log(density^(1/3)))

hist(ARD_3_with_0_new_2$logcube.density) #also still skewed

ARD_3_with_0_new_2 <- ARD_3_with_0_new_2 %>% 
  mutate(cube.density = density^(1/3))

hist(ARD_3_with_0_new_2$cube.density) #also still skewed

# going through the steps of the turorial ---------------------------------

#1) try fitting a normal linear model

# lm_1 <- lm(density ~ days_since_outplanting + complexity + treatment, data = ARD_3_with_0_new_2)
# summary(lm_1)



lm_2 <- lm(density ~ days_since_outplanting + complexity*treatment, data = ARD_3_with_0_new_2)
summary(lm_2)
# plot the lm

ggplot(ARD_3_with_0_new_2, aes(days_since_outplanting, density)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  facet_grid(complexity ~ treatment) 
  # ylim(0,10)

#plot residuals
plot(lm_2)
#no bueno, but they do it for all treat*comps together...

gam_1 <- gam(density ~ s(days_since_outplanting + complexity*treatment),data = ARD_3_with_0_new_2, method = "REML")

ggplot(ARD_3_with_0_new_2, aes(days_since_outplanting, density )) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x)) +
  facet_grid(complexity ~ treatment)
  # ylim(0,8)

par(mfrow = c(2,2))
gam.check(gam_1)
#residuals are not normally distributed, nor does it look like there homogeneity of variance.... but does that matter for a gam?

summary(gam_1)
#significane of smooth terms shows density sig changes over time (p is 0.000119)
# edf (estimated degrees of freedom) - the larger the number the more wiggly the fitted model. if it's around 1, tend to be close to linear
# only 1.35% of deviance explained in this model... Rsq adj is 0.0123

# ok but what if different models fit dif treat*comps differently?

# GAM trial - with mean data ----------------------------------------------

ARD_3_sum <- ARD_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>%
  mutate(complexity = factor(complexity, levels = c ("Low", "High"),
                             labels = c("Low Complexity", "High Complexity"))) %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1539))

write.csv(ARD_3_sum, "ARD_3_sum.csv")

gam_2 <- gam(density.mean ~ s(days_since_outplanting + complexity*treatment),data = ARD_3_sum, method = "REML")

ggplot(ARD_3_sum, aes(days_since_outplanting, density.mean)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~s(x)) +
  facet_grid(complexity ~ treatment) +
  ylim(0,8)

par(mfrow = c(1,1))
gam.check(gam_2)

summary(gam_2)



# quadratic model ---------------------------------------------------------

ARD_3_with_0_new_2$days_since_outplanting2 <- ARD_3_with_0_new_2$days_since_outplanting^2

quad_1 <- lm(density ~ days_since_outplanting + days_since_outplanting2 + treatment*complexity, data = ARD_3_with_0_new_2)
summary(quad_1)
# predictions for High complexity * 70% : density = -0.0008631(days)^2 +  2.7424615

# Call:
#   lm(formula = density ~ days_since_outplanting + days_since_outplanting2 + 
#        treatment * complexity, data = ARD_3_with_0_new_2)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -5.225 -2.353 -1.103  1.104 27.692 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  2.7424615  0.4084964   6.714 2.67e-11 ***
#   days_since_outplanting       0.0680932  0.0253191   2.689 0.007236 ** 
#   days_since_outplanting2     -0.0008631  0.0005402  -1.598 0.110345    
# treatment0%                 -0.2329184  0.4985239  -0.467 0.640412    
# treatment30%                -0.8302384  0.5035296  -1.649 0.099387 .  
# treatment50%                -1.3154424  0.4966276  -2.649 0.008162 ** 
#   treatment70%                -0.5407256  0.5035283  -1.074 0.283049    
# treatment100%               -0.6877254  0.5004803  -1.374 0.169603    
# complexityLow               -1.6538056  0.4977022  -3.323 0.000912 ***
#   treatment0%:complexityLow    2.2042324  0.6954824   3.169 0.001558 ** 
#   treatment30%:complexityLow   1.8190374  0.6972635   2.609 0.009174 ** 
#   treatment50%:complexityLow   2.1851382  0.6934851   3.151 0.001659 ** 
#   treatment70%:complexityLow   2.5116582  0.6984734   3.596 0.000334 ***
#   treatment100%:complexityLow  3.4860353  0.6956612   5.011 6.04e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.908 on 1525 degrees of freedom
# Multiple R-squared:  0.04806,	Adjusted R-squared:  0.03994 
# F-statistic: 5.922 on 13 and 1525 DF,  p-value: 8.104e-11

daysValues <- seq(0,50,0.1)

densityPredict <- predict(quad_1, list(days_since_outplanting = daysValues, days2 = daysValues^2))

ggplot(ARD_3_with_0_new_2, aes(days_since_outplanting, density)) + 
  geom_point(alpha = 0.35) + 
  geom_smooth(method = "lm", formula = y ~ poly(x,2,raw = TRUE)) +
  facet_grid(complexity ~ treatment) +
  ylim(0,4)


plot(ARD_3_with_0_new_2$days_since_outplanting, ARD_3_with_0_new_2$density, pch = 16)


ARD_3_treat_sum <- ARD_3_with_0_new_2 %>% 
  group_by(treatment, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1539))



#plot to visualize treatment as a gradient
ggplot() + 
  geom_point(data = ARD_3_treat_sum, 
             aes(x = treatment, 
                 y = density.mean,
                 colour = complexity),
             size = 2) + 
  geom_errorbar(data = ARD_3_treat_sum,
                aes(x = treatment, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = complexity),
                width = 0.2,
                size = 0.5) +
  theme_classic()

ggplot(data = ARD_3_with_0_new_2,
       aes(x = treatment,
           y = density,
           colour = complexity)) +
  geom_jitter()
  # ylim(0,8)


# non-linear least squares regression -------------------------------------

#from Colden et al 2017
#change the function formula for dif function types (linear, logisit, Gompertz, power, quadratic) to figure out
#which function fits data best. linear is to rule out that the data isn't linear ()
#ok, but if the High comp ones are linear and the low comp ones are, could analyze separately? 

nls_lm <- nls(density ~ days_since_outplanting + treatment*complexity + b, data = ARD_3_with_0_new_2, 
             start = c(b = 0))


