# playing with ANCOVA


# data files --------------------------------------------------------------

ARD_3_dr_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_3/daily rate over time/ARD_3_dr_rate.csv")
ARD_3_dr_rate <- ARD_3_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c( "control","0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c( "High", "Low")))

# packages and pallettes --------------------------------------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(broom)
library(ggpubr)
library(rstatix)
library(MASS)

install.packages("ggpubr")
install.packages("MASS")


# ancova of all data - daily recruitment rate over time -------------------------------------------------------------------------

#same as ANOVA (normality, homogeneity of variance, random independent sames, and linear relationship bw dependent (y) and independent (x))

#daily rate ~ days_since_outplanting (for each treat*comp)

#visually estimate linearity from scatter plots

ggplot(data = ARD_3_dr_rate,
       aes(x = days_since_outplanting,
           y = dr_density)) +
  geom_point()+
  geom_smooth(method = "loess") +
  facet_grid(complexity~treatment) +
  ylim(-0.5,0.5)

#check homogeneity of regression slopes
# checks that there isn't any significant interaction between covariate and grouping variables

#make a new grouping variable called group, compute anova

ARD_3_dr_rate <- ARD_3_dr_rate %>%
  unite(col = "group", treatment, complexity) %>% 
  anova_test(dr_density ~ group*days_since_outplanting)

#there is homogeneity of regression slopes as the interaction terms (bw days since outplanting and the grouping vars
# treatment and complexity) was not significant (p was over 0.05)

#normality of residuals

model <- lm(dr_density ~ days_since_outplanting + treatment*complexity, data = ARD_3_dr_rate)
model.metrics <- augment(model)
head(model.metrics, 3)
shapiro_test(model.metrics$.resid)
plot(model)


#shapiro wilks test was significant..so residuals are not normaly distributed..........

#homogeneity of variances

levene_test(.resid ~ treatment*complexity, data = model.metrics)
#result is not-significant, so we can assume homogeneity of resudal variabces for all groups

#check for outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#I have a LOT of outliers accounding to this test lol. it's making a list of observations whose standard residuals
# are greater than 3 in absolute value



#run the model

#ok I so I have outliers and non-normality of residuals... but i still kind of want to run the model to see what it says
# then try doing a summary of data to compare. (mean dialy recruitment of that days_since_outplant per treat*comp)


res.aov <- ARD_3_dr_rate %>% 
  anova_test(dr_density ~ days_since_outplanting + treatment*complexity)
get_anova_table(res.aov)
#no sig difference in daily recruitment date bw groups. check calculations for daily recruitment rate and
# if you need to transform? #calculations are fine


# ancova of mean daily recruitment rate for each time point (days_since_outplanting) ---------------

ARD_dr_rate_sum <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>% 
  mutate(density.se = density.sd/sqrt(1632))

ggplot()+
  geom_point(data = ARD_dr_rate_sum,
             aes(x = days_since_outplanting, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_errorbar(data = ARD_dr_rate_sum,
                aes(x = days_since_outplanting, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.5,
                size = 0.5)+
  geom_line(data = ARD_dr_rate_sum,
            aes(x = days_since_outplanting,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1,
            alpha = 0.5)+
  geom_smooth(data =  ARD_dr_rate_sum,
              aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              # method = lm,
              se = TRUE,
              alpha = 0.35)+
  facet_grid(complexity~treatment)+
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  # ylim(-2.5,2.5) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50)) +
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Recruitment~rate(fish~m^{2}~day^{1})),
       colour = "Treatment") +
  ggtitle("Daily Recruitment Rate")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  )


# run model:

ARD_3_dr_rate_sum <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>% 
  mutate(density.se = density.sd/sqrt(1632))

#daily rate ~ days_since_outplanting (for each treat*comp)

#visually estimate linearity from scatter plots

ggplot(data = ARD_3_dr_rate_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point()+
  geom_smooth(method = "loess") +
  facet_grid(complexity~treatment)
  # ylim(-0.5,0.5)

#check homogeneity of regression slopes
# checks that there isn't any significant interaction between covariate and grouping variables

#make a new grouping variable called group, compute anova

ARD_3_dr_rate_sum <- ARD_3_dr_rate_sum %>%
  unite(col = "group", treatment, complexity) %>% 
  anova_test(density.mean ~ group*days_since_outplanting)

#there is no homogeneity of regression slopes as the interaction terms (bw days since outplanting and the grouping vars
# treatment and complexity) was significant (p was under 0.05)
# first assumption violation...

#normality of residuals

model <- lm(density.mean ~ days_since_outplanting + treatment*complexity, data = ARD_3_dr_rate_sum)
model.metrics <- augment(model)
head(model.metrics, 3)
shapiro_test(model.metrics$.resid)
plot(model)


#shapiro wilks test was significant..so residuals are not normal..........
#second assumption violation

#homogeneity of variances

levene_test(.resid ~ treatment*complexity, data = model.metrics)
#result is not-significant, so we can assume homogeneity of resudal variabces for all groups

#check for outliers
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#I have a 4 of outliers. ( it's making a list of observations whose standard residuals
# are greater than 3 in absolute value)

#run the model

#ok I have no homogeneity of regression slopes and non normal residuals.. but i still kind of want to run the model to see what it says
# then try doing a summary of data to compare. (mean dialy recruitment of that days_since_outplant per treat*comp)


res.aov2 <- ARD_3_dr_rate_sum %>% 
  anova_test(density.mean ~ days_since_outplanting + treatment* complexity)

#getting error message that cols treatments and complexity don't exist.... :S 
get_anova_table(res.aov)



# ancova of overall recruitment for each treat*comp -----------------------


ARD_dr_rate_sum_box <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment)


#same as ANOVA (normality, homogeneity of variance, random independent sames, and linear relationship bw dependent (y) and independent (x))

# one way ancova
# treat treatment like a continuous response, with 2 groups (complexity - low / high)
#mean rate ~ treatment*complexity

ARD_3_dr_rate_col <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>%
  mutate(density.se = density.sd/sqrt(1632))

#visually estimate linearity from scatter plots

ggplot(data = ARD_3_dr_rate_col,
       aes(x = treatment,
           y = density.mean, 
           colour = complexity)) +
  geom_point()+
  geom_smooth(method = "loess")
  # ylim(-0.5,0.5)

