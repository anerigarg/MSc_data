# Density over time and change in density calculations and visualizations


# packages and pallettes --------------------------------------------------


library(readr)
library(tidyverse)
library(ggplot2)
library(openintro)
library(e1071)
library(ggpubr)


# daily rate  -------------------------------------------------------------



#1 ) import ARD_3 for mean density values
ARD_3 <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_3.csv")
View(ARD_3)

unique(ARD_3$date)

ARD_d3 <-  ARD_3 %>% 
  select(date, plot_grid, survey_number, density) %>% 
  distinct(date, plot_grid, survey_number, density) 

#2) export to use pivot table to get 0 values
write.csv(ARD_d3,"ARD_d3.csv")
#export to make a pivot table --> to get 0 values for dates and plot/grids that didn't see any fish on a given date

#3 ) import and convert to long format
ARD_d_wide <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_d_wide.csv")
View(ARD_d_wide)
#import and convert to long format

names(ARD_d_wide) #to get each date

ARD_d_convert <- ARD_d_wide %>% 
  gather(c( "04-Jun","05-Jun","06-Jun","07-Jun","09-Jun","11-Jun","13-Jun","15-Jun",
            "20-Jun","25-Jun","28-Jun","02-Jul","05-Jul","09-Jul","15-Jul","20-Jul","25-Jul","28-Jul"),
         key = date,
         value = density)

# wide to long: gather() funciton
# gather() . needs 3 pieces of info:
# 1) list vars to be moved using c(var1, var2, var3)
# 2) key = provides name of new var created by gathering ones from old df
# 3) value = provides name of new var accompanying key var

#rename Row Labels col to plot_grid
ARD_d_long <- ARD_d_convert %>% 
  rename(plot_grid = `Row Labels`)

#4) Export long format to make column called delta dentisy 
# = density at time x - density at time x-days since last visit

write.csv(ARD_d_long, "ARD_d_long.csv")
# formula: =(C3-C2)/(B3-B2)
# make sure the first of every plot_grid is 0, used formula =ABS(0)

#5) Import the new sheet with rate of recruitment data (delta/density)
ARD_d_long <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/ARD_d_long.csv")


#join ARD_d_convert to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_delta_D <- full_join(ARD_d_long, ARD_lookup)

#make treatment levels in order from control to 100% and order of dates chronological
ARD_delta_D <- ARD_delta_D %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(date = factor(date, levels = c( "04-Jun","05-Jun","06-Jun","07-Jun","09-Jun","11-Jun",
                                         "13-Jun","15-Jun","20-Jun","25-Jun","28-Jun",
                                         "02-Jul","05-Jul","09-Jul","15-Jul","20-Jul","25-Jul","28-Jul")))

ARD_3_delta_sum <- ARD_delta_D %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(date = factor(date, levels = c( "04-Jun","05-Jun","06-Jun","07-Jun","09-Jun","11-Jun",
                                         "13-Jun","15-Jun","20-Jun","25-Jun","28-Jun",
                                         "02-Jul","05-Jul","09-Jul","15-Jul","20-Jul","25-Jul","28-Jul"))) %>% 
  group_by(complexity, treatment, date) %>% 
  summarize(density.mean = mean(delta.density), density.sd = sd(delta.density)) %>% 
  mutate(density.se = density.sd/sqrt(1728))

ggplot()+
  geom_point(data = ARD_3_delta_sum,
             aes(x = date, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_errorbar(data = ARD_3_delta_sum,
                aes(x = date, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.5,
                size = 0.5)+
  geom_line(data = ARD_3_delta_sum,
            aes(x = date,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1,
            alpha = 0.5)+
  geom_smooth(data =  ARD_3_delta_sum,
              aes(x = date,
                  y = density.mean,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              se = FALSE )+
  facet_grid(complexity~treatment)+
  # ylim(0,2.5) +
  ylab("Recruitment rate (fish/m^2/time)")+
  xlab("Date")+
  ggtitle("Recruitment rate")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 8,angle = -60, hjust = 0.01),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )


hist(ARD_delta_D$delta.density)
#more of a normal dist than below

ARD_3_delta_sum_col <- ARD_delta_D %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(delta.density), density.sd = sd(delta.density)) %>% 
  mutate(density.se = density.sd/sqrt(1728))

#column plot it
ggplot(data = ARD_3_delta_sum_col) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  ylab("recruitment rate (fish/m^2)")+
  ggtitle("mean fish recruitment rate per treatment (daily rate)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# daily rate anova - tutorial ---------------------------------------------
#https://www.datanovia.com/en/lessons/anova-in-r/#two-way-independent-anova 

library(tidyverse)
library(ggpubr)
library(rstatix)
install.packages("datarium")
library(datarium)

#1) Data prep

ARD_delta_low <- ARD_delta_D %>% 
  filter(complexity == "Low")

levels(ARD_delta_low$treatment)

#2) summary stats

ARD_delta_low_sum <- ARD_delta_low %>% 
  group_by(treatment) %>% 
  get_summary_stats(delta.density, type = "mean_se")

#3) visualization

ggboxplot(ARD_delta_low, x = "treatment", y = "delta.density")
#seems to me there are quite a few outliers...

ARD_delta_low_outliers <- ARD_delta_low %>% 
  group_by(treatment) %>% 
  identify_outliers(delta.density)
#oh yeah there are def a lot of outliers.......check calculations then try this for final-initial. 
#they also suggest using the package WRS2 for more robus abova with outliers

#4) try the model
model <- lm(delta.density ~ treatment, data = ARD_delta_low)

#5) check assumptions
ggqqplot(residuals(model)) #this does not look normal
shapiro_test(residuals(model)) #p-value is significant, so NOT normal. can't use this model...
#homogenaity of variance
plot(model,1)
ARD_delta_low %>% levene_test(delta.density ~ treatment) #p-value is significant, so there are differences bw 
#variances across groups... #says to try the welch_anova_test which does not require assumption of equal variances (rstatix) package

#6) trying welch's one-way anova (when variances of groups are heterogeneous)

res.aov2 <- ARD_delta_low %>% welch_anova_test(delta.density ~ treatment)
# ggqqplot(residuals(res.aov2))...these don't work
# plot(res.aov2,1)

#pairwise comparison using Games-Howell (like Tukey post-hoc test, but for welch's test)
pwc2 <- ARD_delta_low %>% games_howell_test(delta.density ~ treatment)

# Visualization: box plots with p-values
pwc2 <- pwc2 %>% add_xy_position(x = "treatment", step.increase = 1)
ggboxplot(ARD_delta_low, x = "treatment", y = "delta.density") +
  stat_pvalue_manual(pwc2, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov2, detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )

#pairwise comparisons using paired t-tests (no assumption of equal variances)
pwc3 <- ARD_delta_low %>% 
  pairwise_t_test(
    delta.density ~ treatment, pool.sd = FALSE,
    p.adjust.method = "bonferroni"
  )
pwc3

#ok so not getting significance from any of these, going to try WRS2 package


#old code from me messing around:

#run type 1 anova for just the low complexity

ARD_delta_M1 <- ARD_delta_D %>%
  filter(complexity == "Low")

delta_M1 <- aov(density ~ treatment, data = ARD_delta_M1)
summary.aov(delta_M1)

plot(delta_M1)
#ok so the results def show non-normality...wonder if it's being driven by a few outliers




#large spread away from means...not normal.....

# kruskal wallis for daily rate -------------------------------------------


#https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/

#ok so start with the regular data visualizations I've already done

res.kruskal.delta <- ARD_delta_low %>% kruskal.test(delta.density ~ treatment)
res.kruskal.delta

ARD_delta_low %>% kruskal_effsize(delta.density ~ treatment)
#small effect of treatment on rate, effect size is 0.00489

#multiple pairwise comparisons - Dunn's test.
#Dunn's test takes into account rankinsts used in KW test 

pwc.kw.delta <- ARD_delta_low %>% dunn_test(delta.density ~ treatment, p.adjust.method = "bonferroni")
pwc.kw.delta
#all are non significant

pwc.kw.delta <- pwc.kw.delta %>% add_xy_position(x = "treatment")
ggboxplot(ARD_delta_low, x = "treatment", y = "delta.density") +
  stat_pvalue_manual(pwc.kw.delta, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal.delta, detailed = TRUE),
    caption = get_pwc_label(pwc.kw.delta)
  )

# plot for methods paer? daily recruitment rate ---------------------------

ARD_3_delta_sum_col_low <- ARD_delta_D %>% 
  filter(complexity == "Low") %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(delta.density), density.sd = sd(delta.density)) %>% 
  mutate(density.se = density.sd/sqrt(864))

ggplot(data = ARD_3_delta_sum_col_low) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) + # ~ are spaces, {is for superscript}, [is for subscript]
  ggtitle("mean fish recruitment rate per treatment (daily rate)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

#boxplot of just low to see outliers
ARD_delta_D_low <- ARD_delta_D %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  filter(complexity == "Low")

ggplot(data = ARD_delta_D_low) +
  geom_boxplot(aes(x = treatment,
                   y = delta.density,
                   fill = treatment))+
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) + # ~ are spaces, {is for superscript}, [is for subscript]
  ggtitle("mean fish recruitment rate per treatment (daily rate)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

# (final - initial / time) rate of recruitment 2.0 -----------------------------------------------------------

#1 ) import ARD_3 for mean density values
ARD_3 <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_3.csv")
View(ARD_3)

unique(ARD_3$date)

ARD_d3_2.0 <-  ARD_3 %>% 
  select(date, plot_grid, density) %>% 
  distinct(date, plot_grid, density)

#2) export to use pivot table to get 0 values
write.csv(ARD_d3_2.0,"ARD_d3_2.0.csv")
#export to make a pivot table --> to get 0 values for dates and plot/grids that didn't see any fish on a given date.
#date in Columns, plot_grid in Rows and density in Values

#3 ) import and convert to long format
ARD_d_wide_2.0 <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/(final-initial over time)/ARD_d_wide_2.0.csv")
#import and convert to long format

names(ARD_d_wide_2.0) #to get each date

ARD_d_convert_2.0 <- ARD_d_wide_2.0 %>% 
  gather(c( "04-Jun","05-Jun","06-Jun","07-Jun","09-Jun","11-Jun","13-Jun","15-Jun",
            "20-Jun","25-Jun","28-Jun","02-Jul","05-Jul","09-Jul","15-Jul","20-Jul","25-Jul","28-Jul"),
         key = date,
         value = density)

# wide to long: gather() funciton
# gather() . needs 3 pieces of info:
# 1) list vars to be moved using c(var1, var2, var3)
# 2) key = provides name of new var created by gathering ones from old df
# 3) value = provides name of new var accompanying key var

#rename Row Labels col to plot_grid
ARD_d_long_2.0 <- ARD_d_convert_2.0 %>% 
  rename(plot_grid = `Row Labels`)

ARD_d_long_2.0 <- ARD_d_long_2.0 %>% 
  filter(date == c("04-Jun","28-Jul"))
#OK PROBLEM HERE, there was no LN sampled on June 4, first one was june 5th

#4) Export long format to make column called rate 
# = =(C3-C2)/54 OR (final density-iniital density)/time (54 days)

write.csv(ARD_d_long_2.0, "ARD_d_long_2.0.csv")
# formula: =(C3-C2)/(B3-B2)
# remove duplicates and import new sheet with just rate

#5) Import the new sheet with rate of recruitment data (delta/density)
ARD_d_rate_2_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/(final-initial over time)/ARD_d_rate_2.0.csv")
View(ARD_d_rate_2_0)


#join ARD_d_convert to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_rate_2.0 <- full_join(ARD_d_rate_2_0, ARD_lookup)
hist(ARD_rate_2.0$rate)

#make treatment levels in order from control to 100% and order of dates chronological
ARD_rate_2.0_sum <- ARD_rate_2.0 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
  mutate(rate.se = rate.sd/sqrt(1728))


#plot it
ggplot(data = ARD_rate_2.0_sum) +
  geom_col(aes(x = treatment,
               y = rate.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  ylab("recruitment rate (fish/m^2)") +
  ggtitle("mean fish recruitment rate per treatment (final - initial)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# pairwise anova (final-initial) ------------------------------------------


#pairwise anova to compare differences bw treatments (just low complexity) --> type I anova.

ARD_rate_2.0_low <- ARD_rate_2.0 %>%
  filter(complexity == "Low")

M1 <- aov(rate ~ treatment, data = ARD_rate_2.0_low)
summary.aov(M1)
#M1 def looks like residuals are not scattered. #check how i see homogeneity of variance (categorical data?)
#but if I'm treating treatment as a continuous variable should I change that....

#try transforming then doing data (check what kind of disttribution to do transformation)
hist(ARD_rate_2.0_low$rate) #right skew
skewness(ARD_rate_2.0_low$rate)
#if the value is greater than 1 or less than -1 then it is highly skewed, I got 1.292665, so yup it's skewed

M2 <- aov(log1p(rate) ~ treatment, data = ARD_rate_2.0_low)
summary(M2)

ARD_rate_2.0_log <- ARD_rate_2.0_low %>% 
  mutate(log.rate = log10(rate))
hist(ARD_rate_2.0_log$log.rate) #still looks preety skewed


# one way anova (final-initial) tutorial ----------------------------------

#1) Data prep

ARD_rate_low <- ARD_rate_2.0 %>% 
  filter(complexity == "Low") %>% 
  reorder_levels(treatment, order = c("control","0%", "30%", "50%", "70%", "100%"))

levels(ARD_rate_low$treatment)
hist(ARD_rate_low$rate)

#2) summary stats

ARD_rate_low_sum <- ARD_rate_low %>% 
  group_by(treatment) %>% 
  get_summary_stats(rate, type = "mean_se")

#3) visualization

ggboxplot(ARD_rate_low, x = "treatment", y = "rate")
#way fewer outliers

ARD_rate_low_outliers <- ARD_rate_low %>% 
  group_by(treatment) %>% 
  identify_outliers(rate)
#ok so only 4 outliers, and none are considered extreme woooo

#4) try the model
model.rate <- lm(rate ~ treatment, data = ARD_rate_low)

#5) check assumptions
ggqqplot(residuals(model.rate)) #this looks more normal, still a bit of an S shape though
shapiro_test(residuals(model.rate)) #p-value is still significant, so NOT normal. can't use this model...
# BUT shapiro wilks is sensistive to large sample sizes (like this) so evan a minor deviation can throw it off
hist(ARD_rate_low$rate) # but also this looks non-normal..
#plot qqplots for each group
ggqqplot(ARD_rate_low, "rate", facet.by = "treatment") # a few points are beyond reference lines...

#homogenaity of variance
plot(model.rate,1)
ARD_rate_low %>% levene_test(rate ~ treatment) #p-value is NOT significant, so there there is homogenaity!

#6) post-hoc test (pairwise comparison with Tukey)
pwc <- ARD_rate_low %>% tukey_hsd(rate ~ treatment)
pwc

#all showing as non-significant...but not normal data, so prob not the right test

res.aov <- ARD_rate_low %>% anova_test(rate ~ treatment)
res.aov

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "treatment")
ggboxplot(ARD_rate_low, x = "treatment", y = "rate") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

#ok so not getting significance from any of these, can try WRS2 package to better accomodate outliers...first ill try w log transform

# log transformed one-way anova (final -initial)/time ---------------------

#to try an normalize data


#1) Data prep

ARD_rate_low_log <- ARD_rate_low %>% 
  mutate(log.rate = log(rate))
hist(ARD_rate_low_log$log.rate)
#still has 0 values...

ARD_rate_low_log10 <- ARD_rate_low %>% 
  mutate(log10.rate = log10(rate))
hist(ARD_rate_low_log10$log.rate)
#ok so log10 gives -inf values when dealing with 0s, so have to use log 1p for log(x+1)

ARD_rate_low_log1p <- ARD_rate_low %>% 
  mutate(log1p.rate = log1p(rate))
hist(ARD_rate_low_log1p$log1p.rate)
#uhhh except this did not change the distribution.... still right skew

ARD_rate_low_sqrt <- ARD_rate_low %>% 
  mutate(sqrt.rate = sqrt(rate))
hist(ARD_rate_low_sqrt$sqrt.rate)

#will start with log10 looks the best...
levels(ARD_rate_low_log10$treatment)

#2) summary stats

ARD_rate_log10_sum <- ARD_rate_low_log10 %>% 
  group_by(treatment) %>% 
  get_summary_stats(rate, type = "mean_se")

#3) visualization

ggboxplot(ARD_rate_low_log10, x = "treatment", y = "log10.rate")
#way fewer outliers but doesn't look like I have homogeneity of variance....
ggboxplot(ARD_rate_low_log, x = "treatment", y = "log.rate")
ggboxplot(ARD_rate_low_sqrt, x = "treatment", y = "sqrt.rate")
#also doesn't look like there's homogeneity of variance

ARD_rate_lowg10_outliers <- ARD_rate_low_log10 %>% 
  group_by(treatment) %>% 
  identify_outliers(log10.rate)
#yes, 2 extreme outliers, 1 in control, 1 in 100%

#4) try the model
model.rate.log10 <- lm(log10.rate ~ treatment, data = ARD_rate_low_log10)

#5) check assumptions
ggqqplot(residuals(model.rate)) #this looks more normal, still a bit of an S shape though
shapiro_test(residuals(model.rate)) #p-value is still significant, so NOT normal. can't use this model...
# BUT shapiro wilks is sensistive to large sample sizes (like this) so evan a minor deviation can throw it off
hist(ARD_rate_low$rate) # but also this looks non-normal..
#plot qqplots for each group
ggqqplot(ARD_rate_low, "rate", facet.by = "treatment") # a few points are beyond reference lines...

#homogenaity of variance
plot(model.rate,1)
ARD_rate_low %>% levene_test(rate ~ treatment) #p-value is NOT significant, so there there is homogenaity!

#6) post-hoc test (pairwise comparison with Tukey)
pwc <- ARD_rate_low %>% tukey_hsd(rate ~ treatment)
pwc

#all showing as non-significant...but not normal data, so prob not the right test

res.aov <- ARD_rate_low %>% anova_test(rate ~ treatment)
res.aov

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "treatment")
ggboxplot(ARD_rate_low, x = "treatment", y = "rate") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )





# kruskall wallis (final-iniital) -----------------------------------------

#https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/

#ok so start with the regular data visualizations I've already done

res.kruskal.rate <- ARD_rate_low %>% kruskal.test(rate ~ treatment)
res.kruskal.rate

ARD_rate_low %>% kruskal_effsize(rate ~ treatment)
#moderate effect of treatment on rate, effect size is 0.0841

#multiple pairwise comparisons - Dunn's test.
#Dunn's test takes into account rankinsts used in KW test 

pwc.kw.rate <- ARD_rate_low %>% dunn_test(rate ~ treatment, p.adjust.method = "bonferroni")
pwc.kw.rate

#can also use Wilcoxon's test
pwc.wilc.rate <- ARD_rate_low %>% wilcox.test(rate ~ treatment, p.adjust.method = "bonferroni")
pwc.wilc.rate
#doesn't look significant again...

pwc.kw.rate <- pwc.kw.rate %>% add_xy_position(x = "treatment")
ggboxplot(ARD_rate_low, x = "treatment", y = "rate") +
  stat_pvalue_manual(pwc.kw.rate, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal.rate, detailed = TRUE),
    caption = get_pwc_label(pwc.kw.rate)
  )

# plot for methods paper (just Low) --------------------------------------------------
ARD_rate_2.0_sum_low <- ARD_rate_2.0 %>% 
  filter(complexity == "Low") %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(treatment) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
  mutate(rate.se = rate.sd/sqrt(864))

ggplot(data = ARD_rate_2.0_sum_low) +
  geom_col(aes(x = treatment,
               y = rate.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) + # ~ are spaces, {is for superscript}, [is for subscript]
  ggtitle("mean fish recruitment rate per treatment (final - initial)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )


#boxplot of just low to see outliers
ARD_rate_2.0_low <- ARD_rate_2.0 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  filter(complexity == "Low")

ggplot(data = ARD_rate_2.0_low) +
  geom_boxplot(aes(x = treatment,
                   y = rate,
                   fill = treatment))+
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) + # ~ are spaces, {is for superscript}, [is for subscript]
  ggtitle("mean fish recruitment rate per treatment (final - initial)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )


# re-calculate ARD_3 to include 0 values GOOD CODE  ---------------------------------

ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all fish 3cm and under that have correct biomass and density calculations
ARD_3_only <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_3_only_plot_grid <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey)

write.csv(ARD_3_only_plot_grid, "ARD_3_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in second col
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate")
# used filter to just get the unique values.
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
#now import as a ARD_3_0_lookup.

ARD_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_0_lookup.csv")

#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_survey values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has density = 0
ARD_3_none <- ARD %>% 
  filter(TL > 4) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_just_0 <- inner_join(ARD_3_none, ARD_3_0_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none

#can now join ARD_3_only and ARD_just_0!!

ARD_3_with_0 <- bind_rows(ARD_3_only,ARD_just_0)
write.csv(ARD_3_with_0, "ARD_3_with_0.csv")

# (final - initial / time) rate of recruitment 3.0 (with 0 values) -----------------------------------------------------------

#1 ) use values from ARD_3_with_0
ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_with_0.csv")

unique(ARD_3_with_0$date)

ARD_rate_with_0 <- ARD_3_with_0 %>% 
  select(date, plot_grid, density) %>% 
  distinct(date, plot_grid, density)

#2) export to use pivot table to get 0 values
write.csv(ARD_rate_with_0,"ARD_rate_with_0.csv")
#export to make a pivot table --> to get 0 values for dates and plot/grids that didn't see any fish on a given date.
#date in Columns (legend), plot_grid in Rows (axis) and density in Values

#3 ) import and convert to long format
ARD_rate_wide <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/(final - initial) over time/ARD_rate_wide.csv")#import and convert to long format

names(ARD_rate_wide) #to get each date

ARD_rate_convert <- ARD_rate_wide %>% 
  gather(c( "04-Jun","05-Jun","06-Jun","07-Jun","09-Jun","11-Jun","13-Jun","15-Jun",
            "20-Jun","25-Jun","28-Jun","02-Jul","05-Jul","09-Jul","15-Jul","20-Jul","25-Jul","28-Jul"),
         key = date,
         value = density)

# wide to long: gather() funciton
# gather() . needs 3 pieces of info:
# 1) list vars to be moved using c(var1, var2, var3)
# 2) key = provides name of new var created by gathering ones from old df
# 3) value = provides name of new var accompanying key var

#rename Row Labels col to plot_grid
ARD_rate_long <- ARD_rate_convert %>% 
  rename(plot_grid = `Row Labels`)

#4) Export long format to make column called rate 
write.csv(ARD_rate_long, "ARD_rate_long.csv")

# ARD_rate_long_first_and_last <- ARD_rate_long %>% 
#   filter(date == c("04-Jun", "05-June", "28-Jul"))
#first post-outplant surveys for LN and LS were June 5th. For some reason this filter isn't working, do manually in excel
#sorted by date and plot_grid, deleted the middle dates, only kept H for Jun 4 and L for Jun 5
#sorted by just plot_grd then used this formula: 
# formula: =(C3-C2)/(B3-B2)
# remove duplicate(blank) rows -->  and import new sheet with just rate and plot_grid colums 

#5) Import the new sheet with rate of recruitment data (delta/density) = rate
ARD_3_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/(final - initial) over time/ARD_3_rate.csv")
View(ARD_3_rate)

#join ARD_3_rate to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_3_rate <- full_join(ARD_3_rate, ARD_lookup)
hist(ARD_3_rate$rate)

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
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) +
  ggtitle("mean fish recruitment rate per treatment (final - initial)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#plot it (just abundance)

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_with_0.csv")
View(ARD_3_with_0)

ARD_3_with_0_sum <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1597))

ggplot(data = ARD_3_with_0_sum) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(complexity~.) +
  labs(x = expression(Treatment),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density per treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

hist(ARD_3_with_0$density)
#very poisson right skew

# one way anova (final-initial) for ARD_3_with_0 just low ----------------------------------

#1) Data prep

ARD_3_rate_low <- ARD_3_rate %>% 
  filter(complexity == "Low") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

hist(ARD_3_rate_low$rate)
#looks normalish...

#2) summary stats

ARD_3_rate_low_sum <- ARD_3_rate_low %>% 
  group_by(treatment) %>% 
  get_summary_stats(rate, type = "mean_se")

#3) visualization

ggboxplot(ARD_3_rate_low, x = "treatment", y = "rate")
#not many outliers, but means look very comparable...

ARD_3_rate_low_outliers <- ARD_3_rate_low %>% 
  group_by(treatment) %>% 
  identify_outliers(rate)
#ok so 3 outliers, and 2 are considered extreme....

#4) try the model
model.rate <- lm(rate ~ treatment, data = ARD_3_rate_low)

#5) check assumptions
ggqqplot(residuals(model.rate)) #this looks a bit weird, like steps with outliers...
shapiro_test(residuals(model.rate)) #p-value is still significant, so NOT normal. can't use this model...
# BUT shapiro wilks is sensistive to large sample sizes (like this) so evan a minor deviation can throw it off
hist(ARD_rate_low$rate) # but also this looks non-normal..
#plot qqplots for each group
ggqqplot(ARD_3_rate_low, "rate", facet.by = "treatment") # a few points are beyond reference lines...

#homogenaity of variance
plot(model.rate,1)
ARD_3_rate_low %>% levene_test(rate ~ treatment) #p-value is NOT significant, so there there is homogenaity!

#6) post-hoc test (pairwise comparison with Tukey)
pwc <- ARD_3_rate_low %>% tukey_hsd(rate ~ treatment)
pwc #all non significant
#all showing as non-significant...but not normal data, so prob not the right test...

res.aov <- ARD_3_rate_low %>% anova_test(rate ~ treatment)
res.aov

# Visualization: box plots with p-values
pwc <- pwc %>% add_xy_position(x = "treatment")
ggboxplot(ARD_3_rate_low, x = "treatment", y = "rate") +
  stat_pvalue_manual(pwc, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )


# kw for ARD_3_rate (with 0 values) ---------------------------------------

#https://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/

#ok so start with the regular data visualizations I've already done

res.kruskal.rate <- ARD_3_rate_low %>% kruskal.test(rate ~ treatment)
res.kruskal.rate

ARD_3_rate_low %>% kruskal_effsize(rate ~ treatment)
#small effect of treatment on rate, effect size is 0.0381

#multiple pairwise comparisons - Dunn's test.
#Dunn's test takes into account rankinsts used in KW test 

pwc.kw.rate <- ARD_3_rate_low %>% dunn_test(rate ~ treatment, p.adjust.method = "bonferroni")
pwc.kw.rate
#all non-signficiant 

#can also use Wilcoxon's test... didn't work says x has to be numeric, but it is...
pwc.wilc.rate <- ARD_3_rate_low %>% wilcox.test(rate ~ treatment, p.adjust.method = "bonferroni")
pwc.wilc.rate
#doesn't look significant again...

pwc.kw.rate <- pwc.kw.rate %>% add_xy_position(x = "treatment")
ggboxplot(ARD_3_rate_low, x = "treatment", y = "rate") +
  stat_pvalue_manual(pwc.kw.rate, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal.rate, detailed = TRUE),
    caption = get_pwc_label(pwc.kw.rate)
  )


# (final - initial) rate using mean of first and last three visits ARD_3 --------

# #1 ) use values from ARD_3_with_0
# ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_with_0.csv")
# 
# unique(ARD_3_with_0$date)
# 
# ARD_rate_with_0 <- ARD_3_with_0 %>% 
#   select(date, plot_grid, density) %>% 
#   distinct(date, plot_grid, density)
# 
# #2) export to use pivot table to get 0 values
# write.csv(ARD_rate_with_0,"ARD_rate_with_0.csv")
# #export to make a pivot table --> to get 0 values for dates and plot/grids that didn't see any fish on a given date.
# #date in Columns (legend), plot_grid in Rows (axis) and density in Values
# 
# #3 ) import and convert to long format
# ARD_rate_wide <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/(final - initial) over time/ARD_rate_wide.csv")#import and convert to long format
# 
# names(ARD_rate_wide) #to get each date
# 
# ARD_rate_convert <- ARD_rate_wide %>% 
#   gather(c( "04-Jun","05-Jun","06-Jun","07-Jun","09-Jun","11-Jun","13-Jun","15-Jun",
#             "20-Jun","25-Jun","28-Jun","02-Jul","05-Jul","09-Jul","15-Jul","20-Jul","25-Jul","28-Jul"),
#          key = date,
#          value = density)
# 
# # wide to long: gather() funciton
# # gather() . needs 3 pieces of info:
# # 1) list vars to be moved using c(var1, var2, var3)
# # 2) key = provides name of new var created by gathering ones from old df
# # 3) value = provides name of new var accompanying key var
# 
# #rename Row Labels col to plot_grid
# ARD_rate_long <- ARD_rate_convert %>% 
#   rename(plot_grid = `Row Labels`)


#### started here because i have the other files in working directory already
#4) Export long format to make column called rate 
write.csv(ARD_rate_long, "ARD_rate_long.csv")

# saved as ARD_rate_long_means_calculations
#HS and LS first survey was june 4(kept 4, 5, 6), HN and LN first survey was june 5 (kept 5, 6, 7)
#sorted by pdate then plot_grid and deleted the middle dates
#sorted by just plot_grd then date and got average of first 3 and average of last 3
# used this formulat for rate: =(D5-D2)/(B6-B3)
# remove duplicate(blank) rows -->  and import new sheet with just rate and plot_grid colums 
# to remove blank rows, used remove duplicates for plot_grid and did ctrl+g, special, blanks, then deleted blanks (shift cells up)

#5) Import the new sheet with rate of recruitment data (delta/density) = rate
ARD_means_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/(final - initial) means over time/ARD_means_rate.csv")
View(ARD_means_rate)

#join ARD_3_rate to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_3_means_rate <- full_join(ARD_means_rate, ARD_lookup)
hist(ARD_3_means_rate$rate)

#make treatment levels in order from control to 100% and order of dates chronological
ARD_3_means_rate_sum <- ARD_3_means_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
  mutate(rate.se = rate.sd/sqrt(96))

ggplot(data = ARD_3_means_rate_sum) +
  geom_col(aes(x= treatment,
               y = rate.mean,
               fill = treatment))
#plot it (rate)
ggplot(data = ARD_3_means_rate_sum) +
  geom_col(aes(x = treatment,
               y = rate.mean,
               fill = treatment),
           alpha = 0.6) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2,
                alpha = 0.8) +
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) +
  ggtitle("mean fish recruitment rate per treatment (final - initial) means")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )




# (final-initial / time) rate of recruitment for ARD_5 (with 0 val --------


#1 ) use values from ARD_3_with_0
ARD_5_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_5/ARD_5_with_0.csv")

unique(ARD_5_with_0$date)

ARD_5_rate_with_0 <- ARD_5_with_0 %>% 
  select(date, plot_grid, density) %>% 
  distinct(date, plot_grid, density)

#2) export to use pivot table to get 0 values
write.csv(ARD_5_rate_with_0,"ARD_5_rate_with_0.csv")
#export to make a pivot table --> to get 0 values for dates and plot/grids that didn't see any fish on a given date.
#date in Columns (legend), plot_grid in Rows (axis) and density in Values

#3 ) import and convert to long format
ARD_5_rate_wide <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_5/(final - initial )/ARD_5_rate_wide.csv")
names(ARD_5_rate_wide) #to get each date

ARD_5_rate_convert <- ARD_5_rate_wide %>% 
  gather(c( "04-Jun","05-Jun","06-Jun","07-Jun","09-Jun","11-Jun","13-Jun","15-Jun",
            "20-Jun","25-Jun","28-Jun","02-Jul","05-Jul","09-Jul","15-Jul","20-Jul","25-Jul","28-Jul"),
         key = date,
         value = density)

# wide to long: gather() funciton
# gather() . needs 3 pieces of info:
# 1) list vars to be moved using c(var1, var2, var3)
# 2) key = provides name of new var created by gathering ones from old df
# 3) value = provides name of new var accompanying key var

#rename Row Labels col to plot_grid
ARD_5_rate_long <- ARD_5_rate_convert %>% 
  rename(plot_grid = `Row Labels`)

#4) Export long format to make column called rate 
write.csv(ARD_5_rate_long, "ARD_5_rate_long.csv")

# ARD_rate_long_first_and_last <- ARD_rate_long %>% 
#   filter(date == c("04-Jun", "05-June", "28-Jul"))
#first post-outplant surveys for LN and LS were June 5th. For some reason this filter isn't working, do manually in excel
#sorted by date and plot_grid, deleted the middle dates, only kept H for Jun 4 and L for Jun 5
#sorted by just plot_grd then used this formula: 
# formula: =(C3-C2)/(B3-B2)
# remove duplicate(blank) rows -->  and import new sheet with just rate and plot_grid colums 

#5) Import the new sheet with rate of recruitment data (delta/density) = rate
ARD_5_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_5/(final - initial )/ARD_5_rate.csv")
View(ARD_5_rate)

#join ARD_3_rate to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_5_rate <- full_join(ARD_5_rate, ARD_lookup)
hist(ARD_5_rate$rate)

#make treatment levels in order from control to 100% and order of dates chronological
ARD_5_rate_sum <- ARD_5_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>% 
  mutate(rate.se = rate.sd/sqrt(96))


#plot it (rate)
ggplot(data = ARD_5_rate_sum) +
  geom_col(aes(x = treatment,
               y = rate.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~Rate~(fish~m^{2}~time^{-1}))) +
  ggtitle("ARD 5 mean fish recruitment rate per treatment (final - initial)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#plot it (just abundance)

ARD_5_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_5/ARD_5_with_0.csv")
View(ARD_5_with_0)

ARD_5_with_0_sum <- ARD_5_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1597))

ggplot(data = ARD_5_with_0_sum) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("ARD 5 mean fish recruit density per treatment")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

ggplot(data = ARD_5_with_0) +
  geom_boxplot(aes(x = treatment, 
                   y = density,
                   fill = treatment)) +
  facet_grid(complexity~.) +
  ylim(0,10)

# plot with time block 
ARD_5_with_0_sum_time <- ARD_5_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  group_by(complexity, treatment, time_block) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1597))

ggplot(data = ARD_5_with_0_sum_time) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment,
               colour = time_block),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = time_block),
                position = position_dodge(0.9),
                width = 0.2) +
  scale_colour_grey(start = 0.9, end = 0.2, name = "Time") +
  facet_grid(complexity~.) +
  labs(x = expression(Treatment),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("ARD 5 mean fish recruit density per treatment over time")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    # legend.title = element_text(size = 14),
    # legend.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

hist(ARD_3_with_0$density)
#very poisson right skew
