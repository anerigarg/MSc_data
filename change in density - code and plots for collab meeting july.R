#Change in Density calculations - taking into account 0 values, anova and kruskal wallis code

# packages and palletes ---------------------------------------------------


library(readr)
library(tidyverse)
library(ggplot2)
library(openintro)
library(e1071)
library(ggpubr)
library(rstatix)
install.packages("datarium")
library(datarium)



# Ch 2 figs ---------------------------------------------------------------


# re-calculate ARD_3 to include 0 values  ---------------------------------

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
  facet_grid(.~complexity) +
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


# daily rate ARD_3 - Sept 10 ----------------------------------------------



#1 ) import ARD_3_with_0_new_2 (has days since outplanting as a col) for mean density values
ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_dr <- ARD_3_with_0_new_2 %>% 
mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                          labels = c("control", "0%", "30%", "50%", "70%", "100%")))
unique(ARD_dr$days_since_outplanting)

ARD_dr <-  ARD_dr %>% 
  select(days_since_outplanting, plot_grid,  density) %>% 
  distinct(days_since_outplanting, plot_grid, density) 

#2) export to use pivot table to get 0 values
write.xlsx(ARD_dr,"ARD_dr.xlsx")
#export to make a pivot table --> to get 0 values for dates and plot/grids that didn't see any fish on a given date

#3 ) import and convert to long format
ARD_dr_wide <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_3/daily rate over time/ARD_dr_wide.csv")
View(ARD_dr_wide)

names(ARD_dr_wide) #to get each date

ARD_dr_long <- ARD_dr_wide %>% 
  gather(c("1","2","3","4","5","7","9","11","13","18","23","26","30","33","37","43","48"),
         key = days_since_outplanting,
         value = density)

#4) Export long format to make column called delta dentisy 
# = density at time x - density at time x-days since last visit

write.xlsx(ARD_dr_long, "ARD_dr_long.xlsx")
# sort by plot_grid, then days_since_out_planting
# formula: =(C3-C2)/(B3-B2)
# make sure the first of every plot_grid is 0, used formula =ABS(0)

#5) Import the new sheet with daily rate of recruitment data (dr_density)
ARD_dr_long_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_3/daily rate over time/ARD_dr_long_rate.csv")


#join ARD_d_convert to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_dr_rate <- full_join(ARD_dr_long_rate, ARD_lookup) %>% 
  mutate(days_since_outplanting = as.numeric(days_since_outplanting))

#make treatment levels in order from control to 100% and order of dates chronological
ARD_dr_rate <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%")))
  # mutate(date = factor(days_since_outplanting, levels = c("1","2","3","4","5","7","9","11","13","18","23","26","30","33","37","43","48")))

ARD_dr_rate_sum <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  # mutate(days_since_outplanting = factor(date, levels = c("1","2","3","4","5","7","9","11","13","18","23","26","30","33","37","43","48"))) %>% 
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


hist(ARD_dr_rate$dr_density)
#more of a normal dist...

ARD_dr_rate_sum_col <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>%
  mutate(density.se = density.sd/sqrt(1632))

#column plot it
ggplot(data = ARD_dr_rate_sum_col) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~rate(fish~m^{2}~day^{1}))) +
  ggtitle("Mean Daily Recruitment Rate")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#boxplot
ARD_dr_rate_sum_box <- ARD_dr_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment)
# summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>%
#   mutate(density.se = density.sd/sqrt(1632))

ggplot(data = ARD_dr_rate_sum_box) +
  geom_boxplot(aes(x = treatment,
               y = dr_density,
               fill = treatment,
               group = treatment)) +
  # geom_errorbar(aes(x = treatment,
  #                   ymin = density.mean - density.se,
  #                   ymax = density.mean + density.se,
  #                   group = treatment),
  #               width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~rate(fish~m^{2}~day^{1}))) +
  ggtitle("Mean Daily Recruitment Rate")+
  # ylim(0,10)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

ggplot(data = ARD_dr_rate_sum_box) +
  geom_violin(aes(x = treatment,
                   y = dr_density,
                   fill = treatment,
                   group = treatment)) +
  # geom_errorbar(aes(x = treatment,
  #                   ymin = density.mean - density.se,
  #                   ymax = density.mean + density.se,
  #                   group = treatment),
  #               width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~rate~(fish~m^{2}~day^{1}))) +
  ggtitle("Mean Daily Recruitment Rate")+
  # ylim(0,10)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )
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


# line plot - just ARD_3 (attraction) -------------------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_with_0.csv")

ARD_3_with_0 <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_3_treatcomp_sum <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1597))

ggplot() +
  facet_grid(complexity~treatment) +
  geom_jitter(data = ARD_3_with_0,
              aes(x = visit,
                  y = density,
                  colour = treatment),
              alpha = 0.1) +
  geom_line(data = ARD_3_treatcomp_sum,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1.5,
            alpha = 0.3) +
  geom_errorbar(data = ARD_3_treatcomp_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_treatcomp_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.3) +
  geom_smooth(data = ARD_3_treatcomp_sum,
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
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )

# line plot just 0 and 100% H and L ---------------------------------------
ARD_3_with_0_0and100 <- ARD_3_with_0 %>% 
  filter(treatment == c("0%","100%"))

ARD_3_treatcomp_sum_0and100 <- ARD_3_with_0 %>% 
  filter(treatment == c("0%","100%")) %>% 
  mutate(treatment = factor(treatment, levels = c("0%",  "100%"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(285))

ggplot() +
  facet_grid(complexity~treatment) +
  # geom_jitter(data = ARD_3_with_0,
  #             aes(x = visit,
  #                 y = density,
  #                 colour = treatment),
  #             alpha = 0.2) +
  geom_line(data = ARD_3_treatcomp_sum_0and100,
            aes(x = visit,
                y = density.mean,
                colour = treatment,
                group = treatment),
            size = 1,
            alpha = 0.5) +
  geom_errorbar(data = ARD_3_treatcomp_sum_0and100,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se,
                    colour = treatment,
                    group = treatment), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_3_treatcomp_sum_0and100,
             aes(x = visit, 
                 y = density.mean,
                 colour = treatment,
                 group = treatment),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  # geom_smooth(data = ARD_3_treatcomp_sum_0and100,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = treatment,
  #                 group = treatment),
  #             size = 2,
  #             method = lm,
  #             se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  # facet_grid(complexity~treatment) +
  # ylim(0,10)+
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 16),
    legend.position = "none",
  )


# boxplot -----------------------------------------------------------------

ARD_3_with_0_box <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

ggplot()+
  geom_boxplot(data = ARD_3_with_0_box,
               aes(x = treatment,
                   y = density,
                   colour = time_block,
                   fill = treatment), alpha = 0.8)+
  scale_colour_grey(start = 0.9, end = 0.5, name = "Time")+
  ylim(0,15) +
  xlab("Treatment") +
  ylab("Fish Density") +
  theme_classic() +
  facet_grid(complexity ~ .) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

#boxplot no time
ggplot()+
  geom_boxplot(data = ARD_3_with_0_box,
               aes(x = treatment,
                   y = density,
                   fill = treatment))+
  ylim(0,15) +
  xlab("Treatment") +
  ylab("Fish Density") +
  theme_classic() +
  facet_grid(complexity ~ .) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

hist(ARD_3_with_0_box$density) #verrrry not normal

# genotype ----------------------------------------------------------------



ggplot()+
  geom_col(data = ARD_3_with_0,
               aes(x = genotype,
                   y = density,
                   fill = genotype)) +
  ylim(0,5) +
  facet_grid(complexity~treatment)

ARD_genotype_sum <- ARD_3_with_0 %>% 
  group_by(genotype) %>% 
  summarise(gen.mean = mean(density))


# Ch 1 figs ---------------------------------------------------------------


# SFD 3 col plot ----------------------------------------------------------
SFD_3 <- read_csv("small fish/data/SFD_3.csv")

SFD_sum3 <- SFD_3 %>% 
  # filter(visit != "6") %>% 
  # filter(visit != "5") %>% #for fully balanced design, only look at visits 1-4, still 1 month period
  # mutate(treatment = factor(treatment, levels = c("NO", "O"),
  #                           labels = c("Unrestored", "Restored"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1181))

#col plot density treatment and complexity, #after com meeting, changed error bars to se 
ggplot(data = SFD_sum3) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = complexity),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = complexity,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = pal) +
  facet_grid(.~treatment) +
  labs(x = expression(Background~Complexity),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean density (fish under 3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# SFD 3 line plot ---------------------------------------------------------
SFD_3_line_sum <- SFD_3 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1181))

ggplot() +
  geom_jitter(data = SFD_3,
              aes(x = visit,
                  y = density),
              alpha = 0.1,
              colour = "coral") +
  geom_line(data = SFD_3_line_sum,
            aes(x = visit,
                y = density.mean,),
            size = 2,
            alpha = 0.7,
            colour = "coral") +
  geom_errorbar(data = SFD_3_line_sum,
                aes(x = visit, 
                    ymin = density.mean - density.se, 
                    ymax = density.mean + density.se), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5,
                colour = "coral") +
  geom_point(data = SFD_3_line_sum,
             aes(x = visit, 
                 y = density.mean),
             position = position_dodge(0.2),
             size = 3,
             colour = "coral") +
  # geom_smooth(data = SFD_3_line_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = fish.size,
  #                 group = fish.size),
  #             size = 2,
  #             method = lm,
  #             se = FALSE ) +
  # scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment)+
  ylim(0,10)+
  labs(x = expression(Visit),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish recruit density over time")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )

# proportions of functional groups in each treat and comp -----------------

SFD_3_troph = SFD_3 %>% 
  select(complexity, treatment, density, MajorTrophicGroup)

SFD_3_troph$treatment = as.factor(SFD_3_troph$treatment)

SFD_3_troph_new <- SFD_3_troph %>%
  group_by(complexity, treatment) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

SFD_fresh = SFD_3 %>% 
  group_by(treatment,complexity,MajorTrophicGroup) %>% 
  summarize(n = length(MajorTrophicGroup)) %>% 
  mutate(proportion = n/sum(n)) 

for_join = SFD_3_troph_new %>% 
  select(density.mean, density.sd, complexity, treatment)

SFD_fresh$complexity = as.factor(SFD_fresh$complexity)
SFD_fresh$treatment = as.factor(SFD_fresh$treatment)
for_join$complexity = as.factor(for_join$complexity)
for_join$treatment = as.factor(for_join$treatment)

SFD_fresh = merge(SFD_fresh, for_join, by.x = c('treatment', 'complexity'), 
                  by.y = c('treatment', 'complexity'))

SFD_fresh = SFD_fresh %>% 
  mutate(prop_fill = proportion*density.mean)

ggplot(data = SFD_fresh) +
  geom_col(aes(x = complexity,
               y = prop_fill, fill= MajorTrophicGroup), 
           position = 'stack') +
  # position = "dodge") +
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group") +
  facet_grid(.~treatment) +
  # ylim(0,40)+
  labs(x = expression(Background~Complexity),
       y = expression(Density~(fish~m^{2}))) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16),
  )


# SFD 3 by site -----------------------------------------------------------

SFD_site <- SFD_3 %>% 
  # filter(visit != "6") %>%
  # filter(visit != "5") %>% 
  group_by(site, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1181))

ggplot()+
  geom_col(data = SFD_site,
           aes(x = site,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(data = SFD_site,
                aes(x = site,
                    fill = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se),
                position = position_dodge(0.2),
                width = 0.2) +
  labs(x = expression(Site),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean fish density per site")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = -60, hjust = 0.1),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    strip.text = element_text(size = 16)
  )


