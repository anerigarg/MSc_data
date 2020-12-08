

# daily rate

# mean daily rate  --------------------------------------------------------


# ARD_3 -------------------------------------------------------------------


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
#export to make a pivot table --> make sure all blanks are 0

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
#change Row Labels to plot_grid
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

write.csv(ARD_dr_rate,"ARD_3_dr_rate.csv")

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
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
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
       y = expression(Recruitment~rate~(fish~m^{2}~day^{1}))) +
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
  ylim(-2,2)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#violin plot
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


# check calculations for mean daily recruitment rate ----------------------

#1 ) import ARD_3_with_0_new_2 (has days since outplanting as a col) for mean density values
ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_dr <- ARD_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))
unique(ARD_dr$days_since_outplanting)

#1) make col that has just days_since, plot_grid,and density
ARD_dr <-  ARD_dr %>% 
  select(days_since_outplanting, plot_grid,  density) %>% 
  distinct(days_since_outplanting, plot_grid, density) 

#2) export and calculate rate of recruitment (density day x - density day x-1 / day x = day x-1)
write.xlsx(ARD_dr, "ARD_dr.xlsx")
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

write.csv(ARD_dr_rate,"ARD_3_dr_rate.csv")

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

#rate over time
ggplot(data = ARD_dr_rate_sum) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = density.mean,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Relative~Density),
       colour = "Treatment") +
  geom_hline(yintercept = 0, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.5) +
  ylim(-0.75, 0.75) +
  # xlim(0,50) +
  ggtitle("Recruitment Rate over time (3nd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

#density over time
ARD_dens_sum <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c ("Low", "High"))) %>% 
  # mutate(days_since_outplanting = factor(date, levels = c("1","2","3","4","5","7","9","11","13","18","23","26","30","33","37","43","48"))) %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1539))

#density over time
ggplot(data = ARD_dens_sum) +
  # facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = density.mean,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,3,raw = TRUE),
              size = 2) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2})),
       colour = "Treatment") +
  # geom_hline(yintercept = 0, 
  #            linetype = "dashed", 
  #            colour = "black", 
  #            size = 0.8, 
  #            alpha = 0.5) +
  # ylim(0, 7) +
  # xlim(0,50) +
  ggtitle("Density over time (3nd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

#density col
ARD_treat_col <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(treatment, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(density.se = density.sd/sqrt(1539))

#column plot it
ggplot(data = ARD_treat_col) +
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
       y = expression(Mean~Density~(fish~m^{2}))) +
  ggtitle("Mean Density")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
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
  ylim(-2,2)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#violin plot
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


# ARD 4to6 daily rate -----------------------------------------------------


#1 ) import ARD_3_with_0_new_2 (has days since outplanting as a col) for mean density values
ARD_4to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0_new_2.csv")
ARD_dr_4to6 <- ARD_4to6_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))
unique(ARD_dr_4to6$days_since_outplanting)

ARD_dr_4to6 <-  ARD_dr_4to6 %>% 
  select(days_since_outplanting, plot_grid,  density) %>% 
  distinct(days_since_outplanting, plot_grid, density) 

#2) export to use pivot table to get 0 values
write.xlsx(ARD_dr_4to6,"ARD_dr_4to6.xlsx")
#export to make a pivot table --> make sure all blanks are 0

#3 ) import and convert to long format
ARD_dr_wide_4to6 <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_4to6/daily rate/all fish/ARD_dr_wide_4to6.csv")
View(ARD_dr_wide_4to6)

names(ARD_dr_wide) #to get each date

ARD_dr_long_4to6 <- ARD_dr_wide_4to6 %>% 
  gather(c("1","2","3","4","5","7","9","11","13","18","23","26","30","33","37","43","48"),
         key = days_since_outplanting,
         value = density)

#4) Export long format to make column called delta dentisy 
# = density at time x - density at time x-days since last visit

write.xlsx(ARD_dr_long_4to6, "ARD_dr_long_4to6.xlsx")
#change Row Labels to plot_grid
# sort by plot_grid, then days_since_out_planting
# formula: =(C3-C2)/(B3-B2)
# make sure the first of every plot_grid is 0, used formula =ABS(0)

#5) Import the new sheet with daily rate of recruitment data (dr_density)
ARD_dr_long_rate_4to6 <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_4to6/daily rate/all fish/ARD_dr_long_rate_4to6.csv")


#join ARD_d_convert to ARD_lookup to get treatment and complexity information
ARD_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_lookup.csv")

ARD_dr_rate_4to6 <- full_join(ARD_dr_long_rate_4to6, ARD_lookup) %>% 
  mutate(days_since_outplanting = as.numeric(days_since_outplanting))

#make treatment levels in order from control to 100% and order of dates chronological
ARD_dr_rate_4to6 <- ARD_dr_rate_4to6 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%")))
# mutate(date = factor(days_since_outplanting, levels = c("1","2","3","4","5","7","9","11","13","18","23","26","30","33","37","43","48")))


write.csv(ARD_dr_rate_4to6,"ARD_dr_rate_4to6.csv")


ARD_dr_4to6_col <- ARD_dr_rate_4to6 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c("Low", "High"))) %>% 
  filter(days_since_outplanting > 30) %>%
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>%
  mutate(density.se = density.sd/sqrt(384))

#column plot it
ggplot(data = ARD_dr_4to6_col) +
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
       y = expression(Recruitment~rate~(fish~m^{2}~day^{1}))) +
  ggtitle("Mean Daily Settlement Rate 4-6cm")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

ARD_4to6_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0_new_2.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)


#density over time
ARD_dens_sum_4to6 <- ARD_4to6_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(complexity = factor(complexity, levels = c ("Low", "High"))) %>% 
  # mutate(days_since_outplanting = factor(date, levels = c("1","2","3","4","5","7","9","11","13","18","23","26","30","33","37","43","48"))) %>% 
  group_by(complexity, treatment, days_since_outplanting) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1539))

#density over time
ggplot(data = ARD_dens_sum_4to6) +
  facet_grid(~complexity) +
  geom_point(aes(x = days_since_outplanting,
                 y = density.mean,
                 colour = treatment),
             size = 2) +
  stat_smooth(aes(x = days_since_outplanting,
                  y = density.mean,
                  colour = treatment),
              method = "lm",
              se = FALSE,
              formula = y ~ poly(x,4,raw = TRUE),
              size = 2) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue"))+
  labs(x = expression(Days~Since~Outplanting),
       y = expression(Density~(fish~m^{2})),
       colour = "Treatment") +
  # geom_hline(yintercept = 0, 
  #            linetype = "dashed", 
  #            colour = "black", 
  #            size = 0.8, 
  #            alpha = 0.5) +
  # ylim(0, 7) +
  # xlim(0,50) +
  ggtitle("Density over time 4-6 (3nd order poly)") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

#compare ARD_4to6_with_0 and ARD_4to6_with_0_new_2

ARD_4to6_1 <- ARD_4to6_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(treatment, complexity) %>% 
  summarise(mean.density = mean(density))

ARD_4to6_2 <- ARD_4to6_with_0_new_2 %>% 
  group_by(treatment, complexity) %>% 
  summarise(mean.density = mean(density))

#pretty much the same, off by like 0.01

# (final - initial) rate using mean of first and last three visits --------


# ARD_3 -------------------------------------------------------------------


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

ARD_means_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_3/(final - initial) means over time/ARD_means_rate.csv")
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

# ggplot()+
#   geom_point(data = ARD_3_means_rate_sum,
#              aes(x = days_since_outplanting, 
#                  y = rate.mean,
#                  colour = treatment,
#                  group = treatment),
#              position = position_dodge(0.2),
#              size = 3,
#              alpha = 0.5) +
#   geom_errorbar(data = ARD_dr_rate_sum,
#                 aes(x = days_since_outplanting, 
#                     ymin = density.mean - density.se, 
#                     ymax = density.mean + density.se,
#                     colour = treatment,
#                     group = treatment), 
#                 position = position_dodge(0.2),
#                 width = 0.5,
#                 size = 0.5)+
#   geom_line(data = ARD_dr_rate_sum,
#             aes(x = days_since_outplanting,
#                 y = density.mean,
#                 colour = treatment,
#                 group = treatment),
#             size = 1,
#             alpha = 0.5)+
#   geom_smooth(data =  ARD_dr_rate_sum,
#               aes(x = days_since_outplanting,
#                   y = density.mean,
#                   colour = treatment,
#                   group = treatment),
#               size = 2,
#               # method = lm,
#               se = TRUE,
#               alpha = 0.35)+
#   facet_grid(complexity~treatment)+
#   scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
#   # ylim(-2.5,2.5) +
#   scale_x_continuous(breaks = c(10, 20, 30, 40, 50)) +
#   labs(x = expression(Days~Since~Outplanting),
#        y = expression(Recruitment~rate(fish~m^{2}~day^{1})),
#        colour = "Treatment") +
#   ggtitle("Daily Recruitment Rate")+
#   theme_classic()+
#   theme(
#     axis.title = element_text(size = 14),
#     axis.text.x = element_text(size = 10),
#     axis.text.y = element_text(size = 12),
#     legend.title = element_text(size = 14),
#     legend.text = element_text(size = 12),
#     strip.text = element_text(size = 14)
#   )


hist(ARD_dr_rate$dr_density)
#more of a normal dist...

ARD_3_means_rate_col <- ARD_3_means_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>%
  mutate(rate.se = rate.sd/sqrt(96))

#column plot it
ggplot(data = ARD_3_means_rate_col) +
  geom_col(aes(x = treatment,
               y = rate.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~rate(fish~m^{2}~day^{1}))) +
  ggtitle("Mean Daily Recruitment Rate - first 3 and last 3 mean")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#boxplot
ARD_3_means_rate_box <- ARD_3_means_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment)
# summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>%
#   mutate(density.se = density.sd/sqrt(1632))

ggplot(data = ARD_3_means_rate_box) +
  geom_boxplot(aes(x = treatment,
                   y = rate,
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
  ggtitle("Mean Daily Recruitment Rate - first 3 and last 3 mean")+
  # ylim(0,10)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#violin plot
ggplot(data = ARD_3_means_rate_box) +
  geom_violin(aes(x = treatment,
                  y = rate,
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
  ggtitle("Mean Daily Recruitment Rate - first 3 and last 3 mean")+
  # ylim(0,10)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# (final - initial / time) rate of recruitment 3.0 (with 0 values) --------


  
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
ARD_3_rate <- read_csv("Artificial coral experiment/data/Art Recruit data/density rate calculations/with 0 values/ARD_3/(final - initial) over time/ARD_3_rate.csv")
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


#plot it (rate) --> #need to update to include day since outplanting...
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


ARD_3_rate_col <- ARD_3_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(rate.mean = mean(rate), rate.sd = sd(rate)) %>%
  mutate(rate.se = rate.sd/sqrt(96))

#column plot it
ggplot(data = ARD_3_rate_col) +
  geom_col(aes(x = treatment,
               y = rate.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = rate.mean - rate.se,
                    ymax = rate.mean + rate.se,
                    group = treatment),
                width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~rate(fish~m^{2}~day^{1}))) +
  ggtitle("Mean Daily Recruitment Rate - final-initial")+
  # ylim(0,10) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#boxplot
ARD_3_rate_box <- ARD_3_rate %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment)
# summarize(density.mean = mean(dr_density), density.sd = sd(dr_density)) %>%
#   mutate(density.se = density.sd/sqrt(1632))

ggplot(data = ARD_3_rate_box) +
  geom_boxplot(aes(x = treatment,
                   y = rate,
                   fill = treatment,
                   group = treatment)) +
  stat_summary(fun = "mean", geom = "point") +
  # geom_errorbar(aes(x = treatment,
  #                   ymin = density.mean - density.se,
  #                   ymax = density.mean + density.se,
  #                   group = treatment),
  #               width = 0.2) +
  facet_grid(.~complexity) +
  scale_fill_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  labs(x = expression(Treatment),
       y = expression(Recruitment~rate(fish~m^{2}~day^{1}))) +
  ggtitle("Mean Daily Recruitment Rate - final-initial")+
  ylim(-0.1,0.2)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#violin plot
ggplot(data = ARD_3_rate_box) +
  geom_violin(aes(x = treatment,
                  y = rate,
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
  ggtitle("Mean Daily Recruitment Rate - final-initial")+
  # ylim(0,10)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )



# deensity calculations (by visit) ----------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
ARD_3_only <- ARD %>%
  filter(TL <= 3) %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
ARD_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(ARD_3_only_plot_grid_visit, "ARD_3_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

ARD_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
ARD_3_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
ARD_just_0 <- inner_join(ARD_3_none, ARD_3_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
ARD_3_with_0n2 <- bind_rows(ARD_3_only, ARD_just_0) %>% 
  # group_by(plot_grid_visit) %>% 
  mutate(density = sum(presence)/0.79)

write.csv(ARD_3_with_0n2, "ARD_3_with_0n2.csv")

# days since out-planting starting all june 5  - ARD 3-----------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

ARD_3_with_0$plot_date <- paste(ARD_3_with_0$plot, "-", ARD_3_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date

ARD_3_with_0_new_2 <- inner_join(ARD_3_with_0, ArtR_days_since_lookup_new)

write.csv(ARD_3_with_0_new_2, "ARD_3_with_0_new_2.csv")



