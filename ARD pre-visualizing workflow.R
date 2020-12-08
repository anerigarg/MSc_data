
# cleaning joined ARD df --------------------------------------------------

names(ARD5)
#get rid of dupilcate collumns i.plot and i.transect_ID
#rename collumn "yes" to "transit"

ARD5[,c('i.plot','i.transect_ID')] <- list(NULL)

ARD5 <- ARD5 %>% 
  rename('transit' = 'yes')

names(ARD5)

#check how each var is categorized
sapply(ARD5, class)

test <- ARD5
#ARD5 <- test

sapply(ARD5, class)
#change:
ARD5$treatment = as.factor(ARD5$treatment) #treatment to factor
ARD5$complexity = as.factor(ARD5$complexity) #complexity as factor
ARD5$life_phase = as.factor(ARD5$life_phase )#life_phase to factor
ARD5$transit = as.factor(ARD5$transit) #transit to factor
ARD5$genotype = as.factor(ARD5$genotype) #genotype to factor

sapply(ARD5, class)
#check


# workflow to calculate density and biomass -------------------------------


#1) make a new collumn called plot_grid_survey that combines plot_grid and survey_number to make a unique identifier for a survey at each cluster
#2) make a col of fish presence, where 0 = absence and 1 = presence
#3) calculate density using using plot_grid_survey and fish presence (add total # of fish and divide by 0.79)
#4) make a new col calculating biomass for each fish --> W = a x (TL^b)

test <- ARD5
#ARD5 <- test

#1) make a new collumn called plot_grid_survey that combines plot_grid and survey_number to make a unique identifier for a survey at each cluster
ARD5$plot_grid_survey <- paste(ARD5$plot_grid, "-", ARD5$survey_number)

#2) make a col of fish presence, where 0 = absence and 1 = presence (I don't have an absence data here, so just a new col with 1 values)

library(dplyr)
ARD_dens <- ARD5 %>% 
  mutate(presence = 1)

write.xlsx(ARD_dens, "ARD_dens post join check 2.xlsx", asTable = FALSE)
#check it

#3) calculate density using using plot_grid_survey and fish presence (add total # of fish and divide by 0.79)

ARD_dens_test <- ARD_dens %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = (sum(presence)/0.79))
#looks good, there's now a density column 

#4) calculate biomass

#rename TL(cm) col to just TL
ARD_dens_test <- ARD_dens_test %>% 
  rename('TL' = 'TL(cm)')

ARD_biomass_test <- ARD_dens_test %>% 
  mutate(biomass = (a*(TL^b)))

ARD_final <- ARD_biomass_test

write.xlsx(ARD_final, "ARD_final_check_1.xlsx", asTable = FALSE)
write.xlsx(ARD_final, "ARD_final_check_2.xlsx", asTable = FALSE)
write.csv(ARD_final, "ARD_final.csv")


ARD_final_baseline <- ARD_final %>% 
  filter(survey_type == "baseline recruit")

# summarize after removing outliers ---------------------------------------

#import df with out-liers filtered out and do the new density calculations 03/02/20
library(readr)
ARD_final_no_outlier <- read_csv("data/Art Recruit data/final clean data frames/ARD_final_no_outlier.csv")
View(ARD_final_no_outlier)

ARD_dens_out <- ARD_final_no_outlier %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = (sum(presence)/0.79))

ARD_final_no <- ARD_dens_out #03/02/20

ARD_baseline_no <- ARD_final_no %>% 
  filter(survey_type == "baseline recruit")

ARD_recruit_no <- ARD_final_no %>% 
  filter(survey_type == "recruit")



# After outlier filtering x2: 

ARD_final_with_outliers_x2 <- read_csv("data/Art Recruit data/final clean data frames/ARD_final_with_outliers.x2.csv")
View(ARD_final_with_outliers_x2)

ARD_dens_out2 <- ARD_final_with_outliers_x2 %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = (sum(presence)/0.79))

ARD_final_no2 <- ARD_dens_out2 #03/02/20

ARD_baseline_no2 <- ARD_final_no2 %>% 
  filter(survey_type == "baseline recruit")

ARD_recruit_no2 <- ARD_final_no2 %>% 
  filter(survey_type == "recruit")

#after filtering x3:

ARD_final_with_outliers_x3 <- read_csv("data/Art Recruit data/final clean data frames/ARD_final_with_outliers.x3.csv")
View(ARD_final_with_outliers_x3)

ARD_dens_out3 <- ARD_final_with_outliers_x3 %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = (sum(presence)/0.79))

ARD_final_no3 <- ARD_dens_out3 #03/02/20

ARD_baseline_no3 <- ARD_final_no3 %>% 
  filter(survey_type == "baseline recruit")

ARD_recruit_no3 <- ARD_final_no3 %>% 
  filter(survey_type == "recruit")
# summarize for visualizing -----------------------------------------------

library(tidyverse)

ARD_dens_sum_treat <- ARD_final %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each treatment

ARD_dens_sum_comp <- ARD_final %>% 
  group_by(complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each complexity

ARD_dens_sum_treat_comp <- ARD_final %>% 
  group_by(treatment, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each treatment and complexity

ARD_biom_sum_treat <- ARD_final %>% 
  group_by(treatment) %>% 
  summarize(biomass.mean = mean(biomass, na.rm = TRUE), biomass.sd = sd(biomass, na.rm = TRUE)) #to exclude NAs

ARD_dens_treat_comp_visit <- ARD_final %>% 
  group_by(treatment, complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_dens_treat_comp_visit_notrans <- ARD_final %>%
  filter(transit != "yes") %>% #figure this out next time (ie exclude any fish in transit and over 3, 5, 10 cm etc)feb 10, 2020
  group_by(treatment, complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))


# summraize filtered by fish < 3 cm ---------------------------------------

#Feb 25, 2020 --> looking at plots for dif sizes of fishes
library(tidyverse)

ARD_final_3 <- ARD_final %>% 
  filter(TL < 4)

ARD_dens_sum_treat_3 <- ARD_final %>% 
  filter(TL < 4) %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each treatment for fish <3cm

ARD_dens_sum_comp_3 <- ARD_final %>% 
  filter(TL < 4) %>% 
  group_by(complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each complexity

ARD_dens_sum_treat_comp_3 <- ARD_final %>% 
  filter(TL < 4) %>% 
  group_by(treatment, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each treatment and complexity

ARD_biom_sum_treat_3 <- ARD_final %>% 
  filter(TL < 4) %>% 
  group_by(treatment) %>% 
  summarize(biomass.mean = mean(biomass, na.rm = TRUE), biomass.sd = sd(biomass, na.rm = TRUE)) #to exclude NAs

ARD_dens_treat_comp_visit_3 <- ARD_final %>% 
  filter(TL < 4) %>% 
  group_by(treatment, complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

# summarize filtered by fish < 5 cm ---------------------------------------

ARD_final_5 <- ARD_final %>% 
  filter(TL < 6)

ARD_recruit_5 <- ARD_recruit %>% 
  filter(TL <6)

ARD_baseline_5 <- ARD_baseline %>% 
  filter(TL < 6)

ARD_dens_sum_treat_5 <- ARD_final %>% 
  filter(TL < 6) %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each treatment for fish <3cm

ARD_dens_sum_comp_5 <- ARD_final %>% 
  filter(TL < 6) %>% 
  group_by(complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each complexity

ARD_dens_sum_treat_comp_5 <- ARD_final %>% 
  filter(TL < 6) %>% 
  group_by(treatment, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
#summary table with means and sds for each treatment and complexity

ARD_biom_sum_treat_5 <- ARD_final %>% 
  filter(TL < 6) %>% 
  group_by(treatment) %>% 
  summarize(biomass.mean = mean(biomass, na.rm = TRUE), biomass.sd = sd(biomass, na.rm = TRUE)) #to exclude NAs

ARD_dens_treat_comp_visit_5 <- ARD_final %>% 
  filter(TL < 6) %>% 
  group_by(treatment, complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
# initial visualizations --------------------------------------------------
# point plots -------------------------------------------------------------

ggplot(data = ARD_dens_sum_treat_3) +
  geom_point(aes(x = treatment,
                 y = density.mean)) +
  geom_errorbar(aes(x = treatment, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  ggtitle("mean overall recruit density by treatment (TL<3)")
#this made a point plot of the means for each treatment group and error bars

ggplot(data = ARD_dens_sum_treat_comp_3) +
  geom_point(aes(x = treatment,
                 y = density.mean,
                 colour = complexity)) +
  geom_errorbar(aes(x = treatment, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  ggtitle("mean overall recruit density by treatment and complexity (TL<3)")
#this made a plot of the means of each treatment group jittered with complexity

#YAS PLOT - showing trend for treatments over time 
ggplot(data = ARD_dens_treat_comp_visit_5, 
       aes(x = visit,
           y = density.mean, 
           colour = treatment)) +
  geom_point() +
  ylim(0,40) +
  facet_grid(complexity ~ .) +
  geom_smooth(method = "lm", fill = NA) +
  ggtitle("mean overall recruit biomass over time by treatment and complexity, ylim 40, TL<5")
   # geom_errorbar(aes(x = visit,
   #                   ymin = density.mean - density.sd, 
   #                   ymax = density.mean + density.sd),
   #               width = .05,
   #               size = 0.5, 
   #               position = position_dodge(0.05)) +


ggplot(data = ARD_dens_treat_comp_visit, 
       aes(x = visit,
           y = density.mean, 
           colour = complexity)) +
  geom_point() +
  ylim(0,40) +
  facet_grid(treatment ~ .) +
  geom_smooth(method = "lm", fill = NA) +
  ggtitle("mean overall recruit biomass over time by treatment and complexity")



#this made a point plot of the means for each treatment group (compexity in dif colours) and error bars

# violin plots ------------------------------------------------------------
library(readr)
ARD_final <- read_csv("data/Art Recruit data/final clean data frames/ARD_final.csv")
View(ARD_final)

library(tidyverse)
sapply(ARD_final,class)
ARD_final$treatment = as.factor(ARD_final$treatment) #treatment to factor
ARD_final$complexity = as.factor(ARD_final$complexity) #complexity as factor
ARD_final$life_phase = as.factor(ARD_final$life_phase )#life_phase to factor
ARD_final$transit = as.factor(ARD_final$transit) #transit to factor
ARD_final$genotype = as.factor(ARD_final$genotype) #genotype to factor
sapply(ARD_final,class)

library(ggplot2)
q <- ggplot(ARD_final, 
            aes(x = treatment,
                y = density)) +
  geom_violin(position = position_dodge(1)) +
  facet_grid(complexity ~ .)
print(q)
#to make a violin plot  
q + 
  geom_boxplot(width = 0.1, 
               fill = "white", 
               outlier.shape = 16, 
               position = position_dodge(1)) + 
  theme_bw() +
  ylim(0,40) +
  ggtitle("mean overall recruit density by treatment and complexity")
print(q)
#added boxplot with mean and outlier points in the middle of the violin plot

#at first this showed much higher density for control plots than expected, going to go look through data: 
# I am highlighting in light red the surveys with highest densities and double checking data sheets in
# ARD_final_checking_1.xlsc

#survey 183, Jul 5 Taylor, grid 17 should be 45 bhw, data was entered twice -> corrected in Art_Recruit_fish_data df
#it's also a school in transit so change to 'yes" in transit -> converted
#survey 183, Jul 5 Taylor, grid 18 , survey also entered in twice ->  checked the entire survey in Art_Recruit_fish_data df
#convert bhw to yes in transit -> converted

#survey 227, Jul 25 Taylor, grid 13, he wrote down 50 bhw, entered correctly bu im skeptical
#convert bhw to yes in transit-> converted

#survey 238, Jul 28 Noelle, grid 6, looks fine
#convert bhw to yes in transit -> converted

#survey 92, Jun 13 Kelsey, grid 16 looks fine 
#convert bhw to yes in transit -> converted


# violin plot - no outliers x1 -----------------------------------------------

library(ggplot2)
ggplot(ARD_recruit_no, 
            aes(x = treatment,
                y = density)) +
  geom_violin(position = position_dodge(1)) +
  facet_grid(complexity ~ .) +
  geom_boxplot(width = 0.1, 
               fill = "white", 
               outlier.shape = 16, 
               position = position_dodge(1)) + 
  theme_bw() +
  ggtitle("mean overall recruit density by treatment and complexity - no  outliers")

write.csv(ARD_final_no, "ARD_final_no.csv")
#still some wild out-liers in the 50L50A Low complexity treatment for recruit...03/02/20

ggplot(ARD_baseline_no, 
       aes(x = treatment,
           y = density)) +
  geom_violin(position = position_dodge(1)) +
  facet_grid(complexity ~ .) +
  geom_boxplot(width = 0.1, 
               fill = "white", 
               outlier.shape = 16, 
               position = position_dodge(1)) + 
  theme_bw() +
  ggtitle("mean overall baseline density by treatment and complexity - no  outliers")




# violin plot - no outliers x2 --------------------------------------------

library(ggplot2)
ggplot(ARD_recruit_no2, 
       aes(x = treatment,
           y = density)) +
  geom_violin(position = position_dodge(1)) +
  facet_grid(complexity ~ .) +
  geom_boxplot(width = 0.1, 
               fill = "white", 
               outlier.shape = 16, 
               position = position_dodge(1)) + 
  theme_bw() +
  ggtitle("mean overall recruit density by treatment and complexity - no  outliers2")

write.csv(ARD_final_no2, "ARD_final_no2.csv")
#still a couple out-liers in the 50L50A Low complexity treatment for recruit...03/02/20

ggplot(ARD_baseline_no2, 
       aes(x = treatment,
           y = density)) +
  geom_violin(position = position_dodge(1)) +
  facet_grid(complexity ~ .) +
  geom_boxplot(width = 0.1, 
               fill = "white", 
               outlier.shape = 16, 
               position = position_dodge(1)) + 
  theme_bw() +
  ggtitle("mean overall baseline density by treatment and complexity - no  outliers2")




# violin plot - no outliers x3 --------------------------------------------

library(ggplot2)
ggplot(ARD_recruit_no3, 
       aes(x = treatment,
           y = density)) +
  geom_violin(position = position_dodge(1)) +
  facet_grid(complexity ~ .) +
  geom_boxplot(width = 0.1, 
               fill = "white", 
               outlier.shape = 16, 
               position = position_dodge(1)) + 
  theme_bw() +
  ggtitle("mean overall recruit density by treatment and complexity - no  outliers3")

write.csv(ARD_final_no3, "ARD_final.csv")
write.csv(ARD_recruit_no3, "ARD_recruit.csv")
write.csv(ARD_baseline_no3, "ARD_baseline.csv")
#a few in the 40s, but nore sure if proper outliers. Use this file for RE Peters

ggplot(ARD_baseline_no3, 
       aes(x = treatment,
           y = density)) +
  geom_violin(position = position_dodge(1)) +
  facet_grid(complexity ~ .) +
  geom_boxplot(width = 0.1, 
               fill = "white", 
               outlier.shape = 16, 
               position = position_dodge(1)) + 
  theme_bw() +
  ggtitle("mean overall baseline density by treatment and complexity - no  outliers2")

# geom_col ----------------------------------------------------------------

ggplot(data = ARD_final,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Density of major trophic groups per complexity and treatment") #just faceted opposite than code below

ggplot(data = ARD_final,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(treatment ~ complexity) +
  theme_dark() +
  labs(title = "Density of major trophic groups per treatment and complexity") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


ggplot(data = ARD_final,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(treatment ~ .) +
  theme_dark() +
  labs(title = "Density of major trophic groups per treatment")

ggplot(data = ARD_final,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid( ~ complexity) +
  theme_dark() +
  labs(title = "Density of major trophic groups per complexity")


ggplot(data = ARD_final,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = life_phase),
       position = "dodge",
       fill = "life_phase") +
  geom_col() +
  facet_grid(. ~ complexity) +
  theme_dark() +
  labs(title = "Density of major trophic groups per complexity and life_phase")


# geom col fish under 5 ---------------------------------------------------


ggplot(data = ARD_recruit_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Density of major trophic groups per complexity and treatment .\n fish < 5cm") #just faceted opposite than code below...#find the carnivore (outlier?)

ARD_recruit_c <- ARD_recruit_5 %>% 
  filter(MajorTrophicGroup == "Carnivore")

ggplot(data = ARD_recruit_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(treatment ~ complexity) +
  theme_dark() +
  labs(title = "Density of major trophic groups per treatment and complexity") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggplot(data = ARD_baseline_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Density of major trophic groups per complexity and treatment .\n fish <5cm (baseline)") #just faceted opposite than code below

ggplot(data = ARD_baseline_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(treatment ~ complexity) +
  theme_dark() +
  labs(title = "Density of major trophic groups per treatment and complexity") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
# histogram ---------------------------------------------------------------

ggplot(data = ARD_final, aes(x = density)) +
  geom_histogram

ARD_norm <- ARD_recruit %>% 
  select(density, biomass,TL, MajorTrophicGroup)

ARD_norm_3 <- ARD_recruit %>% 
  filter(TL < 4) %>% 
  select(density, biomass,TL, MajorTrophicGroup)

ARD_norm_3_treat <- ARD_recruit %>% 
  filter(TL < 4) %>% 
  select(density, biomass,TL, treatment)

ARD_norm_3_comp <- ARD_recruit %>% 
  filter(TL < 4) %>% 
  select(density, biomass,TL, complexity)

ARD_norm_5 <- ARD_recruit %>% 
  filter(TL < 6) %>% 
  select(density, biomass,TL, MajorTrophicGroup)#exclude carnivore? 

install.packages("GGally")
library(GGally)
ggpairs(ARD_norm_3, aes(col = MajorTrophicGroup, alpha = 0.4)) #subset
ggpairs(ARD_norm_3_treat, aes(col = treatment, alpha = 0.4))
ggpairs(ARD_norm_3_comp, aes(col = complexity, alpha = 0.4))

ggpairs(ARD_norm_5, aes(col = MajorTrophicGroup, alpha = 0.4))  
ggpairs(ARD_norm, aes(col = MajorTrophicGroup, alpha = 0.4))  


# removing outliers x1 -------------------------------------------------------
#March 2, 2020: 
#will have to re-calculate density after this:
#removed x50 bhw from survey 227, July 25, HS-13, control --> roaming shoal of fish (not associated with habitat)
#removed x27 bhw and x 16 spf from survey 238, July 28, HS-6, 50/50 --> roaming shoal of herbs
#removed x16bhw and x26 masked gobies from survey 118, June 20, HS-5, control --> roaming shoal of bhw and gobies floating in water col not associated w habitat
#removed x40 bhw from survey 92, June 13, LS, orange --> roaming shoal (transit yes)
#removed x19 bhw from survey 212, Jul 20, LS, orange --> roaming
#removed x45 bhw from survey 183, Jul 5, HN, control --> roaming (transit yes)
#removed x27 bhw from survey 218 Jul 20, HN, Ls-12, green --> roaming
#removed x30bhw from suvey 71, June 9, LS-11, orange --> roaming
#removed x20 bhw from survey 106, June 15, HS-yellow --> roaming
#removed x34 bhw from survey 133, June 25, HN-3, orange --> roaming
#removed duplicates entered of survey 180 (grid entered twice, check whole survey)
#removed x14 clown w from survey 44, June 6, LS-10, green --> shoaling
#removed x14 bhw from survey 58, June 7, HS-6, yellow --> shoaling
#removed x12 bhw from survey 127, June 25, LS-3, yellow --. shoaling
#remoed x8 bhw from survey 153, Jul 2, LS-20, orange - shoaling
#removed x25 bhw from survey 5, Jun 2, LS-11, orange --> shoaling
#removed x20 bhw from survey 19, June 3, HN-1, orange --> shoaling (transit yes)
#removed x20 bhw from survey 73, June 9, HS-1, yellow -> shoaling
#removed x24 bhw from survey 4, June 2, LS-3, yellow --> shoaling
#removed x15 masked goby from survey 55, June 7, LS-2, control --> in water col not associated w habitat
#removed x16 bhw from survey 5, June 2, LS-10, green --> shoaling

#removed out-liers with densities above 30. now going to go plot again 02/03/20


# removing outliers x2 ----------------------------------------------------

#03/02/20 changes made in xsxl file filtering_outliers_x2 (from ARD_final_no)
#removed x51 bhw from survey 92, Jun 13, LS-16, yellow --> shoaling
#removed x20 bhw from survey 116, LS-14, yellow --> shoaling
#removed x12 bhw from survey 26, LS-14, yellow --> shoaling
#removed x28 bhw from survey 146, HN-16, blue --> shoaling
#removed x18 bhw from survey 63, LN-22, green --> shoaling
#removed x26 bhw from survey 240, HS-19, blue --> shoaling
#removed x19 bhw from survey 132, HS-20, pink or red --> shoaling


# removing outliers x3 ----------------------------------------------------

#changes made in xsxl file filtering_outliers_x3 (from ARD_final_no2)
#removed x39 bhw from survey 140, LS-16, yellow --> shoaling
#removed x29 bhw from survey 159, HN-18, red or pink --> shoaling
#removed x25 bhw from survey 85, HN-1, orange --> shoaling
#removed x23 bhw from survey 144, HS-19, blue --> shoaling



# removing outliers from ARD_3_new_2 (the one with days_since_outp --------
#sept 22, re-do for ARD_3_with_0_new_2 
# nevermind, these have already been removed :) 

#removed x50 bhw from survey 227, July 25, HS-13, control --> roaming shoal of fish (not associated with habitat)
#removed x27 bhw and x 16 spf from survey 238, July 28, HS-6, 50/50 --> roaming shoal of herbs
#removed x16bhw and x26 masked gobies from survey 118, June 20, HS-5, control --> roaming shoal of bhw and gobies floating in water col not associated w habitat
#removed x40 bhw from survey 92, June 13, LS, orange --> roaming shoal (transit yes)
#removed x19 bhw from survey 212, Jul 20, LS, orange --> roaming
#removed x45 bhw from survey 183, Jul 5, HN, control --> roaming (transit yes)
#removed x27 bhw from survey 218 Jul 20, HN, Ls-12, green --> roaming
#removed x30bhw from suvey 71, June 9, LS-11, orange --> roaming
#removed x20 bhw from survey 106, June 15, HS-yellow --> roaming
#removed x34 bhw from survey 133, June 25, HN-3, orange --> roaming
#removed duplicates entered of survey 180 (grid entered twice, check whole survey)
#removed x14 clown w from survey 44, June 6, LS-10, green --> shoaling
#removed x14 bhw from survey 58, June 7, HS-6, yellow --> shoaling
#removed x12 bhw from survey 127, June 25, LS-3, yellow --. shoaling
#remoed x8 bhw from survey 153, Jul 2, LS-20, orange - shoaling
#removed x25 bhw from survey 5, Jun 2, LS-11, orange --> shoaling
#removed x20 bhw from survey 19, June 3, HN-1, orange --> shoaling (transit yes)
#removed x20 bhw from survey 73, June 9, HS-1, yellow -> shoaling
#removed x24 bhw from survey 4, June 2, LS-3, yellow --> shoaling
#removed x15 masked goby from survey 55, June 7, LS-2, control --> in water col not associated w habitat
#removed x16 bhw from survey 5, June 2, LS-10, green --> shoaling
#removed x51 bhw from survey 92, Jun 13, LS-16, yellow --> shoaling
#removed x20 bhw from survey 116, LS-14, yellow --> shoaling
#removed x12 bhw from survey 26, LS-14, yellow --> shoaling
#removed x28 bhw from survey 146, HN-16, blue --> shoaling
#removed x18 bhw from survey 63, LN-22, green --> shoaling
#removed x26 bhw from survey 240, HS-19, blue --> shoaling
#removed x19 bhw from survey 132, HS-20, pink or red --> shoaling
#changes made in xsxl file filtering_outliers_x3 (from ARD_final_no2)
#removed x39 bhw from survey 140, LS-16, yellow --> shoaling
#removed x29 bhw from survey 159, HN-18, red or pink --> shoaling
#removed x25 bhw from survey 85, HN-1, orange --> shoaling
#removed x23 bhw from survey 144, HS-19, blue --> shoaling
