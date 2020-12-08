## committee meeting figures - April 2020 - Ch 2 artificial data experiment ##

SFD_3 <- read_csv("small fish/data/SFD_3.csv")
ARD_3new <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_3new.csv")

# packages and pallettes --------------------------------------------------



#goals: make dot plots of density over time for each functional group, traits (like position in water col, max size, feeding guild)
library(readxl)
library(tidyverse)
library(ggplot2)
library(readr)
library(viridis)
library(PNWColors)
library("RColorBrewer")

install.packages("RColorBrewer")


pal <- pnw_palette(2, name = "Starfish", type = "continuous")
pal1 <- pnw_palette(2, name = "Winter", type = "continuous")
pal2 <-pnw_palette(8, name = "Anemone", type = "continuous")
pal3 <-pnw_palette(2, name = "Lake", type = "continuous")

# Ch 1 figures ------------------------------------------------------------

# fish under 3cm ----------------------------------------------------------


#import and view, rename to SFD
SFD_final <- read_excel("C:/Users/anrgr/Dropbox/U of A/data/small fish/data/SFD_final.xlsx")
View(SFD_final)
SFD <- SFD_final

#1)delete old density and biomass rows
#2)filter for fish under 3cm and calculate new density and biomass values
#3)export and remove outliers of roaving shoals (based on highest densities)

#1)#1)delete old density and biomass rows

SFD[,c("density", "biomass")]<- list(NULL)

#2)filter for fish under 3cm and calculate new density and biomass values
SFD_3 <- SFD %>% 
  filter(TL <= 3) %>% 
  group_by(site_grid_survey_point) %>% 
  mutate(density = sum(fish_presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

#3)export and remove outliers of roaving shoals (based on highest densities) for within site questions
write.csv(SFD_3,"SFD_3.csv") #most current version has added trait values (not this one)


# fish under 5cm ----------------------------------------------------------


#import and view, rename to SFD
SFD_final <- read_excel("C:/Users/anrgr/Dropbox/U of A/data/small fish/data/SFD_final.xlsx")
View(SFD_final)
SFD <- SFD_final

#1)delete old density and biomass rows
#2)filter for fish under 3cm and calculate new density and biomass values
#3)export and remove outliers of roaving shoals (based on highest densities)

#1)#1)delete old density and biomass rows

SFD[,c("density", "biomass")]<- list(NULL)

#2)filter for fish under 3cm and calculate new density and biomass values
SFD_5 <- SFD %>% 
  filter(TL <= 5) %>% 
  group_by(site_grid_survey_point) %>% 
  mutate(density = sum(fish_presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

#3)add trait values

Small_Fish_Data_Species_List <- read_csv("small fish/data/raw data/Small Fish Data - Species List.csv")
SFD_select_trait_values <- read_csv("small fish/data/SFD select trait values.csv")

SFD_spp <- Small_Fish_Data_Species_List %>% 
  select(CommonName)

SFD_traits <- left_join(SFD_spp, SFD_select_trait_values)
#had to go and change a few spellings but now they're all there, next join to large SFD df

SFD_traits = SFD_traits %>% 
  rename(common_name = CommonName)

SFD_5 <-full_join(SFD_5, SFD_traits)
#yas it worked, export and save

#removed 80 str pf j from survey 4, GR-C Jun 19 --> video data of fish not all in survey plot, roaving shoal
#removed 30 bhw roaving shoal same survey grid 26
#removed 30 bhw from survey 40 NDR-C grid 89

#re-import and calculate density and biomass
SFD_5 <- read_csv("small fish/data/SFD_5.csv")
View(SFD_5)

#4)export and remove outliers of roaving shoals (based on highest densities) for within site questions
write.csv(SFD_5,"SFD_5.csv") #most current version has added trait values 

SFD_5 <- SFD_5 %>% 
group_by(site_grid_survey_point) %>% 
  mutate(density = sum(fish_presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

write.csv(SFD_5,"SFD_5.csv") 
# points with 0 -----------------------------------------------------------

SFD_final <- read_excel("small fish/data/SFD_final.xlsx")
#need to make a df with all the densit = 0 points to join to SFD_3 and SFD_5
# otherwise the survey_grid_point counts with 0 fish get left behind

SFD.0 <- SFD_final %>% 
  filter(density == 0)
write.csv(SFD.0,"SFD.0.csv")

# added to SFD_5 and SFD_3 on Apr 19

# overall summary plots ---------------------------------------------------


# fish under 3 ------------------------------------------------------------


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
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#just treatment col plot
SFD_sum3_treat <- SFD_3 %>% 
  # filter(visit != "6") %>% 
  # filter(visit != "5") %>% #for fully balanced design, only look at visits 1-4, still 1 month period
  # mutate(treatment = factor(treatment, levels = c("NO", "O"),
  #                           labels = c("Unrestored", "Restored"))) %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = SFD_sum3_treat) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                width = 0.2)+
  scale_fill_manual(values = pal1) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 3cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#just complexity col plot
SFD_sum3_comp <- SFD_3 %>% 
  # filter(visit != "6") %>% 
  # filter(visit != "5") %>% #for fully balanced design, only look at visits 1-4, still 1 month period
  group_by(complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = SFD_sum3_comp) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = complexity)) +
  geom_errorbar(aes(x = complexity,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                width = 0.2)+
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(x = complexity,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                width = 0.2)+
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 3cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )



#geom col of density over time for each treatment, faceted by complexity. 
SFD_sum3_vis <- SFD_3 %>% 
  # filter(visit != "6") %>% 
  # filter(visit != "5") %>% #for fully balanced design, only look at visits 1-4, still 1 month period
  # mutate(treatment = factor(treatment, levels = c("NO", "O"),
                            # labels = c("Unrestored", "Restored"))) %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot()+
  geom_col(data = SFD_sum3_vis,
               aes(x = visit,
                   y = density.mean,
                   fill = treatment),
               alpha = 0.8,
           position = "dodge")+
  # geom_errorbar(aes(x = visit,
  #                   fill = treatment,
  #                   ymin = density.mean - density.sd,
  #                   ymax = density.mean + density.sd),
  #               position = position_dodge(0.9),
  #               width = 0.2)+
  scale_fill_manual(values = pal) +
  facet_grid(.~complexity)+
  ylab("mean fish density (fish/m^2)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )


# fish under 5 ------------------------------------------------------------

SFD_sum5 <- SFD_5 %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

#col plot density treatment and complexity
ggplot(data = SFD_sum5) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = complexity),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = complexity,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = pal) +
  facet_grid(.~treatment) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("Mean density (fish under 5cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#just treatment col plot
SFD_sum5_treat <- SFD_5 %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = SFD_sum5_treat) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                width = 0.2)+
  scale_fill_manual(values = pal1) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("Mean density (fish under 5cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

#just complexity col plot
SFD_sum5_comp <- SFD_5 %>% 
  group_by(complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = SFD_sum5_comp) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = complexity)) +
  geom_errorbar(aes(x = complexity,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                width = 0.2)+
  scale_fill_manual(values = pal) +
  geom_errorbar(aes(x = complexity,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                width = 0.2)+
  ylab("mean fish density (fish/m^2)")+
  ggtitle("Mean density (fish under 5cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )



# grouped by site (overall) -----------------------------------------------

#mean density at each site, included treatment as a grouping var to colour
SFD_site <- SFD_3 %>% 
  # filter(visit != "6") %>%
  # filter(visit != "5") %>% 
  group_by(site, treatment, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1181))

SFD_site_time <- SFD_3 %>% 
  filter(visit != "6") %>%
  # filter(visit != "5") %>% 
  group_by(visit, treatment, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot()+
  geom_point(data = SFD_site_time,
             aes(x = visit,
                 y = density.mean))+
  facet_grid(complexity~treatment)+
  geom_line(data = SFD_site_time,
            aes(x = visit,
                y = density.mean)) #add error bars --> size 

ggplot()+
  geom_jitter(data = SFD_3,
              aes(x = visit,
                  y = density)) +
  facet_grid(complexity~treatment)

ggplot()+
  geom_col(data = SFD_site,
           aes(x = site,
               y = density.mean,
               fill = complexity,
               colour = complexity),
           position = "dodge") +
  geom_errorbar(data = SFD_site,
                aes(x = site,
                    fill = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se),
                position = position_dodge(0.2),
                width = 0.2) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = c("grey30", "grey30")) +
  # scale_alpha_discrete(range = c(0.7, 0.3))+
  ylab("mean fish density (fish/m^2)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = -60, hjust = 0.1),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )


SFD_site_3 <- SFD_3 %>% 
  filter(visit != "6") %>%
  filter(visit != "5") %>% #for fully balanced design, only look at visits 1-4, still 1 month period
  mutate(treatment = factor(treatment, levels = c("NO", "O"),
                            labels = c("Unrestored", "Restored"))) %>% 
  group_by(complexity, treatment, site) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

#geom col of overall density at each site faceted for treat and comp
ggplot()+
  geom_col(data = SFD_site_3,
           aes(x = site,
               y = density.mean,
               fill = complexity),
           position = "dodge") +
  # geom_errorbar(data = SFD_site_3,
  #               aes(x = site,
  #                   fill = treatment,
  #                   ymin = density.mean - density.sd,
  #                   ymax = density.mean + density.sd),
  #               position = position_dodge(0.2),
  #               width = 0.2) +
  scale_fill_manual(values = pal) +
  facet_grid(.~complexity)+
  ylab("mean fish density (fish/m^2)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = -60, hjust = 0.1),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

ggplot()+
  geom_boxplot(data = SFD_site_3,
           aes(x = site,
               y = density.mean,
               fill = treatment)) +
  scale_fill_manual(values = pal) +
  # facet_grid(.~complexity)+
  ylab("mean fish density (fish/m^2)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = -60),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )



#these are showing that in general unrestored sites have higher fish densities compared to restored, except for pickles
#this is a fairly flat reef, so I wonder if the overall reef context has soemthing to do with this more, ie
# ie what the overall rugosity and complexity is like...

# grouped by taxanomic family -------------------------------------------------------------

#grouped by family
SFD_fam_sum <- SFD_3 %>%
  # mutate(treatment = factor(treatment, levels = c("NO", "O"),
  #                           labels = c("Unrestored", "Restored"))) %>% 
  group_by(treatment,complexity, visit, Family) %>% 
  # filter(visit != "6") %>% 
  # filter(visit != "5") %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

fix(SFD_fam_sum)

#line-plot of family over time for each treat and comp
ggplot(data = SFD_fam_sum) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = Family,
                colour = Family),
            size = 2.5,
            alpha = 0.5) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = Family,
                 group = Family,),
             size = 3,
             position = position_dodge(0.2)) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = Family,
                    group = Family), 
                width = .3,
                size = 0.5,
                position = position_dodge(0.2)) +
  facet_grid(complexity~treatment) +
  # ylim(0,15)+
  ylab("mean fish density (fish/m^2)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )


# grouped by Major Triophic Group (functional groups) --------------------------------------------------------------

SFD_func_sum3 <- SFD_3 %>% 
  group_by(treatment,complexity, visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

#line plot - functional group over time for each treat and comp
ggplot() +
  geom_jitter(data = SFD_3,
              aes(x = visit,
                  y = density,
                  colour = MajorTrophicGroup),
              alpha = 0.3) +
  geom_line(data = SFD_func_sum3,
            aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup),
            size = 2,
            alpha = 0.8) +
  geom_point(data = SFD_func_sum3,
             aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup),
             size = 2) +
  geom_errorbar(data = SFD_func_sum3,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  scale_colour_viridis(discrete = TRUE, name = "Trophic Group") +
  facet_grid(complexity~treatment) +
  ylim(0,15)+
  ylab("mean fish density (fish/m^2)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )


SFD_func_sum3col <- SFD_3 %>% 
  group_by(treatment,complexity, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = SFD_func_sum3col) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = MajorTrophicGroup),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = MajorTrophicGroup,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group") +
  facet_grid(.~treatment) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Trophic Group (3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

# proportions of functional groups in each treat and comp -----------------


#proportionate amount of functional groups  each treat and comp #-> next do over time for each
SFD_dens_sum3 <- SFD_3 %>% 
  filter(visit != "6") %>% 
  filter(visit != "5") %>% 
  group_by(treatment,complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(1181))

SFD_troph_prop = SFD_3 %>% 
  group_by(treatment, complexity, MajorTrophicGroup) %>% 
  summarize(n = length(MajorTrophicGroup)) %>% 
  mutate(proportion = n/sum(n))

SFD_func_for_join = SFD_dens_sum3 %>% 
  select(density.mean, density.sd, complexity, treatment)

SFD_troph_prop$complexity = as.factor(SFD_troph_prop$complexity)
SFD_troph_prop$treatment = as.factor(SFD_troph_prop$treatment)
SFD_func_for_join$complexity = as.factor(SFD_func_for_join$complexity)
SFD_func_for_join$treatment = as.factor(SFD_func_for_join$treatment)

SFD_troph_prop = merge (SFD_troph_prop, SFD_func_for_join,
                        by.x = c('treatment', 'complexity'),
                        by.y = c('treatment', 'complexity'))

SFD_troph_prop_plot = SFD_troph_prop %>%
  mutate(treatment = factor(treatment, levels = c("NO", "O"),
                            labels = c("Unrestored", "Restored"))) %>%
  mutate(prop_fill = proportion * density.mean)

ggplot(data = SFD_troph_prop_plot) +
  geom_col(aes(x = complexity,
               y = prop_fill, 
               fill= MajorTrophicGroup), 
           position = 'stack') +
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group") +
  facet_grid(.~treatment) +
  ylab("mean density (fish/m^2)")+
  xlab("Background Complexity")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(hjust = 0.1),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )



# proporiton of functional groups in each site ----------------------------

#proportionate amount of functional groups  each treat and comp #-> next do over time for each
SFD_dens_sum3_site <- SFD_3 %>% 
  filter(visit != "6") %>% 
  filter(visit != "5") %>% 
  group_by(site) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

SFD_troph_prop_site = SFD_3 %>% 
  filter(visit != "6") %>% 
  filter(visit != "5") %>% 
  group_by(site, MajorTrophicGroup) %>% 
  summarize(n = length(MajorTrophicGroup)) %>% 
  mutate(proportion = n/sum(n))

SFD_func_site_for_join = SFD_dens_sum3_site %>% 
  select(density.mean, density.sd, site)

SFD_troph_prop_site$site = as.factor(SFD_troph_prop_site$site)
SFD_func_site_for_join$site = as.factor(SFD_func_site_for_join$site)

SFD_troph_prop_site = merge (SFD_troph_prop_site, SFD_func_site_for_join,
                        by.x = c('site'),
                        by.y = c('site'))

SFD_troph_prop_site_plot = SFD_troph_prop_site %>%
  mutate(prop_fill = proportion * density.mean)

ggplot(data = SFD_troph_prop_site_plot) +
  geom_col(aes(x = site,
               y = prop_fill, 
               fill= MajorTrophicGroup), 
           position = 'stack') +
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group") +
  # facet_grid(.~treatment) +
  ylab("mean density (fish/m^2)")+
  xlab("Background Complexity")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = -60, hjust = 0.1),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )


# joining new traits data (depth range, water column position, behaviour) -------------------------------------------------

#gonna try joining new traits df with spp list, (see if there are any missing) then to SFD_3
Small_Fish_Data_Species_List <- read_csv("small fish/data/raw data/Small Fish Data - Species List.csv")
SFD_select_trait_values <- read_csv("small fish/data/SFD select trait values.csv")

SFD_spp <- Small_Fish_Data_Species_List %>% 
  select(CommonName)

SFD_traits <- left_join(SFD_spp, SFD_select_trait_values)
#had to go and change a few spellings but now they're all there, next join to large SFD df

SFD_traits = SFD_traits %>% 
  rename(common_name = CommonName)

SFD_3 <-full_join(SFD_3, SFD_traits)
#yas it worked, export and save

write.csv(SFD_3, "SFD_3.csv") #this si the most current df to work from in data folder
write.csv(SFD_traits, "SFD_traits.csv")

# Ch 2 Figures ------------------------------------------------------------

ARD_final <- read_csv("data/Art Recruit data/final clean data frames/ARD_final.csv")
View(ARD_final)
ARD_recruit_fish_under_5_with_time_block_and_treat_comp <- read_csv("data/Art Recruit data/final clean data frames/ARD_recruit_fish under 5 with time block and treat_comp.csv")
View(ARD_recruit_fish_under_5_with_time_block_and_treat_comp)

ARD <- ARD_recruit_fish_under_5_with_time_block_and_treat_comp #this file has incorrect density values, make sure to recalculate




# all time points (visit) #these were done with incorrect density values for fish under 5 (ARD) -------------------------------------------------

#grouped and sumarized by treatment and family.
ARD_treat_sum <- ARD %>% 
  group_by(treatment,visit, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_sum) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = Family,
                colour = Family)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = Family)) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  theme_light()

#summarized and grouped by complexity and Family
ARD_comp_sum <- ARD %>% 
  group_by(complexity, visit, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_sum) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = Family,
                colour = Family)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = Family)) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  theme_light()

#summarized and grouped by treatment and functional group
ARD_treat_trophic_sum <- ARD %>% 
  group_by(treatment,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_trophic_sum) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  theme_light()

#summarized and grouped by comlexoty and functional grou
ARD_comp_trophic_sum <- ARD %>% 
  group_by(complexity,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_trophic_sum) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  theme_light()




# just start time points (first 6 visits) #these were also with incorrect density values for fish under 5 (ARD) ---------------------------------

ARD$time_block = as.factor(ARD$time_block)

#sumarized and grouped by treatment and Family
ARD_treat_sum_start <- ARD %>% 
  filter(time_block == "start") %>% 
  # filter(time_block %in% c("start", "middle")) %>% 
  group_by(treatment,visit, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_sum_start) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = Family,
                colour = Family)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = Family)) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  theme_light()

#summarized and grouped by complexity and Family
ARD_comp_sum_start <- ARD %>% 
  filter(time_block == "start") %>% 
  group_by(complexity, visit, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_sum_start) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = Family,
                colour = Family)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = Family)) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  theme_light()

#summarized and grouped by treatment and functional group
ARD_treat_trophic_sum_start <- ARD %>% 
  filter(time_block == "start") %>%
  group_by(treatment,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_trophic_sum_start) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  theme_light()

#summarized and grouped by comlexoty and functional grou
ARD_comp_trophic_sum_start <- ARD %>% 
  filter(time_block == "start") %>%
  group_by(complexity,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_trophic_sum_start) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  theme_light()




#
#
#
# recalculating biomass and densities for ARD under 3 ---------------------

#1)delete old density and biomass rows
#2)filter for fish under 3cm and calculate new density and biomass values
#3)export and remove outliers of roaving shoals (based on highest densities)

#1)#1)delete old density and biomass rows

ARD[,c("density", "biomass")]<- list(NULL)

#2)filter for fish under 3cm and calculate new density and biomass values
ARD_3 <- ARD%>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

#3)export and remove outliers of roaving shoals (based on highest densities)
write.csv(ARD_3,"ARD_3.csv")


# line plots fish under 3 (correct densities) ------------------------------


# family ------------------------------------------------------------------


#grouped and sumarized by treatment and family.
ARD_treat_sum_3 <- ARD_3 %>% 
  group_by(treatment,visit, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_sum_3) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = Family,
                colour = Family)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = Family)) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  theme_classic()

#summarized and grouped by complexity and Family
ARD_comp_sum_3 <- ARD_3 %>% 
  group_by(complexity, visit, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_sum_3) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = Family,
                colour = Family)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = Family)) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  theme_light()


# trophic group -----------------------------------------------------------


#summarized and grouped by treatment and functional group
ARD_treat_trophic_sum_3 <- ARD_3 %>% 
  group_by(treatment,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_trophic_sum_3) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  theme_light()

#summarized and grouped by comlexoty and functional grou
ARD_comp_trophic_sum_3 <- ARD_3 %>% 
  group_by(complexity,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_trophic_sum_3) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  theme_light()

# line plots first 7 visits fish under 3 (also trophic group)----------------------------------

ARD_3$time_block = as.factor(ARD_3$time_block)

#summarized and grouped by treatment and functional group
ARD_treat_trophic_sum_start_3 <- ARD_3 %>% 
  filter(time_block == "start") %>%
  group_by(treatment,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_trophic_sum_start_3) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  theme_light()

#summarized and grouped by comlexoty and functional grou
ARD_comp_trophic_sum_start_3 <- ARD_3 %>% 
  filter(time_block == "start") %>%
  group_by(complexity,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_trophic_sum_start_3) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup)) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  theme_light()


# geom_col fish under 3 ---------------------------------------------------


#overall density for each treat and comp


ARD_sum3 <- ARD_3 %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_sum3) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = complexity)) +
  # fill = "grey80") +
  # scale_fill_viridis(discrete = TRUE) +
  facet_grid(.~treatment) +
  # ylim(0,40)+
  theme_dark()

#proportionate amount of functional groups over time for each treat and comp

ARD_func_sum3 <- ARD_3 %>% 
  group_by(treatment,complexity, visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

#each functional group over time
ggplot(data = ARD_func_sum3) +
  geom_col(aes(x = visit,
               y = density.mean,
               fill= MajorTrophicGroup)) +
  scale_fill_viridis(discrete = TRUE) +
  facet_grid(treatment~complexity) +
  # ylim(0,40)+
  theme_dark()

#overall functional group for each treatment and complexity
ggplot(data = ARD_func_sum3) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill= MajorTrophicGroup)) +
  # position = "dodge") +
  scale_fill_viridis(discrete = TRUE) +
  facet_grid(.~complexity) +
  # ylim(0,40)+
  theme_dark()

#look at proportion of family for each treatment 

ARD_fam_sum3 <- ARD_3 %>% 
  group_by(treatment, complexity, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_fam_sum3) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill= Family)) +
  # position = "dodge") +
  scale_fill_viridis(discrete = TRUE) +
  main = "Recruits < 3"
  # facet_grid(.~complexity) +
  # ylim(0,40)+
  theme_dark()


# boxplots fish under 3 - 3 main hypotheses ----------------------------------------------------------------

ARD_3$time_block = as.factor(ARD_3$time_block)
ARD_3$treatment = as.factor(ARD_3$treatment)
ARD_3$complexity = as.factor(ARD_3$complexity)

ARD_recruit_box_3 <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

#treatment and complexity over time
ggplot()+
  geom_boxplot(data = ARD_recruit_box_3,
               aes(x = treatment,
                   y = density,
                   colour = time_block,
                   fill = treatment), alpha = 0.6)+
  scale_colour_grey(start = 0.9, end = 0.5, name = "Time")+
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

#just treatment
ggplot()+
  geom_boxplot(data = ARD_recruit_box_3,
               aes(x = treatment,
                   y = density,
                   # colour = time_block,
                   fill = treatment), alpha = 0.6)+
  scale_colour_grey(start = 0.9, end = 0.5, name = "Time")+
  xlab("Treatment") +
  ylab("Fish Density") +
  theme_classic() +
  # facet_grid(complexity ~ .) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

#treatment faceted with complexity
ggplot()+
  geom_boxplot(data = ARD_recruit_box_3,
               aes(x = treatment,
                   y = density,
                   # colour = time_block,
                   fill = treatment), alpha = 0.6)+
  scale_colour_grey(start = 0.9, end = 0.5, name = "Time")+
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






# USE THIS - removing outliers for ARD 3 and making new df to work from (ARD_3new)---------------------------------------------

#deleted a school of 16 bhw from survey 73, grid cell 8, June 9
#deleted a school of 14 bhw from survey 190, grid cell 9, July 9
#deleted a school of 8 bhw from survey 58, grid cell 8, June 7
#deleted a school of 13 bhw from survey 158, grid cell 10, July 2
#deleted 6 bhw from survey 191, grid cell 12, July 9 --> incorrect number written from datasheet

#import the "cleaned" sheet , redo density calculations, re-visualize, then also try with excluding masked gobies...

ARD_3_post_filtering <- read_csv("Artificial coral experiment/data/Art Recruit data/outlier filtering/ARD_3/ARD_3 post filtering.csv")

#delete old density and biomass cols
ARD_3_post_filtering[,c("density", "biomass")]<- list(NULL)

#re-do density and biomass calculation
ARD_3new <- ARD_3_post_filtering %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))
  # filter(common_name != "Masked Goby")

#make sure treatment and time_block values are in the "right" order
ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

#make sure these are factors before plotting
ARD_3new$time_block = as.factor(ARD_3new$time_block)
ARD_3new$treatment = as.factor(ARD_3new$treatment)
ARD_3new$complexity = as.factor(ARD_3new$complexity)

#export to save a copy
write.csv(ARD_3new,"ARD_3new.csv")
  
# boxplots with ARD 3 - outliers removed ----------------------------------

ARD_recruit_box_3new <- ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

#just treatment
ggplot()+
  geom_boxplot(data = ARD_recruit_box_3new,
               aes(x = treatment,
                   y = density,
                   # colour = time_block,
                   fill = treatment), alpha = 0.6)+
  scale_colour_grey(start = 0.9, end = 0.5, name = "Time")+
  ylim(0,20)+
  xlab("Treatment") +
  ylab("Fish Density") +
  theme_classic() +
  # facet_grid(complexity ~ .) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )


#treatment faceted with complexity
ggplot()+
  geom_boxplot(data = ARD_recruit_box_3new,
               aes(x = treatment,
                   y = density,
                   # colour = time_block,
                   fill = treatment), alpha = 0.6)+
  scale_colour_grey(start = 0.9, end = 0.5, name = "Time")+
  ylim(0,20)+
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

#treatment and complexity over time
ggplot()+
  geom_boxplot(data = ARD_recruit_box_3new,
               aes(x = treatment,
                   y = density,
                   colour = time_block,
                   fill = treatment), alpha = 0.6)+
  scale_colour_grey(start = 0.9, end = 0.5, name = "Time")+
  ylim(0,20)+
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


#functional groups for each complexity
ggplot()+
  geom_boxplot(data = ARD_func_sum3new,
               aes(x = MajorTrophicGroup,
                   y = density.mean,
                   fill = MajorTrophicGroup))+
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group")+
  facet_grid(.~complexity)+
  ylim(0,10)+
  xlab("Trophic Group") +
  ylab("mean fish density (fish/m^2") +
  theme_classic()+
  theme(
    plot.title = element_text(size = 20),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    # legend.position = "none"
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 16)
  )

                  
# geom_col - ARD_3 outliers removed ---------------------------------------


#overall density for each treat and comp

ARD_sum3new <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>%
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_sum3new_treat <- ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_sum3new_treat_time <- ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  group_by(treatment, time_block) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(ymin.treat = density.mean-density.sd, ymax.treat = density.mean + density.sd)

#col plot for just treatment
ggplot(data = ARD_sum3new_treat) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd),
                size = 1,
                width = 0.2) + 
  ylab("mean density (fish/m^2)") +
  xlab("treatment") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_line(colour = "grey40")
  )

#col plot for treatment over time with errorbars
ggplot(data = ARD_sum3new_treat_time) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment,
               alpha = time_block),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    fill = treatment,
                    alpha = time_block,
                    ymin = ymin.treat, 
                    ymax = ymax.treat),
                position = position_dodge(0.9),
                size = 1,
                width = 0.2,
                colour = "grey80") + 
  # ylim(0,15)+
  scale_alpha_manual(values = c(1,0.8,0.6), name = "Time")+
  ylab("mean density (fish/m^2)") +
  xlab("treatment") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey20"),
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_line(colour = "grey40")
  )

#overall density in each treatment and comp
ggplot(data = ARD_sum3new) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = complexity)) +
  # fill = "grey80") +
  # scale_fill_viridis(discrete = TRUE) +
  facet_grid(.~treatment) +
  # ylim(0,40)+
  ylab("mean density (fish/m^2)")+
  xlab("Complexity")+
  # ylim(0,40)+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_line(colour = "grey40")
  )

#proportionate amount of functional groups over time for each treat and comp

ARD_func_sum3new <- ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  group_by(treatment,complexity, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

# #each functional group over time... a bit of a messy plot
# ggplot(data = ARD_func_sum3new) +
#   geom_col(aes(x = visit,
#                y = density.mean,
#                fill= MajorTrophicGroup)) +
#   scale_fill_viridis(discrete = TRUE) +
#   facet_grid(treatment~complexity) +
#   # ylim(0,40)+


#overall functional group for each treatment and complexity #INTERESTING
ggplot(data = ARD_func_sum3new) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill= MajorTrophicGroup)) +
  # position = "dodge") +
  scale_fill_viridis(discrete = TRUE) +
  facet_grid(.~complexity) +
  ylab("mean density (fish/m^2)")+
  xlab("Treatment - % living coral")+
  # ylim(0,40)+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_line(colour = "grey40")
  )

#prop of functional groups looking at just complexity
ggplot(data = ARD_func_sum3new) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = MajorTrophicGroup)) +
  scale_fill_viridis(discrete = TRUE) +
  ylab("mean density (fish/m^2)")+
  xlab("Background Complexity")+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_line(colour = "grey40")
  )

#prop functional group for each treat and comp, with comp on x axis. good to compare w plot like this that doesn't break into functional group
ggplot(data = ARD_func_sum3new) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill= MajorTrophicGroup)) +
  # position = "dodge") +
  scale_fill_viridis(discrete = TRUE) +
  facet_grid(.~treatment) +
  # ylim(0,40)+
  ylab("mean density (fish/m^2)")+
  xlab("Background Complexity")+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_line(colour = "grey40")
  )


#look at proportion of family for each treatment 

ARD_fam_sum3 <- ARD_3 %>% 
  group_by(treatment, complexity, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_fam_sum3) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill= Family)) +
  # position = "dodge") +
  scale_fill_viridis(discrete = TRUE) +
  main = "Recruits < 3"
# facet_grid(.~complexity) +
# ylim(0,40)+
theme_dark()



# geom_col for max length -------------------------------------------------

ARD_MaxL <- ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(MLbin = cut_interval (MaxLength, n = 5)) %>% 
  group_by(MLbin, treatment, complexity, visit) %>% 
  summarize(summarize(density.mean = mean(density), density.sd = sd(density), count = n()))
  
 
ARD_MaxL_sum3 <- ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  group_by(treatment,complexity, visit, MaxLength) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_MaxL_sum3) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill= MaxLength)) +
  # scale_fill_viridis(discrete = TRUE) +
  facet_grid(.~treatment) +
  # ylim(0,40)+
  ylab("mean density (fish/m^2)")+
  xlab("Background Complexity")+
  # ylim(0,40)+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey30"),
    panel.grid.major = element_line(colour = "grey40"),
    panel.grid.minor = element_line(colour = "grey40")
  )



# line plots - ARD_3 outliers removed -------------------------------------


#summarized and grouped by treatment and functional group
ARD_treat_trophic_sum_3new <- ARD_3new %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  group_by(treatment,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_treat_trophic_sum_3new) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup),
            size = 2,
            alpha = 0.8) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(treatment~.) +
  ylab("mean density (fish/m^2)")+
  xlab("Visit")+
  ylim(0,20)+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey50"),
    panel.grid.major = element_line(colour = "grey60"),
    panel.grid.minor = element_line(colour = "grey60")
  )

#summarized and grouped by comlexoty and functional grou
ARD_comp_trophic_sum_3new <- ARD_3new %>% 
  group_by(complexity,visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ggplot(data = ARD_comp_trophic_sum_3new) +
  geom_line(aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup),
            size = 2,
            alpha = 0.8) +
  geom_point(aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup)) +
  scale_colour_viridis(discrete = TRUE) +
  geom_errorbar(aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd), 
                width = .05,
                size = 0.5, 
                position = position_dodge(0.05)) +
  facet_grid(complexity~.) +
  ylab("mean density (fish/m^2)")+
  xlab("Visit")+
  ylim(0,20)+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16),
    panel.background = element_rect(fill = "grey50"),
    panel.grid.major = element_line(colour = "grey60"),
    panel.grid.minor = element_line(colour = "grey60")
  )
