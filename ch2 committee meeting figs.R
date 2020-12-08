# Committee meeting - Ch 2 fingures

ARD_post_out_plant <- read_csv("data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
# *no 0 values to add since there were no survys with 0 fish observed
ARD_3 <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_3.csv")
ARD_3 <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))
#ARD_3 also has all plot_grid represented, so no 0 values to join
#this used to be called ARD_3new, has outliers removed (shoals of bhw, see script "Commmittee meeting figures" in ARD exp code
ARD_7 <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_7.csv")
View(ARD_7)
ARD_5 <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_5.csv")
ARD_5 <- ARD_5 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

# packages and pallettes --------------------------------------------------

library(readxl)
library(readr)
library(tidyverse)
library(ggplot2)
library(readr)
library(viridis)
library(PNWColors)
library("RColorBrewer")

pal <- pnw_palette(2, name = "Starfish", type = "continuous")
pal1 <- pnw_palette(2, name = "Bay", type = "continuous")
pal2 <- pnw_palette(8, name = "Sunset2", type = "continuous")
pal3 <- pnw_palette(6, name = "Sunset2", type = "continuous")
pal4 <- pnw_palette(13, name = "Starfish", type = "continuous")
pal5 <- pnw_palette(6, name = "Sunset2", type = "continuous")


# filter for fish under 5cm -------------------------------------------------

ARD <- ARD_post_out_plant

ARD[,c("density", "biomass")]<- list(NULL)

ARD_5 <- ARD%>% 
  filter(TL <= 5) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

ARD_5 <- ARD_5 %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A","70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

write.csv(ARD_5,"ARD_5.csv")


# filter for fish under 7cm -----------------------------------------------

ARD <- ARD_post_out_plant

ARD[,c("density", "biomass")]<- list(NULL)

ARD_7 <- ARD%>% 
  filter(TL <= 7) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b)))

ARD_7 <- ARD_7 %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A","70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

write.csv(ARD_7,"ARD_7.csv")


# just treatment ----------------------------------------------------------

ARD_3_treat_sum <- ARD_3 %>% 
  group_by(treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 3)

ARD_5_treat_sum <- ARD_5 %>% 
  group_by(treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 5)

ARD_treat_sum <- rbind(ARD_3_treat_sum,ARD_5_treat_sum)
fix(ARD_treat_sum) #make sure fish.size is character

ARD_treat_sum1 <- ARD_treat_sum %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control")))

#plot from joined df (rbind) to position dodge points - new code to be able to position dodge
ggplot() +
  geom_jitter(data = ARD_3,
              aes(x = visit,
                  y = density),
              alpha = 0.05,
              colour = "coral") +
  geom_jitter(data = ARD_5,
              aes(x = visit,
                  y = density),
              alpha = 0.05,
              colour = "aquamarine") +
  geom_line(data = ARD_treat_sum1,
            aes(x = visit,
                y = density.mean,
                colour = fish.size,
                group = fish.size),
            size = 1,
            alpha = 0.5) +
  geom_errorbar(data = ARD_treat_sum1,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = fish.size,
                    group = fish.size), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_treat_sum1,
             aes(x = visit, 
                 y = density.mean,
                 colour = fish.size,
                 group = fish.size),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.7) +
  geom_smooth(data = ARD_treat_sum1,
              aes(x = visit,
                  y = density.mean,
                  colour = fish.size,
                  group = fish.size),
              size = 2,
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(.~treatment)+
  ylim(0,30)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean fish recruit density over time")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 16)
  )


#col plots
ARD_3_treat_sumcol <- ARD_3 %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 3)

ARD_5_treat_sumcol <- ARD_5 %>% 
  group_by(treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 5)

ARD_treat_sumcol <- rbind(ARD_3_treat_sumcol,ARD_5_treat_sumcol)
fix(ARD_treat_sumcol) #make sure fish.size is character

ARD_treat_sumcol <- ARD_treat_sumcol %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control")))

ggplot(data = ARD_3_treat_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd,
                    group = treatment),
                width = 0.2)+
  # scale_fill_manual(values = pal1) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 3cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

ggplot(data = ARD_5_treat_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd,
                    group = treatment),
                width = 0.2)+
  # scale_fill_manual(values = pal1) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 5cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )



# complexity --------------------------------------------------------------

ARD_3_comp_sum <- ARD_3 %>% 
  group_by(complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 3)

ARD_5_comp_sum <- ARD_5 %>% 
  group_by(complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 5)

ARD_comp_sum <- rbind(ARD_3_comp_sum,ARD_5_comp_sum)
fix(ARD_comp_sum) #make sure fish.size is character
# ARD_comp_sum <- ARD_comp_sum %>% 
#   mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control")))

#plot from joined df (rbind) to position dodge points - new code to be able to position dodge
ggplot() +
  geom_jitter(data = ARD_3,
              aes(x = visit,
                  y = density),
              alpha = 0.05,
              colour = "coral") +
  geom_jitter(data = ARD_5,
              aes(x = visit,
                  y = density),
              alpha = 0.05,
              colour = "aquamarine") +
  geom_line(data = ARD_comp_sum,
            aes(x = visit,
                y = density.mean,
                colour = fish.size,
                group = fish.size),
            size = 1,
            alpha = 0.5) +
  geom_errorbar(data = ARD_comp_sum,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = fish.size,
                    group = fish.size), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_comp_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = fish.size,
                 group = fish.size),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.5) +
  geom_smooth(data = ARD_comp_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = fish.size,
                  group = fish.size),
              size = 2,
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~.)+
  ylim(0,30)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean fish recruit density over time")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )



#colplots

ARD_3_comp_sumcol <- ARD_3 %>% 
  group_by(complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 3)

ARD_5_comp_sumcol <- ARD_5 %>% 
  group_by(complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 5)

ggplot(data = ARD_3_comp_sumcol) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = complexity)) +
  geom_errorbar(aes(x = complexity,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd,
                    group = complexity),
                width = 0.2)+
  scale_fill_manual(values = pal) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 3cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

ggplot(data = ARD_5_comp_sumcol) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill =complexity)) +
  geom_errorbar(aes(x = complexity,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd,
                    group = complexity),
                width = 0.2)+
  scale_fill_manual(values = pal) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 5cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


# treat and comp faceted --------------------------------------------------

ARD_3_treatcomp_sum <- ARD_3 %>% 
  group_by(complexity, treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 3)

ARD_5_treatcomp_sum <- ARD_5 %>% 
  group_by(complexity,treatment, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 5)

ARD_treatcomp_sum <- rbind(ARD_3_treatcomp_sum,ARD_5_treatcomp_sum)
fix(ARD_treatcomp_sum) #make sure fish.size is character
# ARD_comp_sum <- ARD_comp_sum %>% 
#   mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control")))

#plot from joined df (rbind) to position dodge points - new code to be able to position dodge
ggplot() +
  geom_jitter(data = ARD_3,
              aes(x = visit,
                  y = density),
              alpha = 0.05,
              colour = "coral") +
  geom_jitter(data = ARD_5,
              aes(x = visit,
                  y = density),
              alpha = 0.05,
              colour = "aquamarine") +
  geom_line(data = ARD_treatcomp_sum,
            aes(x = visit,
                y = density.mean,
                colour = fish.size,
                group = fish.size),
            size = 1,
            alpha = 0.3) +
  geom_errorbar(data = ARD_treatcomp_sum,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = fish.size,
                    group = fish.size), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_treatcomp_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = fish.size,
                 group = fish.size),
             position = position_dodge(0.2),
             size = 3,
             alpha = 0.2) +
  geom_smooth(data = ARD_treatcomp_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = fish.size,
                  group = fish.size),
              size = 2,
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = c("coral3" ,"aquamarine3"), name = "fish size (cm)")+
  facet_grid(complexity~treatment)+
  ylim(0,30)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean fish recruit density over time")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )


#col plots

ARD_3_treatcomp_sumcol <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(3812)) %>% 
  mutate(fish.size = 3)

ARD_5_treatcomp_sumcol <- ARD_5 %>% 
  mutate(treatment = factor(treatment, levels = c("control","0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity,treatment) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(density.se = density.sd/sqrt(7482)) %>% 
  mutate(fish.size = 5)


ggplot(data = ARD_3_treatcomp_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2)+
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Density~(fish~m^{2}))) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

ggplot(data = ARD_5_treatcomp_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.se,
                    ymax = density.mean + density.se,
                    group = treatment),
                width = 0.2)+
  facet_grid(.~complexity) +
  labs(x = expression(Treatment),
       y = expression(Density~(fish~m^{2}))) +
  ggtitle("mean density (fish under 5cm)") +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


#plot for methods paper (use scale_fill_viridis)
ggplot(data = ARD_3_treatcomp_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd,
                    group = treatment),
                width = 0.2)+
  facet_grid(.~complexity) +
  scale_fill_viridis(discrete = TRUE)+
  labs(x = expression(Treatment),
       y = expression(Density~(fish~m^{2}))) +
  # ggtitle("mean density (fish under 3cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.001),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )

# joining traits ----------------------------------------------------------

traits_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/traits_lookup.csv")

ARD_3_species <- ARD_3 %>% 
  distinct(common_name)

ARD_trait <- semi_join(traits_lookup, ARD_3_species)
#changed rose blenny to rosy blenny, changed "Blue Tang, Blue Doctorfish to Blue Tang"
#added Goby sp. unid., Blenny Family, Scarid - unidentified, Grunt. unid. Species

#still missing a few, export and compare
write.csv(ARD_trait, "ARD_traits.csv")
write.csv(ARD_3_species, "ARD_3_species.csv")

#changed spelling of Roughhead to Roughead Blenny, now it matches up

ARD_3_traits <- full_join(ARD_3,ARD_trait)
#this is the most complete form of ARD_3 now, write as csv and save
write.csv(ARD_3_traits, "ARD_3n.csv"), #ARD_3 in ARD recruit folder is this new version May 19, 2020

# Damselfish presence -----------------------------------------------------

#add Damselfish presence as a variable to ARD_7 

#for some reason this worked for ARD_3 so yay. 
ARD_3 <- ARD_3 %>%
  mutate(DamselfishScore = ifelse(Genus == c("Stegastes","Microspathadon"),1,0))
ARD_3 <- ARD_3 %>%
  mutate(DamselfishPresence = ifelse(DamselfishScore == 1, "present","absent"))

# not working did it right in excel for ARD_7
ARD_7 <- read_csv("data/Art Recruit data/ARD_7.csv")
View(ARD_7)

#1) isolate all plot_grids with damselfish 7cm and under in visit 1
ARD_dampres_tojoin <- ARD_7 %>% 
  filter(visit == c(1,2,3,4,5), DamselfishScore == 1) %>% 
  select(plot_grid) %>% #selects only site_grid var with damselfish present
  distinct(plot_grid) #removes duplicates

#2) only keep observations for those plot_grids by joining with ARD_3

ARD_dampres <- inner_join(ARD_3, ARD_dampres_tojoin)
view(ARD_dampres)

#3) filter data to make 1df with only D present, and the other with all other recruits

ARD_only_D <- ARD_dampres %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>%
  filter(DamselfishScore == 1)

ARD_NO_D <- ARD_dampres %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>%
  filter(DamselfishScore ==0)

#4) Sum stats for both - with treatment and complexity

ARD_only_Dsum <- ARD_only_D %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(Damselfish = "only Damselfish recruits")

ARD_NO_Dsum <- ARD_NO_D %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(Damselfish = "all other recruits")

ARD_dam_sum <- rbind(ARD_only_Dsum,ARD_NO_Dsum)
fix(ARD_dam_sum)

#line plot
ggplot() +
  geom_jitter(data = ARD_NO_D,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "blue") +
  geom_jitter(data = ARD_only_D,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "darkorange") +
  geom_line(data = ARD_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = Damselfish,
                group = Damselfish),
            size = 3,
            alpha = 0.5) +
  geom_errorbar(data = ARD_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = Damselfish,
                    group = Damselfish), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = Damselfish,
                 group = Damselfish),
             position = position_dodge(0.2),
             size = 3) +
  # geom_smooth(data =ARD_dam_sum,
  #             aes(x = visit,
  #                 y = density.mean,
  #                 colour = Damselfish,
  #                 group = Damselfish),
  #             size = 2,
  #             method = lm,
  #             se = FALSE ) +
  scale_colour_manual(values = c("blue3" ,"orange2"), 
                      name = "Damselfish",
                      labels = c("all other recruits", "only Damselfish recruits"))+
  facet_grid(complexity~treatment)+
  ylim(0,18)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean Damselfish density over time - priority effects")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

#it would appear that this does not show any trend, even when filtered for damselfsih under 7cm in first 1, 3, and 5 visits


ARD_dams_sumcol <- ARD_3 %>% 
  group_by(treatment,complexity, DamselfishPresence) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(DamselfishPresence)

fix(ARD_dams_sumcol)

ggplot(data = ARD_dams_sumcol) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = DamselfishPresence),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = DamselfishPresence,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = c("blue3", "darkorange"), name = "Damselfish") +
  facet_grid(.~treatment) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density with Damselfish presence \nand absence (3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )



# Try looking at just 0% and 100% treatments for Damselfish presence --------
#still have to do this

# 1) isolate all plot_grids with damselfish 7cm and under in visit 1
ARD_dampres_tojoin <- ARD_7 %>% 
  filter(visit == 1, DamselfishScore == 1) %>% 
  select(plot_grid) %>% #selects only site_grid var with damselfish present
  distinct(plot_grid) #removes duplicates

#2) only keep observations for those plot_grids by joining with ARD_3

ARD_dampres <- inner_join(ARD_3, ARD_dampres_tojoin)

#3) filter data to make 1df with only D present, and the other with all other recruits

ARD_only_D <- ARD_dampres %>% 
  filter(DamselfishScore == 1)

ARD_NO_D <- ARD_dampres %>% 
  filter(DamselfishScore ==0)


ARD_only_Dsum2 <- ARD_only_D %>% 
  filter(treatment == "100%") %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(Damselfish = "present")

ARD_NO_Dsum2 <- ARD_NO_D %>% 
  filter(treatment == c("100%")) %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>%
  mutate(Damselfish = "absent")

ARD_dam_sum2 <- rbind(ARD_only_Dsum,ARD_NO_Dsum)
fix(ARD_dam_sum)

#line plot
ggplot() +
  geom_jitter(data = ARD_NO_D,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "blue") +
  geom_jitter(data = ARD_only_D,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "darkorange") +
  geom_line(data = ARD_dam_sum,
            aes(x = visit,
                y = density.mean,
                colour = Damselfish,
                group = Damselfish),
            size = 1,
            alpha = 0.5) +
  geom_errorbar(data = ARD_dam_sum,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = Damselfish,
                    group = Damselfish), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_dam_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = Damselfish,
                 group = Damselfish),
             position = position_dodge(0.2),
             size = 3) +
  geom_smooth(data =ARD_dam_sum,
              aes(x = visit,
                  y = density.mean,
                  colour = Damselfish,
                  group = Damselfish),
              size = 2,
              method = lm,
              se = FALSE ) +
  scale_colour_manual(values = c("blue3" ,"orange2"), 
                      name = "Damselfish",
                      labels = c("absent", "present"))+
  facet_grid(treatment~.)+
  ylim(0,18)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean Damselfish density over time - priority effects")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

ARD_dams_sumcol <- ARD_3 %>% 
  group_by(treatment,complexity, DamselfishPresence) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(DamselfishPresence)

fix(ARD_dams_sumcol)

ggplot(data = ARD_dams_sumcol) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = DamselfishPresence),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = DamselfishPresence,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = c("blue3", "darkorange"), name = "Damselfish") +
  facet_grid(.~treatment) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density with Damselfish presence \nand absence (3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )


# Trophic Group -----------------------------------------------------------

#fish under 5
ARD_func_comp_5 <- ARD_5 %>% 
  group_by(complexity,  MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(MajorTrophicGroup)
# fix(ARD_func_5)

ggplot(data = ARD_func_comp_5) +
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
  facet_grid(.~MajorTrophicGroup) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Trophic Group (<= 5cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_blank(),
    strip.background = element_blank()
  )

ARD_func_treat_5 <- ARD_5 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  group_by(treatment,  MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(MajorTrophicGroup)
# fix(ARD_func_5)

ggplot(data = ARD_func_treat_5) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = MajorTrophicGroup),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    fill = MajorTrophicGroup,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group") +
  facet_grid(.~MajorTrophicGroup) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Trophic Group (<=5cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_blank(),
    strip.background = element_blank()
  )


#fish under 3cm all time_blocks

ARD_func_treat_3 <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  group_by(treatment,  MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(MajorTrophicGroup)
# fix(ARD_func_5)

ggplot(data = ARD_func_treat_3) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = MajorTrophicGroup),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    fill = MajorTrophicGroup,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group") +
  facet_grid(.~MajorTrophicGroup) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Trophic Group (<= 3cm)")+
  ylim(0,15)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 15, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_blank(),
    strip.background = element_blank()
  )

ARD_func_comp_3 <- ARD_3 %>% 
  group_by(complexity, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(MajorTrophicGroup)
# fix(ARD_func_comp_3) #add dummy carnivore value

ggplot(data = ARD_func_comp_3) +
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
  facet_grid(.~MajorTrophicGroup) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Trophic Group (<= 3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_blank(),
    strip.background = element_blank()
  )

#treatment and complexity
ARD_func_comp_treat_3 <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  group_by(complexity, treatment, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(MajorTrophicGroup)

ggplot(data = ARD_func_comp_treat_3) +
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
  facet_grid(treatment~MajorTrophicGroup) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Trophic Group (<= 3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text.x = element_blank(),
    strip.text = element_text(size = 12),
    strip.background.x = element_blank(),
    strip.background = element_rect(fill = "grey95")
  )

ggplot(data = ARD_func_comp_treat_3) +
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
  facet_grid(MajorTrophicGroup~treatment) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Trophic Group (<= 3cm)")+
  ylim(0,17)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    # strip.text.x = element_blank(),
    strip.text = element_text(size = 12),
    # strip.background.x = element_blank(),
    strip.background = element_rect(fill = "grey95")
  )

#line plot of trophic groups for fish under 3 over time - just start
ARD_func_sum3_start <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(time_block == "start") %>% 
  group_by(treatment,complexity, visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_3_start <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
 filter(time_block == "start")

ggplot() +
  geom_jitter(data = ARD_3_start,
              aes(x = visit,
                  y = density,
                  colour = MajorTrophicGroup),
              alpha = 0.2) +
  geom_line(data = ARD_func_sum3_start,
            aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup),
            size = 2,
            alpha = 0.8) +
  geom_errorbar(data = ARD_func_sum3_start,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = MajorTrophicGroup), 
                width = 0.75,
                size = 0.5, 
                position = position_dodge(0.1)) +
  geom_point(data = ARD_func_sum3_start,
             aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup),
             size = 2) +
  scale_colour_viridis(discrete = TRUE, name = "Trophic Group") +
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


#line plot of trophic groups for fish under 3 over time - start and middle
ARD_func_sum3_startmid <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(time_block == c("start","middle")) %>% 
  group_by(treatment,complexity, visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_3_startmid <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(time_block == c("start","middle"))

ggplot() +
  geom_jitter(data = ARD_3_startmid,
              aes(x = visit,
                  y = density,
                  colour = MajorTrophicGroup),
              alpha = 0.2) +
  geom_line(data = ARD_func_sum3_startmid,
            aes(x = visit,
                y = density.mean,
                group = MajorTrophicGroup,
                colour = MajorTrophicGroup),
            size = 2,
            alpha = 0.8) +
  geom_errorbar(data = ARD_func_sum3_startmid,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = MajorTrophicGroup), 
                width = 0.75,
                size = 0.5, 
                position = position_dodge(0.1)) +
  geom_point(data = ARD_func_sum3_startmid,
             aes(x = visit, 
                 y = density.mean,
                 colour = MajorTrophicGroup),
             size = 2) +
  scale_colour_viridis(discrete = TRUE, name = "Trophic Group") +
  facet_grid(complexity~treatment) +
  # ylim(0,15)+
  ylab("mean fish density (fish/m^2)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )


# Trophic Group playing with animation ------------------------------------

install.packages("gganimate")
library(gganimate)
install.packages("hrbrthemes")
library(hrbrthemes)


ARD_func_sum3 <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(time_block == "start") %>% 
  group_by(treatment,complexity, visit, MajorTrophicGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))

troph_anim <- ARD_func_sum3 %>% 
  ggplot(aes(x = visit,
             y = density.mean,
             group = MajorTrophicGroup,
             colour = MajorTrophicGroup))+
  geom_line(size = 1.5,
            alpha = 0.8)+
  geom_point(size = 2)+
  scale_color_viridis(discrete = TRUE, name = "Trophic Group") +
  ggtitle("Trophic Group Density over Time") +
  facet_grid(complexity~treatment) +
  ylab("mean fish density(fish/0.79m^2") +
  # theme_ipsum_pub()+
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  transition_reveal(visit)

animate(troph_anim, height = 780, width = 1135)
anim_save("troph_anim.gif", animation = last_animation())

print(troph_anim)
  
anim_save("trophic_test.gif", animation = last_animation(), path = NULL)
# anim_save("trophic_test.gif", anim)


ARD_func_sum3 %>% 
  ggplot(aes(x = visit,
             y = density.mean,
             group = MajorTrophicGroup,
             colour = MajorTrophicGroup))+
  geom_line(size = 1.5,
            alpha = 0.8)+
  geom_point(size = 2)+
  scale_color_viridis(discrete = TRUE, name = "Trophic Group") +
  ggtitle("Trophic Group Density over Time") +
  facet_grid(complexity~treatment) +
  ylab("mean fish density(fish/0.79m^2") +
  # theme_ipsum_pub()+
  theme(
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 11)
  ) +
  transition_reveal(visit)

anim_save("trophic_test.gif", animation = last_animation(), path = NULL)

# Family ------------------------------------------------------------------


ARD_fam_comp_5 <- ARD_5 %>% 
  group_by(complexity, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
  # drop_na(Family)

fix(ARD_fam_5)

ggplot(data = ARD_fam_comp_5) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = Family,
               group = Family),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    group = Family,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.5),
                width = 0.2)+
  scale_fill_manual(values = pal4, name = "Family") +
  facet_grid(.~Family) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Family (<=5cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_blank(),
    strip.background = element_blank()
  )


#by treatment

ARD_fam_treat_5 <- ARD_5 %>% 
  group_by(treatment, Family) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
# drop_na(Family)

fix(ARD_fam_5)

ggplot(data = ARD_fam_treat_5) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = Family,
               group = Family),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    group = Family,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.5),
                width = 0.2)+
  scale_fill_manual(values = pal4, name = "Family") +
  facet_grid(.~Family) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per Family (<=5cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_blank(),
    strip.background = element_blank()
  )



# Max Length --------------------------------------------------------------

# fish under 5cm ----------------------------------------------------------


#fish under 5
unique(ARD_5$MaxLength)

ARD_5_MaxL <- ARD_5 %>% 
  mutate(MaxLengthGroup = case_when(MaxLength >= 1 & MaxLength <= 10 ~ '1-10',
                                    MaxLength >= 11 & MaxLength <= 20 ~ '11-20',
                                    MaxLength >= 21 & MaxLength <= 30 ~ '21-30',
                                    MaxLength >= 31 & MaxLength <= 40 ~ '31-40',
                                    MaxLength >= 41 & MaxLength <= 50 ~ '41-50',
                                    MaxLength >= 51 & MaxLength <= 120 ~ '>50',))

#col plot - treatment
ARD_5_MaxL_treat_col <- ARD_5_MaxL %>% 
  mutate(MaxLengthGroup = factor(MaxLengthGroup,
                                 levels = c("1-10", "11-20", "21-30", "31-40", "41-50", ">50"))) %>% 
  group_by(treatment, MaxLengthGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(MaxLengthGroup)

ggplot(data = ARD_5_MaxL_treat_col) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = MaxLengthGroup),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    fill = MaxLengthGroup,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = pal5, name = "Max. Adult \nLength (cm)") +
  facet_grid(.~MaxLengthGroup) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per max adult length (5cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_blank(),
    strip.background = element_blank()
  )

#col plot - complexity
ARD_5_MaxL_comp_col <- ARD_5_MaxL %>% 
  mutate(MaxLengthGroup = factor(MaxLengthGroup,
                                 levels = c("1-10", "11-20", "21-30", "31-40", "41-50", ">50"))) %>% 
  group_by(complexity, MaxLengthGroup) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(MaxLengthGroup)

ggplot(data = ARD_5_MaxL_comp_col) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = MaxLengthGroup),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = MaxLengthGroup,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = pal5, name = "Max. Adult \nLength (cm)") +
  facet_grid(.~MaxLengthGroup) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per max adult length (5cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12, angle = -60, hjust = 0.01),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_blank(),
    strip.background = element_blank()
  )



# Water col position ------------------------------------------------------

ARD_demer_3 <- ARD_3 %>%
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(position_score == 1) #demersal
ARD_benth_3 <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(position_score == 0) #benthic

ARD_demer_3_sum <- ARD_demer_3 %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(position = "demersal")

ARD_benth_3_sum <- ARD_benth_3 %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(position = "benthic")

ARD_position_sum <- rbind(ARD_demer_3_sum,ARD_benth_3_sum)

ARD_position <- ARD_position_sum %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control")))
fix(ARD_position_sum) #plot then see if any 0s need to be added

#new code to jitter error bars

ggplot() +
  geom_jitter(data = ARD_demer_3,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "cyan") +
  geom_jitter(data = ARD_benth_3,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "darkgoldenrod1") +
  geom_line(data = ARD_position_sum,
            aes(x = visit,
                y = density.mean,
                colour = position,
                group = position),
            size = 2,
            alpha = 0.8) +
  geom_errorbar(data = ARD_position_sum,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = position,
                    group = position), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_position_sum,
             aes(x = visit, 
                 y = density.mean,
                 colour = position,
                 group = position),
             position = position_dodge(0.2),
             size = 3) +
  scale_colour_manual(values = c("darkgoldenrod3", "cyan3"), 
                      name = "position",
                      labels = c("benthic", "demersal"))+
  facet_grid(complexity~treatment)+
  ylim(0,20)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean fish recruit density over time - trait: water column position")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

ARD_position_sumcol <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  group_by(treatment, complexity, position) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(position)

ggplot(data = ARD_position_sumcol) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = position),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = position,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = c("darkgoldenrod3", "cyan3")) +
  facet_grid(.~treatment) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per water \ncolumn position (3cm)")+
  # ylim(0,20)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    # legend.position = "none",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )


#this col plot shows trend better
ggplot(data = ARD_position_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = position),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    fill = position,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = c("darkgoldenrod3", "cyan3")) +
  facet_grid(.~complexity) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per water \ncolumn position (3cm)")+
  # ylim(0,20)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    # legend.position = "none",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )


# just start --------------------------------------------------------------

ARD_demer_3start <- ARD_3 %>%
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(position_score == 1) %>%  #demersal
  filter(time_block =="start") 
ARD_benth_3start <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(position_score == 0) %>%  #benthic
filter(time_block =="start")

ARD_demer_3_sumstart <- ARD_demer_3start %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(position = "demersal")

ARD_benth_3_sumstart <- ARD_benth_3start %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(position = "benthic")

ARD_position_sumstart <- rbind(ARD_demer_3_sumstart,ARD_benth_3_sumstart)

ARD_position <- ARD_position_sumstart %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control")))
fix(ARD_position_sum) #plot then see if any 0s need to be added

#new code to jitter error bars

ggplot() +
  geom_jitter(data = ARD_demer_3start,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "cyan") +
  geom_jitter(data = ARD_benth_3start,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "darkgoldenrod1") +
  geom_line(data = ARD_position_sumstart,
            aes(x = visit,
                y = density.mean,
                colour = position,
                group = position),
            size = 2,
            alpha = 0.8) +
  geom_errorbar(data = ARD_position_sumstart,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = position,
                    group = position), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_position_sumstart,
             aes(x = visit, 
                 y = density.mean,
                 colour = position,
                 group = position),
             position = position_dodge(0.2),
             size = 3) +
  scale_colour_manual(values = c("darkgoldenrod3", "cyan3"), 
                      name = "position",
                      labels = c("benthic", "demersal"))+
  facet_grid(complexity~treatment)+
  ylim(0,20)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean fish recruit density over time - trait: water column position")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )


# Behaviour ---------------------------------------------------------------

ARD_sol_3 <- ARD_3 %>%
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(behavior == "solitary") 
ARD_shoal_3 <- ARD_3 %>%
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(behavior == "shoaling") 
ARD_school_3 <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(behavior == "schooling") 

ARD_sol_3_sum <- ARD_sol_3 %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(behaviour = "solitary")

ARD_shoal_3_sum <- ARD_shoal_3 %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))%>% 
  mutate(behaviour = "shoaling")

ARD_school_3_sum <- ARD_school_3 %>% 
  group_by(treatment,complexity, visit) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))%>% 
  mutate(behaviour = "schooling")

fix(SFD_school_3_sum)
#if i need to add 0 values

ARD_behaviour_sum <- rbind(ARD_sol_3_sum,ARD_shoal_3_sum,ARD_school_3_sum)

fix(ARD_behaviour_sum)
#there's  a lot of 0 values to be added, so going to export and do so in excel
write.csv(ARD_behaviour_sum, "ARD_behaviour_sum_original.csv") 

ARD_behaviour_sum_with0s <- read_csv("Artificial coral experiment/data/Art Recruit data/ARD_behaviour_sum_with0s.csv")
#remember to add mutate levels after re-importing
ARD_behaviour <- ARD_behaviour_sum_with0s %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control")))

#new code to jitter error bars

ggplot() +
  geom_jitter(data = ARD_sol_3,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "slateblue") +
  geom_jitter(data = ARD_shoal_3,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "springgreen") +
  geom_jitter(data = ARD_school_3,
              aes(x = visit,
                  y = density),
              alpha = 0.3,
              colour = "gold") +
  geom_line(data = ARD_behaviour,
            aes(x = visit,
                y = density.mean,
                colour = behaviour,
                group = behaviour),
            size = 2,
            alpha = 0.8) +
  geom_errorbar(data = ARD_behaviour,
                aes(x = visit, 
                    ymin = density.mean - density.sd, 
                    ymax = density.mean + density.sd,
                    colour = behaviour,
                    group = behaviour), 
                position = position_dodge(0.2),
                width = 0.3,
                size = 0.5) +
  geom_point(data = ARD_behaviour,
             aes(x = visit, 
                 y = density.mean,
                 colour =behaviour,
                 group = behaviour),
             position = position_dodge(0.2),
             size = 3) +
  scale_colour_manual(values = c("gold3" ,"springgreen3","slateblue3"), 
                      name = "Behaviour",
                      labels = c("Schooling", "Shoaling", "Solitary"))+
  facet_grid(complexity~treatment)+
  ylim(0,20)+
  ylab("Density (fish/m^2)")+
  xlab("Visit")+
  ggtitle("mean fish recruit density over time - trait: schooling behaviour")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 16)
  )

ARD_behavior_sumcol <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  group_by(treatment, complexity, behavior) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  drop_na(behavior)

fix(ARD_behavior_sumcol) #add 0s
# High: 0, 30, 50, 100 - 0 schooling

ggplot(data = ARD_behavior_sumcol) +
  geom_col(aes(x = complexity,
               y = density.mean,
               fill = behavior),
           position = "dodge") +
  geom_errorbar(aes(x = complexity,
                    fill = behavior,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = c("gold3", "springgreen3", "slateblue3"), name = "Behaviour") +
  facet_grid(.~treatment) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per aggregating behaviour (3cm)")+
  # ylim(0,40)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    # legend.position = "none",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )


#this col plot shows trend better
ggplot(data = ARD_behavior_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = behavior),
           position = "dodge") +
  geom_errorbar(aes(x = treatment,
                    fill = behavior,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd),
                position = position_dodge(0.9),
                width = 0.2)+
  scale_fill_manual(values = c("gold3", "springgreen3", "slateblue3"), name = "Behaviour") +
  facet_grid(.~complexity) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density per \naggregating behaviour (3cm)")+
  # ylim(0,20)+
  theme_classic()+
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    # legend.position = "none",
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  )


# coral genotypes ---------------------------------------------------------------

ARD_3_geno_sumcol <- ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  group_by(complexity, treatment, genotype) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 3)

ARD_5_geno_sumcol <- ARD_5 %>% 
  group_by(complexity,treatment, genotype) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density)) %>% 
  mutate(fish.size = 5)


ggplot(data = ARD_3_geno_sumcol) +
  geom_col(aes(x = treatment,
               y = density.mean,
               fill = treatment)) +
  geom_errorbar(aes(x = treatment,
                    ymin = density.mean - density.sd,
                    ymax = density.mean + density.sd,
                    group = treatment),
                width = 0.2)+
  facet_grid(genotype~complexity) +
  ylab("mean fish density (fish/m^2)")+
  ggtitle("mean density (fish under 3cm)")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 16)
  )


#just genotype fish under 3
ARD_3_geno_sumcol <- ARD_3 %>% 
  group_by(genotype, complexity) %>% 
  summarize(density.mean = mean(density), density.sd = sd(density))
 
  ggplot(data = ARD_3_geno_sumcol) +
    geom_col(aes(x = genotype,
                 y = density.mean)) +
    facet_grid(.~complexity)
    
    
    geom_errorbar(aes(x = treatment,
                      ymin = density.mean - density.sd,
                      ymax = density.mean + density.sd,
                      group = treatment),
                  width = 0.2)+
    facet_grid(.~complexity) +
    ylab("mean fish density (fish/m^2)")+
    ggtitle("mean density (fish under 5cm)")+
    theme_classic()+
    theme(
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
      legend.position = "none",
      strip.text = element_text(size = 16)
    )
    
    
    
  