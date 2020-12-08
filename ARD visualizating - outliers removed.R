## ARD Visualizations ##

Using code from R-file ARD pre-visualizing workflow, but using data sets that have outliers removed (that is also in the same R-file )
#I will now use dfs without outliers for my visuals to see if there's a dif over time
# will bin by visit to have 4 distince time "points" --> basline, start, mid, end



library(GGally)
library(ggplot2)
library(tidyverse)
library(readr)

ARD_final <- read_csv("data/Art Recruit data/final clean data frames/ARD_final.csv")
View(ARD_final)
ARD_recruit <- read_csv("data/Art Recruit data/final clean data frames/ARD_recruit.csv")
View(ARD_recruit)
ARD_baseline <- read_csv("data/Art Recruit data/final clean data frames/ARD_baseline.csv")
View(ARD_baseline)



# binning data by time (visit) and size class ----------------------------------------------------

unique(ARD_recruit$visit)
#ranges from 2-19, could do 3 even sections with 6 visits in each at 7 and 13

ARD_recruit_start_5 <- ARD_recruit %>% 
  filter(visit %in% c(2, 3, 4, 5,6, 7)) %>% 
  filter(TL < 6)

ARD_recruit_mid_5 <- ARD_recruit %>% 
  filter(visit %in% c(8, 9, 10, 11, 12, 13)) %>% 
  filter(TL < 6)

ARD_recruit_end_5 <- ARD_recruit %>% 
  filter(visit %in% c(14, 15, 16, 17, 18, 19)) %>% 
  filter(TL <6)

ARD_baseline_5 <- ARD_baseline %>% 
  filter(TL < 6)

# ggpairs -----------------------------------------------------------------

#filter for just the cols of interest for historograms:
ARD_baseline_norm <- ARD_baseline %>% 
  select(density, biomass, TL, MajorTrophicGroup)
ARD_recruit_start_norm <- ARD_recruit_start %>% 
  select(density, biomass, TL, MajorTrophicGroup)
ARD_recruit_mid_norm <- ARD_recruit_mid %>% 
  select(density, biomass, TL, MajorTrophicGroup)
ARD_recruit_end_norm <- ARD_recruit_end %>% 
  select(density, biomass, TL, MajorTrophicGroup)

ggpairs(ARD_baseline_norm, aes(col = MajorTrophicGroup, alpha = 0.4))
ggpairs(ARD_recruit_start_norm, aes(col = MajorTrophicGroup, alpha = 0.4))
ggpairs(ARD_recruit_mid_norm, aes(col = MajorTrophicGroup, alpha = 0.4))
ggpairs(ARD_recruit_end_norm, aes(col = MajorTrophicGroup, alpha = 0.4))



# boxplots all fish for treatment and complexity----------------------------------------------------------------

ggplot(data = ARD_baseline, 
       aes(x = treatment,
           y = density,
           colour = treatment)) +
  geom_boxplot(varwidth = T) +
  facet_grid(complexity~.)+
  ggtitle("baseline recruit density by treatment and complexity")

ggplot(data = ARD_recruit_start, 
       aes(x = treatment,
           y = density,
           colour = treatment)) +
  geom_boxplot(varwidth = T) +
  facet_grid(complexity~.)+
  ggtitle("start baseline recruit density by treatment and complexity")

ggplot(data = ARD_recruit_mid, 
       aes(x = treatment,
           y = density,
           colour = treatment)) +
  geom_boxplot(varwidth = T) +
  facet_grid(complexity~.)+
  ggtitle("mid recruit density by treatment and complexity")

ggplot(data = ARD_recruit_end, 
       aes(x = treatment,
           y = density,
           colour = treatment)) +
  geom_boxplot(varwidth = T) +
  facet_grid(complexity~.)+
  ggtitle("end recruit density by treatment and complexity")

# ggplot()+
#   geom_boxplot(data = ARD_baseline,
#                aes(x = treatment,
#                y = density),
#                position = 0.5)+
#   geom_boxplot(data = ARD_recruit_start,
#                aes(x = treatment,
#                    y = density),
#                position = 1)



# summarized mean densities -----------------------------------------------



# geom_col all fish for trophic groups -------------------------------------------------------------------

ggplot(data = ARD_baseline,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Baseline Density of major trophic groups per complexity and treatment") #just faceted opposite than code below

ggplot(data = ARD_recruit_start,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Starting Recruit Density of major trophic groups per complexity and treatment") #just faceted opposite than code below

ggplot(data = ARD_recruit_mid,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Mid Recruit Density of major trophic groups per complexity and treatment") #just faceted opposite than code below

ggplot(data = ARD_recruit_end,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "End Recruit Density of major trophic groups per complexity and treatment") #just faceted opposite than code below

# geom_col fish < 5cm for trophic groups ----------------------------------


ggplot(data = ARD_baseline_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Baseline Density of major trophic groups per complexity and treatment (fish < 5cm)") #just faceted opposite than code below

ggplot(data = ARD_recruit_start_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Starting Recruit Density of major trophic groups per complexity and treatment (fish < 5cm)") #just faceted opposite than code below

ggplot(data = ARD_recruit_mid_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "Mid Recruit Density of major trophic groups per complexity and treatment (fish < 5cm)") #just faceted opposite than code below

ggplot(data = ARD_recruit_end_5,
       aes(x = MajorTrophicGroup,
           y = density, 
           fill = MajorTrophicGroup)) +
  geom_col() +
  facet_grid(complexity ~ treatment) +
  theme_dark()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(title = "End Recruit Density of major trophic groups per complexity and treatment (fish < 5cm)") #just faceted opposite than code below