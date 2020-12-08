#Richness, Rarefaction and Diversity (accumulation curves)


# packages and palletes ---------------------------------------------------

library(readr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(PNWColors)
library(dplyr)
library(plyr)
library(openxlsx)
library(vegan)


# df files ----------------------------------------------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))
ARD_3_with_0$treatment_complexity <-paste(ARD_3_with_0$treatment, "-",ARD_3_with_0$complexity)


ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3 (No HN day 4)/ARD_3_with_0_new_2.csv") %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                          labels = c("control", "0%", "30%", "50%", "70%", "100%")))

# Overall richness for each treatment/comp --------------------------------

#using tutorial from:https://www.flutterbys.com.au/stats/tut/tut13.2.html

#export to create spp presence matrix. 

ARD_3_rich_pivot <- ARD_3_with_0 %>%
  select(treatment, complexity, treatment_complexity, common_name, presence) 

write.csv(ARD_3_rich_pivot, "ARD_3_rich_pivot.csv")

ARD_3_rich_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/ARD_3_rich_matrix.csv")

ddply(ARD_3_rich_matrix,~treatment,function(x) {data.frame(RICHNESS=sum(x[-1]>0))})
#overall low complexity sites are more species rich, copy and paste to new excel sheet, did Text to Col in Data to separate into seprate cols

ARD_3_richness <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/ARD_3_richness.csv")
ARD_3_richness <- ARD_3_richness %>% 
mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))
       
ggplot(data = ARD_3_richness) +
  geom_col(aes(x = treatment,
               y = RICHNESS,
               fill = treatment)) +
  facet_grid(~complexity) +
  ggtitle("Species Richness") +
  labs(x = expression(Treatment),
       y = expression(Number~of~Species)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

# unique(ARD_3_with_0_new_2$days_since_outplanting)
# unique(ARD_3_with_0$visit)

# rarefaction ARD_3 -------------------------------------------------------

ARD_3_rich_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/ARD_3_rich_matrix.csv")

ddply(ARD_3_rich_matrix,~treatment,function(x) {data.frame(RAREFY=rarefy(x[-1], sample=10, MARGIN=1))})
 
#oh wait, this is for when total abundance inbalances are due to sampling diferences
#but we had the same sampling diffrences so doesn't apply to this data set


# diversity --------------------------------------------------------------

#takes into account richness  and dominance/eveness

#1) shannon-wiener
# values range from o-5, typically from 1.5-3.5

ARD_3_rich_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/ARD_3_rich_matrix.csv")
 
# > library(plyr)
ddply(ARD_3_rich_matrix,~treatment,function(x) {data.frame(SHANNON=diversity(x[-1], index="shannon"))})
#copied and pested to new csv, called ARD_3_shannon

ARD_3_shannon <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/diversity/ARD_3_shannon.csv")
ARD_3_shannon <- ARD_3_shannon %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

ggplot(data = ARD_3_shannon) +
  geom_col(aes(x = treatment,
               y = SHANNON,
               fill = treatment)) +
  facet_grid(~complexity) +
  ggtitle("shannon-wiener Index of Species Diversity") +
  labs(x = expression(Treatment),
       y = expression(H~value)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

#2) simpson's Index (eveness)

# more of a measure of dominance, weights towards most abundant taxa
# prob 2 individuals drawn at random from com will be dif spp.  (so ranges from 0-1)
# higher values represent higher diversity
# less sensisitve to rare spp than simpson-weiner

ddply(ARD_3_rich_matrix,~treatment,function(x) {data.frame(SIMPSON=diversity(x[-1], index="simpson"))})

ARD_3_simpson <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/diversity/ARD_3_simpson.csv")
ARD_3_simpson <- ARD_3_simpson %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

ggplot(data = ARD_3_simpson) +
  geom_col(aes(x = treatment,
               y = SIMPSON,
               fill = treatment)) +
  facet_grid(~complexity) +
  ggtitle("simpson Index of Species Diversity") +
  labs(x = expression(Treatment),
       y = expression(D~value)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )


# diversity no bhw --------------------------------------------------------

#takes into account richness  and dominance/eveness

#1) shannon-wiener
# values range from o-5, typically from 1.5-3.5

ARD_3_rich_matrix_nobhw <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/No bhw/ARD_3_rich_matrix_nobhw.csv")

# > library(plyr)
ddply(ARD_3_rich_matrix_nobhw,~treatment,function(x) {data.frame(SHANNON=diversity(x[-1], index="shannon"))})
#copied and pested to new csv, called ARD_3_shannon

ARD_3_shannon_nobhw <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/diversity/ARD_3_shannon_nobhw.csv")
ARD_3_shannon_nobhw <- ARD_3_shannon_nobhw %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

ggplot(data = ARD_3_shannon_nobhw) +
  geom_col(aes(x = treatment,
               y = SHANNON,
               fill = treatment)) +
  facet_grid(~complexity) +
  ggtitle("shannon-wiener Index of Species Diversity - no bhw") +
  labs(x = expression(Treatment),
       y = expression(D~value)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

#2) simpson's Index (eveness)

# more of a measure of dominance, weights towards most abundant taxa
# prob 2 individuals drawn at random from com will be dif spp.  (so ranges from 0-1)
# higher values represent higher diversity
# less sensisitve to rare spp than simpson-weiner

ddply(ARD_3_rich_matrix_nobhw,~treatment,function(x) {data.frame(SIMPSON=diversity(x[-1], index="simpson"))})

ARD_3_simpson_nobhw <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/all visits/diversity/ARD_3_simpson_nobhw.csv")
ARD_3_simpson_nobhw <- ARD_3_simpson_nobhw %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

ggplot(data = ARD_3_simpson_nobhw) +
  geom_col(aes(x = treatment,
               y = SIMPSON,
               fill = treatment)) +
  facet_grid(~complexity) +
  ggtitle("simpson Index of Species Diversity - no bhw") +
  labs(x = expression(Treatment),
       y = expression(H~value)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )


# diversity - by time_block -----------------------------------------------

ARD_3_with_0$treatment_complexity_time <- paste(ARD_3_with_0$treatment_complexity, "-",ARD_3_with_0$time_block)
#need to make a col with all crossed factors together (so treatment, complexity and time_block)

ARD_3_rich_pivot_time <- ARD_3_with_0 %>%
  select(treatment_complexity_time, common_name, presence)

write.xlsx(ARD_3_rich_pivot_time, "ARD_3_rich_pivot_time.xlsx")
#make pivot table

ARD_3_rich_matrix_time <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by time_block/ARD_3_rich_matrix_time.csv")

ddply(ARD_3_rich_matrix_time,~site,function(x) {data.frame(SIMPSON=diversity(x[-1], index="simpson"))})
#copy and paste output to new df, use text to columns in Data Tools to separate into cols. 

ARD_3_simpson_time <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by time_block/ARD_3_simpson_time.csv")
ARD_3_simpson_time <- ARD_3_simpson_time %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

ggplot(data = ARD_3_simpson_time) +
  geom_col(aes(x = time_block,
               y = SIMPSON,
               fill = time_block),
           alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  facet_grid(complexity ~ treatment) +
  ggtitle("simpson Index of Species Diversity - time") +
  labs(x = expression(Time),
       y = expression(D~value)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )


# richness - over time ----------------------------------------------------

# ARD_3_with_0$treatment_complexity_time <- paste(ARD_3_with_0$treatment_complexity, "-",ARD_3_with_0$time_block)
# #need to make a col with all crossed factors together (so treatment, complexity and time_block)
# 
# ARD_3_rich_pivot_time <- ARD_3_with_0 %>%
#   select(treatment_complexity_time, common_name, presence)
# 
# write.xlsx(ARD_3_rich_pivot_time, "ARD_3_rich_pivot_time.xlsx")
# #make pivot table

# ARD_3_rich_matrix_time <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by time_block/ARD_3_rich_matrix_time.csv")

ddply(ARD_3_rich_matrix_time,~site,function(x) {data.frame(RICHNESS=sum(x[-1]>0))})
#copy and paste output to new df, use text to columns in Data Tools to separate into cols. 

ARD_3_rich_time <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by time_block/ARD_3_rich_time.csv")
ARD_3_rich_time <- ARD_3_rich_time %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

ggplot(data = ARD_3_rich_time) +
  geom_col(aes(x = time_block,
               y = RICHNESS,
               fill = time_block),
           alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  facet_grid(complexity ~ treatment) +
  ggtitle("species richness over time") +
  labs(x = expression(Time),
       y = expression(Number~of~species)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = -60, hjust = 0.01),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )

ggplot(data = ARD_3_rich_time) +
  geom_point(aes(x = time_block,
                 y = RICHNESS,
                 colour = time_block)) +
  facet_grid(complexity ~ treatment)


# diveristy - by visit ----------------------------------------------------

ARD_3_with_0$treatment_complexity_visit <- paste(ARD_3_with_0$treatment_complexity, "-",ARD_3_with_0$visit)
#need to make a col with all crossed factors together (so treatment, complexity and time_block)

ARD_3_rich_pivot_visit <- ARD_3_with_0 %>%
  select(treatment_complexity_visit, common_name, presence)

write.xlsx(ARD_3_rich_pivot_visit, "ARD_3_rich_pivot_visit.xlsx")
#make pivot table

ARD_3_rich_matrix_visit <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by visit/ARD_3_rich_matrix_visit.csv")

ddply(ARD_3_rich_matrix_visit,~site,function(x) {data.frame(RICHNESS=sum(x[-1]>0))})
#copy and paste output to new df, use text to columns in Data Tools to separate into cols. 

ARD_3_rich_visit <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by visit/ARD_3_rich_visit.csv")
ARD_3_rich_visit <- ARD_3_rich_visit %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(visit = factor(visit, levels = c("1", "2", "3", "4", "5","6", "7", "8", "9", "10","11", "12", "13", "14", "15","16", "17", "18"))) %>% 
  filter(visit != "18")

ggplot(data = ARD_3_rich_visit) +
  geom_point(aes(x = visit,
               y = RICHNESS,
               colour = treatment,
               group = treatment),
           alpha = 0.6,
           size = 2) +
  geom_line(aes(x = visit,
                 y = RICHNESS,
                 colour = treatment,
                 group = treatment),
             alpha = 0.6) +
  geom_smooth(aes(x = visit,
                  y = RICHNESS,
                  colour = treatment,
                  group = treatment),
              size = 2,
              method = lm,
              # formula = y ~ x + I(x^2),
              se = FALSE ) +
  # scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  facet_grid(complexity ~ treatment) +
  scale_x_discrete(breaks = c(5, 10, 15)) +
  ggtitle("Species Richness - visit (linear)") +
  labs(x = expression(Visit),
       y = expression(D~value)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )


# Richness by visit -------------------------------------------------------

ARD_3_with_0$treatment_complexity_visit <- paste(ARD_3_with_0$treatment_complexity, "-",ARD_3_with_0$visit)
#need to make a col with all crossed factors together (so treatment, complexity and time_block)

ARD_3_rich_pivot_visit <- ARD_3_with_0 %>%
  select(treatment_complexity_visit, common_name, presence)

write.xlsx(ARD_3_rich_pivot_visit, "ARD_3_rich_pivot_visit.xlsx")
#make pivot table

ARD_3_rich_matrix_visit <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by visit/ARD_3_rich_matrix_visit.csv")

ddply(ARD_3_rich_matrix_visit,~site,function(x) {data.frame(SIMPSON=diversity(x[-1], index="simpson"))})
#copy and paste output to new df, use text to columns in Data Tools to separate into cols. 

ARD_3_simpson_visit <- read_csv("Artificial coral experiment/data/Art Recruit data/richness and diversity/ARD_3/by visit/ARD_3_simpson_visit.csv")
ARD_3_simpson_visit <- ARD_3_simpson_visit %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  mutate(time_block = factor(visit, levels = c("1", "2", "3", "4", "5","6", "7", "8", "9", "10","11", "12", "13", "14", "15","16", "17", "18"))) %>% 
  filter(visit != "18")

ggplot(data = ARD_3_simpson_visit) +
  geom_point(aes(x = visit,
                 y = SIMPSON,
                 colour = treatment,
                 group = treatment),
             alpha = 0.6,
             size = 2) +
  geom_line(aes(x = visit,
                y = SIMPSON,
                colour = treatment,
                group = treatment),
            alpha = 0.6) +
  # geom_smooth(aes(x = visit,
              #     y = SIMPSON,
              #     colour = treatment,
              #     group = treatment),
              # size = 2,
              # method = lm,
              # formula = y ~ x + I(x^2),
              # se = FALSE ) +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  facet_grid(complexity ~ treatment) +
  ggtitle("simpson Index of Species Diversity - visit") +
  labs(x = expression(Visit),
       y = expression(D~value)) +
  theme_classic()+
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.position = "none",
    strip.text = element_text(size = 14)
  )




# SAC: species accumulation curve - experimentation code ----------------------------------------------

data("BCI")
#ok so the data structure is a matrix of sum of presence (tree counts) for 50 plots (rows)

ARD_3_pivot_SAC_C_H <- ARD_3_with_0 %>% 
  filter(treatment == "control") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_C_H,"ARD_3_pivot_SAC_C_H.xlsx")

SAC_matrix_C_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/High/SAC_matrix_C__H.csv")

#using vegan
sac_exact_C_H <- specaccum(SAC_matrix_C_H, method = "exact")
sac_ran_C_H <- specaccum(SAC_matrix_C_H, method = "random")

#plot to compare
plot(sac_exact_C_H, col = "black", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - High Control")
plot(sac_ran_C_H, col = "black", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC random - High Control")

#estimators
sac_C_H_pool <- poolaccum(SAC_matrix_C_H, permutations = 1000)
plot(sac_C_H_pool)

#messy plots, so use this instead:
plot(sac_C_H_pool, display = "chao", col = "black",  auto.key = FALSE,
     grid = F, strip = FALSE, xlab = "Visit",
     par.settings = list(axis.line = list(col = 0)),
     scales = list(col=1, tck=c(1,0)),
     panel = function(...){
       lims <- current.panel.limits()
       panel.xyplot(...)
       panel.abline(h=lims$ylim[1], v=lims$xlim[1])
     })



# SAC - by visit ----------------------------------------------------------


# SAC: control High -------------------------------------------------------

ARD_3_pivot_SAC_C_H1 <- ARD_3_with_0 %>% 
  filter(treatment == "control") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_C_H,"ARD_3_pivot_SAC_C_H.xlsx")

SAC_matrix_C_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/High/SAC_matrix_C__H.csv")

#using vegan
sac_exact_C_H <- specaccum(SAC_matrix_C_H, method = "exact")

#plot to compare
plot(sac_exact_C_H, col = "red", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - High Control")


# SAC: 0% High ------------------------------------------------------------

ARD_3_pivot_SAC_0_H <- ARD_3_with_0 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_0_H,"ARD_3_pivot_SAC_0_H.xlsx")
#remeber to delete visit column

SAC_matrix_0_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/High/SAC_matrix_0_H.csv")

#using vegan
sac_exact_0_H <- specaccum(SAC_matrix_0_H, method = "exact")

#plot to compare
plot(sac_exact_0_H, col = "orange", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - High 0%")


# SAC: 30% High -----------------------------------------------------------

ARD_3_pivot_SAC_30_H <- ARD_3_with_0 %>% 
  filter(treatment == "30%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_30_H,"ARD_3_pivot_SAC_30_H.xlsx")
#remeber to delete visit column

SAC_matrix_30_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/High/SAC_matrix_30_H.csv")

#using vegan
sac_exact_30_H <- specaccum(SAC_matrix_30_H, method = "exact")

#plot to compare
plot(sac_exact_30_H, col = "green", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - High 30%")


# SAC: 50% High -----------------------------------------------------------

ARD_3_pivot_SAC_50_H <- ARD_3_with_0 %>% 
  filter(treatment == "50%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_50_H,"ARD_3_pivot_SAC_50_H.xlsx")
#remeber to delete visit column

SAC_matrix_50_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/High/SAC_matrix_50_H.csv")

#using vegan
sac_exact_50_H <- specaccum(SAC_matrix_50_H, method = "exact")

#plot to compare
plot(sac_exact_50_H, col = "aquamarine", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - High 50%")


# SAC: 70% High -----------------------------------------------------------

ARD_3_pivot_SAC_70_H <- ARD_3_with_0 %>% 
  filter(treatment == "70%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_70_H,"ARD_3_pivot_SAC_70_H.xlsx")
#remeber to delete visit column

SAC_matrix_70_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/High/SAC_matrix_70_H.csv")

#using vegan
sac_exact_70_H <- specaccum(SAC_matrix_70_H, method = "exact")

#plot to compare
plot(sac_exact_70_H, col = "blue", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - High 70%")


# SAC: 100% High ----------------------------------------------------------

ARD_3_pivot_SAC_100_H <- ARD_3_with_0 %>% 
  filter(treatment == "100%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_100_H,"ARD_3_pivot_SAC_100_H.xlsx")
#remeber to delete visit column

SAC_matrix_100_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/High/SAC_matrix_100_H.csv")

#using vegan
sac_exact_100_H <- specaccum(SAC_matrix_100_H, method = "exact")

#plot to compare
plot(sac_exact_100_H, col = "magenta", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - High 100%")


# SAC: control Low -------------------------------------------------------

ARD_3_pivot_SAC_C_L <- ARD_3_with_0 %>% 
  filter(treatment == "control") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_C_L,"ARD_3_pivot_SAC_C_L.xlsx")

SAC_matrix_C_L <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/Low/SAC_matrix_C_L.csv")

#using vegan
sac_exact_C_L <- specaccum(SAC_matrix_C_L, method = "exact")

#plot to compare
plot(sac_exact_C_L, col = "red", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - Low Control")


# SAC: 0% Low ------------------------------------------------------------

ARD_3_pivot_SAC_0_L <- ARD_3_with_0 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_0_L,"ARD_3_pivot_SAC_0_L.xlsx")

SAC_matrix_0_L <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/Low/SAC_matrix_0_L.csv")

#using vegan
sac_exact_0_L <- specaccum(SAC_matrix_0_L, method = "exact")

#plot to compare
plot(sac_exact_0_L, col = "orange", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - Low 0%")


# SAC: 30% High -----------------------------------------------------------

ARD_3_pivot_SAC_30_L <- ARD_3_with_0 %>% 
  filter(treatment == "30%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_30_L,"ARD_3_pivot_SAC_30_L.xlsx")

SAC_matrix_30_L <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/Low/SAC_matrix_30_L.csv")

#using vegan
sac_exact_30_L <- specaccum(SAC_matrix_30_L, method = "exact")

#plot to compare
plot(sac_exact_30_L, col = "green", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - Low 30%")


# SAC: 50% High -----------------------------------------------------------

ARD_3_pivot_SAC_50_L <- ARD_3_with_0 %>% 
  filter(treatment == "50%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_50_L,"ARD_3_pivot_SAC_50_L.xlsx")

SAC_matrix_50_L <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/Low/SAC_matrix_50_L.csv")

#using vegan
sac_exact_50_L <- specaccum(SAC_matrix_50_L, method = "exact")

#plot to compare
plot(sac_exact_50_L, col = "aquamarine", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - Low 50%")


# SAC: 70% High -----------------------------------------------------------

ARD_3_pivot_SAC_70_L <- ARD_3_with_0 %>% 
  filter(treatment == "70%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_70_L,"ARD_3_pivot_SAC_70_L.xlsx")

SAC_matrix_70_L <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/Low/SAC_matrix_70_L.csv")

#using vegan
sac_exact_70_L <- specaccum(SAC_matrix_70_L, method = "exact")

#plot to compare
plot(sac_exact_70_L, col = "blue", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - Low 70%")


# SAC: 100% High ----------------------------------------------------------

ARD_3_pivot_SAC_100_L <- ARD_3_with_0 %>% 
  filter(treatment == "100%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, presence) 

write.xlsx(ARD_3_pivot_SAC_100_L,"ARD_3_pivot_SAC_100_L.xlsx")

SAC_matrix_100_L <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/Low/SAC_matrix_100_L.csv")

#using vegan
sac_exact_100_L <- specaccum(SAC_matrix_100_L, method = "exact")
#plot to compare
plot(sac_exact_100_L, col = "magenta", lwd = 3, xlab = "Visit", ylab = "Richness",
     main = "SAC exact - Low 100%")

ggplot()+
  geom_line(data = sac_exact_100_L,
            aes(x = visit,
                y = Richness))



# SAC - by days since outplanting -----------------------------------------


# control high ------------------------------------------------------------

ARD_3_pivot_SAC_C_H <- ARD_3_with_0_new_2 %>% 
  filter(treatment == "control") %>% 
  filter(complexity == "High") %>% 
  select(common_name, days_since_outplanting, presence) 

write.xlsx(ARD_3_pivot_SAC_C_H,"ARD_3_pivot_SAC_C_H.xlsx")

SAC_matrix_C_H <- read_csv("Artificial coral experiment/data/Art Recruit data/species accumulation curve/ARD_3/days_since_outplanting (new)/High/SAC_matrix_C_H.csv")
#using vegan
sac_exact_C_H <- specaccum(SAC_matrix_C_H, method = "exact")

#plot to compare
plot(sac_exact_C_H, col = "red", lwd = 3, xlab = "Days Since Outplanting", ylab = "Richness",
     main = "SAC exact - High Control")

