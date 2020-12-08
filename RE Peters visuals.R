## RE Peters ##


library(tidyverse)
library(ggplot2)

ARD_recruit <- read_csv("data/Art Recruit data/final clean data frames/ARD_recruit.csv")

View(ARD_recruit)

#make sure time_block is as a factor
class(ARD_recruit$time_block)
ARD_recruit$time_block = as.factor(ARD_recruit$time_block)
class(ARD_recruit$time_block)

#change order of treatments and label names to show gradient of living corals and time from start to end
ARD_recruit_box <- ARD_recruit %>% 
    mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  filter(TL < 6) #keep fish under 5cm and under
  

#plotted with colour for treatment..... YASSSSSSSSSSSS
ggplot()+
  geom_boxplot(data = ARD_recruit_box,
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

#plot just treatment
ggplot()+
  geom_boxplot(data = ARD_recruit_box,
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

#plot treatment facet compl
ggplot()+
  geom_boxplot(data = ARD_recruit_box,
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

#add column that has treatment and comlexity
ARD_recruit$treatment_complexity <- paste(ARD_recruit$treatment, "_",ARD_recruit$complexity)
view(ARD_recruit)

unique(ARD_recruit$treatment_complexity)

#code to filter df for a boxplot
ARD_recruit_box_both <- ARD_recruit %>% 
  mutate(treatment_complexity = factor(treatment_complexity, 
                                       levels = c( "100A _ Low","100A _ High" ,"30L70A _ Low", "30L70A _ High" ,"50L50A _ Low","50L50A _ High" ,  "70L30A _ Low","70L30A _ High","100L _ Low" ,"100L _ High","control _ Low","control _ High"),
                                       labels = c("0% Low", "0% High", "30% Low", "30% High", "50% Low", "50% High", "70% Low", "70% High", "100% Low", "100% High", "control Low", "control High"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  filter(TL < 6)

write.csv(ARD_recruit_box_both, "ARD_recruit_fish under 5 with time block and treat_comp.csv")

# colour pallettes --------------------------------------------------------

#colour pallettes
install.packages("remotes")
remotes::install_github("jakelawlor/PNWColors")
install.packages("viridis")
library(viridis)

pal <- pnw_palette(3, name = "Starfish", type = "continuous")
pal1 <- pnw_palette(3, name = "Winter", type = "continuous")
pal2 <-pnw_palette(3, name = "Anemone", type = "continuous")
pal3 <-pnw_palette(3, name = "Lake", type = "continuous")
    


# boxplots ----------------------------------------------------------------

   
#plot of fish density (<5cm) for each time period and treatment !! USE THIS
ggplot()+
      geom_boxplot(data = ARD_recruit_box,
                   aes(x = treatment,
                       y = density,
                       fill = time_block)) +
      scale_fill_manual(values = pal,
                        name = "Time" ) +
            theme_classic() +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.position = "top") +
  # facet_grid(complexity~.)

#plot of fish density (<5cm) for each time period and complexity !! USE THIS
ggplot()+
  geom_boxplot(data = ARD_recruit_box,
               aes(x = complexity,
                   y = density,
                   fill = time_block)) +
  scale_fill_manual(values = pal,
                    name = "Time" )+
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.position = "top"
  )
  #facet_grid(.~treatment)

#plot of both together:

ggplot()+
  geom_boxplot(data = ARD_recruit_box_both,
               aes(x = treatment_complexity,
                   y = density,
                   fill = time_block)) +
  scale_fill_manual(values = pal,
                    name = "Time: " ) +
  xlab("Treatment & Complexity") +
  ylab("Fish Density") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 13),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24),
    legend.position = "top"
  )

#overall grouped time:
ggplot()+
  geom_boxplot(data = ARD_recruit_box_both,
               aes(x = treatment_complexity,
                   y = density,
                   fill = treatment)) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(labels = c("Low", "High","Low", "High","Low", "High","Low", "High","Low", "High","Low", "High")) +
  xlab("Complexity") +
  ylab("Fish Density") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 24),
    legend.text = element_text(size = 24)
  )



# line plot for trophic groups --------------------------------------------


ARD_recruit_line <- ARD_recruit %>% 
  filter(TL < 6) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end"))) %>% 
  group_by(MajorTrophicGroup, time_block) %>% 
  summarise(mean.density = mean(density))


ggplot(data = ARD_recruit_line,
       aes(x = time_block,
           y = mean.density,
           colour = MajorTrophicGroup))+
  geom_line(aes(group = MajorTrophicGroup),
            size = 1)+
  geom_point(size = 3)+
  theme_classic()

# summary stats -----------------------------------------------------------

ARD_recruit_treat.table <- ARD_recruit %>% 
  filter(TL < 6) %>% 
  group_by(treatment) %>% 
  summarize(mean.dens = mean(density))

ARD_recruit_comp.table <- ARD_recruit %>% 
  filter(TL < 6) %>% 
  group_by(complexity) %>% 
  summarize(mean.dens = mean(density))

ggplot() +
  geom_point(data = ARD_recruit_treat.table,
            aes(x = treatment,
                y = mean.dens))


# boxplots for trophic groups ---------------------------------------------

ARD_recruit_5 <- ARD_recruit %>% 
  filter(TL < 6) %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control"))) %>% 
  mutate(treatment_complexity = factor(treatment_complexity, 
                                       levels = c( "100A _ Low","100A _ High" ,"30L70A _ Low", "30L70A _ High" ,"50L50A _ Low","50L50A _ High" ,  "70L30A _ Low","70L30A _ High","100L _ Low" ,"100L _ High","control _ Low","control _ High"),
                                       labels = c("0% Low", "0% High", "30% Low", "30% High", "50% Low", "50% High", "70% Low", "70% High", "100% Low", "100% High", "control Low", "control High"))) %>% 
  mutate(time_block = factor(time_block, levels = c("start", "middle", "end")))

ARD_recruit_5_test <- ARD_recruit %>% 
  filter(TL < 6) %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ggplot()+
  geom_boxplot(data = ARD_recruit_5_test,
           aes(x = MajorTrophicGroup,
               y = density,
               fill = MajorTrophicGroup,
               colour = time_block))+
  scale_fill_viridis(discrete = TRUE, name = "Trophic Group")+
  scale_colour_grey(start = 0.9, end = 0.2, name = "Time")+
  facet_grid(complexity~treatment)+
  xlab("Trophic Group") +
  ylab("Fish Density") +
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
