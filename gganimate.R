# Trophic Group playing with animation ------------------------------------

install.packages("gganimate")
library(gganimate)
install.packages("hrbrthemes")
library(hrbrthemes)
library(ggplot2)

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