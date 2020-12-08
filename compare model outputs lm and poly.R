#model selection

# packages ----------------------------------------------------------------




# data files --------------------------------------------------------------

#This one has days since outplanting as a col: 
ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv")
ARD_3_with_0_new_2  <- ARD_3_with_0_new_2 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)
 
#code for summary dfs is in the R file ARD piece wise regression. 

# lm for each treat/compp -------------------------------------------------

#ARD_3_C_H
ARD_3_C_H_lm <- lm(density ~ days_since_outplanting, data = ARD_3_C_H)
summary(ARD_3_C_H_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_C_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_0_H
ARD_3_0_H_lm <- lm(density ~ days_since_outplanting, data = ARD_3_0_H)
summary(ARD_3_0_H_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_0_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_30_H
ARD_3_30_H_lm <- lm(density ~ days_since_outplanting, data = ARD_3_30_H)
summary(ARD_3_30_H_lm)
plot(ARD_3_30_H_lm)

ggplot(data = ARD_3_30_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_50_H
ARD_3_50_H_lm <- lm(density ~ days_since_outplanting, data = ARD_3_50_H)
summary(ARD_3_50_H_lm)
plot(ARD_3_50_H_lm)

ggplot(data = ARD_3_50_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_70_H
ARD_3_70_H_lm <- lm(density ~ days_since_outplanting, data = ARD_3_70_H)
summary(ARD_3_70_H_lm)
plot(ARD_3_70_H_lm)

ggplot(data = ARD_3_70_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_100_H
ARD_3_100_H_lm <- lm(density ~ days_since_outplanting, data = ARD_3_100_H)
summary(ARD_3_100_H_lm)
plot(ARD_3_100_H_lm)

ggplot(data = ARD_3_100_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")


#ARD_3_C_L
ARD_3_C_L_lm <- lm(density ~ days_since_outplanting, data = ARD_3_C_L)
summary(ARD_3_C_L_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_C_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_0_L
ARD_3_0_L_lm <- lm(density ~ days_since_outplanting, data = ARD_3_0_L)
summary(ARD_3_0_L_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_0_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_30_L
ARD_3_30_L_lm <- lm(density ~ days_since_outplanting, data = ARD_3_30_L)
summary(ARD_3_30_L_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_30_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_50_L
ARD_3_50_L_lm <- lm(density ~ days_since_outplanting, data = ARD_3_50_L)
summary(ARD_3_50_L_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_50_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_70_L
ARD_3_70_L_lm <- lm(density ~ days_since_outplanting, data = ARD_3_70_L)
summary(ARD_3_70_L_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_70_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_100_L
ARD_3_100_L_lm <- lm(density ~ days_since_outplanting, data = ARD_3_100_L)
summary(ARD_3_100_L_lm)
plot(ARD_3_0_H_lm)

ggplot(data = ARD_3_100_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  geom_smooth(method = "lm")



# 3rd order polynomial for each treat/comp --------------------------------------------------------------

install.packages("caret")
library(caret)

#ARD_3_C_H
ARD_3_C_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_C_H)
summary(ARD_3_C_H_poly)
plot(ARD_3_0_H_poly)

ggplot(data = ARD_3_C_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,6), size = 1)

#ARD_3_0_H
ARD_3_0_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_0_H)
summary(ARD_3_0_H_poly)

ggplot(data = ARD_3_0_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_30_H
ARD_3_30_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_30_H)
summary(ARD_3_30_H_poly)

ggplot(data = ARD_3_30_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_50_H
ARD_3_50_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_50_H)
summary(ARD_3_50_H_poly)

ggplot(data = ARD_3_50_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_70_H
ARD_3_70_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_70_H)
summary(ARD_3_70_H_poly)

ggplot(data = ARD_3_70_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_100_H
ARD_3_100_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 5, raw = TRUE), data = ARD_3_100_H)
summary(ARD_3_100_H_poly)

ggplot(data = ARD_3_100_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_C_L
ARD_3_C_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_C_L)
summary(ARD_3_C_L_poly)

ggplot(data = ARD_3_C_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_0_L
ARD_3_0_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_0_L)
summary(ARD_3_0_L_poly)

ggplot(data = ARD_3_0_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_30_L
ARD_3_30_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_30_L)
summary(ARD_3_30_L_poly)

ggplot(data = ARD_3_30_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_50_L
ARD_3_50_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_50_L)
summary(ARD_3_50_L_poly)

ggplot(data = ARD_3_50_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_70_L
ARD_3_70_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_70_L)
summary(ARD_3_70_L_poly)

ggplot(data = ARD_3_70_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_100_L
ARD_3_100_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_100_L)
summary(ARD_3_100_L_poly)

ggplot(data = ARD_3_100_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)


# 2rd order polynomial for each treat/comp --------------------------------------------------------------

install.packages("caret")
library(caret)

#ARD_3_C_H
ARD_3_C_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_C_H)
summary(ARD_3_C_H_poly)
plot(ARD_3_0_H_poly)

ggplot(data = ARD_3_C_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_0_H
ARD_3_0_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_0_H)
summary(ARD_3_0_H_poly)

ggplot(data = ARD_3_0_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_30_H
ARD_3_30_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_30_H)
summary(ARD_3_30_H_poly)

ggplot(data = ARD_3_30_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_50_H
ARD_3_50_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_50_H)
summary(ARD_3_50_H_poly)

ggplot(data = ARD_3_50_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_70_H
ARD_3_70_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_70_H)
summary(ARD_3_70_H_poly)

ggplot(data = ARD_3_70_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_100_H
ARD_3_100_H_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_100_H)
summary(ARD_3_100_H_poly)

ggplot(data = ARD_3_100_H,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_C_L
ARD_3_C_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_C_L)
summary(ARD_3_C_L_poly)

ggplot(data = ARD_3_C_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_0_L
ARD_3_0_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_0_L)
summary(ARD_3_0_L_poly)

ggplot(data = ARD_3_0_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_30_L
ARD_3_30_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_30_L)
summary(ARD_3_30_L_poly)

ggplot(data = ARD_3_30_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_50_L
ARD_3_50_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_50_L)
summary(ARD_3_50_L_poly)

ggplot(data = ARD_3_50_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_70_L
ARD_3_70_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_70_L)
summary(ARD_3_70_L_poly)

ggplot(data = ARD_3_70_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

#ARD_3_100_L
ARD_3_100_L_poly <- lm(density ~ poly(days_since_outplanting, degree = 4, raw = TRUE), data = ARD_3_100_L)
summary(ARD_3_100_L_poly)

ggplot(data = ARD_3_100_L,
       aes(x = days_since_outplanting,
           y = density)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), size = 1)

# lm sum for each treat/compp -------------------------------------------------

#don't use this, can't compare from sum data, needs to be all data

#ARD_3_C_H_sum
ARD_3_C_H_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_C_H_sum)
summary(ARD_3_C_H_lm_sum)

ggplot(data = ARD_3_C_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_0_H_sum
ARD_3_0_H_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_0_H_sum)
summary(ARD_3_0_H_lm_sum)

ggplot(data = ARD_3_0_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_30_H_sum
ARD_3_30_H_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_30_H_sum)
summary(ARD_3_30_H_lm_sum)

ggplot(data = ARD_3_30_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_50_H_sum
ARD_3_50_H_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_50_H_sum)
summary(ARD_3_50_H_lm_sum)

ggplot(data = ARD_3_50_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_70_H_sum
ARD_3_70_H_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_70_H_sum)
summary(ARD_3_70_H_lm_sum)

ggplot(data = ARD_3_70_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_100_H_sum
ARD_3_100_H_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_100_H_sum)
summary(ARD_3_100_H_lm_sum)

ggplot(data = ARD_3_100_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_C_L_sum
ARD_3_C_L_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_C_L_sum)
summary(ARD_3_C_L_lm_sum)

ggplot(data = ARD_3_C_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_0_L_sum
ARD_3_0_L_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_0_L_sum)
summary(ARD_3_0_L_lm_sum)

ggplot(data = ARD_3_0_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_30_L_sum
ARD_3_30_L_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_30_L_sum)
summary(ARD_3_30_L_lm_sum)

ggplot(data = ARD_3_30_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_50_L_sum
ARD_3_50_L_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_50_L_sum)
summary(ARD_3_50_L_lm_sum)

ggplot(data = ARD_3_50_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_70_L_sum
ARD_3_70_L_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_70_L_sum)
summary(ARD_3_70_L_lm_sum)

ggplot(data = ARD_3_70_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")

#ARD_3_100_L_sum
ARD_3_100_L_lm_sum <- lm(density.mean ~ days_since_outplanting, data = ARD_3_100_L_sum)
summary(ARD_3_100_L_lm_sum)

ggplot(data = ARD_3_100_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  geom_smooth(method = "lm")


# poly sum for each treat/comp --------------------------------------------

#also don't use this....

#ARD_3_C_H
ARD_3_C_H_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 6, raw = TRUE), data = ARD_3_C_H_sum)
summary(ARD_3_C_H_poly_sum)

ggplot(data = ARD_3_C_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,6), size = 1)

#ARD_3_0_H
ARD_3_0_H_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_0_H_sum)
summary(ARD_3_0_H_poly_sum)

ggplot(data = ARD_3_0_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_30_H
ARD_3_30_H_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_30_H_sum)
summary(ARD_3_30_H_poly_sum)

ggplot(data = ARD_3_30_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_50_H
ARD_3_50_H_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_50_H_sum)
summary(ARD_3_50_H_poly_sum)

ggplot(data = ARD_3_50_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_70_H
ARD_3_70_H_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_70_H_sum)
summary(ARD_3_70_H_poly_sum)

ggplot(data = ARD_3_70_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_100_H
ARD_3_100_H_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_100_H_sum)
summary(ARD_3_100_H_poly_sum)

ggplot(data = ARD_3_100_H_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_C_L
ARD_3_C_L_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_C_L_sum)
summary(ARD_3_C_L_poly_sum)

ggplot(data = ARD_3_C_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_0_L
ARD_3_0_L_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_0_L_sum)
summary(ARD_3_0_L_poly_sum)

ggplot(data = ARD_3_0_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_30_L
ARD_3_30_L_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_30_L_sum)
summary(ARD_3_30_L_poly_sum)

ggplot(data = ARD_3_30_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_50_L
ARD_3_50_L_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_50_L_sum)
summary(ARD_3_50_L_poly_sum)

ggplot(data = ARD_3_50_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_70_L
ARD_3_70_L_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_70_L_sum)
summary(ARD_3_70_L_poly_sum)

ggplot(data = ARD_3_70_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)

#ARD_3_100_L
ARD_3_100_L_poly_sum <- lm(density.mean ~ poly(days_since_outplanting, degree = 3, raw = TRUE), data = ARD_3_100_L_sum)
summary(ARD_3_100_L_poly_sum)

ggplot(data = ARD_3_100_L_sum,
       aes(x = days_since_outplanting,
           y = density.mean)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), size = 1)



# poly relative density models --------------------------------------------

ARD_3_C_H <- ARD_3_effect %>% 
  filter(complexity == "High") %>% 
  filter(treatment == "control")
