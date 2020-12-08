# GLMM

# dfs ---------------------------------------------------------------------

ARD_3_with_0_new_2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0_new_2.csv") %>% 
  group_by(plot_grid) %>% 
  mutate(abundance = sum(presence))

ARD_3 <- ARD_3_with_0_new_2 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%"))) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

ARD_3 <- ARD_3 %>% 
  mutate(log.density = log(density)) %>% 
  mutate(sqrt.density = sqrt(density))

  
# for non-normal data, model the log mean fish density per treat*comp
# measuremtns on related/similar subjects --> ie plot is a random factor (they could be more similar)

# GLMM assumptions: 
# log transform reponse
# random effects are not observed, but still may have an effect on the response...

# glmm package: approximates entire likelihood using monte carlo and importance sampling


# video tutorial R --------------------------------------------------------

#withlm it's just lm(y~x)

lm.1 <- lm(density ~ days_since_outplanting + treatment + treatment*complexity, data = ARD_3)
summary(lm.1)

#with GLM you can specify Family (binomial, gaussian, gamma, inverse.gaussian, poisson, quasi, quasibinomial, quasipoission)
# and link function (each family has a default link function ) for poisson it's default is log identiy link
# glm.model = glm(y~x, family = ___(link = "__"))

glm.1 <- glm(log.density ~ days_since_outplanting + treatment + treatment*complexity, family = poisson, data = ARD_3)
summary(glm.1)
#getting AIC: Inf --> apparently it's cause I've got decimal places (a non -integer value) that's not supported w poisson distributions
# so made a new var called abundance (just sum of presence grouped by plot_grid), try on that

hist(ARD_3$sqrt.density)
hist(ARD_3$abundance)
hist(ARD_3$density)
#also non normal

glm.2 <- glm(abundance ~ days_since_outplanting + treatment + treatment*complexity, family = poisson, data = ARD_3)
summary(glm.2)
#well, am getting an AIC value now so that's good.

GLMM_1 <- nlme(log.density ~ days_since_outplanting + treatment*complexity ..., family = poisson, data = ARD_3)


# plot abundance ----------------------------------------------------------

#looks the exact same as density, good. may need to do the model with abundance to have integer values

ARD_3_abun_sum <- ARD_3 %>% 
  group_by(treatment, complexity) %>% 
  summarize(abundance.mean = mean(abundance), abundance.sd = sd(abundance)) %>% 
  mutate(abundance.se = abundance.sd/sqrt(1539))

ggplot() + 
  geom_point(data = ARD_3_abun_sum, 
             aes(x = treatment, 
                 y = abundance.mean,
                 colour = complexity),
             size = 2) + 
  geom_errorbar(data = ARD_3_abun_sum,
                aes(x = treatment, 
                    ymin = abundance.mean - abundance.se, 
                    ymax = abundance.mean + abundance.se,
                    colour = complexity),
                width = 0.2,
                size = 0.5) +
  theme_classic()

ggplot(data = ARD_3_with_0_new_2,
       aes(x = treatment,
           y = density,
           colour = complexity)) +
  geom_jitter()


# packages ----------------------------------------------------------------

library(mgcv)
library(tidyverse)
library(ggplot2)
library(readr)
library(lme4)
library(nlme)

install.packages("glmm") #this is from christina knudel (sp?) video
library(glmm)
install.packages("lme4")

# GLMM trial - notes from talk with Viktoria --------------------------------------------------------------

#following bolker tutorial: https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html

#start with linear mixed effect model
LMM_1 <- lme(abundance ~ days_since_outplanting + treatment,
              method = "REML",
              random = ~ 1 + days_since_outplanting | plot,
              data = ARD_3,
              control=list(maxIter = 10000, niterEM = 10000))
summary(LMM_1)

#from packaged lme4
GLMM_1 <- glmer(abundance ~ days_since_outplanting + treatment +treatment*complexity + (1|plot),
                data = ARD_3,
                family = "poisson")
print(summary(GLMM_1),correlation = FALSE)
#getting warning that model is nearly unidentifiable: cery large eigenvalye
# - Rescale variables?

GLMM_2 <- lmer(abundance ~ days_since_outplanting + treatment + treatment*complexity + (1|plot),
               data = ARD_3)

GLMM_4 <- lmer(abundance ~ days_since_outplanting + treatment*complexity + (1|plot),
               data = ARD_3)
summary(GLMM_4)

GLMM_3 <- lmer(abundance ~ days_since_outplanting + treatment*complexity + (1|complexity/plot),
               data = ARD_3)

# lsmeans --> plot estimates, or crawley ch on ancova estimates table
# need to model dif autocorrelation structure over time --> tells you in what way response can change --> response could decrease or increase
# or increase over time
# and variation with time --> 
# then do model selection to see which one fits best 

# 1 model with all predictor + autocorrelation
#

# package that calculates AIC based on a bunch of models --> to compare 
# lmer doesn't have function for temportal autocorrelation 
# nlme package, lme function --> has correlation term, weights specifies variance (wtih time) --> maybe variation decreases/ increaes
# construct dif models with combinations for each of these (predictors, with correlation, without etc)
# book suggestion: Viktoria emailed 

# nlme may be more useful -> but only for linear models (single effects) --> one function you're fitting, can still use possion dist data
# could be a squared function (like what I have)
# additive model --> set of different functions I could use to describe dif portions of the data (so a GAM, not gonna do this)
# nlme has a function to specify the random structure, the autocorrelation and the variance structure -->
# can give it a list of the dif autocorrelation 

# full_model <- abundance ~ days_since_outplanting + restore_status + treatment + complexity + treatment*complexity + (1|plot)

# package MuMin for model selection (see email from Sydney)


# GLMM - full model -------------------------------------------------------


GLMM_full <- lme(abundance ~ days_since_outplanting + treatment + complexity + genotype +
                   treatment*complexity + treatment*genotype + treatment*days_since_outplanting +
                   compexity*days_since_outplanting + complexity*genotype +
                   genotype*days_since_outplanting +
                   (1|plot/plot_grid),
               data = ARD_3)

# test_int_GLMM <- nlme(density ~ days_since_outplanting + treatment*complexity + (1|complexity/plot) + (1/surveyor/survey_number), data = ARD_3)
# since complexity is a fixed effect donèt have to nest plot in it, also since experimental design mixed up who was surveying, donèt need to include surveyor as random effect

# test_int_GLMM <- lme(abundance ~ days_since_outplanting + treatment*complexity, data = ARD_3)
# tes1 <- lmer::nlme(abundance ~ days_since_outplanting + treatment*complexity, data = ARD_3)

hist(ARD_3$abundance) #looks like neg binomial dist
hist(ARD_3$density)

ARD_3_sum <- ARD_3 %>% 
  group_by(treatment, complexity) %>% 
  summarize(mean.abundance = mean(abundance), var = var(abundance))

#since variabce is greater than the mean, I likely have a negative binomial distribution, so should use GLMMadmb

# Viktoria code: *don't use ---------------------------------------------------

# I think this is code for a repeated measures anova though, with linear model. 

#group data object?:

# make an object that groups the data so R knows individual plot_grid (coral clusters) were repeated
# (ie that I measured fish density repeatedly for individual clusters over time)

grouped_by_plot_grid <-groupedData(abundance ~ days_since_outplanting|plot_grid, data = ARD_3,
                                   FUN = mean, #
                                   )


# GLMM admb ---------------------------------------------------------------

# can model abundance with negative biomial or poisson dist (see which one has better residuals plotted between -2 and 2 on y axis)
# for my data, neg binomial dist (variance greater than mean)

# admb (automatic differentiation model builder), laplace approximation of high-dimensional integrals, fast, flexible, stable (Fournier et al 2012)



# use nlme::gls to check autocorrelation ----------------------------------

# Viktoria suggestions: 
# 1) use nlme::gls()and check if autocorelation is present --> if not use GLMM
# 2) if autocorrelation is present, explain since equivalent package does not exist you used gls() for your data

gls1 <- nlme::gls(abundance ~ days_since_outplanting + complexity + treatment, data = ARD_3,
                  correlation = )
summary(gls1)

library(broom)
y <- rnorm(10)
x <- 1:10
df <- augment(gls1)
plot(ARD_3$days_since_outplanting, residuals(gls1), type = "o", pch = 16)
