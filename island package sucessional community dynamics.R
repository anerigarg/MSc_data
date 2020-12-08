# Island package for sucessional community dynamics

https://cran.r-project.org/web/packages/island/vignettes/island.html


# packages ----------------------------------------------------------------

install.packages("island")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("openxlsx")
install.packages("readr")

library(tidyverse)
library(ggplot2)
library(openxlsx)
library(island)
library(readr)


# df ----------------------------------------------------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ARD_3_with_0 <- ARD_3_with_0 %>%  
  mutate(treatment = factor(treatment, levels = c("control", "100A", "30L70A", "50L50A", "70L30A", "100L"),
                            labels = c("control", "0%", "30%", "50%", "70%", "100%")))

ARD_3_with_0$treatment_complexity <-paste(ARD_3_with_0$treatment, "-",ARD_3_with_0$complexity)

# df formatting - test (don't use)-----------------------------------------------------------

ARD_3_pivot_island <- ARD_3_with_0 %>% 
  select(common_name, treatment, complexity, visit, MajorTrophicGroup, presence, TrophicLevel)
write.xlsx(ARD_3_pivot_island, "ARD_3_pivot_island.xlsx")
#pivot table 

ARD_3_matrix_island <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/ARD_3_matrix_island.csv")
#this can be filtered by treatment and complexity and functional group to look at further

# I think I'll have to separate for each treat*comp separately so it gets propper 0 and 1 values..
# Nope, df needs to show just presence not sum of presence..

# ARD_3_pivot_island_new <- ARD_3_with_0 %>% 
#   select(common_name, treatment, complexity, visit, MajorTrophicGroup, presence, TrophicLevel) %>% 
#   distinct(common_name, .keep_all = TRUE)
# 
# write.xlsx(ARD_3_pivot_island_new, "ARD_3_pivot_island_new.xlsx")

# theory  -----------------------------------------------------------------


#main theory: the tempoeral variation in the # of spp at a site over time = total rate of arrival of spp from regional pool not already 
# at the site (times colonization) - total rate of spp loss from the site (extinction)

# two assumptions:
# 1)species equivalence (the same parameters apply to a group of species
# 2)species independence (the presence of other species does not affect the dynamics of a focal sp)

# transition probabilities calculates changes in com over time using: # of events of repeated absence, colonization, extinctiton and repeated presence

#rate vs transition probabilities are dimensionless, rates are time^-1 & can estimate trans probs for any time interval


# C & E : All visits ------------------------------------------------------



# C & E : colonization and extinction rates ---------------------------------------

# High control (test)-------------------------------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_C_H <- ARD_3_with_0 %>% 
  filter(treatment == "control") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence
  
write.xlsx(ARD_3_pivot_island_C_H,"ARD_3_pivot_island_C_H.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_C_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_C_H.csv")
View(island_matrix_C_H)


rates_C_H <- regular_sampling_scheme(x = island_matrix_C_H, vector = 4:20) #vector is which cols in the matrix have pres/abs values
rates_C_H

#### - rates per MajorTrophicGroup - ###

MTG_rates_C_H <- regular_sampling_scheme(x = island_matrix_C_H, vector = 4:20, level = "MajorTrophicGroup", n = 5)
MTG_rates_C_H
#error saying level == index[i] must be a simple vector, not a matrix... hmmmm
#try making sure it's a factor --> nope

# island_matrix_C_H <- island_matrix_C_H %>% 
#   mutate(MajorTrophicGroup = factor(MajorTrophicGroup, levels = c("Herbivore", "Omnivore", "Invertivore", "Carnivore")))

# try putting it as last col and see if that works (to mimic their df)

MTG_island_matrix_C_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/MTG_island_matrix_C_H.csv")
View(MTG_island_matrix_C_H)

#Vicente said to try this when importing the csv: (Sept 9)
stringsAsFactors = T
stringsAsFactors = F

MTG_rates_C_H1 <- regular_sampling_scheme(x = MTG_island_matrix_C_H, vector = 3:19, level = "MajorTrophicGroup", n = 5)
MTG_rates_C_H1

#hmm that also didn't work...says it needs to be a simple vector, not a matrix

#try converting MajorTrophicGroup to a factor

MTG_island_matrix_C_H <- MTG_island_matrix_C_H %>% 
  mutate(MajorTrophicGroup = as.factor(MajorTrophicGroup))
  # mutate(common_name = as.factor(common_name)) %>%
  # mutate(TrophicLevel = as.factor(TrophicLevel))

unique(MTG_island_matrix_C_H$MajorTrophicGroup)

test<-regular_sampling_scheme(x = MTG_island_matrix_C_H, vector = 3:19, level= "MajorTrophicGroup", n = 5)
names(MTG_island_matrix_C_H) 
#mope

str(MTG_island_matrix_C_H)
table(MTG_island_matrix_C_H$MajorTrophicGroup, useNA="ifany")

#try converting pres/abs cols to integers instead of numeric
MTG_island_matrix_C_H[3:19] <- lapply(MTG_island_matrix_C_H[3:19], as.integer)

#try converting to a vector

as.list(data.frame(t(MTG_island_matrix_C_H))) #method 1 to conver to vector (didn't compute)
lapply(1:nrow(MTG_island_matrix_C_H), function(i) MTG_island_matrix_C_H[i,]) #method 2 to conver to vector(also didnt work)

MTG_rates_C_H2 <- regular_sampling_scheme(x = MTG_island_matrix_C_H, vector = 4:20, level = "MajorTrophicGroup", n = 5)
MTG_rates_C_H2


#maybe I have to make it a df how they did at first

data(island_matrix_C_H) #hmmm says this is not found...
df <- island_matrix_C_H[[2]]
knitr::kable(head(df))

MTG_rates_C_H <- regular_sampling_scheme(x = df, vector = 4:20, level = "MajorTrophicGroup", n = 5)
MTG_rates_C_H
#getting an errot that says incorrect number of dimenstions

data("alonso15")
#their data is in vector i think because it shows up in Values box...
df<-alonso15[[2]]
#but this shows up in data...
unique(df$Guild)



m <- matrix( letters[1:10], ncol=5)
m
unmatrix(m)
#it can't find function "unmatrix"

as.vector(isalnd_matrix_C_H)
#also not working, says "island_matrix_C_H not found"...weird. maybe it doesn't read dfs? 

scan(island_matrix_C_H)
#error: 'file' must be a character string or connection

#ok so I'll turn it to string, then try
toString(island_matrix_C_H)
#nope didn't work again, still says error: Must subset rows with a valid subscript vector.
# x Subscript `x[, level] == index[i]` must be a simple vector, not a matrix.



#### - estimating confidence intervals ###

# c and e rates have associated uncertainty partly due to data used to calculate estimates
# limits have dif in log-likelihood of 1.96 units around estimator
# just add CI = T to formula

rates_C_H2 <- regular_sampling_scheme(x = island_matrix_C_H, vector = 4:20, CI = T) #vector is which cols in the matrix have pres/abs values
rates_C_H2

#isn't working again when I add MajorTrophicGroup

#### - likelihood profiles - ###

# a way to bound the confidence interval of a parameter
# calculates NLL of a model given it's parameters
# it iteratively calculates the likelihood parameter given the data around the estimated value 
# shape of likelihood profile gives an indication on how peaked around the max our likelihood functions are

seqs <- seq.int(5, 200, by = 5) / 100

NLLs_H_C <- NLL_rss(x = island_matrix_C_H, vector = 4:20, c = seqs * rates_C_H2$c, e = rates_C_H2$e)

plot(seqs * rates_C_H2$c, NLLs_H_C, type = "l",
     xlab = "Colonization rate (day???¹)",
     ylab = "Negative Log-Likelihood", 
     main = "Likelihood profile")
points(rates_C_H2$c, rates_C_H2$NLLs_H_C, pch = 4, col = "magenta")
#for some reason the points aren't showing up...but the likelihood provile curve is



#### - Model Selection - ###

#to identify the best model that fits a df. Akaike Information Criterion (AIC) and associared measure of "weight of evidence"
# used here to choose model (& comppares against several alternative models)

#converts NLLs to AIC using functions akaikeic or akaikeicc.

#then calculates "weight_of_evidence" for competing models that explain obervations

#reminder*: can only compare models of the same df

#in their example they compare model of all spp assemblage together to their model that separates trophic groups.
# I can't do that though right now cause I can't get the MTG to work yet...

#what it does show is whether it's better to group spp by certain groupings (ie majortrophcgroup) or better explained separately





### - Simulating Colonization and Extinction Dynamics = ###

# using functions "data_generation" and "PA_simulation" to generate species richness or presence/absence data
# can simulate temporal evolution in richness

#1) take a look at rates

tp <- cetotrans(c = rates_C_H2$c, e = rates_C_H2$e)

set.seed(10110111)
sim_1 <- PA_simulation(x = df, column = 3, transitions = tp, times = 11)
colSums(sim_1)


# High control (good code) -----------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_C_H <- ARD_3_with_0 %>% 
  filter(treatment == "control") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_C_H,"ARD_3_pivot_island_C_H.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_C_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_C_H.csv")

rates_C_H2 <- regular_sampling_scheme(x = island_matrix_C_H, vector = 4:20, CI = T) #vector is which cols in the matrix have pres/abs values
rates_C_H2

#### - likelihood profiles - ###

# a way to bound the confidence interval of a parameter
# calculates NLL of a model given it's parameters
# it iteratively calculates the likelihood parameter given the data around the estimated value 
# shape of likelihood profile gives an indication on how peaked around the max our likelihood functions are

seqs <- seq.int(5, 200, by = 5) / 100

NLLs_H_C <- NLL_rss(x = island_matrix_C_H, vector = 4:20, c = seqs * rates_C_H2$c, e = rates_C_H2$e)

plot(seqs * rates_C_H2$c, NLLs_H_C, type = "l",
     xlab = "Colonization rate (day???¹)",
     ylab = "Negative Log-Likelihood", 
     main = "Likelihood profile")
points(rates_C_H2$c, rates_C_H2$NLLs_H_C, pch = 4, col = "magenta")
#for some reason the points aren't showing up...but the likelihood provile curve is


# High 0% ----------------------------------------------------------

ARD_3_pivot_island_0_H <- ARD_3_with_0 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_0_H,"ARD_3_pivot_island_0_H.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_0_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_0_H.csv")
View(island_matrix_0_H)

rates_0_H <- regular_sampling_scheme(x = island_matrix_0_H, vector = 4:20, CI = T) #vector is which cols in the matrix have pres/abs values
rates_0_H


# High 30% ---------------------------------------------------------


ARD_3_pivot_island_30_H <- ARD_3_with_0 %>% 
  filter(treatment == "30%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_30_H,"ARD_3_pivot_island_30_H.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_30_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_30_H.csv")
View(island_matrix_30_H)

rates_30_H <- regular_sampling_scheme(x = island_matrix_30_H, vector = 4:20, CI = T) #vector is which cols in the matrix have pres/abs values
rates_30_H


# High 50% ---------------------------------------------------------


ARD_3_pivot_island_50_H <- ARD_3_with_0 %>% 
  filter(treatment == "50%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_50_H,"ARD_3_pivot_island_50_H.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_50_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_50_H.csv")
View(island_matrix_50_H)

rates_50_H <- regular_sampling_scheme(x = island_matrix_50_H, vector = 4:20, CI = T) #vector is which cols in the matrix have pres/abs values
rates_50_H


#  high 70% ---------------------------------------------------------

ARD_3_pivot_island_70_H <- ARD_3_with_0 %>% 
  filter(treatment == "70%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_70_H,"ARD_3_pivot_island_70_H.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_70_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_70_H.csv")
View(island_matrix_70_H)

rates_70_H <- regular_sampling_scheme(x = island_matrix_70_H, vector = 4:20, CI = T) #vector is which cols in the matrix have pres/abs values
rates_70_H


#  high 100% --------------------------------------------------------

ARD_3_pivot_island_100_H <- ARD_3_with_0 %>% 
  filter(treatment == "100%") %>% 
  filter(complexity == "High") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_100_H,"ARD_3_pivot_island_100_H.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_100_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_100_H.csv")
View(island_matrix_100_H)

rates_100_H <- regular_sampling_scheme(x = island_matrix_100_H, vector = 4:20, CI = T) #vector is which cols in the matrix have pres/abs values
rates_100_H



# Low control ------------------------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_C_L <- ARD_3_with_0 %>% 
  filter(treatment == "control") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_C_L,"ARD_3_pivot_island_C_L.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_C_L <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/Low/island_matrix_C_L.csv")
View(island_matrix_C_L)

rates_C_L <- regular_sampling_scheme(x = island_matrix_C_L, vector = 4:21, CI = T) #vector is which cols in the matrix have pres/abs values
rates_C_L


# Low 0% -----------------------------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_0_L <- ARD_3_with_0 %>% 
  filter(treatment == "0%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_0_L,"ARD_3_pivot_island_0_L.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_0_L <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/Low/island_matrix_0_L.csv")
View(island_matrix_0_L)

rates_0_L <- regular_sampling_scheme(x = island_matrix_0_L, vector = 4:21, CI = T) #vector is which cols in the matrix have pres/abs values
rates_0_L


# Low 30% ----------------------------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_30_L <- ARD_3_with_0 %>% 
  filter(treatment == "30%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_30_L,"ARD_3_pivot_island_30_L.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_30_L <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/Low/island_matrix_30_L.csv")
View(island_matrix_30_L)

rates_30_L <- regular_sampling_scheme(x = island_matrix_30_L, vector = 4:21, CI = T) #vector is which cols in the matrix have pres/abs values
rates_30_L


#  Low 50% ----------------------------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_50_L <- ARD_3_with_0 %>% 
  filter(treatment == "50%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_50_L,"ARD_3_pivot_island_50_L.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_50_L <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/Low/island_matrix_50_L.csv")
View(island_matrix_50_L)

rates_50_L <- regular_sampling_scheme(x = island_matrix_50_L, vector = 4:21, CI = T) #vector is which cols in the matrix have pres/abs values
rates_50_L


# Low 70% ---------------------------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_70_L <- ARD_3_with_0 %>% 
  filter(treatment == "70%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_70_L,"ARD_3_pivot_island_70_L.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_70_L <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/Low/island_matrix_70_L.csv")
View(island_matrix_70_L)

rates_70_L <- regular_sampling_scheme(x = island_matrix_70_L, vector = 4:21, CI = T) #vector is which cols in the matrix have pres/abs values
rates_70_L


# Low 100% --------------------------------------------------------

ARD_3_with_0$common_name_visit <-paste(ARD_3_with_0$common_name, "-",ARD_3_with_0$visit)

ARD_3_pivot_island_100_L <- ARD_3_with_0 %>% 
  filter(treatment == "100%") %>% 
  filter(complexity == "Low") %>% 
  select(common_name, visit, MajorTrophicGroup, presence, TrophicLevel, common_name_visit) %>% 
  distinct(common_name_visit, .keep_all = TRUE)

#distinct because the pivot table should just be presence not sum of presence

write.xlsx(ARD_3_pivot_island_100_L,"ARD_3_pivot_island_100_L.xlsx")
#replace any (blank) in TrophicLevel with NA
#tabular form, turn off grand totals and subtotals

island_matrix_100_L <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/Low/island_matrix_100_L.csv")
View(island_matrix_100_L)

rates_100_L <- regular_sampling_scheme(x = island_matrix_100_L, vector = 4:21, CI = T) #vector is which cols in the matrix have pres/abs values
rates_100_L


# plot (all visits) --------------------------------------------------------------

colonization_and_extincion_outputs_ARD_3 <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/colonization and extincion outputs ARD_3.csv")

ARD_3_col <- colonization_and_extincion_outputs_ARD_3 %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

ggplot(data = ARD_3_col) +
  facet_grid(complexity ~ treatment) +
  geom_point(aes(x = x,
                 y = c,
                 colour = treatment),
             size = 3) +
  geom_errorbar(aes(x = x,
                    ymin = c_low,
                    ymax = c_up,
                    colour = treatment),
                width = 0) +
  theme_classic()+
  labs(x = expression(Treatment),
       y = expression(Colonization~rate~(fish~m^{2}~d^{-1}))) +
  ggtitle("Colonization rates based off island package") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggplot(data = ARD_3_col) +
  geom_point(aes(x = c,
                 y = e,
                 colour = treatment,
                 shape = complexity),
             size = 4) +
  theme_classic() +
  labs(x = expression(Colonization~rate~(day^{-1})),
       y = expression(Extinction~rate~(day^{-1}))) +
  ggtitle("Extinction ~ Colonization all visits - regular sampling scheme") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  )



# C & E: First 9 visits, irregular sampling schemes ---------------------------------------------------

ARD_3_first9 <- ARD_3_with_0 %>% 
  filter(visit %in% (1:10))

# High control (test) ------------------------------------------------------------

island_matrix_C_H <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/High/island_matrix_C_H.csv")
View(island_matrix_C_H)

#need package rootsolve for irregular sampling scheme
install.packages("rootSolve")
library(rootSolve)

rates_C_H9 <- irregular_single_dataset(dataframe = island_matrix_C_H,
                                       vector = 4:12, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_C_H9

#estimate col and ext rates for dif func groups (with more than 5 spp)
rates_C_H9_func <- irregular_single_dataset(dataframe = island_matrix_C_H, 
                                            vector = 4:12, c = 0.001, e = 0.001,
                                            column = "MajorTrophicGroup", n = 5, 
                                            assembly = T, jacobian = T)
rates_C_H9_func
#still getting error message:
# Error: Must subset rows with a valid subscript vector.
# x Subscript `dataset[, column] == groups[k]` must be a simple vector, not a matrix


# C & E: first 9 visits, irregular sampling scheme -------------------------------

rates_C_H9 <- irregular_single_dataset(dataframe = island_matrix_C_H,
                                       vector = 4:12, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_C_H9

rates_0_H9 <- irregular_single_dataset(dataframe = island_matrix_0_H,
                                       vector = 4:12, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_0_H9

rates_30_H9 <- irregular_single_dataset(dataframe = island_matrix_30_H,
                                       vector = 4:12, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_30_H9

rates_50_H9 <- irregular_single_dataset(dataframe = island_matrix_50_H,
                                        vector = 4:12, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_50_H9

rates_70_H9 <- irregular_single_dataset(dataframe = island_matrix_70_H,
                                        vector = 4:12, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_70_H9

rates_100_H9 <- irregular_single_dataset(dataframe = island_matrix_100_H,
                                        vector = 4:12, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_100_H9

# LOW: 

rates_C_L9 <- irregular_single_dataset(dataframe = island_matrix_C_L,
                                       vector = 4:12, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_C_L9

rates_0_L9 <- irregular_single_dataset(dataframe = island_matrix_0_L,
                                       vector = 4:12, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_0_L9

rates_30_L9 <- irregular_single_dataset(dataframe = island_matrix_30_L,
                                       vector = 4:12, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_30_L9

rates_50_L9 <- irregular_single_dataset(dataframe = island_matrix_50_L,
                                        vector = 4:12, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_50_L9

rates_70_L9 <- irregular_single_dataset(dataframe = island_matrix_70_L,
                                        vector = 4:12, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_70_L9

rates_100_L9 <- irregular_single_dataset(dataframe = island_matrix_100_L,
                                        vector = 4:12, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_100_L9

#plot:

ARD3_9_irreg <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3_first9/ARD3_9_irreg.csv")
ARD3_9_irreg <- ARD3_9_irreg %>% 
mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

ggplot(data = ARD3_9_irreg) +
  geom_point(aes(x = c,
                 y = e,
                 colour = treatment,
                 shape = complexity),
             size = 4) +
  theme_classic() +
  labs(x = expression(Colonization~rate~(day^{-1})),
       y = expression(Extinction~rate~(day^{-1}))) +
  ggtitle("Extinction ~ Colonization - first 9 visits - irregular sampling scheme") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

# C & E: first all visits, irregular sampling scheme -------------------------------

rates_C_HI <- irregular_single_dataset(dataframe = island_matrix_C_H,
                                       vector = 4:20, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_C_HI

rates_0_HI <- irregular_single_dataset(dataframe = island_matrix_0_H,
                                       vector = 4:20, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_0_HI

rates_30_HI <- irregular_single_dataset(dataframe = island_matrix_30_H,
                                        vector = 4:20, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_30_HI

rates_50_HI <- irregular_single_dataset(dataframe = island_matrix_50_H,
                                        vector = 4:20, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_50_HI

rates_70_HI <- irregular_single_dataset(dataframe = island_matrix_70_H,
                                        vector = 4:20, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_70_HI

rates_100_HI <- irregular_single_dataset(dataframe = island_matrix_100_H,
                                         vector = 4:20, c = 0.001, e = 0.001, 
                                         assembly = T, jacobian = T)
rates_100_HI

# LOW: 

rates_C_LI <- irregular_single_dataset(dataframe = island_matrix_C_L,
                                       vector = 4:21, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_C_LI

rates_0_LI <- irregular_single_dataset(dataframe = island_matrix_0_L,
                                       vector = 4:21, c = 0.001, e = 0.001, 
                                       assembly = T, jacobian = T)
rates_0_LI

rates_30_LI <- irregular_single_dataset(dataframe = island_matrix_30_L,
                                        vector = 4:21, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_30_LI

rates_50_LI <- irregular_single_dataset(dataframe = island_matrix_50_L,
                                        vector = 4:21, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_50_LI

rates_70_LI <- irregular_single_dataset(dataframe = island_matrix_70_L,
                                        vector = 4:21, c = 0.001, e = 0.001, 
                                        assembly = T, jacobian = T)
rates_70_LI

rates_100_LI <- irregular_single_dataset(dataframe = island_matrix_100_L,
                                         vector = 4:21, c = 0.001, e = 0.001, 
                                         assembly = T, jacobian = T)
rates_100_LI

#plot:

ARD3_irreg <- read_csv("Artificial coral experiment/data/Art Recruit data/island community succession/ARD_3/ARD3_irreg.csv")
ARD3_irreg <- ARD3_irreg %>% 
  mutate(treatment = factor(treatment, levels = c("control", "0%", "30%", "50%", "70%", "100%")))

install.packages("scales")
library(scales)

ggplot(data = ARD3_irreg) +
  geom_point(aes(x = c,
                 y = e,
                 colour = treatment,
                 shape = complexity),
             size = 4.5) +
  geom_vline(xintercept = 0.13, 
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  geom_hline(yintercept = 0.56,
             linetype = "dashed", 
             colour = "black", 
             size = 0.8, 
             alpha = 0.3) +
  # scale_y_continuous(breaks = c(0.45, 0.55, 0.65, 0.75)) +
  theme_classic() +
  labs(x = expression(Colonization~rate~(day^{-1})),
       y = expression(Extinction~rate~(day^{-1}))) +
  scale_colour_manual(values = c("gray44", "firebrick", "darkorange1", "goldenrod1", "mediumseagreen", "royalblue")) +
  # scale_colour_viridis(option = "inferno", discrete = TRUE) +
  ggtitle("Extinction ~ colonization - all visits - irregular sampling scheme") +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

