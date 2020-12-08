
# joining ArtR dataframes -------------------------------------------------

library(tidyverse)

#imported "Art_Recruit_fish_data
head(Art_Recruit_fish_data)

ARD <- Art_Recruit_fish_data
test <- ARD
#ARD <- test

###---- join ARD to species list ----###

#import ArtR_species_list
#change col name CommonName to common_name (to join to ARD)
ARD_spp_list <- ArtR_species_list %>% 
  rename(common_name = CommonName)
head(ARD_spp_list)
#change col name species_ID to common_name 
ARD <- ARD %>% 
  rename(common_name = species_ID)
head(ARD)

#delete unecessary collumns BehaviouralGroup, MedianPopPblTime, L-W parameter source, and L-W region
ARD_spp_list[,c('BehaviouralGroup','MedianPopDblTime', 'L-W parameter source', 'L-W region')] <- list(NULL)

#using the package data.table I will now join ARD and ARD_spp_list and name the new df ARD2. ARD2 should have the same number of observations as ARD with more variables 
library(data.table)
setDT(ARD)
setDT(ARD_spp_list)
setkey(ARD, common_name)
setkey(ARD_spp_list, common_name)
ARD2 = ARD_spp_list[ARD]

head(ARD2)

###---- join ARD2 to survey info ----###

#import ArtR_survey_info, link by by survey_number

setDT(ARD2)
setDT(ArtR_survey_info)
setkey(ARD2, survey_number)
setkey(ArtR_survey_info, survey_number)
ARD3 = ArtR_survey_info[ARD2]

head(ARD3)

###---- join ARD3 to plot info ----###

#import ArtR_plot_info, link by plot

setDT(ARD3)
setDT(ArtR_plot_info)
setkey(ARD3, plot)
setkey(ArtR_plot_info, plot)
ARD4 = ArtR_plot_info[ARD3]

head(ARD4)

###---- join ARD4 to transect info ----###
# import ArtR transect info
#need to make a new variable in ARD4 called plot_grid that makes a unique identifier by combining plot and grid_number ( do the same in ArtR_transect_info)

ARD4$plot_grid <- paste(ARD4$plot, "-", ARD4$grid_number)
ArtR_transect_info$plot_grid <- paste(ArtR_transect_info$plot, "-", ArtR_transect_info$grid_numbers)
#remmeber to delete row grid_numbers in ArtR_transect_info after

setDT(ARD4)
setDT(ArtR_transect_info)
setkey(ARD4, plot_grid)
setkey(ArtR_transect_info, plot_grid)
ARD5 = ArtR_transect_info[ARD4]

ARD5[,c('grid_numbers')] <- list(NULL)

head(ARD5)
#ARD5 created Feb 6, 2020 - most recently joined df


# add days_since_outplanting ----------------------------------------------



# test script ------------------------------

# Sept 5, 2020

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ArtR_days_since_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup.csv")
#this lookup has staggered start dates, don't use
#1) make a col in each df that is common (date-plot)

ARD_3_with_0$plot_date <- paste(ARD_3_with_0$plot, "-", ARD_3_with_0$date)
ArtR_days_since_lookup$plot_date <- paste(ArtR_days_since_lookup$plot, "-", ArtR_days_since_lookup$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup, -date)
select(ArtR_days_since_lookup, -plot)

#3) join them by plot-date

ARD_3_with_0_new <- inner_join(ARD_3_with_0, ArtR_days_since_lookup)

write.csv(ARD_3_with_0_new, "ARD_3_with_0_new.csv")


# ARD 3-----------------------------

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

ARD_3_with_0$plot_date <- paste(ARD_3_with_0$plot, "-", ARD_3_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

ArtR_days_since_lookup_new <- ArtR_days_since_lookup_new %>% 
  select( -date) %>% 
  select(-plot)

#3) join them by plot-date

ARD_3_with_0_new_2 <- inner_join(ARD_3_with_0, ArtR_days_since_lookup_new)

write.csv(ARD_3_with_0_new_2, "ARD_3_with_0_new_2.csv")

ARD_3_with_0_new_2sum <- ARD_3_with_0_new_2 %>% 
  group_by(treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_3_with_0sum <- ARD_3_with_0 %>% 
  group_by(treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

# ARD 3 test - test with other spp as NA-----------------------------

ARD_3_with_0n2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_with_0n2.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

ARD_3_with_0n2$plot_date <- paste(ARD_3_with_0n2$plot, "-", ARD_3_with_0n2$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

ArtR_days_since_lookup_new <- ArtR_days_since_lookup_new %>% 
  select( -date) %>% 
  select(-plot)

#3) join them by plot-date

ARD_3_with_0_new_2test <- inner_join(ARD_3_with_0n2, ArtR_days_since_lookup_new)

write.csv(ARD_3_with_0_new_2test, "ARD_3_with_0_new_2test.csv")

ARD_3_with_0_new_2testsum <- ARD_3_with_0_new_2test %>% 
  group_by(treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

#yas! got the same answers finally, must have been an older version of ARD_3_with_0 I was using. 

ARD_3_with_0n2sum <- ARD_3_with_0n2 %>% 
  group_by(treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))


# ARD 4to6 -------------------
#Sept 22
ARD_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

ARD_4to6_with_0$plot_date <- paste(ARD_4to6_with_0$plot, "-", ARD_4to6_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date

ARD_4to6_with_0_new_2 <- inner_join(ARD_4to6_with_0, ArtR_days_since_lookup_new)

write.csv(ARD_4to6_with_0_new_2, "ARD_4to6_with_0_new_2.csv")

# herb 3-----------------------------

herb_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_3_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

herb_3_with_0$plot_date <- paste(herb_3_with_0$plot, "-", herb_3_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date
# literally just calling it 0_new_2 to match other code for plots so I can just do ctl find and replace 
herb_3_with_0_new_2 <- inner_join(herb_3_with_0, ArtR_days_since_lookup_new)

write.csv(herb_3_with_0_new_2, "herb_3_with_0_new_2.csv")

# herb 4to6 -------------------
#Sept 23
herb_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_4to6/herb_4to6_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

herb_4to6_with_0$plot_date <- paste(herb_4to6_with_0$plot, "-", herb_4to6_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date

herb_4to6_with_0_new_2 <- inner_join(herb_4to6_with_0, ArtR_days_since_lookup_new)

write.csv(herb_4to6_with_0_new_2, "herb_4to6_with_0_new_2.csv")


# gob 2.5 -----------------------------------------------------------------

gob_2.5_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_2.5/gob_2.5_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

gob_2.5_with_0$plot_date <- paste(gob_2.5_with_0$plot, "-", gob_2.5_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date
# literally just calling it 0_new_2 to match other code for plots so I can just do ctl find and replace 
gob_2.5_with_0_new_2 <- inner_join(gob_2.5_with_0, ArtR_days_since_lookup_new)

write.csv(gob_2.5_with_0_new_2, "gob_2.5_with_0_new_2.csv")

# gob 3-----------------------------
#oct 11

gob_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_3/gob_3_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

gob_3_with_0$plot_date <- paste(gob_3_with_0$plot, "-", gob_3_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date
# literally just calling it 0_new_2 to match other code for plots so I can just do ctl find and replace 
gob_3_with_0_new_2 <- inner_join(gob_3_with_0, ArtR_days_since_lookup_new)

write.csv(gob_3_with_0_new_2, "gob_3_with_0_new_2.csv")

# gob 3to6 -------------------

gob_3to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_3to6/gob_3to6_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

gob_3to6_with_0$plot_date <- paste(gob_3to6_with_0$plot, "-", gob_3to6_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date

gob_3to6_with_0_new_2 <- inner_join(gob_3to6_with_0, ArtR_days_since_lookup_new)

write.csv(gob_3to6_with_0_new_2, "gob_3to6_with_0_new_2.csv")

# gob 4to6 -------------------

gob_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_4to6/gob_4to6_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

gob_4to6_with_0$plot_date <- paste(gob_4to6_with_0$plot, "-", gob_4to6_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date

gob_4to6_with_0_new_2 <- inner_join(gob_4to6_with_0, ArtR_days_since_lookup_new)

write.csv(gob_4to6_with_0_new_2, "gob_4to6_with_0_new_2.csv")


# dam 3-----------------------------
#oct 12

dam_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_3/dam_3_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

dam_3_with_0$plot_date <- paste(dam_3_with_0$plot, "-", dam_3_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date
# literally just calling it 0_new_2 to match other code for plots so I can just do ctl find and replace 
dam_3_with_0_new_2 <- inner_join(dam_3_with_0, ArtR_days_since_lookup_new)

write.csv(dam_3_with_0_new_2, "dam_3_with_0_new_2.csv")

# herb 4to6 -------------------
#Oct 12
dam_4to6_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_4to6/dam_4to6_with_0.csv")
ArtR_days_since_lookup_new <- read_csv("Artificial coral experiment/data/Art Recruit data/data frames to join/ArtR_days_since_lookup_new.csv")

#1) make a col in each df that is common (date-plot)

dam_4to6_with_0$plot_date <- paste(dam_4to6_with_0$plot, "-", dam_4to6_with_0$date)
ArtR_days_since_lookup_new$plot_date <- paste(ArtR_days_since_lookup_new$plot, "-", ArtR_days_since_lookup_new$date)

#2) delete date and plot cols from the lookup table

select(ArtR_days_since_lookup_new, -date)
select(ArtR_days_since_lookup_new, -plot)

#3) join them by plot-date

dam_4to6_with_0_new_2 <- inner_join(dam_4to6_with_0, ArtR_days_since_lookup_new)

write.csv(dam_4to6_with_0_new_2, "dam_4to6_with_0_new_2.csv")
