library(readr)
library(tidyverse)
library(ggplot2)


`%notin%` = negate(`%in%`)

# re-calculate ARD_3 to include 0 values  ---------------------------------

ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all fish 3cm and under that have correct biomass and density calculations
ARD_3_only <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_3_only_plot_grid <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey)

write.csv(ARD_3_only_plot_grid, "ARD_3_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in second col
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate")
# used filter to just get the unique values.
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
#now import as a ARD_3_0_lookup.

ARD_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_0_lookup.csv")

#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_survey values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has density = 0
ARD_3_none <- ARD %>% 
  filter(TL > 4) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_just_0 <- inner_join(ARD_3_none, ARD_3_0_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none

#can now join ARD_3_only and ARD_just_0!!

ARD_3_with_0 <- bind_rows(ARD_3_only,ARD_just_0)
write.csv(ARD_3_with_0, "ARD_3_with_0.csv")

ARD_3_with_0 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_with_0.csv")

# 0 values for ARD 5 ------------------------------------------------------

ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all fish 5 cm and under that have correct biomass and density calculations
ARD_5_only <- ARD %>% 
  filter(TL <= 5) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_5_only_plot_grid <- ARD %>% 
  filter(TL <= 5) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey)

write.csv(ARD_5_only_plot_grid, "ARD_5_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate")
# used filter to just get the unique values.copy and past unique plot_grid values to new csv file
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
# save as ARD_5_0_lookup import

ARD_5_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_5/ARD_5_0_lookup.csv")
View(ARD_5_0_lookup)

#3) join ARD_5_0_lookup with ARD_5_none but only for the plot_grid_survey values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has density = 0
ARD_5_none <- ARD %>% 
  filter(TL > 6) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_just_0_5 <- inner_join(ARD_5_none, ARD_5_0_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none

#can now join ARD_5_only and ARD_just_0_5!!

ARD_5_with_0 <- bind_rows(ARD_5_only,ARD_just_0_5)
write.csv(ARD_5_with_0, "ARD_5_with_0.csv")


# ARD for fish 2.5 and smaller --------------------------------------------

ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all fish 5 cm and under that have correct biomass and density calculations
ARD_2.5_only <- ARD %>% 
  filter(TL <= 2.5) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_2.5_only_plot_grid <- ARD %>% 
  filter(TL <= 2.5) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey)

write.csv(ARD_2.5_only_plot_grid, "ARD_2.5_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c3
# used filter to just get the unique values.copy and past unique plot_grid values to new csv file
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
# save as ARD_5_0_lookup import

ARD_2.5_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_2.5/ARD_2.5_0_lookup.csv")
View(ARD_2.5_0_lookup)

#3) join ARD_2.5_0_lookup with ARD_5_none but only for the plot_grid_survey values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has density = 0
ARD_2.5_none <- ARD %>% 
  filter(TL > 2.5) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_just_0_2.5 <- inner_join(ARD_2.5_none, ARD_2.5_0_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none

#can now join ARD_5_only and ARD_just_0_5!!

ARD_2.5_with_0 <- bind_rows(ARD_2.5_only,ARD_just_0_2.5)
write.csv(ARD_2.5_with_0, "ARD_2.5_with_0.csv")


# ARD for fishes between 2.6-5cm ------------------------------------------


ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all fish 5 cm and under that have correct biomass and density calculations
ARD_2.5to5_only <- ARD %>% 
  filter(TL > 2.5 & TL < 5) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_2.5to5_only_plot_grid <- ARD %>% 
  filter(TL > 2.5 & TL < 5) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey)

write.csv(ARD_2.5to5_only_plot_grid, "ARD_2.5to5_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c3
# used filter to just get the unique values.copy and past unique plot_grid values to new csv file
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
# save as ARD_5_0_lookup import

ARD_2.5to5_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_2.5-5/ARD_2.5to5_lookup.csv")
View(ARD_2.5to5_lookup)

#3) join ARD_2.5_0_lookup with ARD_5_none but only for the plot_grid_survey values in the lookup table

#df with all other fish that are smaller than 2.5cm and bigger than 5cm,and a col that has density = 0
ARD_2.5to5_none2.5 <- ARD %>% 
  filter(TL < 2.5) %>%
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_2.5to5_none5 <- ARD %>% 
  filter(TL > 5) %>%
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_2.5to5_none <- bind_rows(ARD_2.5to5_none2.5,ARD_2.5to5_none5)

ARD_just_0_2.5to5 <- inner_join(ARD_2.5to5_none, ARD_2.5to5_0_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_2.5to5_none

#can now join ARD_5_only and ARD_just_0_5!!

ARD_2.5to5_with_0 <- bind_rows(ARD_2.5to5_only,ARD_just_0_2.5to5)
View(ARD_2.5to5_with_0)
write.csv(ARD_2.5to5_with_0, "ARD_2.5to5_with_0.csv")



# ARD 6 -------------------------------------------------------------------


ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all fish 3cm and under that have correct biomass and density calculations
ARD_6_only <- ARD %>% 
  filter(TL <= 6) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_6_only_plot_grid <- ARD %>% 
  filter(TL <= 6) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey)

write.csv(ARD_6_only_plot_grid, "ARD_6_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid values to new csv file
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
# save as ARD_5_0_lookup import

ARD_6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_6/ARD_6_0_lookup.csv")

#3) join ARD_6_0_lookup with ARD_6_none but only for the plot_grid_survey values in the lookup table

#df with all other fish that are 6cm and bigger,and a col that has density = 0
ARD_6_none <- ARD %>% 
  filter(TL > 6) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_just_0_6 <- inner_join(ARD_6_none, ARD_6_0_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none

#can now join ARD_5_only and ARD_just_0_5!!

ARD_6_with_0 <- bind_rows(ARD_6_only,ARD_just_0_6)
write.csv(ARD_6_with_0, "ARD_6_with_0.csv")



# ARD 4-6, last 9 visits --------------------------------------------------
ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all fish 4-6cm and under that have correct biomass and density calculations
ARD_4to6_only <- ARD %>% 
  filter(TL %in% (4:6)) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_4to6_only_plot_grid <- ARD %>% 
  filter(TL %in% (4:6)) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>%  
  distinct(plot_grid_survey)

write.csv(ARD_4to6_only_plot_grid, "ARD_4to6_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid values to new csv file
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
# save as ARD_5_0_lookup import

ARD_4to6_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_lookup.csv")

#3) join ARD_6_0_lookup with ARD_6_none but only for the plot_grid_survey values in the lookup table

`%notin%` = negate(`%in%`)

#df with all other fish that are not 4-6cm,and a col that has density = 0
ARD_4to6_none <- ARD %>% 
  filter(TL %notin% (4:6)) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_just_0_4to6 <- inner_join(ARD_4to6_none, ARD_4to6_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none

#can now join ARD_4to6_only and ARD_just_0_4to6!!

ARD_4to6_with_0 <- bind_rows(ARD_4to6_only,ARD_just_0_4to6)
write.csv(ARD_4to6_with_0, "ARD_4to6_with_0.csv")



# OLD CODE ----------------------------------------------------------------


# new ARD_3 trial..somehow this got messed up ---------------------------------------------------------

ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#add col for each plot_grid_visit
ARD$plot_grid_visit <- paste(ARD$plot_grid, "-", ARD$visit)

write.csv(ARD, "ARD_post_out_plant_new.csv")

# #1) one df with all fish 3cm and under that have presence = 1
test_ARD_3_only <- ARD %>%
  filter(TL <= 3) %>%
  group_by(plot_grid_visit) %>%
  mutate(fish.presence = 1) %>%
  distinct(plot_grid_visit, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under (to find out which plot_grid_surveys are missing)
test_ARD_3_only_plot_grid <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(test_ARD_3_only_plot_grid, "test_ARD_3_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

test_ARD_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/test_ARD_3_0_lookup.csv")

#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
test_ARD_3_none <- ARD %>% 
  # filter(TL > 4) %>% #do i need this ?? don't think so
  mutate(fish.presence = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
test_ARD_just_0 <- inner_join(test2_ARD_3_none, test_ARD_3_0_lookup)

#can now join ARD_3_only and ARD_just_0 and calculate new density value

test_ARD_3_with_0 <- bind_rows(test_ARD_3_only,test_ARD_just_0) %>% 
  mutate(density = sum(fish.presence)/0.79)


write.csv(test_ARD_3_with_0, "test_ARD_3_with_0.csv")




# try again...fresh start new trial ---------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
ARD_3_only <- ARD %>%
  filter(TL <= 3) %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)
  # distinct(plot_grid_visit, .keep_all = TRUE)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
ARD_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(ARD_3_only_plot_grid_visit, "ARD_3_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

ARD_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
ARD_3_none <- ARD %>% 
  # filter(TL > 4) %>% #do i need this ?? don't think so
  mutate(presence = 0)
  # distinct(plot_grid_survey, .keep_all = TRUE)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
ARD_just_0 <- inner_join(ARD_3_none, ARD_3_0_lookup)
  # distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value

ARD_3_with_0 <- bind_rows(ARD_3_only, ARD_just_0) %>% 
  mutate(density = sum(presence)/0.79)


write.csv(ARD_3_with_0, "ARD_3_with_0.csv")

# damselfish old --------------------------------------------------------------

# damselfish ARD_3 old --------------------------------------------------------



ARD_post_out_plant <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post out-plant.csv")
ARD <- ARD_post_out_plant
#remove old density and biomass calculations
ARD <- ARD %>% 
  select(-c(density, biomass))

#1) one df with all damselfish 3cm and under that have correct biomass and density calculations
ARD_3_only_dam <- ARD %>% 
  filter(TL <= 3) %>% 
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

#2) one df with the plot_grid_surveys for all fish 3cm and under
ARD_3_only_plot_grid <- ARD %>% 
  filter(TL <= 3) %>% 
  group_by(plot_grid_survey) %>% 
  mutate(density = sum(presence)/0.79) %>% 
  mutate(biomass = (a*(TL^b))) %>% 
  distinct(plot_grid_survey)

write.csv(ARD_3_only_plot_grid, "ARD_3_only_plot_grid.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in second col
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate")
# used filter to just get the unique values.
# this should serve as look-up table to get plot_grid_survey values of fish >3cm that have density of 0
#now import as a ARD_3_0_lookup.

ARD_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_3/ARD_3_0_lookup.csv")

#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_survey values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has density = 0
ARD_3_none <- ARD %>% 
  filter(TL > 4) %>% 
  mutate(density = 0) %>% 
  mutate(biomass = 0) %>% 
  distinct(plot_grid_survey, .keep_all = TRUE)

ARD_just_0 <- inner_join(ARD_3_none, ARD_3_0_lookup)
#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none

#can now join ARD_3_only and ARD_just_0!!

ARD_3_with_0 <- bind_rows(ARD_3_only,ARD_just_0)
write.csv(ARD_3_with_0, "ARD_3_with_0.csv")


# NEW CODE ----------------------------------------------------------------


# ARD_3 -------------------------------------------------------------------


ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

#1) one df with all fish 3cm and under that have presence = 1
ARD_3_only <- ARD %>%
  filter(TL <= 3) %>%
  # group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
ARD_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  # group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(ARD_3_only_plot_grid_visit, "ARD_3_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

ARD_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
ARD_3_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
ARD_just_0 <- inner_join(ARD_3_none, ARD_3_0_lookup) %>% 
distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#need a line of code that makes all the species info NA... maybe I can do it manually?
write.csv(ARD_just_0, "ARD_just_0.csv")
#made all values under SpeciesCode all the way to transit NA

ARD_just_0n2 <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/test ARD_3/ARD_just_0.csv")

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
ARD_3_with_0n2 <- bind_rows(ARD_3_only, ARD_just_0n2) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)
#use this data frame for proportion col plot or when comparing spp over time to each treatment

ARD_3_with_0 <- bind_rows(ARD_3_only, ARD_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(ARD_3_with_0n2, "ARD_3_with_0n2.csv")
write.csv(ARD_3_with_0,"ARD_3_with_0.csv")

#export both and make sure these are the versions in filter 0 values folder

#compare summary stats for both

ARD_3_with_0n2sum <- ARD_3_with_0n2 %>% 
  group_by(visit, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_3_with_0sum <- ARD_3_with_0 %>% 
  group_by(visit, treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

#they are the same, so up to this step is fine. oct 13 10:45am

ARD_3_with_0n2sum2 <- ARD_3_with_0n2 %>% 
  group_by(treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

ARD_3_with_0sum2 <- ARD_3_with_0 %>% 
  group_by(treatment, complexity) %>%
  summarize(density.mean = mean(density), density.sd = sd(density))

#these are also the same..


# ARD_6 -------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
ARD_6_only <- ARD %>%
  filter(TL <= 6) %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
ARD_6_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 6) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(ARD_6_only_plot_grid_visit, "ARD_6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

ARD_6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_6/ARD_6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
ARD_6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
ARD_6_just_0 <- inner_join(ARD_6_none, ARD_6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
ARD_6_with_0 <- bind_rows(ARD_6_only, ARD_6_just_0) %>% 
  mutate(density = sum(presence)/0.79)

write.csv(ARD_6_with_0, "ARD_6_with_0.csv")


# ARD_4to6 ----------------------------------------------------------------


ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
ARD_4to6_only <- ARD %>%
  filter(TL %in% (4:6)) %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
ARD_4to6_only_plot_grid_visit <- ARD %>% 
  filter(TL %in% (4:6)) %>%
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(ARD_4to6_only_plot_grid_visit, "ARD_4to6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

ARD_4to6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/ARD_4to6/ARD_4to6_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
ARD_4to6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
ARD_4to6_just_0 <- inner_join(ARD_4to6_none, ARD_4to6_0_lookup)
  # distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
ARD_4to6_with_0 <- bind_rows(ARD_4to6_only, ARD_4to6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)

write.csv(ARD_4to6_with_0, "ARD_4to6_with_0.csv")


# DAMSELFISH --------------------------------------------------------------


# dam_3 -------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all damselfish 3cm and under that have presence = 1
dam_3_only <- ARD %>%
  filter(TL <= 3) %>%
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

damcount3 <- dam_3_only %>% 
  group_by(common_name) %>% 
  count(common_name)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
dam_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(dam_3_only_plot_grid_visit, "dam_3_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

dam_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_3/dam_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
dam_3_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
dam_just_0 <- inner_join(dam_3_none, dam_3_0_lookup) %>% 
distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
dam_3_with_0 <- bind_rows(dam_3_only, dam_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(dam_3_with_0, "dam_3_with_0.csv")




# dam_6 -------------------------------------------------------------------


ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
dam_6_only <- ARD %>%
  filter(TL <= 6) %>%
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
dam_6_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 6) %>% 
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(dam_6_only_plot_grid_visit, "dam_6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

dam_6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_6/dam_6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
dam_6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
dam_6_just_0 <- inner_join(dam_6_none, dam_6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
dam_6_with_0 <- bind_rows(dam_6_only, dam_6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(dam_6_with_0, "dam_6_with_0.csv")


# dam_4to6 ----------------------------------------------------------------


ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
dam_4to6_only <- ARD %>%
  filter(TL %in% (4:6)) %>%
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

damcount4to6 <- dam_4to6_only %>% 
  group_by(common_name) %>% 
  count(common_name)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
dam_4to6_only_plot_grid_visit <- ARD %>% 
  filter(TL %in% (4:6)) %>%
  filter(Genus %in% c("Microspathodon", "Stegastes")) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(dam_4to6_only_plot_grid_visit, "dam_4to6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

dam_4to6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/damselfish/dam_4to6/dam_4to6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
dam_4to6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
dam_4to6_just_0 <- inner_join(dam_4to6_none, dam_4to6_0_lookup) %>% 
distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
dam_4to6_with_0 <- bind_rows(dam_4to6_only, dam_4to6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(dam_4to6_with_0, "dam_4to6_with_0.csv")


# PARROTFISH --------------------------------------------------------------


# par_3 -------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all damselfish 3cm and under that have presence = 1
par_3_only <- ARD %>%
  filter(TL <= 3) %>%
  filter(Family == "Scaridae") %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
par_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  filter(Family == "Scaridae") %>%
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(par_3_only_plot_grid_visit, "par_3_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

par_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/parrotfish/par_3/par_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
par_3_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
par_just_0 <- inner_join(par_3_none, par_3_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
par_3_with_0 <- bind_rows(par_3_only, par_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(par_3_with_0, "par_3_with_0.csv")


# par_6 -------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
par_6_only <- ARD %>%
  filter(TL <= 6) %>%
  filter(Family == "Scaridae") %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
par_6_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 6) %>% 
  filter(Family == "Scaridae") %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(par_6_only_plot_grid_visit, "par_6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

par_6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/parrotfish/par_6/par_6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
par_6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
par_6_just_0 <- inner_join(par_6_none, par_6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
par_6_with_0 <- bind_rows(par_6_only, par_6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(par_6_with_0, "par_6_with_0.csv")


# par_4to6 ----------------------------------------------------------------


ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
par_4to6_only <- ARD %>%
  filter(TL %in% (4:6)) %>%
  filter(Family == "Scaridae") %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
par_4to6_only_plot_grid_visit <- ARD %>% 
  filter(TL %in% (4:6)) %>%
  filter(Family == "Scaridae") %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(par_4to6_only_plot_grid_visit, "par_4to6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

par_4to6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/parrotfish/par_4to6/par_4to6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
par_4to6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
par_4to6_just_0 <- inner_join(par_4to6_none, par_4to6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
par_4to6_with_0 <- bind_rows(par_4to6_only, par_4to6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(par_4to6_with_0, "par_4to6_with_0.csv")


# COMMERCIAL --------------------------------------------------------------


# com_3 -------------------------------------------------------------------

#groupers, snappers and grunts --> no groupers. only 1 observaiton LOL...maybe not worth it

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all damselfish 3cm and under that have presence = 1
com_3_only <- ARD %>%
  filter(TL <= 3) %>%
  filter(Family %in% c("Haemulidae","Lutjanidae")) %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
com_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  filter(Family %in% c("Haemulidae","Lutjanidae")) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(com_3_only_plot_grid_visit, "com_3_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

par_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/parrotfish/par_3/par_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
par_3_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
par_just_0 <- inner_join(par_3_none, par_3_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
par_3_with_0 <- bind_rows(par_3_only, par_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(par_3_with_0, "par_3_with_0.csv")





# GOBIES ------------------------------------------------------------------


# gob_2.5 -----------------------------------------------------------------

#1) one df with all gobies and blennies (except masked) 2.5cm and under that have presence = 1
gob_2.5_only <- ARD %>%
  filter(TL <= 2.5) %>%
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>% 
  filter(common_name %notin% "Masked Goby") %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

gobcount2.5 <- gob_2.5_only %>% 
  group_by(common_name) %>% 
  count(common_name)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing in the next step)
gob_2.5_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 2.5) %>%
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>% 
  filter(common_name %notin% "Masked Goby") %>%
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(gob_2.5_only_plot_grid_visit, "gob_2.5_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

gob_2.5_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_2.5/gob_2.5 _0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with  fish.presence = 0 (doesn't matter what the spp are, just need the distinct plot_grid_visit for pres = 0)
gob_2.5_none <- ARD %>%
  mutate(presence = 0)

#only keeps records that have matching plot_grid_vist in both lookup and gob_2.5_none
gob_2.5_just_0 <- inner_join(gob_2.5_none, gob_2.5_0_lookup) %>% 
distinct(plot_grid_visit, .keep_all = TRUE)
 # check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
gob_2.5_with_0 <- bind_rows(gob_2.5_only, gob_2.5_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(gob_2.5_with_0, "gob_2.5_with_0.csv")


# gpb 3to6 --------------------------------------------------------------

#exclude masked goby, # 1) one df with all fish 3-6cm that have presence = 1
gob_3to6_only <- ARD %>%
  filter(TL %in% (3:6)) %>%
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>% 
  filter(common_name %notin% "Masked Goby") %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

gobcount3to6 <- gob_3to6_only %>% 
  group_by(common_name) %>% 
  count(common_name)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
gob_3to6_only_plot_grid_visit <- ARD %>% 
  filter(TL %in% (3:6)) %>%
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>% 
  filter(common_name %notin% "Masked Goby") %>%
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(gob_3to6_only_plot_grid_visit, "gob_3to6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

gob_3to6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_3to6/gob_3to6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with  fish.presence = 0 (doesn't matter what the spp are, just need the distinct plot_grid_visit for pres = 0)
gob_3to6_none <- ARD %>%
  mutate(presence = 0)

#only keeps records that have matching plot_grid_vist in both lookup and gob_2.5_none
gob_3to6_just_0 <- inner_join(gob_3to6_none, gob_3to6_0_lookup) %>% 
  distinct(plot_grid_visit, .keep_all = TRUE)
# check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
gob_3to6_with_0 <- bind_rows(gob_3to6_only, gob_3to6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(gob_3to6_with_0, "gob_3to6_with_0.csv")

# gob_3 -------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all damselfish 3cm and under that have presence = 1
gob_3_only <- ARD %>%
  filter(TL <= 3) %>%
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)


#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
gob_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(gob_3_only_plot_grid_visit, "gob_3_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

gob_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_3/gob_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
gob_3_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
gob_just_0 <- inner_join(gob_3_none, gob_3_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
gob_3_with_0 <- bind_rows(gob_3_only, gob_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(gob_3_with_0, "gob_3_with_0.csv")


# gob_6 -------------------------------------------------------------------


ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 3cm and under that have presence = 1
gob_6_only <- ARD %>%
  filter(TL <= 6) %>%
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>%  
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
gob_6_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 6) %>% 
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(gob_6_only_plot_grid_visit, "gob_6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

gob_6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_6/gob_6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
gob_6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
gob_6_just_0 <- inner_join(gob_6_none, gob_6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
gob_6_with_0 <- bind_rows(gob_6_only, gob_6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(gob_6_with_0, "gob_6_with_0.csv")


# gob_4to6 ----------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all fish 4-6cm that have presence = 1
gob_4to6_only <- ARD %>%
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>%  
  filter(TL %in% (4:6)) %>%
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
gob_4to6_only_plot_grid_visit <- ARD %>% 
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>%
  filter(TL %in% (4:6)) %>%
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.csv(gob_4to6_only_plot_grid_visit, "gob_4to6_only_plot_grid_visit.csv")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_6_0_lookup import

gob_4to6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/gobies and blennies/gob_4to6/gob_4to6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
gob_4to6_none <- ARD %>% 
  filter(Family %in% c("Gobiidae","Labrisomidae")) %>%
  filter(TL %in% (4:6)) %>%
  mutate(presence = 0)

#only keeps records that have matching plot_grid_survey in both lookup and ARD_3_none
gob_4to6_just_0 <- inner_join(gob_4to6_none, gob_4to6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value
gob_4to6_with_0 <- bind_rows(gob_4to6_only, gob_4to6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(gob_4to6_with_0, "gob_4to6_with_0.csv")
#nope still have some parrotfish in there... and damselfish. somewhere they are not being filtered out. 

# HERBIVORES --------------------------------------------------------------


# herb_3 ------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all damselfish 3cm and under that have presence = 1
herb_3_only <- ARD %>%
  filter(TL <= 3) %>%
  filter(MajorTrophicGroup == "Herbivore") %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
herb_3_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 3) %>% 
  filter(MajorTrophicGroup == "Herbivore") %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.xlsx(herb_3_only_plot_grid_visit, "herb_3_only_plot_grid_visit.xlsx")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

herb_3_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_3_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
herb_3_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_visit in both lookup and ARD_3_none
herb_just_0 <- inner_join(herb_3_none, herb_3_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
herb_3_with_0 <- bind_rows(herb_3_only, herb_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(herb_3_with_0, "herb_3_with_0.csv")


# herb_6 ------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all damselfish 3cm and under that have presence = 1
herb_6_only <- ARD %>%
  filter(TL <= 6) %>%
  filter(MajorTrophicGroup == "Herbivore") %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
herb_6_only_plot_grid_visit <- ARD %>% 
  filter(TL <= 6) %>% 
  filter(MajorTrophicGroup == "Herbivore") %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.xlsx(herb_6_only_plot_grid_visit, "herb_6_only_plot_grid_visit.xlsx")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as ARD_3_0_lookup import

herb_6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_6/herb_6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
herb_6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_visit in both lookup and ARD_3_none
herb_6_just_0 <- inner_join(herb_6_none, herb_6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
herb_6_with_0 <- bind_rows(herb_6_only, herb_6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(herb_6_with_0, "herb_6_with_0.csv")

# herb_4to6 ------------------------------------------------------------------

ARD_post_out_plant_new <- read_csv("Artificial coral experiment/data/Art Recruit data/final clean data frames/ARD_post_out_plant_new.csv")
ARD <- ARD_post_out_plant_new

# #1) one df with all damselfish 3cm and under that have presence = 1
herb_4to6_only <- ARD %>%
  filter(TL %in% (4:6)) %>%
  filter(MajorTrophicGroup == "Herbivore") %>% 
  group_by(plot_grid_visit) %>%
  mutate(presence = 1)

#2) one df with the plot_grid_visits for all fish 3cm and under (to find out which plot_grid_visits are missing)
herb_4to6_only_plot_grid_visit <- ARD %>% 
  filter(TL %in% (4:6)) %>%
  filter(MajorTrophicGroup == "Herbivore") %>% 
  group_by(plot_grid_visit) %>%
  distinct(plot_grid_visit)

write.xlsx(herb_4to6_only_plot_grid_visit, "herb_4to6_only_plot_grid_visit.xlsx")
#exported just the distinct plot_grid_survey. copied the plot_gri_survey from ARD_post_out_plant in first col (delete duplicates)
#compared cols to remove duplicates using this formula:=IF(ISERROR(MATCH(A2,$B$1:$B$10000,0)),"Unique","Duplicate") in c2
# used filter to just get the unique values.copy and past unique plot_grid_survey values from 1st col to new csv file
# this should serve as look-up table to get plot_grid_visit values of fish not 3cm and smaller (that have presence of 0)
# save as herb_4to6_0_lookup import

herb_4to6_0_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/filter 0 values/herbivores/herb_4to6/herb_4to6_0_lookup.csv")
#3) join ARD_3_0_lookup with ARD_3_none but only for the plot_grid_visit values in the lookup table

#df with all other fish that are 4cm and bigger,and a col that has fish.presence = 0
herb_4to6_none <- ARD %>% 
  mutate(presence = 0)

#only keeps records that have matching plot_grid_visit in both lookup and ARD_3_none
herb_4to6_just_0 <- inner_join(herb_4to6_none, herb_4to6_0_lookup)
# distinct(plot_grid_visit, .keep_all = TRUE)
#just to check if it's the same number of plot_grid_visits as lookup, it is.

#can now join ARD_3_only and ARD_just_0 and calculate new density value...based on plot_grid_visit
herb_4to6_with_0 <- bind_rows(herb_4to6_only, herb_4to6_just_0) %>% 
  group_by(plot_grid_visit) %>%
  mutate(density = sum(presence)/0.79)

write.csv(herb_4to6_with_0, "herb_4to6_with_0.csv")



