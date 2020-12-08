## NMDS and PerMANOVA on ARD ##

#imported ARD_final
# subset into ARD_baseline and ARD_recruit to see if there's a dif bw those coms overall to startlibrary(tidyverse)

unique(ARD_final$survey_type)

ARD_baseline <- ARD_final %>% 
  filter(survey_type == "baseline recruit")
write.csv(ARD_baseline,"ARD_baseline.csv")

ARD_recruit <- ARD_final %>% 
  filter(survey_type =="recruit")
write.csv(ARD_recruit,"ARD_recruit.csv")

# Turning df into Wide form Matrix ----------------------------------------

ARD_baseline_matrix <- ARD_baseline %>%
  select(treatment, complexity, common_name, presence)
# 
# library(dplyr)
# ARD_baseline_matrix <- rownames_to_column(ARD_baseline_matrix, "ID2")
# 
# 
# ARD_baseline_matrix %>% 
#   spread(key = ID2, value = presence)


write.csv(ARD_baseline_matrix, "ARD_matrix.csv")
##exported to excel and used pivot table to make distance matrix


# round 1 nmds - using code straight from class --------------------------------------------------

#imported ARD_matrix_common_name

#add rownames and create separate df for treatment labels we will need later

treatlabels <- ARD_matrix_common_name[,1:2]
ARD_base_dat <- ARD_matrix_common_name[,3:34]
head(treatlabels)
head(ARD_base_dat)

library(ecodist)
bcARD = bcdist(ARD_base_dat)

nmds_out = nmds(bcARD, mindim = 2, maxdim = 2)
scores = nmds.min(nmds_out)
nmds_out$stress

sort(unique(treatlabels$Treatment))
mycol = c("green", "yellow", "purple", "blue", "orange","red")

plot(scores,
     pch = 19, 
     col = mycol[treatlabels$Treatment])
text(scores,
     labels = treatlabels$Complexity)
vectors = vf(scores, bcARD, nperm = 10)
plot(vectors,
     col = "black")

library(vegan)
mycol = c("green", "yellow", "purple", "blue", "orange","red")
ordiellipse(scores,
            treatlabels$Treatment,
            conf = 0.6, 
            col = mycol)

#not realy sure what any of this means...


# trial 2 nmds of recruit data - plot by cluster ------------------------------------------

ARD_recruit_pivot <- ARD_recruit %>%
  select(plot_grid, treatment, complexity, common_name, presence) 

ARD_recruit_pivot$treat_comp <- paste(ARD_recruit_pivot$treatment, "_", ARD_recruit_pivot$complexity)
#make a new col that is treatment and complexity

write.csv(ARD_recruit_pivot,"ARD_recruit_pivot.csv")

#import recruit_pivot_lookup.csv to merge plot and grid cols, then export to create a lookup table
recruit_pivot_lookup$plot_grid <- paste(recruit_pivot_lookup$plot, "-", recruit_pivot_lookup$grid_numbers)

write.csv(recruit_pivot_lookup,"ARD_lookup.csv")

#import ARD_recruit_matrix.csv to run the nmds on

#making new df with treatment labels so nmds can run on just the data (save treatlabels for later)
treatlabels2 <- ARD_recruit_matrix[1:3]
ARD_matrix2 <- ARD_recruit_matrix[,4:50]
head(treatlabels2)
head(ARD_matrix2)

library(ecodist)
nmdsARD2 = bcdist(ARD_matrix2)

nmds_out2 = nmds(nmdsARD2, mindim = 2, maxdim = 2)
scores2 = nmds.min(nmds_out2)
nmds_out2$stress

sort(unique(treatlabels2$treatment))
mycol = c("green", "yellow", "purple", "blue", "orange","red")

plot(scores2,
     pch = 19)
ordiellipse(scores2,
            treatlabels2$treatment,
            conf = 0.6, 
            col = mycol)
vectors = vf(scores2, bcARD, nperm = 10)

plot(vectors,
     col = "black")
title(main = "NMDS overall recruit foreach treatment")
# text(scores,
# labels = treatlabels2$complexity)



# using metaMDS and vegan -------------------------------------------------

#continuing mar 2, 2020

library(tidyverse)
treatlabels3 <- ARD_recruit_matrix[1:3]
ARD_matrix3 <- ARD_recruit_matrix[,4:50]
head(treatlabels3)
head(ARD_matrix3)
treat3 <- treatlabels3[,2]
compl3 <- treatlabels3[,3]
head(compl3)
compl3 %>% 
  mutate(as.factor(complexity))

library(ecodist)
dm3 = sqrt(distance (ARD_matrix3, "bray"))

library(vegan)
ARDmeta.mds3 <- metaMDS(dm3, k = 2, trymax = 500)
ARDmeta.mds3

ARDmeta.mds <-metaMDS(ARD_matrix3,
                      distance = "bray",
                      k = 3,
                      trymax = 500)

# #playing with this plot
# mycol3 = c("orange", "purple", "blue","yellow", "red", "pink")
# mycol2 = c("green", "black")
# plot(ARDmeta.mds$points, pch = 19, col = mycol2[treatlabels3$complexity], xlim = c(-0.5, 0.3), ylim = c(-0.3, 0.3))
# text(ARDmeta.mds$points-1, labels = treatlabels3$plot_grid, xlim = c(-0.5, 0.3), ylim = c(-0.3, 0.3))
# vectors = vf(ARDmeta.mds$points, ARD_matrix3, nperm = 10)
# plot(vectors, len = 0.1, col = "black")
# mygroups = sort(unique(treatlabels3$complexity))


##YAS this NMDS plot is good code. adapt to use for each complexity and make a legend for ellipses. next: bin for time and size
# mycol1 = c("orange", "blue") -- for complexity only
# plot(ARDmeta.mds, xlim = c(-1, 2), ylim = c(-1, 1)) #type = "t")
ordiplot(ARDmeta.mds, type = "n", xlim = c(-1, 1.5), ylim = c(-1, 1))
orditorp(ARDmeta.mds, display = "sites", col = "grey", air = 1, xlim = c(-1, 1.5), ylim = c(-1, 1))
orditorp(ARDmeta.mds, display = "species", col = "black", air = 1, xlim = c(-1, 1.5), ylim = c(-1, 1))
title(main = "NMDS for overall recruit communities by complexity")
#ellipses for treatments:
treats = sort(unique(treatlabels3$treatment))
mycol3 = c("orange", "purple", "blue","yellow", "red", "pink")
for(i in 1:6) {
  ordiellipse(ARDmeta.mds$points, treatlabels3$treatment,
              conf = 0.6, col = mycol3[i],
              show.groups = treats[i])
}
#ellipses for complexities:
comp = sort(unique(treatlabels3$complexity))
mycol4 = c("orange", "green")
for(i in 1:2) {
  ordiellipse(ARDmeta.mds$points, treatlabels3$complexity,
              conf = 0.6, col = mycol4[i],
              show.groups = comp[i])
}
legend('topright', legend = unique(comp), col = mycol4, pch = 16)

# using ggplot to make nmds plots -----------------------------------------

ARD.mds.scores <- as.data.frame(scores(ARDmeta.mds))
#Using the scores function from vegan to extract the site scores and convert to a data.frame
ARD.mds.scores$site <- rownames(ARD.mds.scores) 
# create a column of site names, from the rownames of data.scores
ARD.mds.scores$complexity <- compl3$complexity #  add the grp variable created earlier
head(ARD.mds.scores)  #look at the data


ARD.species.scores <- as.data.frame(scores(ARDmeta.mds, "species"))  
#Using the scores function from vegan to extract the species scores and convert to a data.frame
ARD.species.scores$species <- rownames(ARD.species.scores) 
# create a column of species, from the rownames of species.scores
head(ARD.species.scores)  #look at the data

ggplot() + 
  geom_text(data=ARD.species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=ARD.mds.scores,aes(x=NMDS1,y=NMDS2,colour=complexity),size=3) + # add the point markers
  # geom_text(data=ARD.mds.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  # scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  
  theme_bw() +
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) +
  ordiellipse()


#code to try and make an ellipse object to add to ggplot

ord<-ordiellipse(ARD.mds.scores, MyMeta$amt, display = "sites", 
                 kind = "se", conf = 0.95, label = T)

# use this code for nmds for RE peters ------------------------------------


library(tidyverse)
library(ecodist)
library(vegan)

library(readr)
ARD_recruit_matrix <- read_csv("data/Art Recruit data/NMDS/trial 2 nmds/ARD_recruit_matrix.csv")
View(ARD_recruit_matrix)

treatlabels3 <- ARD_recruit_matrix[1:3]
ARD_matrix3 <- ARD_recruit_matrix[,4:50]
head(treatlabels3)
head(ARD_matrix3)
treat3 <- treatlabels3[,2]
compl3 <- treatlabels3[,3]
head(compl3)
compl3 %>% 
  mutate(as.factor(complexity))

library(ecodist)
dm3 = sqrt(distance (ARD_matrix3, "bray"))

library(vegan)
ARDmeta.mds3 <- metaMDS(dm3, k = 2, trymax = 500)
ARDmeta.mds3

ARDmeta.mds <-metaMDS(ARD_matrix3,
                      distance = "bray",
                      k = 3,
                      trymax = 500)


ordiplot(ARDmeta.mds, type = "n", xlim = c(-1, 1.5), ylim = c(-1, 1))
orditorp(ARDmeta.mds, display = "sites", col = "grey", air = 1, xlim = c(-1, 1.5), ylim = c(-1, 1))
orditorp(ARDmeta.mds, display = "species", col = "black", air = 1, xlim = c(-1, 1.5), ylim = c(-1, 1))
title(main = "NMDS for overall recruit communities by compl")
#ellipses for treatments:
treats = sort(unique(treatlabels3$treatment))
mycol3 = c("orange", "purple", "blue","yellow", "red", "pink")
for(i in 1:6) {
  ordiellipse(ARDmeta.mds$points, treatlabels3$treatment,
              conf = 0.6, col = mycol3[i],
              show.groups = treats[i])
}
legend('topright', legend = unique(comp), col = mycol3, pch = 16)
#ellipses for complexities:
comp = sort(unique(treatlabels3$complexity))
mycol4 = c("orange", "green")
for(i in 1:2) {
  ordiellipse(ARDmeta.mds$points, treatlabels3$complexity,
              conf = 0.6, col = mycol4[i], fill = TRUE, fill.alpha = 0.3,
              show.groups = comp[i])
}
legend('topright', legend = unique(comp), col = mycol4, pch = 16)
