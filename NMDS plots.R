## NMDS  over time ##


# making distance matrices --------------------------------------------------



ARD_start_matrix_5 <- ARD_recruit_start %>%
  filter(TL < 6) %>% 
  select(plot_grid, treatment, complexity, common_name, presence) 

write.csv(ARD_start_matrix_5, "ARD_start_matrix_5.csv")
##exported to excel and used pivot table to make distance matrix

#use ARD_lookup to put treatment and complexity values with proper cluster values after making distance matrix

# code for overall nmds plots  --------------------------------------------


library(tidyverse)
library(ecodist)
library(vegan)
library(readr)
# install.packages("xlsx")
library(xlsx)
library(openxlsx)

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

# library(ecodist)
dm3 = sqrt(distance (ARD_matrix3, "bray"))

# library(vegan)
ARDmeta.mds3 <- metaMDS(dm3, k = 2, trymax = 500)
ARDmeta.mds3

ARDmeta.mds <-metaMDS(ARD_matrix3,
                      distance = "bray",
                      k = 2,
                      trymax = 500)


ordiplot(ARDmeta.mds, type = "n", xlim = c(-1, 1.5), ylim = c(-1, 1))
orditorp(ARDmeta.mds, display = "sites", col = "grey", air = 1, xlim = c(-1, 1.5), ylim = c(-1, 1))
orditorp(ARDmeta.mds, display = "species", col = "black", air = 1, xlim = c(-1, 1.5), ylim = c(-1, 1))
title(main = "NMDS for overall recruit communities by treatment")
#ellipses for treatments:
treats = sort(unique(treatlabels3$treatment))
mycol3 = c("pink", "blue", "orange","yellow", "green", "red")
for(i in 1:6) {
  ordiellipse(ARDmeta.mds$points, treatlabels3$treatment,
              conf = 0.6, col = mycol3[i],
              show.groups = treats[i])
}
legend('topright', legend = unique(treats), col = mycol3, pch = 16)
#ellipses for complexities:
comp = sort(unique(treatlabels3$complexity))
mycol4 = c("orange", "green")
for(i in 1:2) {
  ordiellipse(ARDmeta.mds$points, treatlabels3$complexity,
              conf = 0.6, col = mycol4[i], fill = TRUE, fill.alpha = 0.3,
              show.groups = comp[i])
}
legend('topright', legend = unique(comp), col = mycol4, pch = 16)



# start  - old code --------------------------------------------------------------

library(readr)
ARD_start_matrix <- read_csv("data/Art Recruit data/NMDS/RE Peters/ARD_start_matrix.csv")
View(ARD_start_matrix)

library(tidyverse)
library(ecodist)
library(vegan)
library(readr)

treatlabelsstart <- ARD_start_matrix[1:3]
ARD_start_m <- ARD_start_matrix[,4:32]
head(treatlabelsstart)
head(ARD_start_m)

treatlabelsstart_1 <- treatlabelsstart %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ARDstartmeta.mds <- metaMDS(ARD_start_m,
                            distance = "bray",
                            k = 3,
                            trymax = 500)
ARDstartmeta.mds


ordiplot(ARDstartmeta.mds, type = "n", xlim = c(-1, 1))
orditorp(ARDstartmeta.mds, display = "sites", col = "grey", air = 1, xlim = c(-1, 1))

#ellipses for treatments:
treats = sort(unique(treatlabelsstart_1$treatment))
mycol3 = c("pink", "orange", "yellow","green", "blue", "red")
for(i in 1:6) {
  ordiellipse(ARDstartmeta.mds$points, treatlabelsstart_1$treatment,
              conf = 0.6, col = mycol3[i],
              show.groups = treats[i])
}
# legend('topright', legend = unique(treats), col = mycol3, pch = 16)
orditorp(ARDstartmeta.mds, display = "species", col = "black", air = 1)
title(main = "Start")

#ellipses for complexities:
comp = sort(unique(treatlabelsstart$complexity))
mycol4 = c("orange", "green")
for(i in 1:2) {
  ordiellipse(ARDstartmeta.mds, treatlabelsstart$complexity,
              conf = 0.6, col = mycol4[i], fill = TRUE, fill.alpha = 0.3,
              show.groups = comp[i])
}
# legend('topright', legend = unique(comp), col = mycol4, pch = 16)
orditorp(ARDstartmeta.mds, display = "species", col = "black", air = 1)
title(main = "Start")


# nmds - middle - old code --------------------------------------------------------------

#imported ARD_recruit from "final clean data frames"

ARD_recruit$time_block = as.factor(ARD_recruit$time_block)

ARD_recruit_middle <- ARD_recruit %>% 
  filter(time_block == "middle")

ARD_middle_matrix_5 <- ARD_recruit_middle %>%
  filter(TL < 6) %>% 
  select(plot_grid, treatment, complexity, common_name, presence) 

write.csv(ARD_middle_matrix_5, "ARD_middle_matrix_5.csv")
##exported to excel and used pivot table to make distance matrix

#use ARD_lookup to put treatment and complexity values with proper cluster values after making distance matrix

ARD_middle_matrix <- read_csv("data/Art Recruit data/NMDS/RE Peters/ARD_middle_matrix.csv")
View(ARD_middle_matrix)

treatlabelsmiddle <- ARD_middle_matrix[1:3]
ARD_middle_m <- ARD_middle_matrix[,4:36]
head(treatlabelsmiddle)
head(ARD_middle_m)

treatlabelsmiddle_1 <- treatlabelsmiddle %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ARDmiddlemeta.mds <- metaMDS(ARD_middle_m,
                            distance = "bray",
                            k = 3,
                            trymax = 500)
ARDmiddlemeta.mds 


ordiplot(ARDmiddlemeta.mds , type = "n", xlim = c(-1, 1))
orditorp(ARDmiddlemeta.mds , display = "sites", col = "grey", air = 1, xlim = c(-1, 1))

#ellipses for treatments:
treats = sort(unique(treatlabelsmiddle_1$treatment))
mycol3 = c("pink", "orange", "yellow","green", "blue", "red")
for(i in 1:6) {
  ordiellipse(ARDmiddlemeta.mds $points, treatlabelsmiddle_1$treatment,
              conf = 0.6, col = mycol3[i],
              show.groups = treats[i])
}
# legend('topright', legend = unique(treats), col = mycol3, pch = 16)
orditorp(ARDmiddlemeta.mds , display = "species", col = "black", air = 1)
title(main = "Middle")


#ellipses for complexities:
comp = sort(unique(treatlabelsmiddle$complexity))
mycol4 = c("orange", "green")
for(i in 1:2) {
  ordiellipse(ARDmiddlemeta.mds , treatlabelsmiddle$complexity,
              conf = 0.6, col = mycol4[i], fill = TRUE, fill.alpha = 0.3,
              show.groups = comp[i])
}
# legend('topright', legend = unique(comp), col = mycol4, pch = 16)
orditorp(ARDmiddlemeta.mds , display = "species", col = "black", air = 1, xlim = c(-1, 1))
title(main = "Middle")



# nmds - end - old code--------------------------------------------------------------



#imported ARD_recruit from "final clean data frames"

ARD_recruit$time_block = as.factor(ARD_recruit$time_block)

ARD_recruit_end <- ARD_recruit %>% 
  filter(time_block == "end")

ARD_end_matrix_5 <- ARD_recruit_end %>%
  filter(TL < 6) %>% 
  select(plot_grid, treatment, complexity, common_name, presence) 

write.csv(ARD_end_matrix_5, "ARD_end_matrix_5.csv")
##exported to excel and used pivot table to make distance matrix

#use ARD_lookup to put treatment and complexity values with proper cluster values after making distance matrix

ARD_end_matrix <- read_csv("data/Art Recruit data/NMDS/RE Peters/ARD_end_matrix.csv")
View(ARD_end_matrix)

treatlabelsend <- ARD_end_matrix[1:3]
ARD_end_m <- ARD_end_matrix[,4:34]
head(treatlabelsend)
head(ARD_end_m)

treatlabelsend_1 <- treatlabelsend %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ARDendmeta.mds <- metaMDS(ARD_end_m,
                             distance = "bray",
                             k = 3,
                             trymax = 500)
ARDendmeta.mds 


ordiplot(ARDendmeta.mds , type = "n")
orditorp(ARDendmeta.mds , display = "sites", col = "grey", air = 1)

#ellipses for treatments:
treats = sort(unique(treatlabelsend_1$treatment))
mycol3 = c("pink", "orange", "yellow","green", "blue", "red")
for(i in 1:6) {
  ordiellipse(ARDendmeta.mds$points, treatlabelsend_1$treatment,
              conf = 0.6, col = mycol3[i],
              show.groups = treats[i])
}
legend('topright', legend = unique(treats), col = mycol3, pch = 16)
orditorp(ARDendmeta.mds , display = "species", col = "black", air = 1)
title(main = "End")


#ellipses for complexities:
comp = sort(unique(treatlabelsend$complexity))
mycol4 = c("orange", "green")
for(i in 1:2) {
  ordiellipse(ARDendmeta.mds , treatlabelsend$complexity,
              conf = 0.6, col = mycol4[i], fill = TRUE, fill.alpha = 0.3,
              show.groups = comp[i])
}
legend('topright', legend = unique(comp), col = mycol4, pch = 16)
orditorp(ARDendmeta.mds , display = "species", col = "black", air = 1)
title(main = "End")


# some better code for making ellipses in base ------------------------------------


library(readr)
ARD_start_matrix <- read_csv("data/Art Recruit data/NMDS/RE Peters/ARD_start_matrix.csv")
View(ARD_start_matrix)

library(tidyverse)
library(ecodist)
library(vegan)
library(readr)

treatlabelsstart <- ARD_start_matrix[1:3]
ARD_start_m <- ARD_start_matrix[,4:32]
head(treatlabelsstart)
head(ARD_start_m)

treatlabelsstart_1 <- treatlabelsstart %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ARDstartmeta.mds <- metaMDS(ARD_start_m,
                            distance = "bray",
                            k = 3,
                            trymax = 500)
ARDstartmeta.mds
#could try "try" instead of "trymax" and it will run all permutations then give best out-put


#treatment:
ordiplot(ARDstartmeta.mds, type = "n", xlim = c(-1, 1))
orditorp(ARDstartmeta.mds, display = "sites", col = "grey", air = 1, xlim = c(-1, 1))
treats = sort(unique(treatlabelsstart_1$treatment))
mycol3 = c("pink", "orange", "yellow","green", "blue", "red")
ordiellipse(ARDstartmeta.mds$points,
            treatlabelsstart_1$treatment,
            conf = 0.6, col = mycol3,
            show.groups = treats,
            cex = 10)
orditorp(ARDstartmeta.mds, display = "species", col = "black", air = 1)
title(main = "Start")



#complexity:
ordiplot(ARDstartmeta.mds, type = "n", xlim = c(-1, 1))
orditorp(ARDstartmeta.mds, display = "sites", col = "grey", air = 1, xlim = c(-1, 1))
comp = sort(unique(treatlabelsstart$complexity))
mycol4 = c("orange", "green")
ordiellipse(ARDstartmeta.mds, 
            treatlabelsstart$complexity,
            conf = 0.6, 
            col = mycol4, 
            show.groups = comp)
orditorp(ARDstartmeta.mds, display = "species", col = "black", air = 1)
title(main = "Start")



# NMDS code from emily - ggplot -------------------------------------------


library(vegan)
library(ecodist)
library(RVAideMemoire)

nametab<-na.omit(metab)
nametab$NEP<-nametab$GPP/(abs(nametab$ER))
#Distance Matrix
dm<-distance(nametab[,c(8,9)], "mahal") # calculates squared Mahalanobis
?metaMDS
#metaMDS
nmds_out<-metaMDS(dm)
nmds_out$stress
summary(nmds_out)
mdsscores<-nmds_out$points

#Check significance
output<-adonis(dm~Ecoregion, data=nametab, distance="mahal") #or monhalibis
output
mrpp(dm, group=nametab$Ecoregion, distance="mahal")
adonis(dm~nametab$Ecoregion, distance="mahal")

?adonis
pairwise.perm.manova(dm, fact=nametab$Ecoregion, p.method="holm")

library(ggplot2)
scoresdata<-cbind(nametab, mdsscores)
ggplot(data=scoresdata, aes(x=MDS1, y=MDS2, color=Ecoregion, fill=Ecoregion)) +  geom_point(size=3) +
  geom_point(shape = 1,size = 3,colour = "black") +
  scale_color_manual(values=cols) +
  scale_fill_manual(values=cols)+
  stat_ellipse(type = "norm", geom="polygon", alpha=0.3) +
  theme1


# nmds in ggplot ----------------------------------------------------------


library(vegan)
library(ecodist)
library(RVAideMemoire)

treatlabels <- treatlabelsstart %>% 
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ARD_start_m

#try running with just vegan then getting output from this
ARDstartmeta.mds <- metaMDS(ARD_start_m,
                            distance = "bray",
                            k = 3,
                            trymax = 500)
nmds_out_start_test <- ARDstartmeta.mds
nmds_out_start_test$stress
nmdsscores.start_test <- nmds_out_start_test$points
view(nmdsscores.start_test)

species.scores.start <- as.data.frame(scores(ARDstartmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.start$species <- rownames(species.scores.start)  # create a column of species, from the rownames of species.scores
view(species.scores.start)  #look at the data

library(ggplot2)
scoresdata.start.test<-cbind(treatlabels, nmdsscores.start_test)
view(scoresdata.start.test)

#code for "start" ndms plot for treatment
ggplot() +  
  stat_ellipse(data = scoresdata.start.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.start.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.start.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  geom_text_repel(data = species.scores.start,
            aes(x =  NMDS1,
                y = NMDS2,
                label = species),
          alpha = 0.8,
          size = 5) +
  theme_classic() +
  ggtitle("Start") +
  theme(
    plot.title = element_text(size = 27),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_blank(),
    legend.title = element_blank()
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  ) 


#Distance Matrix - code from emily
# dm.start<-distance(ARD_start_m, "bray") # calculates squared bray
# ?metaMDS
# #metaMDS
# nmds_out_start <- metaMDS(dm.start)
# nmds_out_start$stress
# summary(nmds_out_start)
# mdsscores.start<-nmds_out_start$points

#Check significance
# output_start<-adonis(dm~Ecoregion, data=nametab, distance="mahal") #or monhalibis
# output
# mrpp(dm, group=nametab$Ecoregion, distance="mahal")
# adonis(dm~nametab$Ecoregion, distance="mahal")
# 
# ?adonis
# pairwise.perm.manova(dm, fact=nametab$Ecoregion, p.method="holm")

pal <- pnw_palette(6, name = "Starfish", type = "continuous")

library(ggplot2)
scoresdata.start<-cbind(treatlabels, mdsscores.start)
ggplot(data=scoresdata.start, 
       aes(x = MDS1, 
           y= MDS2, 
           color = treatment, 
           fill = treatment)) +  
  geom_point(size = 2) +
  geom_point(shape = 1,
             size = 2,
             colour = "black") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal)+
  stat_ellipse(type = "norm", 
               geom="polygon", 
               alpha=0.1) +
  theme_classic()

pal5 <-pnw_palette(2, name = "Bay", type = "continuous")

library(ggplot2)
scoresdata.start<-cbind(treatlabels, mdsscores.start)
ggplot(data = scoresdata.start, 
       aes(x = MDS1, 
           y= MDS2, 
           color = complexity, 
           fill = complexity)) +  
  geom_point(size = 2) +
  geom_point(shape = 1,
             size = 2,
             colour = "black") +
  scale_color_manual(values = pal5) +
  scale_fill_manual(values = pal5)+
  stat_ellipse(type = "norm", 
               geom = "polygon", 
               alpha = 0.3) +
  theme_classic()



# nmds in ggplot - Start, with permanova code --------------------------------------------------

library(vegan)

#ARD_start_matrix is in Art Recruit data --> NMDS -> matrices with ggplot (used to be called RE Peters)
ARD_start_matrix <- read_csv("data/Art Recruit data/NMDS/matrices with ggplot/ARD_start_matrix.csv")

treatlabelsstart <- ARD_start_matrix[1:3]
ARD_start_m <- ARD_start_matrix[,4:32]
head(treatlabelsstart)#labels
head(ARD_start_m)#species matrix

treatlabels <- treatlabelsstart %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

treatlabels$treatment_complexity <- paste(treatlabels$treatment, " ",treatlabels$complexity)
head(treatlabels)#treatment labels


#run metaMDS from vegan
ARDstartmeta.mds <- metaMDS(ARD_start_m,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_start_m~treatment, data=treatlabels, distance="bray")
adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_start_test <- ARDstartmeta.mds #name output as an object
nmds_out_start_test$stress #gives stress value
nmdsscores.start_test <- nmds_out_start_test$points 
view(nmdsscores.start_test)

species.scores.start <- as.data.frame(scores(ARDstartmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.start$species <- rownames(species.scores.start)  # create a column of species, from the rownames of species.scores
view(species.scores.start)  #check new data frame

library(ggplot2)
scoresdata.start.test<-cbind(treatlabels, nmdsscores.start_test) #bind treatment labels and score values
view(scoresdata.start.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.start.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.start.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.start.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  geom_text_repel(data = species.scores.start,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 5) +
  theme_classic() +
  ggtitle("Start") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
    )

#treatment_complexity plot
ggplot() +  
  stat_ellipse(data = scoresdata.start.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment_complexity, 
                   fill = treatment_complexity), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.start.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment_complexity, 
                 fill = treatment_complexity), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.start.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment_complexity,
                 fill = treatment_complexity),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment and Complexity") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment and Complexity") +
  geom_text_repel(data = species.scores.start,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 5) +
  theme_classic() +
  ggtitle("Start") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    # legend.position = "none"
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )


#complexity plot
ggplot() +  
  stat_ellipse(data = scoresdata.start.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = complexity, 
                   fill = complexity), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.start.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = complexity, 
                 fill = complexity), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.start.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = complexityt,
                 fill = complexity),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Complexity") +
  scale_fill_viridis(discrete = TRUE, name = "Complexity") +
  geom_text_repel(data = species.scores.start,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 5) +
  theme_classic() +
  ggtitle("Start") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )


# nmds in ggplot - Middle -------------------------------------------------


library(vegan)

treatlabels <- treatlabelsstart %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ARD_middle_m #my species matrix

#run metaMDS from vegan
ARD.miiddle.meta.mds <- metaMDS(ARD_middle_m,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

nmds_out_middle <- ARD.miiddle.meta.mds #name output as an object
nmds_out_middle$stress #gives stress value
nmdsscores.middle <- nmds_out_middle$points 
view(nmdsscores.middle)

species.scores.middle <- as.data.frame(scores(ARD.miiddle.meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.middle$species <- rownames(species.scores.middle)  # create a column of species, from the rownames of species.scores
view(species.scores.middle)  #look at the data

library(ggplot2)
scoresdata.middle<-cbind(treatlabels, nmdsscores.middle) #bind treatment labels and score values
view(scoresdata.middle)

#Treatment
ggplot() +  
  stat_ellipse(data = scoresdata.middle, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.middle, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.middle,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  geom_text_repel(data = species.scores.middle,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 5) +
  theme_classic() +
  ggtitle("Middle") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )

#complexity
ggplot() +  
  stat_ellipse(data = scoresdata.middle, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = complexity, 
                   fill = complexity), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.middle, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = complexity, 
                 fill = complexity), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.middle,
             aes(x = MDS1,
                 y = MDS2,
                 color = complexityt,
                 fill = complexity),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Complexity") +
  scale_fill_viridis(discrete = TRUE, name = "Complexity") +
  geom_text_repel(data = species.scores.middle,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 5) +
  theme_classic() +
  ggtitle("Middle") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )




# nmds in ggplot - End ----------------------------------------------------




library(vegan)

treatlabels <- treatlabelsstart %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

ARD_end_m #my species matrix
view(ARD_end_m)

#run metaMDS from vegan
ARD.end.meta.mds <- metaMDS(ARD_end_m,
                                distance = "bray",
                                k = 3,
                                trymax = 500)

nmds_out_end <- ARD.end.meta.mds #name output as an object
nmds_out_end$stress #gives stress value
nmdsscores.end <- nmds_out_end$points 
view(nmdsscores.end)

species.scores.end <- as.data.frame(scores(ARD.end.meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.end$species <- rownames(species.scores.end)  # create a column of species, from the rownames of species.scores
view(species.scores.end)  #look at the data

species.scores.middle <- as.data.frame(scores(ARD.miiddle.meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.middle$species <- rownames(species.scores.middle)  # create a column of species, from the rownames of species.scores
view(species.scores.middle) 

library(ggplot2)
scoresdata.end<-cbind(treatlabels, nmdsscores.end) #bind treatment labels and score values
view(scoresdata.end)

#Treatment
ggplot() +  
  stat_ellipse(data = scoresdata.end, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.end, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.end,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  geom_text_repel(data = species.scores.end,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 5) +
  theme_classic() +
  ggtitle("End") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )

#complexity
ggplot() +  
  stat_ellipse(data = scoresdata.end, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = complexity, 
                   fill = complexity), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.end, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = complexity, 
                 fill = complexity), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.end,
             aes(x = MDS1,
                 y = MDS2,
                 color = complexityt,
                 fill = complexity),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Complexity") +
  scale_fill_viridis(discrete = TRUE, name = "Complexity") +
  geom_text_repel(data = species.scores.end,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 5) +
  theme_classic() +
  ggtitle("End") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )


# NMDS each time_block ready, ellipsed by treatment -----------------------


# nmds Start Low ---------------------------------------------------------


ARD_start_pivot_3 <- ARD_3 %>%
  filter(time_block == "start") %>% 
  select(plot_grid, treatment, complexity, common_name, presence) 

write.csv(ARD_start_pivot_3, "ARD_start_pivot_3.csv")
##exported to excel and used pivot table to make distance matrix

#use ARD_lookup to put treatment and complexity values with proper cluster values after making distance matrix

ARD_3_L_start_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/committee meeting/ARD_3_L_start_matrix.csv")
view(ARD_3_L_start_matrix)

treatlabelsstart_L <- ARD_3_L_start_matrix[1:3]
ARD_start_m_L <- ARD_3_L_start_matrix[,4:28]
head(treatlabelsstart_L)#labels
head(ARD_start_m_L)#species matrix

treatlabels <- treatlabelsstart_L %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

treatlabels$treatment_complexity <- paste(treatlabels$treatment, " ",treatlabels$complexity)
head(treatlabels)#treatment labels

library(vegan)
#run metaMDS from vegan
ARDstartLmeta.mds <- metaMDS(ARD_start_m_L,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_start_m_L~treatment, data=treatlabels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_startL_test <- ARDstartLmeta.mds #name output as an object
nmds_out_startL_test$stress #gives stress value
nmdsscores.startL_test <- nmds_out_startL_test$points 
view(nmdsscores.startL_test)

species.scores.startL <- as.data.frame(scores(ARDstartLmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.startL$species <- rownames(species.scores.startL)  # create a column of species, from the rownames of species.scores
view(species.scores.startL)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.startL.test<-cbind(treatlabels, nmdsscores.startL_test) #bind treatment labels and score values
view(scoresdata.startL.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.startL.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.startL.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.startL.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("Start Low") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )


# Start High --------------------------------------------------------------

ARD_3_H_start_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/committee meeting/ARD_3_H_start_matrix.csv")
View(ARD_3_H_start_matrix)

treatlabelsstart_H <- ARD_3_H_start_matrix[1:3]
ARD_start_m_H <- ARD_3_H_start_matrix[,4:28]
head(treatlabelsstart_H)#labels
head(ARD_start_m_H)#species matrix

treatlabels <- treatlabelsstart_H %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

treatlabels$treatment_complexity <- paste(treatlabels$treatment, " ",treatlabels$complexity)
head(treatlabels)#treatment labels

library(vegan)
#run metaMDS from vegan
ARDstartHmeta.mds <- metaMDS(ARD_start_m_H,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_start_m_H~treatment, data=treatlabels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_startH_test <- ARDstartHmeta.mds #name output as an object
nmds_out_startH_test$stress #gives stress value
nmdsscores.startH_test <- nmds_out_startH_test$points 
view(nmdsscores.startH_test)

species.scores.startH <- as.data.frame(scores(ARDstartHmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.startH$species <- rownames(species.scores.startH)  # create a column of species, from the rownames of species.scores
view(species.scores.startH)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.startH.test<-cbind(treatlabels, nmdsscores.startH_test) #bind treatment labels and score values
view(scoresdata.startH.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.startH.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.startH.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.startH.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  # geom_text_repel(data = species.scores.startH,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("Start High") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )


# Middle Low --------------------------------------------------------------

ARD_middle_pivot_3 <- ARD_3 %>%
  filter(time_block == "middle") %>% 
  select(plot_grid, treatment, complexity, common_name, presence) 

write.csv(ARD_middle_pivot_3, "ARD_middle_pivot_3.csv")
##exported to excel and used pivot table to make distance matrix

#use ARD_lookup to put treatment and complexity values with proper cluster values after making distance matrix

ARD_3_L_middle_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/committee meeting/ARD_3_middle_L_matrix.csv")
view(ARD_3_L_middle_matrix)

treatlabelsmiddle_L <- ARD_3_L_middle_matrix[1:3]
ARD_middle_m_L <- ARD_3_L_middle_matrix[,4:30]
head(treatlabelsmiddle_L)#labels
head(ARD_middle_m_L)#species matrix

treatlabels <- treatlabelsmiddle_L %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

treatlabels$treatment_complexity <- paste(treatlabels$treatment, " ",treatlabels$complexity)
head(treatlabels)#treatment labels

library(vegan)
#run metaMDS from vegan
ARDmiddleLmeta.mds <- metaMDS(ARD_middle_m_L,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_middle_m_L~treatment, data=treatlabels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_middleL_test <- ARDmiddleLmeta.mds #name output as an object
nmds_out_middleL_test$stress #gives stress value
nmdsscores.middleL_test <- nmds_out_middleL_test$points 
view(nmdsscores.middleL_test)

species.scores.middleL <- as.data.frame(scores(ARDmiddleLmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.middleL$species <- rownames(species.scores.middleL)  # create a column of species, from the rownames of species.scores
view(species.scores.middleL)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.middleL.test<-cbind(treatlabels, nmdsscores.middleL_test) #bind treatment labels and score values
view(scoresdata.middleL.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.middleL.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.middleL.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.middleL.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  geom_text_repel(data = species.scores.startL,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 4) +
  theme_classic() +
  ggtitle("Middle Low") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )


# Middle High -------------------------------------------------------------

ARD_3_H_middle_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/committee meeting/ARD_3_middle_L_matrix.csv")
head(ARD_3_H_middle_matrix)

treatlabelsmiddle_H <- ARD_3_H_middle_matrix[1:3]
ARD_middle_m_H <- ARD_3_H_middle_matrix[,4:30]
head(treatlabelsmiddle_H)#labels
head(ARD_middle_m_H)#species matrix

treatlabels <- treatlabelsmiddle_H %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

treatlabels$treatment_complexity <- paste(treatlabels$treatment, " ",treatlabels$complexity)
head(treatlabels)#treatment labels

library(vegan)
#run metaMDS from vegan
ARDmiddleHmeta.mds <- metaMDS(ARD_middle_m_H,
                              distance = "bray",
                              k = 3,
                              trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_middle_m_H~treatment, data=treatlabels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_middleH_test <- ARDmiddleHmeta.mds #name output as an object
nmds_out_middleH_test$stress #gives stress value
nmdsscores.middleH_test <- nmds_out_middleH_test$points 
view(nmdsscores.middleH_test)

species.scores.middleH <- as.data.frame(scores(ARDmiddleHmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.middleH$species <- rownames(species.scores.middleH)  # create a column of species, from the rownames of species.scores
view(species.scores.middleH)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.middleH.test<-cbind(treatlabels, nmdsscores.middleH_test) #bind treatment labels and score values
view(scoresdata.middleH.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.middleH.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.middleH.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.middleH.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("Middle High") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none"
    # legend.title = element_text(size = 20),
    # legend.text = element_text(size = 20)
  )


# End Low -----------------------------------------------------------------

ARD_end_pivot_3 <- ARD_3 %>%
  filter(time_block == "end") %>% 
  select(plot_grid, treatment, complexity, common_name, presence) 

write.csv(ARD_end_pivot_3, "ARD_end_pivot_3.csv")
##exported to excel and used pivot table to make distance matrix

#use ARD_lookup to put treatment and complexity values with proper cluster values after making distance matrix

ARD_3_L_end_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/committee meeting/ARD_3_L_end_matrix.csv")
View(ARD_3_L_end_matrix)

treatlabelsend_L <- ARD_3_L_end_matrix[1:3]
ARD_end_m_L <- ARD_3_L_end_matrix[,4:29]
head(treatlabelsend_L)#labels
head(ARD_end_m_L)#species matrix

treatlabels <- treatlabelsend_L %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

treatlabels$treatment_complexity <- paste(treatlabels$treatment, " ",treatlabels$complexity)
head(treatlabels)#treatment labels

library(vegan)
#run metaMDS from vegan
ARDendLmeta.mds <- metaMDS(ARD_end_m_L,
                              distance = "bray",
                              k = 3,
                              trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_end_m_L~treatment, data=treatlabels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_endL_test <- ARDendLmeta.mds #name output as an object
nmds_out_endL_test$stress #gives stress value
nmdsscores.endL_test <- nmds_out_endL_test$points 
view(nmdsscores.endL_test)

species.scores.endL <- as.data.frame(scores(ARDendLmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.endL$species <- rownames(species.scores.endL)  # create a column of species, from the rownames of species.scores
view(species.scores.middleL)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.endL.test<-cbind(treatlabels, nmdsscores.endL_test) #bind treatment labels and score values
view(scoresdata.endL.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.endL.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.endL.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.endL.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("End Low") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    # legend.position = "none"
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )


# End High ----------------------------------------------------------------


ARD_3_H_end_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/committee meeting/ARD_3_H_end_matrix.csv")
View(ARD_3_H_end_matrix)

treatlabelsend_H <- ARD_3_H_end_matrix[1:3]
ARD_end_m_H <- ARD_3_H_end_matrix[,4:29]
head(treatlabelsend_H)#labels
head(ARD_end_m_H)#species matrix

treatlabels <- treatlabelsend_H %>% #my treatment labels df
  mutate(treatment = factor(treatment, levels = c("100A", "30L70A", "50L50A", "70L30A", "100L", "control"),
                            labels = c("0%", "30%", "50%", "70%", "100%", "control")))

treatlabels$treatment_complexity <- paste(treatlabels$treatment, " ",treatlabels$complexity)
head(treatlabels)#treatment labels

library(vegan)
#run metaMDS from vegan
ARDendHmeta.mds <- metaMDS(ARD_end_m_H,
                           distance = "bray",
                           k = 3,
                           trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_end_m_H~treatment, data=treatlabels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_endH_test <- ARDendHmeta.mds #name output as an object
nmds_out_endH_test$stress #gives stress value
nmdsscores.endH_test <- nmds_out_endH_test$points 
view(nmdsscores.endH_test)

species.scores.endH <- as.data.frame(scores(ARDendHmeta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.endH$species <- rownames(species.scores.endH)  # create a column of species, from the rownames of species.scores
view(species.scores.middleH)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.endH.test<-cbind(treatlabels, nmdsscores.endH_test) #bind treatment labels and score values
view(scoresdata.endH.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.endH.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = treatment, 
                   fill = treatment), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.endH.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = treatment, 
                 fill = treatment), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.endH.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = treatment,
                 fill = treatment),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Treatment") +
  scale_fill_viridis(discrete = TRUE, name = "Treatment") +
  geom_text_repel(data = species.scores.startL,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  alpha = 0.8,
                  size = 4) +
  theme_classic() +
  ggtitle("End High") +
  theme(
    plot.title = element_text(size = 27),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    # legend.position = "none"
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )


# NMDS each treat/comp separately, ellipses for time period ---------------



# control High ------------------------------------------------------------

ARD_C_H_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "control") %>% 
  filter(complexity == "High") %>% 
  select(plot_grid, time_block, common_name, presence) 

write.xlsx(ARD_C_H_pivot_3, "ARD_C_H_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_C_H_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/High/ARD_3_C_H_matrix.csv")

ARD_3_C_H_labels <- ARD_3_C_H_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_C_H <- ARD_3_C_H_matrix[,3:19]
head(ARD_3_C_H_labels)#labels
head(ARD_3_C_H)#species matrix

library(vegan)
#run metaMDS from vegan
ARD_C_H_meta.mds <- metaMDS(ARD_3_C_H,
                           distance = "bray",
                           k = 3,
                           trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_3_C_H ~ time_block, data = ARD_3_C_H_labels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_C_H_test <- ARD_C_H_meta.mds #name output as an object
nmds_out_C_H_test$stress #gives stress value
nmdsscores._C_H_test <- nmds_out_C_H_test$points 
view(nmdsscores._C_H_test)

species.scores.C_H <- as.data.frame(scores(ARD_C_H_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.C_H$species <- rownames(species.scores.C_H)  # create a column of species, from the rownames of species.scores
view(species.scores.C_H)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.C_H.test<-cbind(ARD_3_C_H_labels, nmdsscores._C_H_test) #bind treatment labels and score values
view(scoresdata.C_H.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.C_H.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.C_H.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.C_H.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("Control High, R2 = 0.0148, p = 0.99") +
  theme(
    plot.title = element_text(size = 19),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 0% High -----------------------------------------------------------------

ARD_0_H_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "0%") %>% 
  filter(complexity == "High") %>% 
  select(plot_grid, time_block, common_name, presence) 

write.xlsx(ARD_0_H_pivot_3, "ARD_0_H_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_0_H_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/High/ARD_3_0_H_matrix.csv")

ARD_3_0_H_labels <- ARD_3_0_H_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_0_H <- ARD_3_0_H_matrix[,3:16]
head(ARD_3_0_H_labels)#labels
head(ARD_3_0_H)#species matrix

library(vegan)
#run metaMDS from vegan
ARD_0_H_meta.mds <- metaMDS(ARD_3_0_H,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_3_0_H ~ time_block, data = ARD_3_0_H_labels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_0_H_test <- ARD_0_H_meta.mds #name output as an object
nmds_out_0_H_test$stress #gives stress value
nmdsscores._0_H_test <- nmds_out_0_H_test$points 
# view(nmdsscores._0_H_test)

species.scores.0_H <- as.data.frame(scores(ARD_0_H_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.0_H$species <- rownames(species.scores.0_H)  # create a column of species, from the rownames of species.scores
# view(species.scores.0_H)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.0_H.test<-cbind(ARD_3_0_H_labels, nmdsscores._0_H_test) #bind treatment labels and score values
# view(scoresdata.0_H.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.0_H.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.0_H.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.0_H.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("0% High, R2 = 0.0669, p = 0.61") +
  theme(
    plot.title = element_text(size = 19),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 30% High ----------------------------------------------------------------

ARD_30_H_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "30%") %>% 
  filter(complexity == "High") %>% 
  select(plot_grid, time_block, common_name, presence) 

write.xlsx(ARD_30_H_pivot_3, "ARD_30_H_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_30_H_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/High/ARD_3_30_H_matrix.csv")

ARD_3_30_H_labels <- ARD_3_30_H_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_30_H <- ARD_3_30_H_matrix[,3:20]
head(ARD_3_30_H_labels)#labels
head(ARD_3_30_H)#species matrix

library(vegan)
#run metaMDS from vegan
ARD_30_H_meta.mds <- metaMDS(ARD_3_30_H,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_3_30_H ~ time_block, data = ARD_3_30_H_labels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_30_H_test <- ARD_30_H_meta.mds #name output as an object
nmds_out_30_H_test$stress #gives stress value
nmdsscores._30_H_test <- nmds_out_30_H_test$points 
# view(nmdsscores._30_H_test)

species.scores.30_H <- as.data.frame(scores(ARD_30_H_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.30_H$species <- rownames(species.scores.30_H)  # create a column of species, from the rownames of species.scores
# view(species.scores.0_H)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.30_H.test<-cbind(ARD_3_30_H_labels, nmdsscores._30_H_test) #bind treatment labels and score values
# view(scoresdata.0_H.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.30_H.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.30_H.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.30_H.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("30% High, R2 = 0.0254, 0.99") +
  theme(
    plot.title = element_text(size = 19),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 50% High ----------------------------------------------------------------

ARD_50_H_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "50%") %>% 
  filter(complexity == "High") %>% 
  select(plot_grid, time_block, common_name, presence) 

write.xlsx(ARD_50_H_pivot_3, "ARD_50_H_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_50_H_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/High/ARD_3_50_H_matrix.csv")

ARD_3_50_H_labels <- ARD_3_50_H_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_50_H <- ARD_3_50_H_matrix[,3:23]
head(ARD_3_50_H_labels)#labels
head(ARD_3_50_H)#species matrix

library(vegan)
#run metaMDS from vegan
ARD_50_H_meta.mds <- metaMDS(ARD_3_50_H,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_3_50_H ~ time_block, data = ARD_3_50_H_labels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_50_H_test <- ARD_50_H_meta.mds #name output as an object
nmds_out_50_H_test$stress #gives stress value
nmdsscores._50_H_test <- nmds_out_50_H_test$points 
# view(nmdsscores._50_H_test)

species.scores.50_H <- as.data.frame(scores(ARD_50_H_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.50_H$species <- rownames(species.scores.50_H)  # create a column of species, from the rownames of species.scores
# view(species.scores.0_H)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.50_H.test<-cbind(ARD_3_50_H_labels, nmdsscores._50_H_test) #bind treatment labels and score values
# view(scoresdata.0_H.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.50_H.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.50_H.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.50_H.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("50% High, R2 = 0.0534, p = 0.79") +
  theme(
    plot.title = element_text(size = 19),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 70% High ----------------------------------------------------------------

ARD_70_H_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "70%") %>% 
  filter(complexity == "High") %>% 
  select(plot_grid, time_block, common_name, presence) 

write.xlsx(ARD_70_H_pivot_3, "ARD_70_H_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_70_H_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/High/ARD_3_70_H_matrix.csv")

ARD_3_70_H_labels <- ARD_3_70_H_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_70_H <- ARD_3_70_H_matrix[,3:22]
head(ARD_3_70_H_labels)#labels
head(ARD_3_70_H)#species matrix

library(vegan)
#run metaMDS from vegan
ARD_70_H_meta.mds <- metaMDS(ARD_3_70_H,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_3_70_H ~ time_block, data = ARD_3_70_H_labels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_70_H_test <- ARD_70_H_meta.mds #name output as an object
nmds_out_70_H_test$stress #gives stress value
nmdsscores._70_H_test <- nmds_out_70_H_test$points 
# view(nmdsscores._50_H_test)

species.scores.70_H <- as.data.frame(scores(ARD_70_H_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.70_H$species <- rownames(species.scores.70_H)  # create a column of species, from the rownames of species.scores
# view(species.scores.0_H)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.70_H.test<-cbind(ARD_3_70_H_labels, nmdsscores._70_H_test) #bind treatment labels and score values
# view(scoresdata.0_H.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.70_H.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.70_H.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.70_H.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("70% High, R2 = 0.0671, p = 0.67") +
  theme(
    plot.title = element_text(size = 19),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 100% High ---------------------------------------------------------------

ARD_100_H_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "100%") %>% 
  filter(complexity == "High") %>% 
  select(plot_grid, time_block, common_name, presence) 

write.xlsx(ARD_100_H_pivot_3, "ARD_100_H_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_100_H_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/High/ARD_3_100_H_matrix.csv")

ARD_3_100_H_labels <- ARD_3_100_H_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_100_H <- ARD_3_100_H_matrix[,3:20]
head(ARD_3_100_H_labels)#labels
head(ARD_3_100_H)#species matrix

library(vegan)
#run metaMDS from vegan
ARD_100_H_meta.mds <- metaMDS(ARD_3_100_H,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
# adonis(ARD_start_m~treatment_complexity, data=treatlabels, distance="bray")
# adonis(ARD_start_m~treatment*complexity, data=treatlabels, distance="bray")#treat_comp
adonis(ARD_3_100_H ~ time_block, data = ARD_3_100_H_labels, distance="bray")
# adonis(ARD_start_m~complexity, data=treatlabels, distance="bray")


#converting nmds output to objects
nmds_out_100_H_test <- ARD_100_H_meta.mds #name output as an object
nmds_out_100_H_test$stress #gives stress value
nmdsscores._100_H_test <- nmds_out_100_H_test$points 
# view(nmdsscores._100_H_test)

species.scores.100_H <- as.data.frame(scores(ARD_100_H_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.100_H$species <- rownames(species.scores.100_H)  # create a column of species, from the rownames of species.scores
# view(species.scores.0_H)  #check new data frame

library(ggplot2)
library(devtools)
library(ggrepel)
scoresdata.100_H.test<-cbind(ARD_3_100_H_labels, nmdsscores._100_H_test) #bind treatment labels and score values
# view(scoresdata.0_H.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.100_H.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.100_H.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.100_H.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.startL,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 4) +
  theme_classic() +
  ggtitle("100% High, R2 = 0.0470, p = 0.78") +
  theme(
    plot.title = element_text(size = 19),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )



# control Low -------------------------------------------------------------

ARD_C_L_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "control") %>% 
  filter(complexity == "Low") %>% 
  select(plot_grid, time_block, common_name, presence) 

write.xlsx(ARD_C_L_pivot_3, "ARD_C_L_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_C_L_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/Low/ARD_3_C_L_matrix.csv")

ARD_3_C_L_labels <- ARD_3_C_L_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_C_L <- ARD_3_C_L_matrix[,3:27]
head(ARD_3_C_L_labels)#labels
head(ARD_3_C_L)#species matrix

# library(vegan)
#run metaMDS from vegan
ARD_C_L_meta.mds <- metaMDS(ARD_3_C_L,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
adonis(ARD_3_C_L ~ time_block, data = ARD_3_C_L_labels, distance="bray")
# check assumption of homogeneity of multivariate dispersion (so p value should be bigger than 0.05)
distances_3_C_L <- vegdist(ARD_3_C_L)
anova(betadisper(distances_3_C_L, ARD_3_C_L_labels$time_block))

#converting nmds output to objects
nmds_out_C_L_test <- ARD_C_L_meta.mds #name output as an object
nmds_out_C_L_test$stress #gives stress value
nmdsscores._C_L_test <- nmds_out_C_L_test$points 
view(nmdsscores._C_L_test)

species.scores.C_L <- as.data.frame(scores(ARD_C_L_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.C_L$species <- rownames(species.scores.C_L)  # create a column of species, from the rownames of species.scores
view(species.scores.C_L)  #check new data frame

# library(ggplot2)
# library(devtools)
# library(ggrepel)
scoresdata.C_L.test<-cbind(ARD_3_C_L_labels, nmdsscores._C_L_test) #bind treatment labels and score values
view(scoresdata.C_L.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.C_L.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.C_L.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.C_L.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.C_L,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 3) +
  # geom_point(data = species.scores.C_L,
  #            aes(x =  NMDS1,
  #                y = NMDS2),
  #            alpha = 0.7,
  #            size = 1) +
  theme_classic() +
  ggtitle("Control Low, R2 = 0.0686, p = 0.68") +
  theme(
    plot.title = element_text(size = 19),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 0% Low ------------------------------------------------------------------

ARD_0_L_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "0%") %>% 
  filter(complexity == "Low") %>% 
  select(plot_grid, time_block, common_name, presence)

write.xlsx(ARD_0_L_pivot_3, "ARD_0_L_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_0_L_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/Low/ARD_3_0_L_matrix.csv")

ARD_3_0_L_labels <- ARD_3_0_L_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_0_L <- ARD_3_0_L_matrix[,3:19]
head(ARD_3_0_L_labels)#labels
head(ARD_3_0_L)#species matrix

# library(vegan)
#run metaMDS from vegan
ARD_0_L_meta.mds <- metaMDS(ARD_3_0_L,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
adonis(ARD_3_0_L ~ time_block, data = ARD_3_0_L_labels, distance="bray")
# check assumption of homogeneity of multivariate dispersion (so p value should be bigger than 0.05)
distances_3_0_L <- vegdist(ARD_3_0_L)
anova(betadisper(distances_3_0_L, ARD_3_0_L_labels$time_block))


#converting nmds output to objects
nmds_out_0_L_test <- ARD_0_L_meta.mds #name output as an object
nmds_out_0_L_test$stress #gives stress value
nmdsscores._0_L_test <- nmds_out_0_L_test$points 
view(nmdsscores._0_L_test)

species.scores.0_L <- as.data.frame(scores(ARD_0_L_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.0_L$species <- rownames(species.scores.0_L)  # create a column of species, from the rownames of species.scores
view(species.scores.0_L)  #check new data frame

# library(ggplot2)
# library(devtools)
# library(ggrepel)
scoresdata.0_L.test<-cbind(ARD_3_0_L_labels, nmdsscores._0_L_test) #bind treatment labels and score values
view(scoresdata.0_L.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.0_L.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.0_L.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.0_L.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.0_L,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 3) +
  theme_classic() +
  ggtitle("0% Low, R2 = 0.0334, p = 0.90") +
  theme(
    plot.title = element_text(size = 18),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )



# 30% Low -----------------------------------------------------------------

ARD_30_L_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "30%") %>% 
  filter(complexity == "Low") %>% 
  select(plot_grid, time_block, common_name, presence)

write.xlsx(ARD_30_L_pivot_3, "ARD_30_L_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_30_L_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/Low/ARD_3_30_L_matrix.csv")

ARD_3_30_L_labels <- ARD_3_30_L_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_30_L <- ARD_3_30_L_matrix[,3:19]
head(ARD_3_30_L_labels)#labels
head(ARD_3_30_L)#species matrix

# library(vegan)
#run metaMDS from vegan
ARD_30_L_meta.mds <- metaMDS(ARD_3_30_L,
                            distance = "bray",
                            k = 3,
                            trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
adonis(ARD_3_30_L ~ time_block, data = ARD_3_30_L_labels, distance="bray")
# check assumption of homogeneity of multivariate dispersion (so p value should be bigger than 0.05)
distances_3_30_L <- vegdist(ARD_3_30_L)
anova(betadisper(distances_3_30_L, ARD_3_30_L_labels$time_block))


#converting nmds output to objects
nmds_out_30_L_test <- ARD_30_L_meta.mds #name output as an object
nmds_out_30_L_test$stress #gives stress value
nmdsscores._30_L_test <- nmds_out_30_L_test$points 
view(nmdsscores._30_L_test)

species.scores.30_L <- as.data.frame(scores(ARD_30_L_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.30_L$species <- rownames(species.scores.30_L)  # create a column of species, from the rownames of species.scores
view(species.scores.30_L)  #check new data frame

# library(ggplot2)
# library(devtools)
# library(ggrepel)
scoresdata.30_L.test<-cbind(ARD_3_30_L_labels, nmdsscores._30_L_test) #bind treatment labels and score values
view(scoresdata.30_L.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.30_L.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.30_L.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.30_L.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.0_L,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 3) +
  theme_classic() +
  ggtitle("30% Low, R2 = 0.0733, p = 0.61") +
  theme(
    plot.title = element_text(size = 18),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )



# 50% Low -----------------------------------------------------------------

ARD_50_L_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "50%") %>% 
  filter(complexity == "Low") %>% 
  select(plot_grid, time_block, common_name, presence)

write.xlsx(ARD_50_L_pivot_3, "ARD_50_L_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_50_L_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/Low/ARD_3_50_L_matrix.csv")

ARD_3_50_L_labels <- ARD_3_50_L_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_50_L <- ARD_3_50_L_matrix[,3:23]
head(ARD_3_50_L_labels)#labels
head(ARD_3_50_L)#species matrix

# library(vegan)
#run metaMDS from vegan
ARD_50_L_meta.mds <- metaMDS(ARD_3_50_L,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
adonis(ARD_3_50_L ~ time_block, data = ARD_3_50_L_labels, distance="bray")
# check assumption of homogeneity of multivariate dispersion (so p value should be bigger than 0.05)
distances_3_50_L <- vegdist(ARD_3_50_L)
anova(betadisper(distances_3_50_L, ARD_3_50_L_labels$time_block))


#converting nmds output to objects
nmds_out_50_L_test <- ARD_50_L_meta.mds #name output as an object
nmds_out_50_L_test$stress #gives stress value
nmdsscores._50_L_test <- nmds_out_50_L_test$points 
view(nmdsscores._50_L_test)

species.scores.50_L <- as.data.frame(scores(ARD_50_L_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.50_L$species <- rownames(species.scores.50_L)  # create a column of species, from the rownames of species.scores
view(species.scores.50_L)  #check new data frame

# library(ggplot2)
# library(devtools)
# library(ggrepel)
scoresdata.50_L.test<-cbind(ARD_3_50_L_labels, nmdsscores._50_L_test) #bind treatment labels and score values
view(scoresdata.50_L.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.50_L.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.50_L.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.50_L.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.0_L,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 3) +
  theme_classic() +
  ggtitle("50% Low, R2 = 0.0423, p = 0.90") +
  theme(
    plot.title = element_text(size = 18),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 70% High ----------------------------------------------------------------

ARD_70_L_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "70%") %>% 
  filter(complexity == "Low") %>% 
  select(plot_grid, time_block, common_name, presence)

write.xlsx(ARD_70_L_pivot_3, "ARD_70_L_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_70_L_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/Low/ARD_3_70_L_matrix.csv")

ARD_3_70_L_labels <- ARD_3_70_L_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_70_L <- ARD_3_70_L_matrix[,3:21]
head(ARD_3_70_L_labels)#labels
head(ARD_3_70_L)#species matrix

# library(vegan)
#run metaMDS from vegan
ARD_70_L_meta.mds <- metaMDS(ARD_3_70_L,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
adonis(ARD_3_70_L ~ time_block, data = ARD_3_70_L_labels, distance="bray")
# check assumption of homogeneity of multivariate dispersion (so p value should be bigger than 0.05)
distances_3_70_L <- vegdist(ARD_3_70_L)
anova(betadisper(distances_3_70_L, ARD_3_70_L_labels$time_block))


#converting nmds output to objects
nmds_out_70_L_test <- ARD_70_L_meta.mds #name output as an object
nmds_out_70_L_test$stress #gives stress value
nmdsscores._70_L_test <- nmds_out_70_L_test$points 
view(nmdsscores._70_L_test)

species.scores.70_L <- as.data.frame(scores(ARD_70_L_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.70_L$species <- rownames(species.scores.70_L)  # create a column of species, from the rownames of species.scores
view(species.scores.70_L)  #check new data frame

# library(ggplot2)
# library(devtools)
# library(ggrepel)
scoresdata.70_L.test<-cbind(ARD_3_70_L_labels, nmdsscores._70_L_test) #bind treatment labels and score values
view(scoresdata.70_L.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.70_L.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.3) + 
  geom_point(data = scoresdata.70_L.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.5) +
  geom_point(data = scoresdata.70_L.test,
             aes(x = MDS1,
                 y = MDS2,
                 color = time_block,
                 fill = time_block),
             shape = 1,
             size = 3,
             alpha = 0.5,
             colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  # geom_text_repel(data = species.scores.0_L,
  #                 aes(x =  NMDS1,
  #                     y = NMDS2,
  #                     label = species),
  #                 alpha = 0.8,
  #                 size = 3) +
  theme_classic() +
  ggtitle("70% Low, R2 = 0.0696, p = 0.67") +
  theme(
    plot.title = element_text(size = 18),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )


# 100% low ----------------------------------------------------------------

ARD_100_L_pivot_3 <- ARD_3_with_0 %>%
  filter(treatment == "100%") %>% 
  filter(complexity == "Low") %>% 
  select(plot_grid, time_block, common_name, presence)

write.xlsx(ARD_100_L_pivot_3, "ARD_100_L_pivot_3.xlsx")
##exported to excel and used pivot table to make distance matrix

#copy values to new csv, rename col Sum of Presence to time_block

ARD_3_100_L_matrix <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/Low/ARD_3_100_L_matrix.csv")

ARD_3_100_L_labels <- ARD_3_100_L_matrix[1:2] %>% 
  mutate(time_block = factor(time_block,levels = c("start","middle","end"),
                             labels = c("Start", "Middle", "End")))
ARD_3_100_L <- ARD_3_100_L_matrix[,3:20]
head(ARD_3_100_L_labels)#labels
head(ARD_3_100_L)#species matrix

# library(vegan)
#run metaMDS from vegan
ARD_100_L_meta.mds <- metaMDS(ARD_3_100_L,
                             distance = "bray",
                             k = 3,
                             trymax = 500)

#Check significance with perMANOVA, check for treat, comp and interaction
adonis(ARD_3_100_L ~ time_block, data = ARD_3_100_L_labels, distance="bray")
# check assumption of homogeneity of multivariate dispersion (so p value should be bigger than 0.05)
distances_3_100_L <- vegdist(ARD_3_100_L)
anova(betadisper(distances_3_100_L, ARD_3_100_L_labels$time_block))


#converting nmds output to objects
nmds_out_100_L_test <- ARD_100_L_meta.mds #name output as an object
nmds_out_100_L_test$stress #gives stress value
nmdsscores._100_L_test <- nmds_out_100_L_test$points 
view(nmdsscores._100_L_test)

species.scores.100_L <- as.data.frame(scores(ARD_100_L_meta.mds, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores.100_L$CommonName <- rownames(species.scores.100_L)# create a column of species, from the rownames of species.scores --> called CommonName (to join to species info lookup)

# species.scores.100_L_new <- species.scores.100_L%>%  #not sure I actually need this...
#   filter(!is.nan(NMDS1))
# view(species.scores.100_L_new) #check new data frame that took out NaN values

species_lookup <- read_csv("Artificial coral experiment/data/Art Recruit data/NMDS/by treatment and complexity/species lookup.csv")

species.scores.100_L_full <- left_join(species.scores.100_L, species_lookup)
View(species.scores.100_L_full)

# library(ggplot2)
# library(devtools)
# library(ggrepel)
scoresdata.100_L.test<-cbind(ARD_3_100_L_labels, nmdsscores._100_L_test) #bind treatment labels and score values
view(scoresdata.100_L.test)

#Treatment plot
ggplot() +  
  stat_ellipse(data = scoresdata.100_L.test, 
               aes(x = MDS1, 
                   y = MDS2, 
                   color = time_block, 
                   fill = time_block), 
               type = "norm", 
               geom = "polygon", 
               alpha = 0.2) + 
  geom_point(data = scoresdata.100_L.test, 
             aes(x = MDS1, 
                 y = MDS2, 
                 color = time_block, 
                 fill = time_block), 
             size = 3,
             alpha = 0.3) +
  # geom_point(data = scoresdata.100_L.test,
  #            aes(x = MDS1,
  #                y = MDS2,
  #                color = time_block,
  #                fill = time_block),
  #            shape = 1,
  #            size = 3,
  #            alpha = 0.5,
  #            colour = "black") +
  scale_color_viridis(discrete = TRUE, name = "Time Block") +
  scale_fill_viridis(discrete = TRUE, name = "Time Block") +
  geom_text_repel(data = species.scores.0_L,
                  aes(x =  NMDS1,
                      y = NMDS2,
                      label = species),
                  size = 4) +
# geom_point(data = species.scores.100_L_full,
#            aes(x =  NMDS1,
#                y = NMDS2,
#                shape = MajorTrophicGroup),
#            alpha = 0.7,
#            size = 3,
#            fill = "grey90") +
  # scale_shape_manual(values = c(0,2,5,6)) +
# geom_point(data = species.scores.100_L_full,
#              aes(x =  NMDS1,
#                  y = NMDS2,
#                  shape = Genus),
#              alpha = 0.7,
#              size = 4) +
  theme_classic() +
  ggtitle("100% Low, R2 = 0.0988, p = 0.34") +
  theme(
    plot.title = element_text(size = 18),
    # plot.title = element_text(hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.position = "none",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )



