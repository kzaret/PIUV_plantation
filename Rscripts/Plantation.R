#Summary, visualization and analysis of experimental plantation data from the burned site (LC), 2013 - 2019. Observations/measurements made (for some variables) in 2013, 2014, 2016, 2017, 2018, 2019.

#Response variables: total height, substrate surface to tip, percent survival, vigor (repeated measures over time).

#Predictor variables: patch, [transect,] group, treatment [, seedling]

#===============================================================================
# SETUP
#===============================================================================

#change library path
.libPaths(c("C:/Program Files/R/R-4.1.1/library"))

#my custom stuff
myPaths <- .libPaths()   # get the paths
myPaths <- c(myPaths[2], myPaths[1])  # switch them
.libPaths(myPaths)  # reassign them

library(here)
library(dplyr)
library(tidyverse)
library(psych)
library(nlme)

#===============================================================================
# READ IN & TIDY DATA
#===============================================================================

# read in dataset
epl <- read.csv(here("data/ExperimentalPlantation_long.csv"), header = TRUE)

glimpse(epl)


#===============================================================================
# SUMMARIZE
#===============================================================================

# how many seedlings per treatment? how many groups per patch?

epl %>% 
  filter(year=="2013") %>%
  group_by (treat_simp) %>%
  summarise(n = sum(!is.na(treat_simp)))

epl.s <- epl %>% 
  filter(year=="2013") %>%
  count(patch, group)

write.table(epl.s, "clipboard", sep="\t", row.names=TRUE)


#===============================================================================
# FIGURES
#===============================================================================

# specific factor levels
epl$patch <- factor(epl$patch, levels = c("Burned Forest", "Transition", "Cushion"))
epl$treat_simp <- factor(epl$treat_simp, level = c("Low", "Medium", "High", "SPMA", "Other", "Lawn", "Pool"))
                        
# select colors of palette for LC to match plots with patches from both sites
epl_pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(4, 5, 6)]
epl_bwpal <- RColorBrewer::brewer.pal(9, "Greys")[c(3, 5, 9)]

#===============================================================================
# Total height


epl %>%
  ggplot (aes(x=year_month, y=total_height, group=treat_simp, color=patch, linetype=treat_simp)) +
  stat_summary(fun = "mean", geom = "point", size=3) +
  stat_summary(fun = "mean", geom = "line") +
  theme_bw() +
  scale_color_manual(values=epl_pal) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylab("Mean Total Height (cm)") +
  xlab("Year-Month")


#===============================================================================
# sUBSTRATE SURFACE TO TIP


epl %>%
  ggplot (aes(x=year_month, y=subsurf_to_tip, group=treat_simp, color=patch, linetype=treat_simp)) +
  stat_summary(fun = "mean", geom = "point", size=3) +
  stat_summary(fun = "mean", geom = "line") +
  theme_bw() +
  scale_color_manual(values=epl_pal) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylab("Mean Distance from Substrate Surface to\n Seedling Tip (cm)") +
  xlab("Year-Month")


#===============================================================================
# Percent survival

glimpse(epl)

# summarize

epl.ps <- epl %>%
  group_by (year, patch, treat_simp) %>%
  summarize(sum_dead=sum(dead, na.rm=TRUE),
            sum_acctfor=sum(accounted_for, na.rm=TRUE)) %>%
  mutate(perc_survival=((sum_acctfor-sum_dead)/sum_acctfor)*100)

View(epl.ps)  

# plot

epl.ps %>%
  ggplot (aes(x=year, y=perc_survival, group=treat_simp, color=patch, linetype=treat_simp)) +
  geom_point(size = 3) +
  geom_line()+
  theme_bw() +
  scale_color_manual(values=epl_pal) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylab("Percent Survival") +
  xlab("Year")

#===============================================================================
# vigor

glimpse(epl)

epl <- epl %>%
  mutate(vigor2 = (vigor_all*-1)) # re-order vigor categories so that 4 (dead/brown) is lowest, 1 is highest.
View(epl)

epl %>%
  ggplot (aes(x=year_month, y=vigor2, group=treat_simp, color=patch, linetype=treat_simp)) +
  stat_summary(fun = "mean", geom = "point", size=3) +
  stat_summary(fun = "mean", geom = "line") +
  theme_bw() +
  scale_color_manual(values=epl_pal) +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylab("Mean Vigor") +
  xlab("Year-Month")


#===============================================================================
# Define model & conduct analysis of deviance using same process as in ff?
#https://rcompanion.org/handbook/I_09.html
#===============================================================================


?corClasses

