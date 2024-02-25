
################################################################################################
## Title: Income and Racial Disparity in Household Publicly Available EV Infrastructure Accessibility
## R script for creating the Figure 3  
## AUTHOR:            Jiehong Lou
## Start Date: 2022 Dec 29
################################################################################################
rm(list = ls())
require(grid)
require(ggplot2)
require(gridExtra)
#require(plyr)
require(xlsx)
#require(tidyverse)
require(tidyr)
require(dplyr)
require(ggplot2)
require(readr)
require(stringr)
require(lemon)
library(ggthemes)
library(png)
library(ggpubr)
library(usmap)


#####################################################################
#load mapping file 
#####################################################################

setwd("")
state_mapping <-read.csv("state_mapping.csv")
state_mapping$fips <-str_pad(state_mapping$fips, 2, pad = "0")


#####################################################################
# coefficients and lasso (simplied version in map)
#####################################################################

lasso_rs<-read.csv('Figure3a.csv')
lasso_us<-read.csv('Figure3b.csv')
lasso_rm<-read.csv('Figure3c.csv')
lasso_um<-read.csv('Figure3d.csv')

png(paste0(fig_dir, "/Figure 3.png"),width = 10, height=10, units="in",res=300) 
p1 <- plot_usmap(data = lasso_rs, values  = "code", color="grey") +
  scale_fill_manual(values = c(`income negative` = "#006d77", `income positive` = "#83c5be",`race negative` = "#ffddd2"), name = "values") + 
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "a.single family in rural") 

p2 <-plot_usmap(data = lasso_us, values  = "code", color="grey") +
  scale_fill_manual(values = c(`income negative` = "#006d77", `income positive` = "#83c5be",`race negative` = "#ffddd2",`race positive` = "#e29578",`equal ` = "#ffbd00"), name = "values") + 
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "b.single family in urban") 

p3 <-plot_usmap(data = lasso_rm, values  = "code", color="grey") +
  scale_fill_manual(values = c(`income negative` = "#006d77", `income positive` = "#83c5be",`race negative` = "#ffddd2",`race positive` = "#e29578"), name = "values") + 
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "c.multiple unit in rural") 

p4 <-plot_usmap(data = lasso_um, values  = "code", color="grey") +
  scale_fill_manual(values = c(`income negative` = "#006d77",`income positive` = "#83c5be", `race negative` = "#ffddd2",`race positive` = "#e29578",`equal ` = "#ffbd00"), name = "values") + 
  theme(legend.position = "right") +
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "d.multiple unit in urban") 

p1+p2+p3+p4 

dev.off() 
