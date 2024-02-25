################################################################################################
## Title: Income and Racial Disparity in Household Publicly Available EV Infrastructure Accessibility
## R script for creating the Figure 34 
## AUTHOR:            Jiehong Lou
## Start Date: 2022 Dec 29
################################################################################################
library(usmap)
library(ggplot2)
library(tidycensus)
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library("cowplot")

######################################################################
# coefficients gap _Standardized
######################################################################
data<-read.csv("Figure4.csv") %>%
  filter(type=="income") %>%
  filter(location=="rural") 
pd = position_dodge(.1) 
data$key<-factor(data$key,
                 levels=c('MUD gap','SFD gap',"density gap",'income gap','black percent gap',
                          'highway gap'), order=T)  

plot1<-ggplot(data,            
              aes(x     = beta,
                  y     = key))+
  
  geom_point(shape = 16,
             size  = 2.5,
             position = pd) +  
  scale_color_manual(values = c("#FF7F00", "#0c0cae"))+
  
  geom_errorbar(aes(xmin  = beta - 1.65*se,
                    xmax  = beta + 1.65*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title = element_text(size=7)) +
  ylab("")+
  xlab("Accessiblity gap by income in rural")+
  geom_vline(xintercept=0, linetype="dashed", size=0.5)+
  theme(axis.text.x = element_text(size=7)) +
  theme(axis.text.y = element_text(size=7))

data<-read.csv("Figure4.csv") %>%
  filter(type=="income") %>%
  filter(location=="urban")

pd = position_dodge(.1) 
data$key<-factor(data$key,
                 levels=c('MUD gap','SFD gap',"density gap",'income gap','black percent gap',
                          'highway gap'), order=T)  

plot2<-ggplot(data,            
              aes(x     = beta,
                  y     = key))+
  
  geom_point(shape = 16,
             size  = 2.5,
             position = pd) +  
  scale_color_manual(values = c("#FF7F00", "#0c0cae"))+
  
  geom_errorbar(aes(xmin  = beta - 1.65*se,
                    xmax  = beta + 1.65*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("Accessiblity gap by income in urban")+
  theme(axis.title = element_text(size=7)) +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)+
  theme(axis.text.x = element_text(size=7)) +
  theme(axis.text.y = element_text(size=7))

data<-read.csv("Figure4.csv") %>%
  filter(type=="race") %>%
  filter(location=="rural")

pd = position_dodge(.1) 
data$key<-factor(data$key,
                 levels=c('MUD gap','SFD gap',"density gap",'income gap','poverty gap',
                          'highway gap'), order=T)  

plot3<-ggplot(data,            
              aes(x     = beta,
                  y     = key))+
  
  geom_point(shape = 16,
             size  = 2.5,
             position = pd) +  
  scale_color_manual(values = c("#FF7F00", "#0c0cae"))+
  
  geom_errorbar(aes(xmin  = beta - 1.65*se,
                    xmax  = beta + 1.65*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("Accessiblity gap by race in rural")+
  theme(axis.title = element_text(size=7)) +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)+
  theme(axis.text.x = element_text(size=7)) +
  theme(axis.text.y = element_text(size=7))

data<-read.csv("Figure4.csv") %>%
  filter(type=="race") %>%
  filter(location=="urban")
pd = position_dodge(.1) 
data$key<-factor(data$key,
                 levels=c('MUD gap','SFD gap',"density gap",'income gap','poverty gap',
                          'highway gap'), order=T)  

plot4<-ggplot(data,            
              aes(x     = beta,
                  y     = key))+
  
  geom_point(shape = 16,
             size  = 2.5,
             position = pd) +  
  scale_color_manual(values = c("#FF7F00", "#0c0cae"))+
  
  geom_errorbar(aes(xmin  = beta - 1.65*se,
                    xmax  = beta + 1.65*se),
                width = 0.0,
                size  = 1.2,
                position = pd) +
  theme_bw() +
  theme(axis.title = element_text(face = "bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("")+
  xlab("Accessiblity gap by race in urban")+
  theme(axis.title = element_text(size=7)) +
  geom_vline(xintercept=0, linetype="dashed", size=0.5)+
  theme(axis.text.x = element_text(size=7)) +
  theme(axis.text.y = element_text(size=7)) 

multiplot <- align_plots(plot1,plot2,plot3,plot4, align = "hv") 

pic<- ggdraw() + draw_grob(multiplot[[1]], 0,0.5,.5,.5) +
  draw_grob(multiplot[[3]], 0.5,0.5,0.5,0.5) + 
  draw_grob(multiplot[[2]], 0, 0, 0.5,0.5) +
  draw_grob(multiplot[[4]], 0.5, 0, 0.5, 0.5) +
  draw_plot_label(label = c("a", "c", "b","d"), size = 12,
                                     x = c(0, 0.5, 0,0.5), y = c(1, 1, 0.5,0.5)) 

