
################################################################################################
## Title: Income and Racial Disparity in Household Publicly Available EV Infrastructure Accessibility
## R script for creating the Figure 2
## AUTHOR:            Jiehong Lou
## Start Date: 2022 Dec 29
################################################################################################

library(patchwork)
library(stringr)
require(grid)
require(ggplot2)
require(gridExtra)
require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)
library(png)
library(data.table)
library(ggpubr)
library(colorspace)
library(scales)
library(usmap)

#####################################################################
# Creating a US file with longitude and latitude since the original code is no long working
#####################################################################
setwd("")

#load mapping file 
state_mapping <-read.csv("state_mapping.csv")
state_mapping$fips <-str_pad(state_mapping$fips, 2, pad = "0")

# creating the state boundary 
state <- map_data("state")
sf_object <- st_as_sf(state, coords = c("long", "lat"), crs = 4326)
us_state <- st_transform(sf_object, st_crs(data))
us_state$latitude <- st_coordinates(us_state$geometry)[, 2]
us_state$longitude <- st_coordinates(us_state$geometry)[, 1]

#####################################################################
# income _ rural and urban
#####################################################################

income_state<- read.csv ("Figure5ab.csv") 

income_rural_state <- income_state %>%
  filter(rural==1)

income_urban_state <- income_state %>%
  filter(rural==0)

income_rural_state$gap_bin <- cut(income_rural_state$gap,breaks = c(-80,-10,-5,-2,-1,0,1, 2, 5,10, 80))
income_urban_state$gap_bin <- cut(income_urban_state$gap,breaks = c(-80,-2.8,-1.5,-1,0,1.5, 2.8, 80))

income_county<- read.csv("income_agg_rural_county.csv") %>%
  rename(code=STATE) %>%
  left_join(state_mapping, by="code") 
income_county$county_FIP<-str_pad(income_county$county_FIP, 3, pad = "0")

#####################################################################
# race _ rural and urban
#####################################################################
race_state<- read.csv ("Figure5cd.csv") %>%
  
race_rural_state <- race_state %>%
  filter(rural==1)

race_urban_state <- race_state %>%
  filter(rural==0)

race_rural_state$gap_bin <- cut(race_rural_state$gap,breaks = c(-133,-4, -2,-1,0,1, 2,4, 133))
race_urban_state$gap_bin <- cut(race_urban_state$gap,breaks = c(-70,-4,-2,-1,0,1, 2, 4 ,70))


p1 <- plot_usmap(data = income_rural_state, values  = "gap_bin",color = "transparent",size = 0.01) +
  scale_fill_brewer(palette="PiYG",aesthetics = "fill",na.value = "grey50", direction=-1)+
  theme(legend.position = "right") +
  geom_polygon(data = us_state,
               aes(x=longitude, y=latitude, group = group), fill = NA, size = 0.15, color = "black")+
  theme(panel.background = element_rect(colour = "black"))+
  labs(title = "a.State income gap rural") 


p2 <- plot_usmap(data = income_urban_state, values  = "gap_bin",color = "transparent",size = 0.01) +
  scale_fill_brewer(palette="PiYG",aesthetics = "fill",na.value = "grey50", direction=-1)+
  theme(legend.position = "right") +
  geom_polygon(data = us_state,
               aes(x=longitude, y=latitude, group = group), fill = NA, size = 0.15, color = "black")+
  theme(panel.background = element_rect(colour = "black"))+
  labs(title = "b.State income gap urban") 

p3 <- plot_usmap(data = race_rural_state, values = "gap_bin", color = "transparent",size = 0.01) +
  scale_fill_brewer(palette="PiYG",aesthetics = "fill",na.value = "grey50", direction=-1)+   
  theme(legend.position = "right") +
  geom_polygon(data = us_state,
               aes(x=longitude, y=latitude, group = group), fill = NA, size = 0.15, color = "black")+
  theme(panel.background = element_rect(colour = "black"))+
  labs(title = "c. State racial gap rural") 


p4 <- plot_usmap(data = race_urban_state, values = "gap_bin", color = "transparent",size = 0.01) +
  scale_fill_brewer(palette="PiYG",aesthetics = "fill",na.value = "grey50", direction=-1)+   
  theme(legend.position = "right") +
  geom_polygon(data = us_state,
               aes(x=longitude, y=latitude, group = group), fill = NA, size = 0.15, color = "black")+
  theme(panel.background = element_rect(colour = "black")) +
  labs(title = "d. State racial gap urban") 

