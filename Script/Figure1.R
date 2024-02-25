################################################################################################
## Title: Income and Racial Disparity in Household Publicly Available EV Infrastructure Accessibility
## R script for creating the Figure 1  
## AUTHOR:            Jiehong Lou
## Start Date: 2022 Dec 29
################################################################################################

require(grid)
require(ggplot2)
require(gridExtra)
require(xlsx)
require(tidyr)
require(dplyr)
require(ggplot2)
require(lemon)
library(png)
library(data.table)
library(ggpubr)
library(colorspace)
library(scales)

#####################################################################
# Scattered plot with fitted line (lOESS) Panel A 
#####################################################################
setwd("")

data <- fread("Figure1.csv")
data$dp <- factor(data$dp,      # Reordering group factor levels
                         levels = c("dp1", "dp2", "dp3", "dp4", "dp5"))

data<-data %>%
  mutate(dp=recode(dp,"dp1" ="avg.dist.of 1 \n nearby station","dp2"="avg.dist.of 2 \n nearby stations",
                   "dp3"="avg.dist.of 3 \n nearby stations","dp4"="avg.dist.of 4 \n nearby stations",
                   "dp5"="avg.dist.of 5 \n nearby stations"))

custom_colors <- c("Asian" = "#377eb8", "Hispanic" = "#984ea3", "Black or African American" = "#ff7f00", "White" = "#4daf4a")

p1 <- ggplot(data %>%
               filter(rural=="Urban"), aes(x = FIND_DIV_1000, 
                                           y = distance, 
                                           fill = Ethnic,
                                           color = Ethnic)) + 
  theme_bw() +
  facet_wrap(~dp,ncol = 5) + 
  labs(y =" ", x = "Urban: income ($1000) ") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) +
  theme(strip.text = element_text(size = 12))  +
  theme(legend.position="bottom")+
  geom_smooth(method = "loess", se = TRUE) +
  scale_color_manual(values = custom_colors) + # Set custom colors
  scale_fill_manual(values = custom_colors) +
  theme(legend.position = "none")  

p2 <- ggplot(data %>%
               filter(rural=="Rural"), aes(x = FIND_DIV_1000, 
                                           y = distance, 
                                           fill = Ethnic,
                                           color = Ethnic)) + 
  theme_bw() +
  facet_wrap(~dp,ncol = 5) + 
  labs(y ="                                                              distance to public charging station (km)", x = "Rural: income ($1000)") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) +
  theme(strip.text = element_text(size = 12))  +
  theme(legend.position="bottom")+
  geom_smooth(method = "loess", se = TRUE) +
  scale_color_manual(values = custom_colors) + # Set custom colors
  scale_fill_manual(values = custom_colors) 

ggarrange(p1, p2, ncol = 1, nrow = 2,common.legend = TRUE, legend="right")

##########################################
# summary by income and by race, Panel B
##########################################

data <- fread("Figure1.csv")
data$dp <- factor(data$dp,      # Reordering group factor levels
                  levels = c("dp1", "dp2", "dp3", "dp4", "dp5"))

df_urban<- data %>%
  filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
  filter(rural=="Urban") %>%
  filter(dp=="dp1") %>%
  rename(distance1=distance) %>%
  select(-dp)%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Urban") %>%
              filter(dp=="dp2") %>%
              select(-dp)%>%
              rename(distance2=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin"))%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Urban") %>%
              filter(dp=="dp3") %>%
              select(-dp)%>%
              rename(distance3=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin"))%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Urban") %>%
              filter(dp=="dp4") %>%
              select(-dp)%>%
              rename(distance4=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin"))%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Urban") %>%
              filter(dp=="dp5") %>%
              select(-dp)%>%
              rename(distance5=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin")) %>%
  mutate(diff2=distance2-distance1) %>%
  mutate(rate2=diff2/distance1*100) %>%
  mutate(diff3=distance3-distance1) %>%
  mutate(rate3=diff3/distance1*100) %>%
  mutate(diff4=distance4-distance1) %>%
  mutate(rate4=diff4/distance1*100) %>%
  mutate(diff5=distance5-distance1) %>%
  mutate(rate5=diff5/distance1*100) 

data_urban_plot<- df_urban %>%
  select(rate2, rate3, rate4,rate5, Ethnic, FIND_DIV_1000,rural) %>%
  mutate(FIND_DIV_1000 = as.character(FIND_DIV_1000)) %>%
  mutate(FIND_DIV_1000=recode(FIND_DIV_1000,"5" ="low_income","500"="high_income")) %>%
  gather(rate, value, rate2:rate5,  factor_key=TRUE ) 


df_rural<- data %>%
  filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
  filter(rural=="Rural") %>%
  filter(dp=="dp1") %>%
  rename(distance1=distance) %>%
  select(-dp)%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Rural") %>%
              filter(dp=="dp2") %>%
              select(-dp)%>%
              rename(distance2=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin"))%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Rural") %>%
              filter(dp=="dp3") %>%
              select(-dp)%>%
              rename(distance3=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin"))%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Rural") %>%
              filter(dp=="dp4") %>%
              select(-dp)%>%
              rename(distance4=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin"))%>%
  left_join(data %>%
              filter(FIND_DIV_1000==5 | FIND_DIV_1000==500) %>%
              filter(rural=="Rural") %>%
              filter(dp=="dp5") %>%
              select(-dp)%>%
              rename(distance5=distance), by=c("rural","FIND_DIV_1000",'Ethnic',"income_bin"))%>%
  mutate(diff2=distance2-distance1) %>%
  mutate(rate2=diff2/distance1*100) %>%
  mutate(diff3=distance3-distance1) %>%
  mutate(rate3=diff3/distance1*100) %>%
  mutate(diff4=distance4-distance1) %>%
  mutate(rate4=diff4/distance1*100) %>%
  mutate(diff5=distance5-distance1) %>%
  mutate(rate5=diff5/distance1*100) 

data_rural_plot<- df_rural %>%
  select(rate2, rate3, rate4,rate5, Ethnic, FIND_DIV_1000,rural) %>%
  mutate(FIND_DIV_1000 = as.character(FIND_DIV_1000)) %>%
  mutate(FIND_DIV_1000=recode(FIND_DIV_1000,"5" ="low_income","500"="high_income")) %>%
  gather(rate, value, rate2:rate5,  factor_key=TRUE ) 


data_plot<-data_rural_plot%>%
  bind_rows(data_urban_plot) %>%
  rename(Income=FIND_DIV_1000) 

data_plot$rural <- factor(data_plot$rural,      # Reordering group factor levels
                          levels = c("Urban","Rural" ))
data_plot<- data_plot%>%
  mutate(rate=recode(rate,"rate2" ="2nd vs.1st station", "rate3"="3rd vs. 2nd station",
                     "rate4" ="4th vs. 3rd station", "rate5"="5th vs. 4th station")) 


p3<-ggplot(data_plot %>%
         mutate(Ethnic = recode(Ethnic, 'Black or African American' = 'Black')),
                aes(x=rate, y=value, colour=Income, shape = Ethnic,
                      group=interaction(Income, Ethnic))) + 
  facet_wrap(~rural, ncol=2,scale="free") +
  geom_point() + geom_line() +
  theme_bw() +
  theme_classic() +
  ylab("Increased rate (%) in distance")+
  xlab("") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold")) +
  theme(strip.text = element_text(size = 12))  +
  # theme(legend.position="bottom")+
  scale_color_manual(values = c( "#000000","#FF0000")) 

ggarrange(p1, p2,p3, ncol = 1, nrow = 3,common.legend = FALSE, legend="right")

