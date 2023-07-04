setwd("/Users/alisa/Desktop/22 Spring/PLSC 341/Final Project/Final Paper/PLSC. 341 Final Project. Alisa Tian")
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(patchwork)
library(randomizr)
library(texreg)
library(estimatr)
library(wesanderson)
library(RColorBrewer)

#Dripping Speed
d<-read.csv("Dcleaned.csv")
# Group Means 
GroupMeans<-d%>% 
  group_by(Z) %>% 
  summarise(tidy(lm_robust(Time~1,data=cur_data())))
# Difference in Means
dim_fine<-d %>% 
  summarise(tidy(difference_in_means(Time~Fine,data=cur_data())))
#Visualization
#Scatterplots
S1<-ggplot(data=d,
           aes(x=Z,y=Time, color=Z))+
  geom_jitter(width=0.1)+
  geom_point(data=GroupMeans,
             aes(x=Z,y=estimate),
             color="#FF9AA1")+
  geom_linerange(data=GroupMeans,
                 aes(x=Z,y=estimate,
                     ymin=conf.low,
                     ymax=conf.high),color="#FF9AA1")+
  xlab("Treatment Groups") +
  theme_bw()  +
  scale_color_manual(values=c("#85D4E3","#B3E2CD"),
                     name =NULL)+
  theme(axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position ="none")
S1

#Coefficient plot
C1<-ggplot(data=dim_fine,aes(x=term,y=estimate,color=term))+
  scale_color_manual(values = c( "#9986A5"),
                     name =NULL)+
  geom_point()+
  geom_linerange(aes(ymin=conf.low,ymax=conf.high))+
  geom_hline(yintercept = 0,
             linetype="dashed",
             color="#F4B5BD")+
  theme_bw()+
  theme(axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position ="none")+
  ylab("Difference in Means")+
  ggtitle("Coefficient Plot of Dripping Time")
C1
A<-S1+C1
A
ggsave("DrippingSpeed.png")#regression tables

lmFine<-lm_robust(Time~Fine,data=d)



#Bitterness 
b<-read.csv("Bcleaned.csv")
GroupMeans2<-b%>% 
  group_by(Z) %>% 
  summarise(tidy(lm_robust(as.numeric(Ratings)~1,data=cur_data())))


b1<-ggplot(data=b,aes(x=Z,y=as.numeric(Ratings),
                      color=Z))+
  ylab("Bitterness")+
  geom_point()+geom_jitter(width=0.1,alpha=0.6)+
  theme_bw()+
  geom_linerange(data=GroupMeans2,
                 aes(x=Z,y=estimate,
                     ymin=conf.low,
                     ymax=conf.high),color="#FF9AA1")+
  xlab("Treatment Groups") +
  theme_bw()  +
  scale_color_manual(values=c("#85D4E3","#B3E2CD"),
                     name =NULL)+
  theme(axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        legend.position ="none")+
  geom_point(data=GroupMeans2,
             aes(x=Z,y=estimate),
             color="#FF9AA1")
b1

dim_b<-b %>% 
  summarise(tidy(difference_in_means(as.numeric(Ratings)~Fine,data=cur_data())))
dim_b

#coe
b2 <- ggplot(data = dim_b, aes(x = term, y = estimate, color = term)) +
  scale_color_manual(values = c("#9986A5"),
                     name = NULL) +
  geom_point() +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "#F4B5BD") +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    legend.position ="none"
  ) +
  ggtitle("Coefficient Plot of Bitterness") +
  ylab("Difference in Means")
b2

B<-b1+b2
B
ggsave("Bitterness.png")
lmRating<-lm_robust(Ratings~Fine,
                    data=b)
#make the table
screenreg(list(lmFine,lmRating),
          include.ci=FALSE,
          caption = "Difference in Means",
          custom.model.names = c("Dripping Time", 
                                 "Bitterness"),
          digits = 3)

#Checking the homogeneity among different raters
LmRater<-lm_robust(Ratings ~ Z*By,
                   data = b)


screenreg(list(LmRater),
          include.ci=FALSE,
          caption = "Difference in Means",
          custom.model.names = c("Bitterness by Raters"),
          digits = 3)
