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
d<-read.csv("Dripping Speed. Alisa Tian.csv")
# Dripping Speed
#create new columns
d<-d %>% mutate(
  Fine=case_when(Z=="Fine"~1,Z=="Coarse"~0),
  Coarse=case_when(Z=="Fine"~0,Z=="Coarse"~1)
)

write.csv(d,"Dcleaned.csv")

#Bitterness
b <- read.csv("Bitterness2.csv")
b <- b %>% mutate(
  Fine = case_when(Z == "Fine" ~ 1,
                   Z == "Coarse" ~ 0),
  Coarse = case_when(Z == "Fine" ~ 0,
                     Z == "Coarse" ~ 1)
)

write.csv(b,"Bcleaned.csv")
