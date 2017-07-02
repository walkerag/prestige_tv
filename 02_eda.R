###################################################
#Name: IMDB EDA
#Date: July 2017
#Purpose: Create charts for TV outlier blog post
###################################################

rm(list=ls())
gc()

options(scipen=999)

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(ggrepel)
library(animation)
library(tweenr)
library(magick)
library(knitr)
library(gganimate)
library(grid)
library(gtable)

path<-'/Users/walkerag/Documents/prestige_tv/dat/'

#Load data
imdb_dat<-readRDS(file = paste0(path,"imdb_tv_dat.rds"))

##################################
#GENERAL SHOWS CHART
##################################

general<-imdb_dat[imdb_dat$imdbRating_num>=7 & imdb_dat$hours>0,]

#Remove gameshows and talk shows
general<-general[!grepl("Talk",general$Genre),]
general<-general[!grepl("Game",general$Genre),]

#Remove Frank
general<-general[general$ID!="tt4202274",]

general$alpha<-1

#Specify color to highlight certain shows
general$color<-"darkgrey"
general[general$hours>300 | general$imdbRating_num>=9.5 | (general$imdbRating_num>8.5 & general$hours>200),"color"]<-"forestgreen"

general$fontface<-'bold'

ggplot(general,aes(hours,imdbRating_num,label=Title)) +
  ggtitle("Sustained TV Excellence Is Very Rare!") +
  xlab("Total Runtime (Hours)") +
  ylab("IMDB Rating") +
  geom_point(
    alpha = general$alpha
    ,color=general$color
  ) +
  geom_text_repel(data=(general[general$hours>300 | general$imdbRating_num>=9.5 | (general$imdbRating_num>8.5 & general$hours>200)
                                ,]), aes(label=Title)
                  ,alpha = general[general$hours>300 | general$imdbRating_num>=9.5 | (general$imdbRating_num>8.5 & general$hours>200),"alpha"]
                  ,color=general[general$hours>300 | general$imdbRating_num>=9.5 | (general$imdbRating_num>8.5 & general$hours>200),"color"]
                  ,force=1
                  ,size=6
                  #                  ,nudge_y=-0.04
  ) +  scale_y_continuous(limits = c(7,10),breaks=c(7,7.5,8,8.5,9,9.5,10)) +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 24,family="Trebuchet MS")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
  )


################################################
#SHOWS BY NUMBER OF RATINGS
################################################

rating<-imdb_dat[imdb_dat$imdbRating_num>=7 & imdb_dat$hours>0 & imdb_dat$totalratingsnum>0,]

#Remove Frank
rating<-rating[rating$ID!="tt4202274",]

rating$alpha<-1
rating$color<-"darkgrey"
rating[rating$totalratingsnum>500000 | rating$imdbRating_num>=9.5,"color"]<-"chocolate2"

rating$fontface<-'plain'
rating[rating$totalratingsnum>800000,"fontface"]<-"bold"

ggplot(rating,aes(totalratingsnum,imdbRating_num,label=Title)) +
  ggtitle("Game of Thrones And Breaking Bad Are Popular, Beloved Outliers") +
  xlab("IMDB User Ratings") +
  ylab("IMDB Rating") +
  geom_point(
    alpha = rating$alpha
    ,color=rating$color
  ) +
  geom_text_repel(data=(rating[rating$totalratingsnum>500000 | rating$imdbRating_num>=9.5,]), aes(label=Title)
                  ,alpha = rating[rating$totalratingsnum>500000 | rating$imdbRating_num>=9.5,"alpha"]
                  ,color=  rating[rating$totalratingsnum>500000 | rating$imdbRating_num>=9.5,"color"]
                  ,force=1.5
                  ,size=6
                  ,fontface=rating[rating$totalratingsnum>500000 | rating$imdbRating_num>=9.5,"fontface"]
  ) +  
  scale_y_continuous(limits = c(7,10),breaks=c(7,7.5,8,8.5,9,9.5,10)) +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,1200000),breaks=seq(0,1200000,by=300000)) +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 22,family="Trebuchet MS")
    ,title = element_text(face="bold")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
  )

################################################
#GOLDEN GLOBE GIF
################################################

#Adjust Office titles
imdb_dat[imdb_dat$ID=="tt0290978","Title"]<-'The Office (UK)'
imdb_dat[imdb_dat$ID=="tt0386676","Title"]<-'The Office (US)'

#Get shows
prestige<-imdb_dat[imdb_dat$imdbRating_num>=7 & imdb_dat$wins>0 & imdb_dat$hours>0,]

summary(prestige$releaseyear)
summary(prestige$hours)
summary(prestige$imdbRating_num)

#Create frames for gganimate
prestige_all<-NULL
for(i in 2000:2016){
  
  print(i)
  prestige_year<-prestige
  prestige_year$alpha<-ifelse(prestige$releaseyear<=i,1,0)
  prestige_year$gif_year<-i
  prestige_all<-rbind(prestige_all,prestige_year)
  
}


#Have alpha change depending on time since release
prestige_all$years_since_release<-prestige_all$gif_year-prestige_all$releaseyear
prestige_all$alpha<-0
prestige_all[prestige_all$years_since_release==0,"alpha"]<-1
prestige_all[prestige_all$years_since_release>=1,"alpha"]<-0.7
prestige_all[prestige_all$years_since_release>=3,"alpha"]<-0.5
prestige_all[prestige_all$years_since_release>=5,"alpha"]<-0.3
prestige_all[prestige_all$years_since_release>=7,"alpha"]<-0.2

prestige_all$cols<-"darkblue"

p<-ggplot(prestige_all,aes(hours,imdbRating_num,label=Title,frame=gif_year)) +
  ggtitle("Golden Globe Winners by Release Year: ") +
  xlab("Total Runtime (Hours)") +
  ylab("IMDB Rating") +
  geom_point(alpha = prestige_all$alpha
             ,color=prestige_all$cols
  ) +
  geom_text_repel(data=prestige_all, aes(label=Title)
                  ,alpha = prestige_all$alpha
                  ,color=prestige_all$cols
                  ,force=0.2
                  ,size=12
                  ,nudge_y=-0.04
  ) +
  scale_x_continuous(limits = c(-20,240),breaks=seq(from=0,to=240,by=40)) +
  scale_y_continuous(limits = c(6.8,10),breaks=c(7,7.5,8,8.5,9,9.5,10)) +
  theme(
    panel.grid.minor = element_blank() # get rid of minor grid
    ,text = element_text(size = 40,family="Trebuchet MS")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
  )

animation::ani.options(interval = 1 / 0.65)
gganimate(p, title_frame = TRUE, paste0(path,"tv_globes.gif"),ani.width=1500, ani.height=1400)