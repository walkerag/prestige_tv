###################################################
#Name: IMDB Data Pull
#Date: July 2017
#Purpose: Pull TV series metadata for blog post
###################################################

rm(list=ls())
gc()

options(scipen=999)

library(tidyverse)
library(lubridate)
library(stringr)
library(reshape2)
library(jsonlite)
library(httr)
library(RSelenium)
library(V8)
library(rvest)
library(knitr)

path<-'/Users/walkerag/Documents/prestige_tv/dat/'

############################################
#Feed in some general imdb searches to grab series IDs
#Looking for every relevant show from 2000+
############################################

feed_urls<-c(
  "http://www.imdb.com/chart/toptv/?ref_=nv_tvv_250_3"
  ,"http://www.imdb.com/search/title?title_type=tv_series"
  ,"http://www.imdb.com/search/title?title_type=tv_series&sort=num_votes,desc"
  ,"http://www.imdb.com/list/ls033255261/"
  ,"http://www.imdb.com/chart/tvmeter"
  ,"http://www.imdb.com/chart/tvmeter?sort=ir,desc&mode=simple&page=1"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=user_rating"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=moviemeter"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=num_votes"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=user_rating,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=moviemeter,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=num_votes,desc&page=2&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=user_rating,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=moviemeter,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=num_votes,desc&page=3&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=user_rating,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=moviemeter,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2017,2017&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2016,2016&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2015,2015&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2014,2014&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2013,2013&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2012,2012&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2011,2011&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2010,2010&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2009,2009&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2008,2008&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2007,2007&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2006,2006&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2005,2005&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2004,2004&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2003,2003&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2002,2002&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2001,2001&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt"
  ,"http://www.imdb.com/search/title?year=2000,2000&title_type=tv_series&sort=num_votes,desc&page=4&ref_=adv_nxt")


#Initialize scraper
rD<-rsDriver()
remDr <- rD[["client"]]
remDr$open()

t_all<-c()
#Loop through the URLs
for(f in 1:length(feed_urls)){
  
  #Be considerate
  Sys.sleep(2)
  
  furl<-feed_urls[f]
  
  #SCRAPE
  remDr$navigate(furl)
  page_source<-remDr$getPageSource()
  feed_dat <- read_html(page_source[[1]])
  rm(page_source)
  
  #Get IMDB links
  links<-html_nodes(feed_dat,"[href*='/title/tt']") %>% html_nodes(xpath='@href') %>% html_text()
  
  #Pull out series IDs and format
  t1<-substr(links,regexpr('tt',links)[1],nchar(links))
  t2<-substr(t1,1,regexpr('/',t1)[1]-1)
  t2<-unique(t2)
  length(t2)
  head(t2)
  
  #Add to overall vector
  t_all<-append(t_all,t2)
  rm(t2)
  
  #Make sure IDs unique
  t_all<-unique(t_all)
  
  #Save output
  saveRDS(t_all, file = paste0(path,"imdb_titles.rds"))
  
  rm(t1)
  rm(links)
  rm(feed_dat)
  rm(furl)
  
}
saveRDS(t_all, file = paste0(path,"imdb_titles.rds"))
remDr$close()
rD[["server"]]$stop() 

#t_all<-readRDS(file = paste0(path,"imdb_titles.rds"))

length(t_all)
head(t_all)
tail(t_all)

#########################
#API PULL
#Now use the Open Movie API to pull metadata for the collected series IDs
#Get key here:http://www.omdbapi.com/
#########################

your_api_key<-'123someapikey'

show_all<-{}
for(t in 1:length(t_all)){
  
  Sys.sleep(1.25)
  
  show_id<-t_all[t]
  print(t)
  print(show_id)
  
  api_url <- paste0('http://www.omdbapi.com/?i=',show_id,'&apikey=',your_api_key)
  try(req <- httr::GET(api_url, timeout(10)))
  
  if(exists('req')){
    json <- httr::content(req, as = "text")
    api_dat <- fromJSON(json)
    rm(json)
    rm(req)

    show <- list(show_id, api_dat)
    show_all[[t]]<-show
    rm(show)
    rm(api_dat)
    rm(show_id)
    
    #Save output regularly
    if((t %% 50)==0){
      saveRDS(show_all, file = paste0(path,"api_show_all.rds"))
    }
  }
  
}
saveRDS(show_all, file = paste0(path,"api_show_all.rds"))

#show_all<-readRDS(file = paste0(path,"api_show_all.rds"))

#Total shows
show_num<-length(show_all)

#####################################
#Convert API data into nice dataframe
#####################################

Title<-rep(NA,show_num)
Year<-rep(NA,show_num)
Released<-rep(NA,show_num)
Runtime<-rep(NA,show_num)
imdbVotes<-rep(NA,show_num)
imdbRating<-rep(NA,show_num)
Type<-rep(NA,show_num)
totalSeasons<-rep(NA,show_num)
for(i in 1:show_num){
  print(i)
  Title[i]<-ifelse(is.null(show_all[[i]][[2]]$Title),NA,show_all[[i]][[2]]$Title)
  Year[i]<-ifelse(is.null(show_all[[i]][[2]]$Year),NA,show_all[[i]][[2]]$Year)
  Released[i]<-ifelse(is.null(show_all[[i]][[2]]$Released),NA,show_all[[i]][[2]]$Released)
  Runtime[i]<-ifelse(is.null(show_all[[i]][[2]]$Runtime),NA,show_all[[i]][[2]]$Runtime)
  imdbVotes[i]<-ifelse(is.null(show_all[[i]][[2]]$imdbVotes),NA,show_all[[i]][[2]]$imdbVotes)
  imdbRating[i]<-ifelse(is.null(show_all[[i]][[2]]$imdbRating),NA,show_all[[i]][[2]]$imdbRating)
  Type[i]<-ifelse(is.null(show_all[[i]][[2]]$Type),NA,show_all[[i]][[2]]$Type)
  totalSeasons[i]<-ifelse(is.null(show_all[[i]][[2]]$totalSeasons),NA,show_all[[i]][[2]]$totalSeasons)
  
}

comb<-data.frame(Title,Year,Released,Runtime,imdbVotes,imdbRating,Type,totalSeasons)
head(comb)

###############################
#KEEP 'PRESTIGE' SHOWS
###############################

#Remove all NA value rows
comb<-comb[rowSums(is.na(comb)) != ncol(comb),]
dim(comb)

#Remove movies
comb<-comb[comb$Type=="series",]
dim(comb)

#Format ratings field
table(comb$imdbRating)
comb$totalratings<-gsub(",","",comb$"imdbVotes")

#Remove shows with no ratings
comb<-comb[comb$totalratings!="N/A",]
dim(comb)

#Numeric total user ratings
comb$totalratingsnum<-as.numeric(comb$totalratings)
summary(comb$totalratingsnum)

#At least 5000 user ratings
comb<-comb[comb$totalratingsnum>=5000,]
summary(comb$totalratingsnum)
dim(comb)

#Numeric show rating
comb$ratingnum<-as.numeric(comb$imdbRating)
summary(comb$ratingnum)

#At least 7.0 rating
comb<-comb[comb$ratingnum>=7,]
dim(comb)

#Take first four of year
comb$releaseyear<-as.numeric(substr(comb$Year,1,4))
table(comb$releaseyear)

#Only shows released on/after year 2000
comb<-comb[comb$releaseyear>=2000,]
dim(comb)

#Must have English language version (not necessarily shot in English)
comb<-comb[grepl('English',comb$Language),]
dim(comb)

#Get show running time
table(comb$Runtime)
#Only had minutes in data so can go straight to numeric (be careful with movies as may be in hours)
comb$runtime_num<-as.numeric(regmatches(comb$Runtime, gregexpr("[[:digit:]]+", comb$Runtime)))

#Set NAs to zero
comb$runtime_num<-ifelse(comb$Runtime=="N/A",0,comb$runtime_num)
summary(comb$runtime_num)
#Now have approximate runtime

######################################
#Scrape show metadata for remaining shows
######################################

#Title IDs to scrape
scrape_t_all<-t_all[as.numeric(rownames(comb))]

#Initialize scraper
rD<-rsDriver()
remDr <- rD[["client"]]
remDr$open()

scrape_show_all<-{}
for(s in 1:length(scrape_t_all)){
  
  #Be considerate
  Sys.sleep(2)
  
  print(s)
  show_id<-scrape_t_all[s]

  #Go to page, get source, then get HTML
  scrape_url<-paste0('http://www.imdb.com/title/',show_id)
  remDr$navigate(scrape_url)
  page_source<-remDr$getPageSource()
  scrape_dat <- read_html(page_source[[1]])
  rm(page_source)
  
  show <- list(show_id, scrape_dat)
  scrape_show_all[[s]]<-show
  rm(show)
  rm(scrape_dat)
  rm(show_id)
  
  #Save output
  if((s %% 20)==0){
    saveRDS(scrape_show_all, file = paste0(path,"scrape_show_all.rds"))
  }
  
}
saveRDS(scrape_show_all, file = paste0(path,"scrape_show_all.rds"))
remDr$close()
rD[["server"]]$stop() 

#scrape_show_all<-readRDS(file = paste0(path,"scrape_show_all.rds"))

#Get number of episodes
ep_summ_all<-NULL
for(e in 1:length(scrape_t_all)){
  
  print(e)
  x<-scrape_show_all[[e]][[2]]
  
  #Epsiodes
  episodes<-html_nodes(x,xpath="//*[@id='title-overview-widget']/div[2]/div[3]/a/div/div/span") %>% html_text()
  
  #Get number
  episode_num<-as.numeric(regmatches(episodes, gregexpr("[[:digit:]]+", episodes)))
  
  #Put all together
  ep_summ<-data.frame(episodes,episode_num,stringsAsFactors = FALSE)
  
  #Add if episodes found
  if(dim(ep_summ)[1]>0){
    ep_summ$ID<-scrape_show_all[[e]][[1]]
    
    #Add to all
    ep_summ_all<-rbind(ep_summ_all,ep_summ)
  }
  
  rm(ep_summ)
  rm(episodes)
  rm(episode_num)
  rm(x)
  
}

head(ep_summ_all)
tail(ep_summ_all)

#Merge to comb
comb<-merge(comb,ep_summ_all,by=c("ID"))

#Merge runtime with episode count, accounting for how IMDB deals with one season shows
comb$runtime_num_total<-ifelse(comb$totalSeasons=="1" & comb$runtime_num>100,comb$runtime_num,comb$runtime_num*comb$episode_num)
summary(comb$runtime_num_total)

##################################
#Scrape IMDB awards page
##################################

#Initialize scraper
rD<-rsDriver()
remDr <- rD[["client"]]
remDr$open()

scrape_awards_all<-{}
for(s in 1:length(scrape_t_all)){
  
  Sys.sleep(2)
  
  print(s)
  show_id<-scrape_t_all[s]
  print(show_id)
  
  #Go to page, get source, then get HTML
  scrape_url<-paste0('http://www.imdb.com/title/',show_id,'/awards?ref_=tt_awd')
  remDr$navigate(scrape_url)
  page_source<-remDr$getPageSource()
  scrape_dat <- read_html(page_source[[1]])
  rm(page_source)
  
  show <- list(show_id, scrape_dat)
  scrape_awards_all[[s]]<-show
  rm(show)
  rm(scrape_dat)
  rm(show_id)
  
  #Save output
  if((s %% 20)==0){
    saveRDS(scrape_awards_all, file = paste0(path,"scrape_awards_all.rds"))
  }
  
}

#Summarize awards won
award_summ_all<-NULL
for(a in 1:length(scrape_t_all)){
  
  print(a)
  x<-scrape_awards_all[[a]][[2]]
  
  #Rows
  awards<-html_nodes(x, "td.title_award_outcome") %>% html_nodes(xpath='@rowspan') %>% html_text()
  #Type
  type<-html_nodes(x, "td.title_award_outcome") %>% html_text()
  
  #Flag if a win or nomination
  win_flag<-ifelse(grepl(" Won",type),1,0)
  nominated_flag<-ifelse(grepl(" Nominated",type),1,0)
  
  #Remove those patterns
  type<-gsub("Won", "", type)
  type<-gsub("Nominated", "", type)
  
  #Remove spaces
  type<-gsub(" ", "", type)
  type<-gsub("\n", "", type)
  
  #Put all together
  award_summ<-data.frame(type,win_flag,nominated_flag,awards)
  
  #Add if awards found
  if(dim(award_summ)[1]>0){
    award_summ$ID<-scrape_awards_all[[a]][[1]]
    
    #Add to all
    award_summ_all<-rbind(award_summ_all,award_summ)
  }
  
  rm(award_summ)
  rm(type)
  rm(win_flag)
  rm(nominated_flag)
  rm(x)
  rm(awards)
  
}


#Summarize total awards
award_summ_all$type<-as.character(award_summ_all$type)
award_summ_all$awards<-as.numeric(award_summ_all$awards)

# type_totals<-award_summ_all %>% group_by(type) %>% summarise(
#   total=n()
#   ,awards=sum(awards))
# View(type_totals)

###################################
#Ran into issues calculating show runtime as some of IMDB's listed episodes were unaired
#Get episode info for latest season
#Check if unaired episodes are being listed
#Keep going back through seasons until only aired episodes are found
###################################

#Initialize scraper
rD<-rsDriver()
remDr <- rD[["client"]]
remDr$open()

unaired_summ_all<-NULL
for(s in 1:length(scrape_t_all)){
  
  print(s)
  show_id<-scrape_t_all[s]

  #Get total seasons
  total_seasons<-comb[comb$ID==show_id,"totalSeasons_num"]
  
  #Set unaired flag
  unaired_flag<-1
  
  while(total_seasons>1 & unaired_flag==1){
    
    print(total_seasons)
    
    Sys.sleep(2)
    
    #Go to page, get source, then get HTML
    scrape_url<-paste0('http://www.imdb.com/title/',show_id,'/episodes?season=',total_seasons)
    remDr$navigate(scrape_url)
    page_source<-remDr$getPageSource()
    scrape_dat <- read_html(page_source[[1]])
    rm(page_source)
    
    #Get episode titles and details
    desc<-html_nodes(scrape_dat, "div.clear div.info strong a") %>% html_nodes(xpath='@title') %>% html_text()
    
    #Episode description  
    details<-html_nodes(scrape_dat, "div.clear div.info div.item_description")  %>% html_text()
    
    #Unaired episodes
    unaired_total<-sum(grepl("Episode #",desc) & nchar(details)<70)
    
    #Put all together
    unaired_summ<-data.frame(unaired_total,show_id)
    unaired_summ_all<-rbind(unaired_summ_all,unaired_summ)
    
    #If no unaired episode, update flag
    if(unaired_total==0){
      unaired_flag<-0
    }
    
    #Go back a season
    total_seasons<-total_seasons-1
    
    rm(desc)
    rm(details)
    rm(unaired_summ)
    
  }
  
}

head(unaired_summ_all)
unaired_summ_all.cp<-unaired_summ_all
unaired_summ_all$ID<-as.character(unaired_summ_all$show_id)
unaired_summ_all<-subset(unaired_summ_all,select=-c(show_id))

#Sum up
unaired_summ_all_grp<-unaired_summ_all %>% group_by(ID) %>% summarise(unaired_total=sum(unaired_total))

#Merge to comb
comb<-merge(comb,unaired_summ_all_grp,by=c("ID"),all.x=TRUE)
comb[is.na(comb$unaired_total),"unaired_total"]<-0
sum(comb$unaired_total)

#Fix a few
comb[comb$ID=="tt1231460","unaired_total"]<-0
comb[comb$ID=="tt0350448","unaired_total"]<-0
comb[comb$ID=="tt0421460","unaired_total"]<-0
comb[comb$ID=="tt1442550","unaired_total"]<-0

#Redo total episodes, minutes metrics
comb$episode_num_v2<-comb$episode_num-comb$unaired_total
summary(comb$unaired_total)

#Merge runtime with episode count, accounting for how IMDB deals with one season shows
comb$runtime_num_total_v2<-ifelse(comb$totalSeasons=="1" & comb$runtime_num>100,comb$runtime_num,comb$runtime_num*comb$episode_num_v2)
summary(comb$runtime_num_total_v2)

#Minutes per awards
comb$mins_per_award<-ifelse(comb$wins>0,comb$runtime_num_total_v2/comb$wins,0)

#Hours of show
comb$hours<-comb$runtime_num_total_v2/60

#Save output
saveRDS(comb, file = paste0(path,"imdb_tv_dat.rds"))
