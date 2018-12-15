#
# set working directory
#

setwd("C:/Users/cburdick/OneDrive - NINTHDECIMAL/NewMetric/Attribution/Version2/Attribution")

#
# load necessary & useful libraries - NOTE: Hadley Wickham is a god!
#

library(tidyverse)

#
# set seed for reproducible results
#

set.seed(123)

#
# Stop annoying r behavior
#

options(stringsAsFactors=FALSE)

#
# Generate simulated synthetic user data including ad exposure & visitation
#

source("CreateUsersFun.R")

user.df = create.users.fun(dummy.arg = NULL)

#print(summary(user.df))
#write_csv(user.df,"users_verSun09Dec18.csv")


source("AttribWeightFun.R")

campaign.length = max(user.df$day)

attrib.wts.lst = list(
  
  winNO.none = attrib.weight.fun( campaign.length , campaign.length ,  1.0 ) , 
  win30.none = attrib.weight.fun( campaign.length ,       30        ,  1.0 ) ,
  win30.geom = attrib.weight.fun( campaign.length ,       30        ,  0.1 ) ,
  win14.none = attrib.weight.fun( campaign.length ,       14        ,  1.0 ) ,
  win14.geom = attrib.weight.fun( campaign.length ,       14        ,  0.1 ) 

  )



temp.df = user.df %>% group_by(scale) %>% nest() %>% group_by(user)
  group_by(user) %>% nest() %>% 
  group_by(scale) %>% nest() %>% 
  mutate_at( vars( ends_with(".exp") ) , 
             funs(  eff.winNO.none =
                      . %*% as.matrix(attrib.wts.lst$winNO.none) ) ) %>% 
  mutate_at( vars( ends_with(".exp") ) , 
             funs(  eff.win30.none =
                      . %*% as.matrix(attrib.wts.lst$win30.none) ) ) %>% 
  mutate_at( vars( ends_with(".exp") ) , 
             funs(  eff.win30.geom =
                      . %*% as.matrix(attrib.wts.lst$win30.geom) ) ) %>% 
  mutate_at( vars( ends_with(".exp") ) , 
             funs(  eff.win14.none =
                      . %*% as.matrix(attrib.wts.lst$win14.none) ) ) %>% 
  mutate_at( vars( ends_with(".exp") ) , 
             funs(  eff.win14.geom =
                      . %*% as.matrix(attrib.wts.lst$win14.geom) ) ) %>%
  unnest() %>% ungroup() %>% 
  unnest() %>% ungroup()


# user.df = user.df %>% 
#   rename_at( vars( contains(".exp_eff") ) , funs( str_replace( . , "exp_" , "" ) ) ) 
# 
# temp = user.df  %>% gather(attrib,eff,starts_with("ad1.eff."))





