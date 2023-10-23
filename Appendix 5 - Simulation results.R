
# library(RCurl)
library(tidyverse)
library(ctmm)
library(gdata)
library(sp)
library(openxlsx)
library(maptools)
# library(unmarked)
# library(marcher)



rm(list=ls())

#----------------------------------------------------------------------



rm(list=ls())

setwd("C:/Users/jjv0016/Box/Wood Thrush Project/Manuscripts/Viteks WOTH paper/SimResults3")

tmp = list.files(pattern='*.csv')
tmpFiles = lapply(tmp, read.delim)
test = do.call(rbind, tmpFiles)

setwd("C:/Users/jjv0016/Box/Wood Thrush Project/Manuscripts/Viteks WOTH paper/SimResultsLowDensity05")
tmp4 = list.files(pattern='*.csv')
tmpFiles4 = lapply(tmp4, read.delim)
test4 = do.call(rbind, tmpFiles4)

test = rbind(test, test4)

datas = separate(test, colnames(test), sep=',', into=c('X', 'psi', 'p', 'surveyLength', 'intervalLength', 'nSurveys', 'radius', 'placement', 'instOcc', 'dailyOcc', 'seasonOcc', 'density', 'iteration')) %>% 
  mutate(X = as.numeric(X),
         psi = as.numeric(psi),
         p = as.numeric(p),
         surveyLength = as.numeric(surveyLength),
         nSurveys = as.numeric(nSurveys),
         radius = as.numeric(radius),
         instOcc = as.numeric(instOcc),
         dailyOcc = as.numeric(dailyOcc),
         seasonOcc = as.numeric(seasonOcc),
         density = as.numeric(density),
         iteration = as.numeric(iteration)) %>% 
  mutate(psi = plogis(psi),
         p = plogis(p)) %>% 
  mutate(surveyLength = paste(surveyLength, 'min', sep='')) %>% 
  mutate(nSurveys = paste(nSurveys, 'survs', sep='')) %>% 
  mutate(radius = paste(radius, 'm', sep='')) %>% 
  mutate(placement = ifelse(placement == 'random', 'rand', 'syst')) %>% 
  mutate(density = paste(density, 'birds/ha', sep='')) %>% 
  unite(col='spaceLabel', c(placement, radius), sep='.', remove=F) %>% 
  unite(col='timeLabel', c(nSurveys, surveyLength, intervalLength), sep='.', remove=F) %>% 
  unite(col='label', c(spaceLabel, timeLabel), sep='|') %>% 
  mutate(instOccBias = psi - instOcc,
         dailyOccBias = psi - dailyOcc,
         seasonOccBias = psi - seasonOcc) %>% 
  mutate(surveyLength = factor(surveyLength, levels=c('3min', '10min', '30min'))) %>% 
  mutate(radius = factor(as.character(radius), levels=c('50m', '75m', '100m'))) %>% 
  filter(density != '0.3birds/ha')

# %>% 
#   filter(nSurveys!='3survs')

#3 densities
#3 survey lengths
#3 interval lengths
#3 nSurveys
#3 radius
#2 placements
