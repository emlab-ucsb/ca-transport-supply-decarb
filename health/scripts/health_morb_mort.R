# CA transportation decarb: Mortality vs morbidity costs
# vthivierge@ucsb.edu
# created: 11/30/2021
# updated: 11/30/2021

# set up environment ########################################

rm(list=ls())

#Packages

packages=c("xlsx", "gdata", "dplyr","tidyr", "stringr", "fuzzyjoin", "stringr", "tictoc","maptools",
           "ggplot2", "stargazer", "plm", "cowplot", "sf", "lwgeom","data.table", "foreign", "purrr",
           "future", "furrr", "tidyverse")

lapply(1:length(packages), function(x) 
  ifelse((require(packages[x],character.only=TRUE)==FALSE),install.packages(packages[x]),
         require(packages[x],character.only=TRUE)))

#Set directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
setwd('../../..') #Goes back to home project directory
getwd()

sourceFiles <- "G:/Shared drives/emlab/projects/current-projects/calepa-cn/data/benmap/results"
setwd(sourceFiles) #Goes back to home project directory

# Load mort and morb

morb <-read_csv("./ct_morb_cost.csv")
