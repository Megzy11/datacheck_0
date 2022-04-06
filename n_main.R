rm = list(ls())
site_based_drivers_HH <- readRDS("C:/Users/mmmmm/Desktop/datachecking/dc/site_based_drivers_HH.rds")
#need:sitename, gpp?
library(tidyverse)
library(ggplot2)
library(lubridate)
#1.predealing
source("n_functions.R")

#dealing withe the data first convert into daily
dat1 <- Pre(mydata = site_based_drivers_HH, site = "SE-Nor")
#add outliers
dat2 <- add_outliers(mydata = dat1)
#add drift detection
dat3 <- add_drift(mydata = dat2)
#add spurious data
dat4 <- add_spurious(mydata = dat3)
#add the growing seasons
dat5 <- GrowingSeason(mydata = dat4)
##add step change detection, but creat a different dataset because use the monthly data
#should add into the initial dataset as well?
dat6 <- add_step_change(mydata = dat5)
