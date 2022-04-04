site_based_drivers_HH <- readRDS("C:/Users/mmmmm/Desktop/datachecking/dc/site_based_drivers_HH.rds")
#need:sitename, gpp?
library(tidyverse)
library(ggplot2)
library(lubridate)
#1.predealing
my_file <- site_based_drivers_HH %>% 
  select(starts_with("date"), 
         gpp,
         temp,
         prec,
         ppfd,
         sitename) %>% 
  filter(sitename == "SE-Nor" )
#replace -9999 with NAs
my_file <- my_file %>% 
  na_if(-9999)
#have the daily data
my_daily_file$date
my_daily<- my_file %>%
  group_by(date) %>%
  summarise(gpp = mean(gpp, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            prec = mean(prec, na.rm = TRUE),
            ppdf = mean(ppfd, na.rm = TRUE))
head(my_daily)

#2.outliers
t_lm <- lm(gpp ~
             temp+
             prec+
             ppdf,
           data = my_daily)
res <- t_lm$residuals
outres <- boxplot.stats(res)$out
#some gpp is not exist, what should be done? i use the na.omit...
n1_mydaily <- na.omit(my_daily_file) %>% mutate(res = res) %>% 
  mutate(outline = if_else(res %in% outres,"out", "in"))

#3.drift_detection
drift_dat <- my_daily %>%
  mutate(year_dec = year(date)+ (lubridate::yday(date) - 1)/365)
#create the daily data for using
lm_d <- lm(gpp ~ temp +
             prec+
             ppdf+
             year_dec,
           data = drift_dat)
s <- summary(lm_d)
drift_dat$tfordrift <- s$coefficients["year_dec",3]#?add the t value

#4.spurious_values
c <- my_daily%>% select(gpp)
m <- c$gpp
c1 <- c %>% table() %>% as.data.frame()
crep <- c1[cline,1]#get the rep value
#update: add the extra column here for the freq
c_new <- sapply(c$gpp,function(x){m <- c1[c1$.== x, "Freq"]})
b <- rep(NA,length(c_new))
for (i in 1:length(c_new)){
  b[i] <- c_new[[i]]
}
my_daily <- my_daily %>% mutate(rep = b)

#5.step change
library(strucchange)
test <- my_daily %>% mutate(month_year = month(date), year = year(date))
test <- test %>% mutate(month_dec = year+(month_year-1)/12)
test <- test %>% group_by(month_dec) %>% summarise(gpp = mean(gpp),
                                                   prec = mean(prec),
                                                   ppdf = mean(ppdf),
                                                   temp = mean(temp))

mlmt <- lm(gpp ~ prec+ppdf+temp, data = test)
nrow(test)
sctest(gpp ~ prec+ppdf+temp, data = test, type = "Chow", point =77)#why 5~77
test <- test %>% mutate(tp = NA)
for (i in 5:(nrow(test)-7)){
  tsc <- sctest(gpp ~ prec+ppdf+temp, data = test, type = "Chow",point = i)
  test$tp[i] <- tsc$p.value
}
test <- test %>% mutate(SCpoint = if_else(tp < 0.05,"SC","NON")) %>% 
  mutate()
a <- 1
test$SCvalue <- NA
for(j in 1:nrow(test)){
  if(is.na(test$SCpoint[j])){
    test$SCvalue[j] <- a  
  } else if(test$SCpoint[j] == "SC") {
    a <- a+1
  } else {
    a <- a
  }
  
  test$SCvalue[j] <- a
}
