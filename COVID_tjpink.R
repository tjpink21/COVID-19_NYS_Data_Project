rm(list = ls())
graphics.off()
library(ggplot2)
library(caret)
covid_data <- read.csv("~/Desktop/Data Mining/us-counties.csv")
set.seed(1111)
library(corrplot)
#Only NYS
i<-1
nys_covid<- data.frame()
for (i in 1:nrow(covid_data)){
  if (covid_data$state[i]=="New York"){
    nys_covid <- rbind(nys_covid,covid_data[i,])
  }
}
nys_covid <- nys_covid[,-c(4)]
covid_by_day <-data.frame()
for (cnt in unique(nys_covid$county)) {
  curr <- data.frame()
  for (i in 1:nrow(nys_covid)){
    if(nys_covid$county[i]==cnt) {
      curr<- rbind(curr,nys_covid[i,])
    }
  }
  for (j in nrow(curr):2){
    curr$cases[j]<-curr$cases[j]-curr$cases[j-1]
    curr$deaths[j]<-curr$deaths[j]-curr$deaths[j-1]
  }
  j<- nrow(curr)
  while (j>=2){
    if (curr$date[j]=="2020-04-19") {
      merp<-(curr$deaths[j])
      curr$deaths[j-2]<-(merp)/3
      curr$deaths[j-1]<-(merp)/3
      curr$deaths[j]<-(merp)/3
      j<-j-2
    }
    j<-j-1
  } 
  covid_by_day <- rbind(covid_by_day,curr)
}
nys_covid<-covid_by_day
nys_covid[is.na(nys_covid)] <-0
for (k in 1:nrow(nys_covid)) {
  if (nys_covid$cases[k]<0) {
    nys_covid$cases[k] <-0
  }
  if (nys_covid$deaths[k]<0) {
    nys_covid$deaths[k] <-0
  }
}

edu_data <- readxl::read_excel("~/Desktop/Data Mining/Education.xls", skip = 4)
#Only NYS
i<-1
while (i<nrow(edu_data)+1)
{
  if (edu_data$State[i]=="NY"){
    i<-i+1
  }
  else{
    
    edu_data <- edu_data[-i,]
  }
}
#most recent
rec_edu_data <- edu_data[,c(3,41,42,43,44,45,46,47)]

poverty_data <- readxl::read_excel("~/Desktop/Data Mining/PovertyReport.xlsx", skip = 4)
poverty_data <- poverty_data[-c(64,65,66,67),]
poverty_data <- poverty_data[,c(4,7,8,9)]

NYS_pop_from1970 <- read.csv("~/Desktop/Data Mining/Annual_Population_Estimates_for_New_York_State_and_Counties__Beginning_1970.csv")
#Most recent 
NYS_pop<- data.frame()
for (i in 1:nrow(NYS_pop_from1970)){
  if (NYS_pop_from1970$Year[i]=="2019"){
    NYS_pop <- rbind(NYS_pop,NYS_pop_from1970[i,])
  }
}

#race data
race_data <- readxl::read_excel("~/Desktop/Data Mining/PopCompare-Race-Ethnicity-2000-2010.xls", skip = 4)
race_data <- race_data[,-c(2,4,5,6,7,8,9)]

#populaion density:
pop_dens <- readxl::read_excel("~/Desktop/Data Mining/a-5.xlsx", skip = 4)
pop_dens <- pop_dens[-c(2,3,4,6,7,8,9,10,11,19,26,33,40,47,54,61,68,75,79,80,81,82,83,84,85,86,87,88,89),]
#Convert cases to cases per 1,000 ppl

NYS_pop$Geography <- as.character(NYS_pop$Geography)

nys_covid$county <- as.character(nys_covid$county)

NYS_pop$Geography = substr(NYS_pop$Geography ,1,nchar(NYS_pop$Geography )-7)

nys_covid$pop <-NA

for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(NYS_pop)) {
    if(NYS_pop$Geography[j]==nys_covid$county[i])
      nys_covid$pop[i]<-NYS_pop$Population[j]
  }
}
for (i in 1:nrow(nys_covid)) {
  if (nys_covid$county=="New York City") {
    nys_covid$pop[i]<-sum(NYS_pop$Population[4],NYS_pop$Population[25],NYS_pop$Population[32],NYS_pop$Population[42],NYS_pop$Population[44])
  }
}
nys_covid$case_ratio <-NA #infections per 1000 ppl
nys_covid$death_ratio <-NA #deaths per 1000 ppl
nys_covid$death_rate <- NA #deaths/infections
nys_covid$case_ratio<- nys_covid$cases*1000/nys_covid$pop
nys_covid$death_ratio<- nys_covid$deaths*1000/nys_covid$pop
nys_covid$death_rate<- nys_covid$deaths/nys_covid$cases

nys_covid$pop_density <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(pop_dens)) {
    if(pop_dens$...1[j]==nys_covid$county[i])
      nys_covid$pop_density[i]<-pop_dens$...8[j]
  }
}
#Top infected counties: New York City, Nassau, Westcheter, Rockland, Orange, Erie, Dutchess, Monroe, Ulster, Albany

nys_covid <-nys_covid[order(nys_covid$date,nys_covid$cases),]
nyc<-data.frame()
Nassau<-data.frame()
Westchester<-data.frame()
Rockland<-data.frame()
Orange<-data.frame()
Erie<-data.frame()
Dutchess<-data.frame()
Monroe<-data.frame()
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "New York City") {
    nyc<-rbind(nyc,nys_covid[k,])
  }
}
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "Nassau") {
    Nassau<-rbind(Nassau,nys_covid[k,])
  }
}
for (i in 1:4){ Nassau<-rbind(Nassau[2+i,],Nassau)
}
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "Westchester") {
    Westchester<-rbind(Westchester,nys_covid[k,])
  }
}
for (i in 1:3){ Westchester<-rbind(Nassau[2+i,],Westchester)
}
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "Rockland") {
    Rockland<-rbind(Rockland,nys_covid[k,])
  }
}
for (i in 1:5){ Rockland<-rbind(Rockland[1+i,],Rockland)
}
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "Orange") {
    Orange<-rbind(Orange,nys_covid[k,])
  }
}
for (i in 1:11){ Orange<-rbind(Orange[3+i,],Orange)
}
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "Erie") {
    Erie<-rbind(Erie,nys_covid[k,])
  }
}
for (i in 1:14){ Erie<-rbind(Erie[3+i,],Erie)
}
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "Dutchess") {
    Dutchess<-rbind(Dutchess,nys_covid[k,])
  }
}
for (i in 1:11){ Dutchess<-rbind(Erie[4,],Dutchess)
}
for (k in 1:nrow(nys_covid)) {
  if(nys_covid$county[k] == "Monroe") {
    Monroe<-rbind(Monroe,nys_covid[k,])
  }
}
for (i in 1:10){ Monroe<-rbind(Monroe[1+i,],Monroe)
}

ggplot() +
  geom_line(data = nyc, aes(x = 1:nrow(nyc), y = case_ratio, color = "New York City",group=1), size = 1)+
  geom_line(data = Nassau, aes(x = 1:nrow(Nassau), y = case_ratio, color = "Nassau",group=1), size = 1)+
  geom_line(data = Westchester, aes(x = 1:nrow(Westchester), y = case_ratio, color = "Westchester",group=1), size = 1)+
  geom_line(data = Rockland, aes(x = 1:nrow(Rockland), y = case_ratio, color = "Rockland",group=1), size = 1)+
  geom_line(data = Orange, aes(x = 1:nrow(Orange), y = case_ratio, color = "Orange",group=1), size = 1)+
  geom_line(data = Erie, aes(x = 1:nrow(Erie), y = case_ratio, color = "Erie",group=1), size = 1)+
  geom_line(data = Dutchess, aes(x = 1:nrow(Dutchess), y = case_ratio, color = "Dutchess",group=1), size = 1)+
  geom_line(data = Monroe, aes(x = 1:nrow(Monroe), y = case_ratio, color = "Monroe",group=1), size = 1)+
  xlab("day since first case") +
  ylab("cases per 1000 people")+
  ggtitle("COVID-19 cases in top NYS Counties")
ggplot() +
  geom_line(data = nyc, aes(x = 1:nrow(nyc), y = death_ratio, color = "New York City",group=1), size = 1)+
  geom_line(data = Nassau, aes(x = 1:nrow(Nassau), y = death_ratio, color = "Nassau",group=1), size = 1)+
  geom_line(data = Westchester, aes(x = 1:nrow(Westchester), y = death_ratio, color = "Westchester",group=1), size = 1)+
  geom_line(data = Rockland, aes(x = 1:nrow(Rockland), y = death_ratio, color = "Rockland",group=1), size = 1)+
  geom_line(data = Orange, aes(x = 1:nrow(Orange), y = death_ratio, color = "Orange",group=1), size = 1)+
  geom_line(data = Erie, aes(x = 1:nrow(Erie), y = death_ratio, color = "Erie",group=1), size = 1)+
  geom_line(data = Dutchess, aes(x = 1:nrow(Dutchess), y = death_ratio, color = "Dutchess",group=1), size = 1)+
  geom_line(data = Monroe, aes(x = 1:nrow(Monroe), y = death_ratio, color = "Monroe",group=1), size = 1)+
  xlab("day since first case") +
  ylab("deaths per 1000 people")+
  ggtitle("COVID-19 deaths in top NYS Counties")
#merge w poverty, race, edu data
nys_covid$Poverty_Rate <- 15.6
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(poverty_data)) {
    if(poverty_data$Name[j]==nys_covid$county[i])
      nys_covid$Poverty_Rate[i]<-poverty_data$Percent...7[j]
  }
}


nys_covid$percent_white <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(race_data)) {
    if(race_data$Area[j]==nys_covid$county[i])
      nys_covid$percent_white[i]<-race_data$`% White*...10`[j]
  }
}

nys_covid$percent_black <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(race_data)) {
    if(race_data$Area[j]==nys_covid$county[i])
      nys_covid$percent_black[i]<-race_data$`% Black*...11`[j]
  }
}

nys_covid$percent_asian <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(race_data)) {
    if(race_data$Area[j]==nys_covid$county[i])
      nys_covid$percent_asian[i]<-race_data$`% Asian*...12`[j]
  }
}

nys_covid$percent_hispanic <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(race_data)) {
    if(race_data$Area[j]==nys_covid$county[i])
      nys_covid$percent_hispanic[i]<-race_data$`% Hispanic...13`[j]
  }
}

rec_edu_data <- rec_edu_data[-c(1),]
rec_edu_data$`Area name` <- as.character(rec_edu_data$`Area name`)
rec_edu_data$`Area name` = substr(rec_edu_data$`Area name`,1,nchar(rec_edu_data$`Area name`)-7)
rec_edu_data$`Area name`[31]<-"New York City"
nys_covid$per_no_hs_diploma <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(rec_edu_data)) {
    if(rec_edu_data$`Area name`[j]==nys_covid$county[i])
      nys_covid$per_no_hs_diploma[i]<-rec_edu_data$`Percent of adults with less than a high school diploma, 2014-18`[j]
  }
}

nys_covid$per_only_hs_diploma <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(rec_edu_data)) {
    if(rec_edu_data$`Area name`[j]==nys_covid$county[i])
      nys_covid$per_only_hs_diploma[i]<-rec_edu_data$`Percent of adults with a high school diploma only, 2014-18`[j]
  }
}

nys_covid$per_some_college <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(rec_edu_data)) {
    if(rec_edu_data$`Area name`[j]==nys_covid$county[i])
      nys_covid$per_some_college[i]<-rec_edu_data$`Percent of adults completing some college or associate's degree, 2014-18`[j]
  }
}

nys_covid$per_bachelor_or_higher <- NA
for (i in 1:nrow(nys_covid)) {
  for (j in 1:nrow(rec_edu_data)) {
    if(rec_edu_data$`Area name`[j]==nys_covid$county[i])
      nys_covid$per_bachelor_or_higher[i]<-rec_edu_data$`Percent of adults with a bachelor's degree or higher, 2014-18`[j]
  }
}

#The nys_covid data set now contains all important predictors

NY_covid_over_time<-NA
for (i in 1:nrow(covid_data)){
  if(covid_data$state[i]=="New York"){
    NY_covid_over_time <- rbind(NY_covid_over_time,covid_data[i,])
  }
}
nys_covid[is.na(nys_covid)] <-0
bite<-NYS_pop$Geography[!NYS_pop$Geography %in% nys_covid$county]
bite # 4 counties not in covid dataset are: Bronx, Kings, Queens, Richmond, they are all part of NYC

a<-cor(nys_covid[c("case_ratio","death_ratio","pop_density","Poverty_Rate","percent_white","percent_hispanic","percent_black","percent_asian","per_no_hs_diploma","per_only_hs_diploma","per_some_college","per_bachelor_or_higher")])
corrplot(a,method= "circle")
nys_covid$pop_density<-as.numeric(nys_covid$pop_density)
#split into test n train
set.seed(1111)
sample <- sample.int(n=nrow(nys_covid), size=floor(.75*nrow(nys_covid)), replace = F)
train <- nys_covid[sample, ]
test <- nys_covid[-sample, ]
library(caret)
library(randomForest)
library(rJava)
library(bartMachine)
library(glmnet)
library(lme4)
library(tree)
library(bootstrap)
library(rpart)
library(gbm)
library(ipred)
library(car)
#LM:
lm.case<-lm(case_ratio~pop_density+Poverty_Rate+percent_white+percent_hispanic+percent_black+percent_asian+per_no_hs_diploma+per_only_hs_diploma+per_some_college+per_bachelor_or_higher, data=train)
summary(lm.case)
y_hat.lm <- predict(lm.case, newdata = test)
resc <- test$case_ratio-y_hat.lm
lm_RMSE_C <- RMSE(test$case_ratio,y_hat.lm)
varImp(lm.case)
qqPlot(resc,main = "LM residuals")
lm.dead<-lm(death_ratio~pop_density+Poverty_Rate+percent_white+percent_hispanic+percent_black+percent_asian+per_no_hs_diploma+per_only_hs_diploma+per_some_college+per_bachelor_or_higher, data=train)
summary(lm.dead)
y_hat.lm <- predict(lm.dead, newdata = test)
resd <- test$death_ratio-y_hat.lm
lm_RMSE_D <- RMSE(test$death_ratio,y_hat.lm)
varImp(lm.dead)
#plot(lm.case)
qqPlot(resc,main = "LM residuals")
#RandomForest:

y_true<-test$case_ratio
rf_c <- randomForest(case_ratio~pop_density+Poverty_Rate+percent_white+percent_hispanic+percent_black+percent_asian+per_no_hs_diploma+per_only_hs_diploma+per_some_college+per_bachelor_or_higher, data=train, n.tree=2000)
rf_c
y_hat.rf_c <- predict(rf_c, newdata = test)
y_hat.rf_c <- as.numeric(y_hat.rf_c)
#rf_c_error <- sum(abs(y_true-y_hat.rf_c))/length(y_hat.rf_c)
rf_c_rmse <- RMSE(y_true,y_hat.rf_c)
res.rf_c <- y_true-y_hat.rf_c
qqPlot(res.rf_c,main = "rf_c residuals")
importance(rf_c)
varImpPlot(rf_c)

y_true<-test$death_ratio
rf_d <- randomForest(death_ratio~pop_density+Poverty_Rate+percent_white+percent_hispanic+percent_black+percent_asian+per_no_hs_diploma+per_only_hs_diploma+per_some_college+per_bachelor_or_higher, data=train, n.tree=2000)
rf_d
y_hat.rf_d <- predict(rf_d, newdata = test)
y_hat.rf_d <- as.numeric(y_hat.rf_d)
#rf_d_error <- sum(abs(y_true-y_hat.rf_d))/length(y_hat.rf_d)
rf_d_rmse <- RMSE(y_true,y_hat.rf_d)
res.rf_d <- y_true-y_hat.rf_d
qqPlot(res.rf_d,main = "rf_d residuals")
importance(rf_d)
varImpPlot(rf_d)
#did not work well:
#boost <- gbm(case_ratio~pop_density+Poverty_Rate+percent_white+percent_hispanic+percent_black+percent_asian+per_no_hs_diploma+per_only_hs_diploma+per_some_college+per_bachelor_or_higher, data=nys_covid, n.tree=1000, shrinkage = .1, interaction.depth = 3)
#boost
#y_hat.bo <- predict(boost, newdata = nys_covid, n.trees = 1000)
#boost_rmse <- RMSE(y_hat.bo,y_true)
#res.boo <- y_true-y_hat.bo
#qqPlot(res.boo,main = "Boosting residuals")
y_true<-test$case_ratio
bagg <- bagging(case_ratio~pop_density+Poverty_Rate+percent_white+percent_hispanic+percent_black+percent_asian+per_no_hs_diploma+per_only_hs_diploma+per_some_college+per_bachelor_or_higher, data=train, nbagg=200, coob= TRUE, control= rpart.control(minsplit = 2,cp=0))
bagg
y_hat.bag <- predict(bagg, newdata = test, n.trees = 2000)
bag_rmse <- RMSE(y_hat.bag,y_true)
res.bag <- y_true-y_hat.bag
qqPlot(res.bag,main = "Bagging residuals")
varImp(bagg)
varImpPlot(bagg)
y_true<-test$death_ratio
bagg <- bagging(death_ratio~pop_density+Poverty_Rate+percent_white+percent_hispanic+percent_black+percent_asian+per_no_hs_diploma+per_only_hs_diploma+per_some_college+per_bachelor_or_higher, data=train, nbagg=200, coob= TRUE, control= rpart.control(minsplit = 2,cp=0))
bagg
y_hat.bag <- predict(bagg, newdata = test, n.trees = 2000)
bag_rmse <- RMSE(y_hat.bag,y_true)
res.bag <- y_true-y_hat.bag
qqPlot(res.bag,main = "Bagging residuals")
varImp(bagg)

#partial dependence plots? for random forests

