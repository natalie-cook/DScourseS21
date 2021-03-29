#install.packages("mice")
#install.packages("modelsummary")
library(mice)
library(modelsummary)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)

#read in wage data
df <- read_csv("ProblemSets/PS7/wages.csv")
View(df)

#dropping observations with missing hgc or tenure
df %<>% drop_na(hgc,tenure)
#make summary table
datasummary_skim(df,histogram=F,output = "latex")

#estimate with only complete cases ie listwise deletion of logwage
est1 <- lm(logwage~hgc+as.factor(college)+poly(tenure,2,raw=T)+age+as.factor(married), data=df)

#use mean imputation to fill in logwage missing values 
#fill in missing values by creating logwage2
df%<>% mutate(logwage2=mean(df$logwage, na.rm=T))
head(df)
df%<>%mutate(logwage2=case_when(!is.na(logwage)~logwage, is.na(logwage)~mean(df$logwage,na.rm = T)))
#run another regression with the new logwage2 which has the overall mean for the missing values
est2 <- lm(logwage2~hgc+as.factor(college)+poly(tenure,2,raw=T)+age+as.factor(married), data=df)

#impute using predicted values from above
#make the thing you're going to plug in 
pred.data=predict(est2, newdata=df)
head(pred.data)
#put it into the data set where values are missing by creating a new logwage3 variable
df%<>% mutate(logwage3=case_when(!is.na(logwage)~logwage, is.na(logwage)~pred.data))
#run the regression with the new logwage3 which imputes using predicted values
est3 <- lm(logwage3~hgc+as.factor(college)+poly(tenure,2,raw=T)+age+as.factor(married), data=df)

#do multiple imputation regression using mice
#make the thing you're gonna plug in
mice.logwage <- mice(df$logwage, m=5, printFlag = FALSE)
#put it in the data set
df%<>% mutate(logwage4=case_when(!is.na(logwage)~logwage, is.na(logwage)~ mice.logwage))
#this is not working for some reason - idk why
#run the regression
est4 <- lm(logwage4~hgc+as.factor(college)+poly(tenure,2,raw=T)+age+as.factor(married), data=df)

#make the nice table to summarize the regressions
modelsummary(list(est1,est2,est3), output = "latex")
#copy past into latex

