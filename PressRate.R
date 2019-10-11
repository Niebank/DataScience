#load libraries
library(DBI)
library(odbc)
library(stringr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(glmnet)
library(data.table)
library(tidyverse)
library(caret)
library(skimr)
library(RANN)
library(randomForest)
library(kernlab)
library(dataPreparation)
library(broom)
library(glmnetUtils)
library(usdm)
library(MASS)
#connect to db
conep <- DBI::dbConnect(odbc::odbc(), Driver = "PostgreSQL ANSI(x64)", 
                        Server = "172.16.0.50", database = "epace", UID = ("epace_read"), 
                        PWD = ("epace"), port = 5432, timeout = 10)
#get job details and substraight
JobSS <- dbGetQuery(conep, "select ccmasterid, ccjobpart, jcmasterid, icmasterid from jobcost where postingstatus='Closed'and icmasterid !='0' and jcmasterid != '000' and jcstartdate >= '2019-01-01'and (jcmasterid = '01525' or jcmasterid = '1537' or jcmasterid = '1528' or jcmasterid = '1534' or jcmasterid = '1538' or jcmasterid = '1542' or jcmasterid = '1543' or jcmasterid = '1505' or jcmasterid = '1515' or jcmasterid = '41510')")
#JobSS[is.na(JobSS)] <- 0
JobSS$icmasterid <- str_trim(JobSS$icmasterid, side = c("both", "left", "right"))
JobSS$MatType <- ifelse(JobSS$icmasterid %in% c("MTADHCLREMO05412", "OVL004CLEMBOSS05412", "OVL004CLEMBOSS06112", "OVL005POLYPRM05112", "VBN013W2S012612",
              "CV3M40CMW006012", "CV3MIJ40C20W5412", "CV23MTDDIST4812", "CV23MTDDUST6012", "CV43MTDFROS4812", "VPERW006CL05412", "VPS004WR5285412", "VWP020DRCLA05412", "VWP020DRTERR05412", "PAB020SIGNWRIT04812", "PP008TPQ2BO05712", "PSV008RAMW05412", "VBN0131SDULT05412", "VBN0131SDULT06312", "VBN013W2SBO05412", 'VMG020W04000120', "VMR010WFRP06012", "VPS004CLAD05412", "VPS004WBOPM5412",
              "VPS004WBOPM6012", "VPS004WHFG05412", "VPS004WPRM05412", "VPS004WRMV05412", "VPS3MIJ180C5412", "VPS3MIJ180C6012", "VWP008NOLAR05412", "VWP020DRMAT5412", "VWP020DRMAT6012", "VWP020DRSUE5412", "VWP020DSSUE6012"), "roll", ifelse(JobSS$jcmasterid %in% c("1537", "1528", "1534", "1538", "1542", "1543", "1505","1515", "41510"), "ink", "sheet"))
JobSS <- setnames(JobSS, old = c("ccmasterid", "ccjobpart", "jcmasterid", "icmasterid"), new = c("job", "jobpart", "matsactivitycd", "partnum"))

#get press ops
JobPrs <- dbGetQuery(conep, "select ccmasterid, ccjobpart, dcmasterid, jcmasterid, jcacthours from jobcost where postingstatus='Closed' and jcmasterid != '000' and jcstartdate >= '2019-01-01'and (jcmasterid = '72210' or jcmasterid = '71600'  or jcmasterid = '72110' or jcmasterid = '72510' or jcmasterid = '71610' or jcmasterid = '71520'
                       ) order by ccmasterid")
JobPrs[is.na(JobPrs)] <- 0
JobPrs <- aggregate(. ~ ccmasterid+ccjobpart+jcmasterid+dcmasterid, JobPrs, sum)
JobPrs <- setnames(JobPrs, old = c("ccmasterid", "ccjobpart", "jcmasterid", "jcacthours", "dcmasterid"), new = c("job", "jobpart", "pressactivitycd", "hours", "Operator"))

# Join with Press form data
Pressform <- dbGetQuery(conep, "select job, jobpart, presssheets, runimpressions, runmethod, impressionsperhour,  colorsside1, colorsside2, runsizeheight, runsizewidth from jobpartpressform")
JobPrsSide <- left_join(x=JobPrs, y=Pressform, by.x=c("job", "jobpart"), by.y =c("job", "jobpart"))
PressOps <- left_join(x=JobPrsSide, y=JobSS, by.x=c("job", "jobpart"), by.y =c("job", "jobpart"))

#Join with cost center details
ActivityCd <- dbGetQuery(conep, "select jcmasterid, jcdescription from activitycode where active = TRUE")
ActivityCd$jcdescription <- str_trim(ActivityCd$jcdescription, side = c("both", "left", "right"))
ActivityCd <- setnames(ActivityCd, old = c("jcmasterid", "jcdescription"), new = c("pressactivitycd", "Press"))
PressOps <- left_join(x=PressOps, y=ActivityCd, by.x="pressactivitycd", by.y="jcmasterid")
PressOps$Operator <- str_trim(PressOps$Operator, side = c("both", "left", "right"))


#filter and format
fltrPressOps <- filter(PressOps, hours > .1, MatType == "sheet", partnum != "INKDURST", partnum != "INKONSETWHT")
fltrPressOps <- distinct(fltrPressOps, job, jobpart, .keep_all = TRUE)
fltrPressOps$brshr <- (fltrPressOps$presssheets / fltrPressOps$hours)
fltrPressOps$presssheets <- cut(fltrPressOps$presssheets, c(0, 50, 120, 500))
fltrPressOps$Operator <- as.factor(fltrPressOps$Operator)
fltrPressOps$presssheets <- as.factor(fltrPressOps$presssheets)
fltrPressOps$colorsside1 <- as.factor(as.integer(fltrPressOps$colorsside1))
fltrPressOps$colorsside2 <- as.factor(as.integer(fltrPressOps$colorsside2))
fltrPressOps$Press <- as.factor(fltrPressOps$Press)
fltrPressOps <- filter(fltrPressOps, between(fltrPressOps$brshr,10, 150))
drops <- c("job", "jobpart", "matsactivitycd", "pressactivitycd", "MatType", "runsizewidth", "hours")
fltrPressOps <- fltrPressOps[ , !(names(fltrPressOps) %in% drops)]
fltrPressOps <- na.omit(fltrPressOps)
#random sample of dataset
fltrPressOps <- fltrPressOps[sample(nrow(fltrPressOps), 200),]
#fltrPressOps <- fltrPressOps[-c(1216,1196,1168,980,649,643,172,131),]

#Histogram
Histdata <- filter(fltrPressOps, Press == "Inca X2 Run")
x <- fltrPressOps$brshr
h<-hist(x, breaks=20, col="green", xlab="Boars/hr",
        main="Inca X2 Rate All")
xfit<-seq(min(x),max(x),length=50)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

#Run Linear model
fit <- lm(log(brshr)~ presssheets+Press+colorsside1+colorsside2, data = fltrPressOps)
par(mfrow = c(2,2))
Plot1 <- plot(fit)
fitresid <- resid(fit)
Plotfit <- plot(exp(predict(fit)), fltrPressOps$brshr, xlab="Predicted", ylab="Actual")+ abline(a=0, b=1)

summary(fit)
confint(fit)




#ML Process
#Impute
preProcess_missingdata_model <- preProcess(fltrPressOps, method='knnImpute')
fltrPressOps <- predict(preProcess_missingdata_model, newdata = fltrPressOps)
anyNA(fltrPressOps)

#Preprocessing
preProcess_range_model <- preProcess(fltrPressOps, method='range')
fltrPressOps <- predict(preProcess_range_model, newdata = fltrPressOps)

#Split data into training and test sets
set.seed(100)
trainRowNumbers <- createDataPartition(fltrPressOps$brshr, p=0.8, list=FALSE)
trainSet <- fltrPressOps[trainRowNumbers,]
testSet <- fltrPressOps[-trainRowNumbers,]

x = trainSet[, 1:5]
y = trainSet$brshr

#Create dummy vars
dummies_model <- dummyVars(brshr ~ ., data=trainSet)
trainSet_mat <- predict(dummies_model, newdata = trainSet)
trainSet <- data.frame(trainSet_mat)

# Append the Y variable and verify min max values 0, 1
trainSet$brshr <- y
apply(trainSet[, 1:13], 2, FUN=function(x){c('min'=min(x), 'max'=max(x))})

#Model Feature selection
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#Run Modles
set.seed(825)
model_lm = train(brshr ~ ., data=trainSet, method='lm', trControl = control, verbose = FALSE)

model_lm



#Predict on Test set
preProcess_missingdata_model <- preProcess(testSet, method='knnImpute')
testSet <- predict(preProcess_missingdata_model, newdata = testSet)
anyNA(testSet)

z <- testSet$brshr

dummies_model <- dummyVars(brshr ~ ., data=testSet)
testSet_mat <- predict(dummies_model, newdata = testSet)
testSet <- data.frame(testSet_mat)

testSet$brshr <- z

preProcess_range_model <- preProcess(testSet, method='range')
testSet <- predict(preProcess_range_model, newdata = testSet)

predicted_lm <- predict(model_lm, testSet)


