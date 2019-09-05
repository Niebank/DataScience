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
#connect to db
conep <- DBI::dbConnect(odbc::odbc(), Driver = "PostgreSQL ANSI(x64)", 
                        Server = "", database = "", UID = (""), 
                        PWD = (""), port = 5432, timeout = 10)
#get job substraight
JobSS <- dbGetQuery(conep, "select ccmasterid, ccjobpart, jcmasterid, icmasterid from jobcost where postingstatus='Closed'and icmasterid !='0' and icmasterid !='MTADHCLREMO05412' and icmasterid !='OVL004CLEMBOSS05412' and icmasterid !='OVL004CLEMBOSS06112' and icmasterid !='OVL005POLYPRM05112' and icmasterid !='VBN013W2S012612'
                    and icmasterid !='CV3M40CMW006012' and icmasterid !='CV3MIJ40C20W5412' and icmasterid !='PAB020SIGNWRIT04812' and icmasterid !='PP008TPQ2BO05712' and icmasterid !='PSV008RAMW05412' and icmasterid !='VBN0131SDULT05412' and icmasterid != 'VBN0131SDULT06312' and icmasterid != 'VBN013W2SBO05412' and icmasterid !='VMG020W04000120'
                    and icmasterid != 'VMR010WFRP06012' and icmasterid !='VPS004CLAD05412' and icmasterid !='VPS004WBOPM5412' and icmasterid !='VPS004WBOPM6012' and icmasterid !='VPS004WHFG05412' and icmasterid !='VPS004WPRM05412' and icmasterid !='VPS004WRMV05412'
                    and icmasterid !='VPS3MIJ180C5412' and icmasterid !='VPS3MIJ180C6012' and icmasterid !='VWP008NOLAR05412' and icmasterid !='VWP020DRMAT5412'  and icmasterid !='VWP020DRMAT6012' and icmasterid !='VWP020DRSUE5412' and icmasterid !='VWP020DSSUE6012' and jcmasterid != '000' and jcstartdate >= '2018-01-01'and jcmasterid = '01525'")
JobSS[is.na(JobSS)] <- 0
#JobSS <- aggregate(. ~ ccmasterid+ccjobpart+jcmasterid+icmasterid, JobSS, mean)
JobSS <- setnames(JobSS, old = c("ccmasterid", "ccjobpart", "jcmasterid", "icmasterid"), new = c("job", "jobpart", "matsactivitycd", "partnum"))

#get press ops
JobPrs <- dbGetQuery(conep, "select ccmasterid, ccjobpart, jcmasterid, jcacthours from jobcost where postingstatus='Closed' and jcmasterid != '000' and jcstartdate >= '2018-01-01'and (jcmasterid = '72210' or jcmasterid = '72110' or jcmasterid = '72510' or jcmasterid = '71610' or jcmasterid = '71520'
                       or jcmasterid = '71710') order by ccmasterid")
JobPrs[is.na(JobPrs)] <- 0
JobPrs <- aggregate(. ~ ccmasterid+ccjobpart+jcmasterid, JobPrs, sum)
JobPrs <- setnames(JobPrs, old = c("ccmasterid", "ccjobpart", "jcmasterid", "jcacthours"), new = c("job", "jobpart", "pressactivitycd", "hours"))


# Press ops
Pressform <- dbGetQuery(conep, "select job, jobpart, runhours, presssheets, press, colorsside1, colorsside2 from jobpartpressform")

JobPrsSide <- left_join(x=JobPrs, y=Pressform, by.x=c("job", "jobpart"), by.y =c("job", "jobpart"))

PressOps <- left_join(x=JobPrsSide, y=JobSS, by.x=c("job", "jobpart"), by.y =c("job", "jobpart"))


#get cost center details
ActivityCd <- dbGetQuery(conep, "select jcmasterid, jcdescription from activitycode where active = TRUE")
ActivityCd <- setnames(ActivityCd, old = c("jcmasterid"), new = c("pressactivitycd"))
PressOps <- left_join(x=PressOps, y=ActivityCd, by.x="pressactivitycd", by.y="jcmasterid")

fltrPressOps <- filter(PressOps, hours > 1.0, (str_detect(partnum, "INKDURST")==FALSE))
fltrPressOps <- distinct(fltrPressOps, job, jobpart, .keep_all = TRUE)
fltrPressOps$brshr <- (fltrPressOps$presssheets / fltrPressOps$hours)

fltrPressOps <- filter(fltrPressOps, fltrPressOps$brshr < 200 & fltrPressOps$colorsside2 == 0 & fltrPressOps$colorsside1 == 4)

#create bins
qtybins <- fltrPressOps %>% mutate(category=cut(presssheets, breaks=c(0, 9, 50, 100, 300, 500, Inf), labels=c("<10","10-50", "51-100", "101-300", "301-500", ">500")))

fitpress <- aov(brshr ~ category, data = qtybins)
print(fitpress)
summary(fitpress)
PressRate <- ggplot(fltrPressOps, aes(x=partnum, y=brshr)) + geom_boxplot(aes(fill=partnum), show.legend = FALSE) + scale_fill_viridis_d() + theme(axis.text.x = element_text(angle = 90, hjust = 2.5, size = 8))
print(PressRate)



#get estimated job cost data
#EstCost <- dbGetQuery(conep, "select ccmasterid, jcmasterid, jcestimatedcost, jcesthours, jcestinventoryqty from jobcost where postingstatus='Closed'and jcmasterid >0 and jcstartdate > 2016-12-31")
#EstCost[is.na(EstCost)] <- 0

#aggregate cost lines by costcenter and job
#AgJobcst <- aggregate(. ~ jcmasterid+ccmasterid, JobCost, sum)
#AgEstcst <- aggregate(. ~ jcmasterid+ccmasterid, EstCost, sum)

#merge actual and estimated cost
#CostComb <- full_join(x=AgJobcst, y=AgEstcst, by.x=c("ccmasterid","jcmasterid"), by.y=c("ccmasterid","jcmasterid"))


write.csv(fltrPressOps, file = "Pressform.csv")
#merge cost and costcenter
#CtrComb <- merge(x=CostComb, y=ActivityCd, by.x="jcmasterid", by.y="jcmasterid", all.x=TRUE)
Hrsmerge <- left_join(x=JobCost, y=ActivityCd, by.x="jcmasterid", by.y="jcmasterid")


#get meta data
JbDt <- dbGetQuery(conep, "select ccmasterid, ccdatesetup, jobtype, ccstatus from job")
#EstMeta <- dbGetQuery(conep, "select estimatenumber, customerprospectname, estimatorid, deliverydate from estimate order by estimatenumber")
JbEsid <- dbGetQuery(conep, "select ccmasterid, jcmasterid, icmasterid from jobcost")

#merge meta fields
#EstComb <- merge(x=EstMeta, y=JbEsid, by.x="estimatenumber", by.y="esmasterid", all.x = TRUE)
Hrscomb <- left_join(x=Hrsmerge, y=JbDt, by.x=c("ccmasterid", "jcmasterid"), by.y=c("ccmasterid", "jcmasterid"))
HrsComb2 <- left_join(x=Hrscomb, y=JbEsid, by.x=c("ccmasterid", "jcmasterid"), by.y=c("ccmasterid", "jcmasterid"))
#unique job numbers only
#DistEstComb <- distinct(HrsComb2, ccmasterid, .keep_all=TRUE)

#merge meta fields with cost
#Alljb <- merge(x=DistEstComb, y=JbDt, by.x = "ccmasterid", by.y = "ccmasterid", all.x = TRUE)
#AllComb <- merge(x=CtrComb, y=Alljb, by.x="ccmasterid", by.y="ccmasterid", all.x=TRUE)



#calculate cost variance
#AllComb$var <- (AllComb$jcestimatedcost - AllComb$jcactcost)
#AllComb$pct_var <-ifelse(AllComb$jcestimatedcost == 0 & AllComb$jcactcost > 0, -100, ifelse (AllComb$jcestimatedcost ==0 & AllComb$jcactcost==0, 0, ((AllComb$jcestimatedcost-AllComb$jcactcost)/AllComb$jcestimatedcost) * 100))

#subset data by date
#Jbcstfiltered <- filter(AllComb, ccdatesetup >= "2018-01-01")

#write to csv
#write.csv(Jbcstfiltered, file = "Jbcstfiltered.csv")

#Jbcstfiltered2 <- filter(Jbcstfiltered, between(Jbcstfiltered$pct_var, -1000, 1000))
#Jbcstfiltered3 <- filter(Jbcstfiltered, between(Jbcstfiltered$var, -5000, 5000))

#theme_set(theme_minimal())
#fit3 <- aov(var ~ jccostcenterid, data = Jbcstfiltered2)

#PctVarplt <- ggplot(Jbcstfiltered2, aes(x=jcdescription, y=pct_var)) + geom_boxplot(aes(fill=jcdescription), show.legend = FALSE) + scale_fill_viridis_d()

#Varplt <- ggplot(Jbcstfiltered3, aes(x=jcdescription, y=var)) + geom_boxplot(aes(fill=jcdescription), show.legend = FALSE) + scale_fill_viridis_d()


