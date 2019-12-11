#load libraries
library(DBI)
library(odbc)
library(stringr)
library(data.table)
library(plyr)
library(dplyr)
library(skimr)
library(lubridate)

#connect to db
conep <- DBI::dbConnect(odbc::odbc(), Driver = "PostgreSQL ANSI(x64)", 
                        Server = "172.16.0.50", database = "epace", UID = ("epace_read"), 
                        PWD = ("epace"), port = 5432, timeout = 10)

#get press data
JobPrs <- dbGetQuery(conep, "select ccmasterid, ccjobpart, dcmasterid, jcmasterid, jcstartdate, jcacthours from jobcost where postingstatus='Closed' and jcmasterid != '000' and jcacthours > 0 and jcstartdate >= '2018-01-01' 
                     and (jcmasterid = '72210' or jcmasterid = '71600'  or jcmasterid = '72110' or jcmasterid = '72510' or jcmasterid = '71610' or jcmasterid = '71520' or jcmasterid = '71710')")
JobPrs <- aggregate(. ~ ccmasterid+ccjobpart+jcmasterid+dcmasterid+jcstartdate, JobPrs, sum)
JobPrs <- setnames(JobPrs, old = c("ccmasterid", "ccjobpart", "jcmasterid", "jcacthours", "dcmasterid"), new = c("job", "jobpart", "pressactivitycd", "runhours", "Operator"))

#get MakeReady hours
JobMr <- dbGetQuery(conep, "select ccmasterid, ccjobpart, jcacthours from jobcost where postingstatus='Closed' and jcmasterid != '000' and jcstartdate >= '2018-01-01' 
                    and (jcmasterid = '72105' or jcmasterid = '72205' or jcmasterid = '72505' or jcmasterid = '71705' or jcmasterid = '71715' or jcmasterid = '71605' or jcmasterid = '71705' or jcmasterid = '71505' or jcmasterid = '71535')")
JobMr <- aggregate(. ~ ccmasterid+ccjobpart, JobMr, sum)
JobMr <- setnames(JobMr, old = c("ccmasterid", "ccjobpart", "jcacthours"), new = c("job", "jobpart", "MRhours"))

#Join run and mr hours
JobPrs <- join(JobPrs, JobMr, by=c("job", "jobpart"), type = "left", match ="all")
JobPrs[is.na(JobPrs)] <- 0
JobPrs$hours <- (JobPrs$runhours+JobPrs$MRhours)
JobPrs$pctMR <- (JobPrs$MRhours/JobPrs$hours)*100
JobPrs$pctRun <- (JobPrs$runhours/JobPrs$hours)*100

# Join with Press form data
Pressform <- dbGetQuery(conep, "select job, jobpart, presssheets, colorsside1, colorsside2 from jobpartpressform")
PressOps <- join(JobPrs, Pressform, by = c("job", "jobpart"), type = "left", match = "all")

#Join with cost center details
ActivityCd <- dbGetQuery(conep, "select jcmasterid, jcdescription from activitycode where active = TRUE")
ActivityCd$jcdescription <- str_trim(ActivityCd$jcdescription, side = c("both", "left", "right"))
ActivityCd <- setnames(ActivityCd, old = c("jcmasterid", "jcdescription"), new = c("pressactivitycd", "Press"))
PressOps <- join(PressOps, ActivityCd, by="pressactivitycd", type = "left", match = "all")


Nonchghrs <- dbGetQuery(conep, "select nonchargeabletype, startdate, enddate, starttime, endtime, employeetime from nonchargeabletime where startdate >= '2019-01-01' and (nonchargeabletype = '5026' 
                        or nonchargeabletype = '5017' or nonchargeabletype = '5027' or nonchargeabletype = '5041' or nonchargeabletype = '5031' or nonchargeabletype = '5011' or nonchargeabletype = '5033'
                        or nonchargeabletype = '5042' or nonchargeabletype = '1' or nonchargeabletype = '5003' or nonchargeabletype = '5004' or nonchargeabletype = '5010' or nonchargeabletype = '5012' 
                        or nonchargeabletype = '5024' or nonchargeabletype = '5028' or nonchargeabletype = '5029' or nonchargeabletype = '5030' or nonchargeabletype = '5032' or nonchargeabletype = '5036'
                        or nonchargeabletype = '5040')")
Nonchgtyp <- dbGetQuery(conep, "select id, costcenter, description from nonchargeabletype")
Nonchghrs <- setnames(Nonchghrs, old = "nonchargeabletype", new = "type")
Nonchgtyp <- setnames(Nonchgtyp, old = "id", new = "type")
Nonchg <- left_join(x=Nonchghrs, y=Nonchgtyp, by.x=c("type"), by.y=c("type"))
Nonchg$tottime <- (Nonchg$endtime-Nonchg$starttime)/60
Nonchg$tottime <- as.numeric(Nonchg$tottime)

PressOps <- distinct(PressOps, job, jobpart, pressactivitycd, .keep_all = TRUE)
#filter and format
PressOps$brshr <- (PressOps$presssheets / PressOps$hours)
PressOps <- filter(PressOps, hours >0, presssheets >0)

PressOps$Process <- ifelse(PressOps$colorsside1 < 5 & PressOps$colorsside2 ==  0, "Oneside no White", ifelse(PressOps$colorsside1 >= 5 & PressOps$colorsside2 == 0, "Oneside White",
                              ifelse(PressOps$colorsside1 < 5 & PressOps$colorsside2 < 5, "Twoside no White", "Twoside White")))


PressOps$mth <- month(as.POSIXlt(PressOps$jcstartdate, format="%Y-%m-%d"))
PressOps$MTD <- month(as.POSIXlt(cut(Sys.Date(),"month"), format="%Y-%m-%d"))
PressMTD <- filter(PressOps, mth == MTD)



Histdata <- filter(PressMTD, brshr <= 300, Press == "S50 Run")
x <- Histdata$brshr
h<-hist(x, breaks=20, col="green", xlab="Boards per Hour",
        main="MTD Production Rate")
xfit<-seq(min(x),max(x),length=50)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2)

Brdhroutliers <- filter(PressMTD, brshr >= 300)

