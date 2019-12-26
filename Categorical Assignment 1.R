library(Hmisc)
library(epitools)
library(DescTools)
library(MASS)
library(epiDisplay)
#Problem 1
sample1 <- rbinom(1000, 8, .08)
confexact <- binom.exact(sample1, 8)
confexact$pass <- ifelse(confexact$lower <= .08 & confexact$upper >= .08, 1, 0)
coverageex <- mean(confexact$pass)
confwald <- BinomCI(sample1, 8, conf.level = .95, method = "wald")
confwald <- as.data.frame(confwald)
confwald$pass <- ifelse(confwald["lwr.ci"]<= .08 & confwald["upr.ci"]>= .08, 1, 0)
coveragewal <- mean(confwald$pass)
print(coverageex)
print(coveragewal)


#Problem 2
Politics = matrix( c(192, 75, 8, 459, 586, 471), nrow=2, ncol=3,byrow = TRUE)
dimnames(Politics) = list(Race = c("Black", "White"), Party = c("Democrat", "Republican", "Independent"))

chisq <- chisq.test(Politics)
print(chisq)
chisq$stdres
LikRat <- loglm( ~ Race + Party, data = Politics)
print(LikRat)


Politics2 = matrix( c(192, 8, 459, 471), nrow=2, ncol=2,byrow = TRUE)
dimnames(Politics2) = list(Race = c("Black", "White"), Party = c("Democrat", "Independent"))

chisq2 <- chisq.test(Politics2)
print(chisq2)
chisq2$stdres

Politics3 = matrix( c(200, 75, 930, 586), nrow=2, ncol=2,byrow = TRUE)
dimnames(Politics3) = list(Race = c("Black", "White"), Party = c("Dem/Ind", "Republican"))

chisq3 <- chisq.test(Politics3)
print(chisq3)
chisq3$stdres


#Problem 3
Cancer = matrix( c(21, 2, 15, 3), nrow=2, ncol=2,byrow = TRUE)
dimnames(Cancer) = list(Treatment = c("Surgery", "Radiation"), Outcome = c("Controlled", "Not_Controlled"))

FTest <- fisher.test(Cancer)
print(FTest)
FTest_gtone <- fisher.test(Cancer, alternative = "greater")
print(FTest_gtone)


