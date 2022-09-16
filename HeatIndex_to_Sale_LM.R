#load libraries

library(caret)

#read datafiles
simplinear <- read.csv("C:/Users/.csv")
multilinear <- read.csv("C:/Users/")
polyreg <- read.csv("C:/Users.csv")

#Problem 1 - Simple linear model
#segment data train and test sets
set.seed(100)
row.number1 <- createDataPartition(simplinear$Salary, p = .7, list = FALSE)
train1 = simplinear[row.number1,]
test1 = simplinear[-row.number1,]

#linear model on train
fit1 <- train(Salary ~ .,data = train1, method = "lm")

Pred1<- predict(fit1, test1)

#plot predicted on linear model
Plotfit1 <- plot(test1$Years_of_Expertise, Pred1, xlab="HeatIndex", ylab="Predicted Sale")+abline(lm(Miles ~ Age, data = simplinear))

#Problem 2 - Multiple linear model

#segment data train and test sets
set.seed(100)
row.number2 <- createDataPartition(multilinear$Profit, p = .7, list = FALSE)
train2 = multilinear[row.number2,]
test2 = multilinear[-row.number2,]

model<-lm(formula = Profit ~ Product_1 + Product_2 + Product_3 + Location +
Product_1 * Location + Product_2 * Location + Product_3 *
Location, data = multilinear)

#multilinear model on train
fit2 <- train(Profit ~ Product_1+Product_3, data = train2, method = "lm")

Pred <- predict(fit2, test2)

pltfit2 <- plot(train2$Profit, predict(fit2), xlab = "Predicted Profit", ylab = "Actual Profit")+abline(a=0,b=1)


#Determine polynomial
cverror <- numeric(5)
for(i in 1:6){
  train_control <- trainControl(method='LOOCV')
  f <- bquote(HeatIndex ~ poly(Level, .(i)))
  models <- train(as.formula(f), data = polyreg, trControl=train_control, method='glm')
  cverror[i] <- ((models$results$RMSE)^2*.0000001)
  }
cverror

#train poly model
fit3 <- lm(Salary ~ poly(Level,5), data = polyreg)
new <- data.frame(Level = c(6.5))
predvalue <- predict(fit3, new)
print(predvalue)

plot(polyreg$Level, predict(fit3),col='red', xlab = 'Level', ylab = 'Predicted Sale', type = "l")
options(scipen=5)


