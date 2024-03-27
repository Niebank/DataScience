
#read interatation data that contains coefficient limits for variables
interactions <- read.csv("model_variables.csv")
#interactions <- read.Alteryx("#2", mode="data.frame")


#import one line row that allows program to see what original names were before R changes them
headerimport <- read.csv("model_header.csv")
#headerimport <- read.Alteryx("#3", mode="data.frame")
cleanheaders <- as.character(unlist(headerimport))
#read dependedent/independent variable data
mydata <- read.csv("model_measures.csv")
#mydata <- read.Alteryx("#1", mode="data.frame")
headers <- names(mydata)

#remove DNA_ from variable names
alterheaders <- gsub("DNA_","",headers)
badheaders <- gsub("DNA_","",headers)
cleanheaders <- gsub("DNA_","",cleanheaders)

#grab comp number from interacting columns
alterheaders <- gsub("Interacting_","",alterheaders)
comps <- c()
for (i in alterheaders) {
  new <- substr(i,1,regexpr("\\_[^\\_]*$", i)[1]-1)
  comps <- c(comps,new)
}

#create lookup table for variable names that has original names,clean names and comp names
lookup <- data.frame(headers,cleanheaders,badheaders,comps,stringsAsFactors=FALSE)
lookup$comps[lookup$comps==""]<-"same"
temp <- c()
for (i in 1:nrow(lookup)) {
  row<- lookup[i,]
  name <- as.character(row$cleanheaders)
  comp <- as.character(row$comps)
  rename <- gsub(paste("Interacting_",comp,"_",sep=""),"Interacting ",name)
  rename <- gsub("DC-\\d+","DC-",rename)
  rename <- gsub("FSI-\\d+","FSI-",rename)
  rename <- gsub("IRC\\d+","IRC",rename)
  temp <- c(temp,rename)
}
lookup$cleanheaders <- temp
temp <- c()
for (i in 1:nrow(lookup)) {
  row<- lookup[i,]
  name <- as.character(row$badheaders)
  comp <- as.character(row$comps)
  rename <- gsub(paste("Interacting.",comp,".",sep=""),"Interacting.",name)
  rename <- gsub("DC-\\d+","DC-",rename)
  rename <- gsub("IRC\\d+","IRC",rename)
  rename <- gsub("FSI\\d+","FSI-",rename)
  temp <- c(temp,rename)
}
lookup$badheaders <- temp

#read decomp mapping for coefficient restriction
#variableMap <- read.csv("C:\\Pricing\\PRISM\\Database\\Ref\\Decomp Model.csv",check.names=FALSE)
variableMap <- read.csv("//pksfile2/BUSDEV/Pricing Trade Strategy/Kefeli-Active/PRISM/PRISM Digital Core Update/Database/Ref/Decomp Model.csv",check.names=FALSE)



#initialize output with headers
scoredData <- mydata[1,]
scoredData$Score <- 0
scoredOutput <<- data.frame(scoredData$Group,scoredData$Time,scoredData$Score)[FALSE,]
names(scoredOutput) <- c("Group","Time","Score")
variableOutput <- data.frame("Remove","Name",0,0,"Remove")
names(variableOutput) <- c("Group","Name","Coef","Dev","Type")

#create list of packs we will iterate through
groups <- unique(mydata$Group)

#initializeCoefficient&VifFactors
coefOutput <<- data.frame(Doubles=double(),
                          Ints=integer(),
                          Factors=factor(),
                          Logicals=logical(),
                          Characters=character(),
                          stringsAsFactors=FALSE)

#import the following libraries
library(car)
library(plyr)
library(doParallel)
library(splines)
library(data.table)
library(foreach)
library(parallel)
cl <- makeCluster(4)
registerDoParallel(cl)





RidgeModel <- function(group,price.break.value = NULL) {
  library(plyr)
  library(car)
  library(glmnet)
  #groupname <- as.character("KR Corp w/ Roundy's-RMA - Food | ANGEL SOFT BASE : 24 EQ : 2X : 12 CT")  

  groupname <- as.character("Publix Corp-RMA - Food|ANGEL SOFT BASE : 24 EQ : 2X : 12 CT")
  #Assign to data to variable
  mydata.clean <- mydata[mydata$Group==groupname,]
  mydata.clean <- mydata.clean[colSums(!is.na(mydata.clean))>0]
  mydata.clean <- mydata.clean[colSums(mydata.clean!=0)!=0]
  
  #pull interactions for group
  myinteractors <- interactions[interactions$Group==groupname,]
  
  # Create basis partition for price promotional variable
  if (any(names(mydata.clean)=="DNA_Price.Promotional....of.Base.Price.") & price.break.value!=9999){
    Price.x <- mydata.clean$DNA_Price.Promotional....of.Base.Price.
    if (is.null(price.break.value)){
      price.break.value <- log(100)
    }
    Varname <- make.names(paste("Price",as.character(substr(exp(price.break.value),1,2)),sep="."))
    spline.degree <- 1
    spline.X1 <- outer(Price.x,1:spline.degree,"^")
    spline.X2 <- outer(Price.x,price.break.value,"<=")*outer(Price.x,price.break.value,"-")
    basis.matrix <- cbind(spline.X1,spline.X2)
    
    #colnames(basis.matrix)<-c("Price1","Price2","Price3")
    colnames(basis.matrix)<-c("Price1",Varname)
    mydata.clean <- data.frame(mydata.clean,basis.matrix)
    #setnames(mydata.clean,c("X1","X2","X3"),c("Price1","Price2","Price3"))
    #Retrieve all column names
    x.vars <- names(mydata.clean)
    #Retrieve only all column names aside from dependent variable and time
    formula.names <- x.vars[!x.vars %in% c("DNA_Volume..Units.","Time","Group","DNA_Price.Promotional....of.Base.Price.")]
  }else{
    #Retrieve all column names
    x.vars <- names(mydata.clean)
    #Retrieve only all column names aside from dependent variable and time
    formula.names <- x.vars[!x.vars %in% c("DNA_Volume..Units.","Time","Group")]
    # Create the Varname as NULL so that it does not error out in the condition later
    Varname = ""
  }
  
  
  
  
  
  #output formula names if needed
  #create empty list to add columns we want to remove later on
  remove.name <- data.frame("Name",0,0,"Remove")
  names(remove.name) <- c("Name","Coef","Dev","Type")
  
  
  #acv base weighted distribution can hit the .1 limit easily due to deflated actuals.I've encorporated an adjust that will grow the limit
  base_acv_adjust <- 0
  
  
  
  
  
  
  #we repeat only because we need to remove variables at their limit
  repeat {
    #create formula out of variable names and without the names we want removed
    formula.names.loop <- formula.names[!formula.names %in% remove.name$Name]
    x <- paste(formula.names.loop,collapse = "+")
    formula <- paste("DNA_Volume..Units. ~ ",x,sep="")
    
    #responsevector is the dependent variable
    responsevector <- as.vector(mydata.clean[,"DNA_Volume..Units."])
    #peanlized matrix contains independent variables
    penalizedmatrix <- as.matrix(mydata.clean[,formula.names.loop])
    #initialize vectors for ridge weights and limits
    lowervector <- c()
    uppervector <- c()
    ridge.weights <- c()
    #loopthrough decomp mapping to grab limits and weights
    for (z in formula.names.loop) {
      if(z=="(Intercept)") {
        lowervector <- c(lowervector,-Inf)
        uppervector <- c(uppervector,Inf)
      }else if ((z=="Price1")||(z==Varname)){
        if (z=="Price1"){
          lowervector <- c(lowervector,-8)
          uppervector <- c(uppervector,0)
          ridge.weights <- c(ridge.weights,0.5)
        }else{
          lowervector <- c(lowervector,0)
          uppervector <- c(uppervector,8)
          ridge.weights <- c(ridge.weights,0.5)
        }
      }else {
        lookupSubset <- lookup[lookup$headers==z,]
        lookupVar <- as.character(lookupSubset$cleanheaders)
        lookupBad <- as.character(lookupSubset$badheaders)
        lookupComp <- as.character(lookupSubset$comps)
        decompSubset <- variableMap[variableMap$Name==lookupVar,]
        avg <- decompSubset$`Coefficient Average`
        decompDirection <- as.character(decompSubset$`Coefficient Direction`)
        if (decompSubset$`Coefficient Test`=="None") {
          lowervector <- c(lowervector,-Inf)
          uppervector <- c(uppervector,Inf)
        } else if (decompSubset$`Coefficient Test`=="Fixed") {
          if (z=="ACV.Base.Weighted.Distribution") {
            limitadjust = base_acv_adjust
          } else {
            limitadjust = 0
          }
          lowervector <- c(lowervector,decompSubset$`Coefficient Min`)
          uppervector <- c(uppervector,(decompSubset$`Coefficient Max`+limitadjust))
        } else if (decompSubset$`Coefficient Test`=="Variable") {
          lookupCoef <- myinteractors[myinteractors$CompName==lookupComp,lookupBad]
          if (length(lookupCoef)!=1||is.na(lookupCoef)||is.null(lookupCoef)) {
            if (decompSubset$`Coefficient Direction`=="Positive") {
              lowervector <- c(lowervector,0)
              uppervector <- c(uppervector,Inf)
            } else if (decompSubset$`Coefficient Direction`=="Negative") {
              lowervector <- c(lowervector,-Inf)
              uppervector <- c(uppervector,0)
            } else {
              var.dev <- 0
            }
          }else if (decompSubset$`Coefficient Direction`=="Positive") {
            lowervector <- c(lowervector,0)
            uppervector <- c(uppervector,lookupCoef)
          } else if (decompSubset$`Coefficient Direction`=="Negative") {
            lowervector <- c(lowervector,lookupCoef)
            uppervector <- c(uppervector,0)
          }
          
        } else {
          lowervector <- c(lowervector,-Inf)
          uppervector <- c(uppervector,Inf)
        }
        
        lookupWeightMultiplier <- myinteractors[myinteractors$CompName==lookupComp,"Euclidean.Distance"]
        lookupWeightMultiplier <- ifelse(is.na(lookupWeightMultiplier)||lookupWeightMultiplier==0,decompSubset$`Weight`,(lookupWeightMultiplier^2))
        ridge.weights <- c(ridge.weights,lookupWeightMultiplier)
      }
    }
    #run glmnet
    set.seed(1)
    model.ridge <- cv.glmnet(penalizedmatrix,responsevector,alpha=0.05,lambda.min.ratio = 0.0001, nlambda=1000,family = "gaussian",lower=lowervector,upper=uppervector,penalty.factor=ridge.weights)
    #retrieve the final coefficients
    set.seed(1)
    ridge.coefficients <- coef(model.ridge,s="lambda.min")
    coef_table <- data.frame(Variable = ridge.coefficients@Dimnames[[1]][ridge.coefficients@i+1], Coefficient = ridge.coefficients@x)
    #loop through coefficients
    limitcheck <- data.frame(formula.names.loop,lowervector,uppervector)
    names(limitcheck) <- c("Variable","Lower","Upper")
    
    coef_table <- merge(x=coef_table,y=limitcheck,by = "Variable",all.x=TRUE)
    coef_table$RemoveOrder <- 0
    
    for (variables in 2:nrow(coef_table)) {
      variable_temp <- coef_table[variables,]
      order <- 0
      if (abs(variable_temp$Coefficient-variable_temp$Lower)<.00001 || abs(variable_temp$Coefficient-variable_temp$Upper)<.00001) {
        if (grepl("Interacting",variable_temp$Variable)) {
          order <- 3
          #We remove ACV Feature
        } else if (grepl("Feature",variable_temp$Variable)) {
          order <- 2
          #We remove ACV Display
        } else if (grepl("Display",variable_temp$Variable)) {
          order <- 2
          #We remove ACV Distribution
        } else if (grepl("ACV",variable_temp$Variable)) {
          order <- 2
          #The last column we remove is Price
        } else if (grepl("Price",variable_temp$Variable)) {
          # if ((variable_temp$Variable=="Price1")||(variable_temp$Variable=="Price2")||(variable_temp$Variable=="Price3")){
          #   order<-0
          # }else{order <- 1} 
          order <- 1
        } else {
          #If a variable did not fall into the above categories, we remove it just before price
          order <- 1.5
        }
      }
      coef_table$RemoveOrder[variables] <- order
    }
    
    #multiply by negative 1 in order to order ascending and pull variable name if first row
    coef_table <- coef_table[order(coef_table$RemoveOrder*-1),]
    #find coefficient needed to remove
    removeVariable<- coef_table[coef_table$RemoveOrder>0,][1,]
    
    if (sum(coef_table$RemoveOrder)==0) {
      break
    } else if (removeVariable$Variable!="ACV.Base.Weighted.Distribution") {
      new <- data.frame(removeVariable$Variable,removeVariable$Coefficient,0,"At Limit")
      names(new) <- c("Name","Coef","Dev","Type")
      remove.name <- rbind(remove.name,new)
    } else {
      base_acv_adjust = base_acv_adjust + 0.01
    }
    #create empty data frame from calculation coefficient deviations
    #for each variable that was included in final model, we need to test how far out of range the coefficient is
  }
  #score ouput
  if (class(model.ridge)=="try-error") {
    
    mergedStats <- data.frame("Variable" = c("NoModel"),"Coefficient" = c(0.0),check.names=FALSE)
    mergedStats$Group <- groupname
    scoreAdd <- data.frame("Group"=c(groupname),"Time"=c("NoModel"),"Score"=c(0))
    remove.name$Group <- groupname
    #variableAdd <- data.frame("Remove","Name",0,0,"Remove")
    #names(variableAdd) <- c("Group","Name","Coef","Dev","Type")
    output <- list(mergedStats,scoreAdd,remove.name)
    return(output)
  }
  mydata.clean$Score <- predict(model.ridge,penalizedmatrix,s=model.ridge$lambda.min)
  if(exists("price.break.value") && exists("Price.x")){
    mydata.clean$PricePoint <- price.break.value
    mydata.clean$PriceMin <- min(Price.x)
  }else{
    mydata.clean$PricePoint <- 9999
    mydata.clean$PriceMin <- 9999
  }
  scoreAdd <- data.frame(mydata.clean$Group,mydata.clean$Time,mydata.clean$Score,mydata.clean$PricePoint,mydata.clean$PriceMin,mydata.clean$DNA_Volume..Units.)
  names(scoreAdd) <- c("Group","Time","Score","PricePoint","PriceMin","DNA_Volume..Units.")
  
  # Prepare the full coefficient results table, which includes the p-values
  set.seed(1)
  ridge.coefficients <- coef(model.ridge,s="lambda.min")
  coef_table <- data.frame(name = ridge.coefficients@Dimnames[[1]][ridge.coefficients@i+1], coefficient = ridge.coefficients@x)
  # Provide R friendly names to the table fields
  names(coef_table) <- c("Variable", "Coefficient")
  # Add Pricebreak coefficient even if it is zero
  if(exists("Varname") && !any(coef_table$Variable==Varname)&& !any(remove.name$Name==Varname)){
    coef_table <- rbind(coef_table,data.frame(Variable = Varname, Coefficient = 0 ))
  }else if(exists("Varname") && any(index <- remove.name$Name==Varname)){
    coef_table <- rbind(coef_table,data.frame(Variable = Varname, Coefficient = remove.name$Coef[index] ))
  }
  if(exists("Varname") &&!any(coef_table$Variable=="Price1")&& !any(remove.name$Name=="Price1")){
    coef_table <- rbind(coef_table,data.frame(Variable = "Price1", Coefficient =0))
  }else if(exists("Varname") && any(index <-remove.name$Name=="Price1")){
    coef_table <- rbind(coef_table,data.frame(Variable = "Price1", Coefficient = remove.name$Coef[index]))
  } 
  coef_table$Coefficient[coef_table$Variable == Varname]<- coef_table$Coefficient[coef_table$Variable == "Price1"] + coef_table$Coefficient[coef_table$Variable == Varname]
  mergedStats <- coef_table
  mergedStats$Group <- groupname
  remove.name$Group <- groupname
  #variableAdd <- data.frame("Remove","Name",0,0,"Remove")
  #names(variableAdd) <- c("Group","Name","Coef","Dev","Type")
  output <- list(mergedStats,scoreAdd,remove.name)
  return(output)
}








execModel <- foreach(group=groups[1],combine=rbind) %dopar% {
  pricelist <- list()
  price.point <- 9999
  PriceCoef <-c()
  price <- c()
  PricePromoCoef <- c()
  price.value <- c()
  i <- 1
  NoThresholdParam <- ("NTP"=="False")
  repeat{
    model <- try(RidgeModel(groups[group],price.point),silent=TRUE)
    if(class(model)=="try-error") {
      mergedStats <- data.frame("Group"=c(as.character(groups[group])),"Variable" = c("Error"),"Coefficient" = c(0.0),check.names=FALSE)
      scoreAdd <- data.frame("Group"=c(as.character(groups[group])),"Time"=c("Error"),"Score"=c(0))
      variableAdd <- data.frame("Remove","Name",0,0,"Remove")
      names(variableAdd) <- c("Group","Name","Coef","Dev","Type")
      model <- list(mergedStats,scoreAdd,variableAdd)
      break
    }
    price.point <- model[[2]]$PricePoint[1]
    price <- c(price,price.point)
    if (log(exp(price.point)-2)<= model[[2]]$PriceMin[1] || (price.point==9999 & i>1) || (NoThresholdParam)){
      pricelist[[i]]<- model
      break
    }else{
      price.value <- price.point
      price.name <- make.names(paste("Price",as.character(substr(exp(price.value),1,5)),sep="."))
      if(any(index <- model[[1]]$Variable==price.name)){
        PriceCoef <-c(PriceCoef,model[[1]]$Coefficient[index])
        
      }else{PriceCoef <-c(PriceCoef,0)
      #price <- c(price,price.value)
      }
      if(any(index<- model[[1]]$Variable== "DNA_Price.Promotional....of.Base.Price.")){
        PricePromoCoef <-c(PricePromoCoef,model[[1]]$Coefficient[index])
      }else{PricePromoCoef <-c(PricePromoCoef,0)
      }
    }
    if (i==1) {
      price.point <- log(100)
    } else {
      price.point <- log(exp(price.point)-2)# reduce the price promotion 2% in every iteration
    }
    pricelist[[i]]<- model
    i <- i + 1
  }
  new <- pricelist
}

stopCluster(cl)




## Combine all the coefficeints and add iteration column 
myfun <- function(x){mapply(cbind,lapply(x, '[[',1),"Iteration" = 1:length(lapply(x,'[[',1)),SIMPLIFY = F)}
MyCoeftable <- rbindlist(lapply(lapply(execModel[(sapply(execModel,length)!=0)],myfun),rbindlist))
## Combine all the Scored_data and add iteration column 
myfun <- function(x){mapply(cbind,lapply(x, '[[',2),"Iteration" = 1:length(lapply(x,'[[',2)),SIMPLIFY = F)}
MyScore <- rbindlist(lapply(lapply(execModel[(sapply(execModel,length)!=0)],myfun),rbindlist))
## Combine all the Variable_data and add iteration column 
myfun <- function(x){mapply(cbind,lapply(x, '[[',3),"Iteration" = 1:length(lapply(x,'[[',2)),SIMPLIFY = F)}
MyVariable <- rbindlist(lapply(lapply(execModel[(sapply(execModel,length)!=0)],myfun),rbindlist))

## Calculate Score
MyScore[,Rsq:=  1 - sum((exp(Score)-exp(DNA_Volume..Units.))^2)/sum((mean(exp(Score))-exp(DNA_Volume..Units.))^2),by = c("Group","Iteration")]
MyScore <- merge(MyScore,MyCoeftable[grepl("Price[.]\\d+",MyCoeftable$Variable),],by=c("Group","Iteration"),all.x=TRUE)
MyScore <- MyScore[Coefficient<=0 | is.na(Coefficient),]
MyCoeftable <- MyCoeftable[MyCoeftable$Coefficient!=0,]
model.maxr <- MyScore[,{.(Maxid = Iteration[which.max(Rsq)])},by = c("Group")]

## Parse out the model with maximum R square
coefOutput <- merge(MyCoeftable,model.maxr, by.x=c("Group","Iteration"),by.y = c("Group","Maxid"),all=FALSE)
scoredOutput <- merge(MyScore,model.maxr, by.x=c("Group","Iteration"),by.y = c("Group","Maxid"),all=FALSE)
variableOutput <- merge(MyVariable,model.maxr, by.x=c("Group","Iteration"),by.y = c("Group","Maxid"),all=FALSE)
#(scoredOutput,nOutput=1)
#write.Alteryx(coefOutput,nOutput=2)
#write.Alteryx(variableOutput,nOutput=3)
