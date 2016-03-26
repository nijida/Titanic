## Data Exploration
# Load train data, choose file = titanic-train.csv
 rawTrain <- read.csv(file=file.choose(),na.strings=c(""," ","NA"))
 rowTrain <- nrow(rawTrain)
 length(rawTrain)
 
# Load test data, choose file = titanic-test.csv
 rawTest <- read.csv(file=file.choose(),na.strings=c(""," ","NA"))
 rowTest <- nrow(rawTest)
 length(rawTest)
 
# Add empty column named "Survived" to test data
 rawTest$Survived <- NA
 
# Merge Training and Test data
 rawTitanic <- rbind(rawTrain, rawTest[,names(rawTrain)])
 dataTitanic <- rawTitanic

# Check missing values
 sapply(dataTitanic, function(x) sum(is.na(x)|x==""|x=="NA")) 
#PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch      Ticket        Fare       Cabin 
#          0         418           0           0           0         263           0           0           0           1        1014 
#   Embarked 
#          2  

## Impute the most common value to the missing Embarked
# Variable "Embarked"
 table(dataTitanic$Embarked)
#  C   Q   S 
#270 123 914 
 dataTitanic$Embarked[which(is.na(dataTitanic$Embarked)|dataTitanic$Embarked==""|dataTitanic$Embarked=="NA")] <- 'S'

# Variable "Fare"
# Impute the median fare for particular class to the missing Fare
 fareClass <- aggregate(dataTitanic[which(!is.na(dataTitanic$Fare)),"Fare"],list(dataTitanic$Pclass[which(!is.na(dataTitanic$Fare))]), median)
 fareClass
#  Group.1       x
#1       1 60.0000
#2       2 15.0458
#3       3  8.0500
 dataTitanic$Fare[which(is.na(dataTitanic$Fare))] <- fareClass$x[which(dataTitanic$Pclass[which(is.na(dataTitanic$Fare))]==fareClass$Group.1)]
 
# Variable "Name"
# Extract Title(title) from Name
 dataTitanic$Name <- as.character(dataTitanic$Name) 
 dataTitanic$title <- lapply(as.character(dataTitanic$Name), FUN=function(x) strsplit(x, split='[,.]')[[1]][2])
 dataTitanic$title <- sub(' ','',dataTitanic$title)
 dataTitanic$title[which(dataTitanic$title %in% c('Mme','Mlle'))] = 'Miss'
 dataTitanic$title[which(dataTitanic$title %in% c('Lady','Ms','the Countess','Dona'))] = 'Mrs'
 dataTitanic$title[which(dataTitanic$title=='Dr' & dataTitanic$Sex=='female')] = 'Mrs'
 dataTitanic$title[which(dataTitanic$title=='Dr' & dataTitanic$Sex=='male')] = 'Mr'
 dataTitanic$title[which(dataTitanic$title %in% c('Capt','Col','Don','Jonkheer','Major','Rev','Sir'))] = 'Mr'
 dataTitanic$title <- as.factor(dataTitanic$title) 

# Extract Last Name(lName) and Alternative Last Name(plName)
 dataTitanic$lName <- word(dataTitanic$Name, start=1, sep = fixed(","))
 dataTitanic$plName <- gsub("[^[:alnum:] ]", "", ifelse(str_detect(dataTitanic$Name, as.character("\\)")) & dataTitanic$Sex=="female",word(dataTitanic$Name, start=-1, sep = fixed(" ")),""))

# Extract Given Name(gName) & Spouse Name (hgName)
 start <- str_locate(dataTitanic$Name,"Mrs.")
 end <- str_locate(dataTitanic$Name,"\\(")
 dataTitanic$hgName <- ifelse(str_detect(dataTitanic$Name, as.character("\\)")) & dataTitanic$title=="Mrs", str_sub(dataTitanic$Name,start=start[,2]+2, end=end[,1]-2),"")
 start <- str_locate(dataTitanic$Name,"\\(")
 end <- str_locate(dataTitanic$Name,"\\)")
 dataTitanic$gName <- ifelse(str_detect(dataTitanic$Name, as.character("\\)")) & dataTitanic$title =='Mrs', word(str_sub(dataTitanic$Name,start=start[,2]+1, end=end[,1]-1),start=1, sep = fixed(" ")), word(word(word(dataTitanic$Name, start=-1, sep=fixed(".")), start=1, sep=fixed('"')), start=1, sep=fixed('(')))
 dataTitanic$gName <- str_trim(dataTitanic$gName)
 
# Variable "Age" using linear model to predict ageFit
 age.lm.mdl <- lm(Age~., data=dataTitanic[which(! is.na(dataTitanic$Age)),c('SibSp','Parch','title','Age')])
 age.lm.pred <- predict(age.lm.mdl,dataTitanic,interval='confidence')
 age.lm.pred <- data.frame(age.lm.pred)
 dataTitanic$ageFit <- ifelse(is.na(dataTitanic$Age), age.lm.pred$fit, dataTitanic$Age)

# Find if Spouse Survived(Sp.sur) & Have Spouse(Sp.no)
 lName.hgName <- dataTitanic[which(dataTitanic$SibSp > 0 & dataTitanic$hgName!='' & dataTitanic$ageFit>14),c("PassengerId","lName","hgName","Survived")]
 lName.hgName.sur <- dataTitanic[which(dataTitanic$SibSp > 0 & dataTitanic$ageFit>14 & (dataTitanic$gName %in% lName.hgName$hgName| word(dataTitanic$gName, sep = fixed(" ")) %in% lName.hgName$hgName) & dataTitanic$lName %in% lName.hgName$lName),c("PassengerId","lName","gName","Survived")]
 for(i in 1:nrow(dataTitanic)){ 
  tmp <- if(length(lName.hgName.sur[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName.sur[,"lName"] & dataTitanic[i,"hgName"]==lName.hgName.sur[,"gName"]),"Survived"])!=0){
			lName.hgName.sur[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName.sur[,"lName"] & dataTitanic[i,"hgName"]==lName.hgName.sur[,"gName"]),"Survived"]}	    
		else if(length(lName.hgName[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName[,"lName"] & dataTitanic[i,"gName"]==lName.hgName[,"hgName"]),"Survived"])!=0){
			lName.hgName[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName[,"lName"] & dataTitanic[i,"gName"]==lName.hgName[,"hgName"]),"Survived"]}
		else if(length(lName.hgName[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName[,"lName"] & word(dataTitanic[i,"gName"], sep = fixed(" "))==lName.hgName[,"hgName"]),"Survived"])!=0){
			lName.hgName[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName[,"lName"] & word(dataTitanic[i,"gName"], sep = fixed(" "))==lName.hgName[,"hgName"]),"Survived"]}
	    else if(length(lName.hgName.sur[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName.sur[,"lName"] & dataTitanic[i,"hgName"]==word(lName.hgName.sur[,"gName"], sep = fixed(" "))),"Survived"])!=0){
			lName.hgName.sur[which(dataTitanic[i,"SibSp"] > 0 & dataTitanic[i,"lName"]==lName.hgName.sur[,"lName"] & dataTitanic[i,"hgName"]==word(lName.hgName.sur[,"gName"], sep = fixed(" "))),"Survived"]}			
		else {-9}
  dataTitanic[i,"Sp.sur"] <- tmp
 }
dataTitanic$Sp.sur <- ifelse(is.na(dataTitanic$Sp.sur),0,dataTitanic$Sp.sur) #Value: 1 = Yes, 0 = No or Unknow (default: -9 = Not found)
dataTitanic$Sp.no <- ifelse(dataTitanic$Sp.sur!=-9,1,0) #Value: 1 = Yes, 0 = No, Unknown or Not found

# Find Number of Siblings(Sib.no) & Siblings Survived(Sib.sur)
 dataTitanic$Sib.no <- ifelse(dataTitanic$SibSp>0,dataTitanic$SibSp - dataTitanic$Sp.no,0) 
 Sib.lName.sur <- dataTitanic[which(dataTitanic$Sib.no!=0),c("PassengerId","lName","plName","Survived")]
 for(i in 1:nrow(dataTitanic)){
  tmp <- if(length(Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"lName"]==Sib.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])!=0){
 			Reduce('+',Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"lName"]==Sib.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])}
 	    else if(length(Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"plName"]==Sib.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])!=0){
 			Reduce('+',Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"plName"]==Sib.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])}
 		else if(length(Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"lName"]==Sib.lName.sur[,"plName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])!=0){
 			Reduce('+',Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"lName"]==Sib.lName.sur[,"plName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])}
 		else if(length(Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"plName"]==Sib.lName.sur[,"plName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])!=0){
 			Reduce('+',Sib.lName.sur[which(dataTitanic[i,"Sib.no"]>0 & dataTitanic[i,"plName"]==Sib.lName.sur[,"plName"] & dataTitanic[i,"PassengerId"]!=Sib.lName.sur[,"PassengerId"]),"Survived"])}
 		else {-9}
   dataTitanic[i,"Sib.sur"] <- tmp
  }
 dataTitanic$Sib.sur <- ifelse(is.na(dataTitanic$Sib.sur),0,dataTitanic$Sib.sur) #Value: 1 = Yes, 0 = No or Unknow (default: -9 = Not found) 

# Re-estimate ageFit using linear model
 age.lm.mdl <- lm(Age~., data=dataTitanic[which(!is.na(dataTitanic$Age)),c('title','Age','Sp.no','Sib.no')])
 age.lm.pred <- predict(age.lm.mdl,dataTitanic,interval='confidence')
 age.lm.pred <- data.frame(age.lm.pred)
 dataTitanic$ageFit <- ifelse(is.na(dataTitanic$Age), ifelse(age.lm.pred$fit<0,0.5,age.lm.pred$fit), dataTitanic$Age)
  
# Find Parents Survived(Par.sur) & Number of Parents(Par.no)
 Par.lName <- dataTitanic[which(dataTitanic$Parch>0 & dataTitanic$title %in% c('Mrs','Mr')),c("PassengerId","lName","gName","ageFit","Survived")]
 Par.lName.sur <- dataTitanic[which(dataTitanic$Parch>0 & dataTitanic$title %in% c('Mrs','Mr') & dataTitanic$lName %in% Par.lName$lName),c("PassengerId","lName","gName","ageFit","Survived")]
 for(i in 1:nrow(dataTitanic)){
   tmp <- if(length(Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"lName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"])!=0){
 			Reduce('+', Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"lName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"]==1)}
 		else if(length(Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"plName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"])!=0){
			Reduce('+', Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"plName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"]==1)}
		else {-9}
 	dataTitanic[i,"Par.sur"] <- tmp
 	}
 for(i in 1:nrow(dataTitanic)){
   tmp <- if(length(Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"lName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"])!=0){
 			length(Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"lName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"])}
 		else if(length(Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"plName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"])!=0){
			length(Par.lName.sur[which(dataTitanic[i,"Parch"]>0 & dataTitanic[i,"plName"]==Par.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Par.lName.sur[,"PassengerId"] & Par.lName.sur$ageFit-dataTitanic[i,"ageFit"]>14),"Survived"])}
		else {0}
 	dataTitanic[i,"Par.no"] <- tmp
 	}
 dataTitanic$Par.sur <- ifelse(is.na(dataTitanic$Par.sur),0,dataTitanic$Par.sur)  #Value: 1 = Yes, 0 = No or Unknow (default: -9 = Not found)
	
# Find Number of Chilren(Ch.no) & Children Survived(Ch.sur)
 dataTitanic$Ch.no <- ifelse(dataTitanic$Parch>0,dataTitanic[,"Parch"]-dataTitanic[,"Par.no"],0)
 Ch.lName.sur <- dataTitanic[which(dataTitanic$Parch>0),c("PassengerId","lName","gName","ageFit","Survived")]	
 for(i in 1:nrow(dataTitanic)){
    tmp <- if(length(Ch.lName.sur[which(dataTitanic[i,"Ch.no"]>0 & dataTitanic[i,"lName"]==Ch.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Ch.lName.sur[,"PassengerId"] & dataTitanic[i,"ageFit"]-Ch.lName.sur$ageFit>14),"Survived"])!=0){
  			Reduce('+',Ch.lName.sur[which(dataTitanic[i,"Ch.no"]>0 & dataTitanic[i,"lName"]==Ch.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Ch.lName.sur[,"PassengerId"] & dataTitanic[i,"ageFit"]-Ch.lName.sur$ageFit>14),"Survived"])}
  		else if(length(Ch.lName.sur[which(dataTitanic[i,"Ch.no"]>0 & dataTitanic[i,"plName"]==Ch.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Ch.lName.sur[,"PassengerId"] & dataTitanic[i,"ageFit"]-Ch.lName.sur$ageFit>14),"Survived"])!=0){
  			Reduce('+',Ch.lName.sur[which(dataTitanic[i,"Ch.no"]>0 & dataTitanic[i,"plName"]==Ch.lName.sur[,"lName"] & dataTitanic[i,"PassengerId"]!=Ch.lName.sur[,"PassengerId"] & dataTitanic[i,"ageFit"]-Ch.lName.sur$ageFit>14),"Survived"])}
  		else {-9}
  	dataTitanic[i,"Ch.sur"] <- tmp
  	}	
 dataTitanic$Ch.sur <- ifelse(is.na(dataTitanic$Ch.sur),0,dataTitanic$Ch.sur) #Value: 1 = Yes, 0 = No or Unknow (default: -9 = Not found)
 
# Calculate Family Size(fam.size)
 dataTitanic$fam.size <- dataTitanic$SibSp+dataTitanic$Parch+1
 
# Define Family ID(fam.id)
 dataTitanic$fam.id <- ifelse(dataTitanic$fam.size<=2,paste('small',dataTitanic$fam.size,sep=''),paste(dataTitanic$lName,dataTitanic$fam.size,sep=''))
 
# Variable Ticket Count(ticket.count), counting people with the same ticket
 dataTitanic$Ticket <- as.character(dataTitanic$Ticket)
 ticket.count <- table(dataTitanic$Ticket)
 dataTitanic$ticket.count <- lapply(dataTitanic$Ticket, function(x) ticket.count[which(names(ticket.count)==x)])
 dataTitanic$ticket.count <- as.numeric(dataTitanic$ticket.count)
 
# Variable "Fare", using linear model to predict Fare for Fare=0
 dataTitanic$Fare[which(is.na(dataTitanic$Fare)|dataTitanic$Fare==""|dataTitanic$Fare=="NA")] <- 0
 fare.lm.mdl <- lm(Fare~., data=dataTitanic[dataTitanic$Fare != 0, c('Pclass','ageFit','SibSp','Parch','Embarked','Fare')])
 fare.lm.pred <- predict(fare.lm.mdl,dataTitanic,interval='confidence')
 fare.lm.pred <- data.frame(fare.lm.pred)
# Individual Fare(indFare: fareFit/ticket.count)
 dataTitanic$fareFit <- ifelse(dataTitanic$Fare == 0, fare.lm.pred$fit, dataTitanic$Fare)
 dataTitanic$indFare <- dataTitanic$fareFit / dataTitanic$ticket.count

# Adjust data type and split Training data and Test set
 dataTitanic$fam.id<- as.factor(dataTitanic$fam.id)
 dataTitanic$title<- as.factor(dataTitanic$title)
 dataTitanic$Embarked<- as.factor(dataTitanic$Embarked)
 dataTitanic$Sex<- as.factor(dataTitanic$Sex)
 dataTitanic$Pclass<- as.factor(dataTitanic$Pclass)
 train <- dataTitanic[1:rowTrain,]
 test <- dataTitanic[rowTrain+1:rowTest,]
 levels(train$fam.id) = levels(test$fam.id)

# Training algorithm: cforest
 train.cforest.mdl <- cforest(Survived~as.factor(Pclass)+as.factor(Sex)+indFare+as.factor(Embarked)+as.factor(title)+fam.size+fam.id ,data=train, controls=cforest_unbiased(ntree=501, mtry=3))
# Predict Survived on test set
 submit <- cbind(test, predict(train.cforest.mdl, newdata=test[,-2], OOB=TRUE, type = "response"))
 titanic_submit <- data.frame(PassengerId = submit$PassengerId, Survived = ifelse(submit[,length(submit)] > 0.6, 1, 0))
 write.csv(titanic_submit, file="titanic-submit.csv", row.names = FALSE)






	