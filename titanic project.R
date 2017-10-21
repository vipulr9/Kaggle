#loading Data
setwd("D:/kaggle/titanic")

training = read.csv('train.csv',header = T,stringsAsFactors=FALSE,na.strings=c("","NA"))
test = read.csv('test.csv',header = T,stringsAsFactors=FALSE,na.strings=c("","NA"))

#Data Cleaning
test$Survived = NA

# Re Arranging col
colnames(test)
colname_training=colnames(training)

#install.packages("data.table")
library(data.table)
setcolorder(test, colname_training)
colnames(test)

#marking Test and Training Data
training$IsTraining =T
test$IsTraining = F

#Joining Both Training and Test Table
Titanic_full = rbind(training,test)

# cheaking for missing value
table(is.na(Titanic_full$PassengerId))
table(is.na(Titanic_full$Survived))
table(is.na(Titanic_full$Pclass))
table(is.na(Titanic_full$Age))
table(is.na(Titanic_full$Ticket))
table(is.na(Titanic_full$Fare))
table(is.na(Titanic_full$Cabin))
table(is.na(Titanic_full$Embarked))

#replacing missing Value
mean(Titanic_full$Age,na.rm = T)
median(Titanic_full$Age,na.rm = T)
Titanic_full$Age[is.na(Titanic_full$Age)]=28

mean(Titanic_full$Fare,na.rm = T)
median(Titanic_full$Fare,na.rm = T)
Titanic_full$Fare[is.na(Titanic_full$Fare)]=33

table(Titanic_full$Embarked)
Titanic_full$Embarked[is.na(Titanic_full$Embarked)]="S"

#Converting Catagory as Factor
Titanic_full$Sex =as.factor(Titanic_full$Sex)
Titanic_full$Embarked =as.factor(Titanic_full$Embarked)

#spliting Training and Test Table 
training_2 = subset(Titanic_full,IsTraining == TRUE)
test_2 = subset(Titanic_full,IsTraining == FALSE)

#Decision Trees
# Classification Tree with rpart
library(rpart)

# grow tree 
fit = rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, method="class", data=training_2)
fit 

printcp(fit)
plotcp(fit)
pfit = prune(fit, cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

predTs = predict(pfit, test_2)
predTs1 = apply(predTs, 1, which.max)
grpTs = as.numeric(predTs1==2)
Survived = grpTs

PassengerId = test_2$PassengerId
output =data.frame(PassengerId)
output$Survived = Survived


write.csv(output,"Titanic_Submission.csv",row.names = F)
