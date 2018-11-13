library(tidyverse)
setwd('~/Documentos/R')
titanic.train<-read.csv(file = "input/titanic/train.csv", stringsAsFactors=FALSE, header = TRUE)
titanic.test<-read.csv(file = "input/titanic/test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$isTrainSet<-TRUE
titanic.test$isTrainSet<-FALSE
titanic.test$Survived<-NA

titanic.full<-rbind(titanic.train,titanic.test)

#NA
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked=='', 'Embarked']<-'S'

table(is.na(titanic.full$Age))
age.median<- mean(titanic.full$Age, na.rm=TRUE)
titanic.full[is.na(titanic.full$Age), 'Age']<-age.median

table(is.na(titanic.full$Fare))
fare.median<-mean(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), 'Fare']<-fare.median

#Categorical variable
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Pclass<-as.factor(titanic.full$Pclass)
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

#Split data
titanic.train<-titanic.full[titanic.full$isTrainSet==TRUE, ]
titanic.test<-titanic.full[titanic.full$isTrainSet==FALSE, ]

#For binary clasification
titanic.train$Survived<-as.factor(titanic.train$Survived)

library(randomForest)
survived.formula<-as.formula("Survived~Pclass+Sex+SibSp+Parch+Embarked")

titanic.model<-randomForest(formula = survived.formula, data = titanic.train, ntree=400, mtry=3, nodesize=0.01*nrow(titanic.test))
Survived<-predict(titanic.model, newdata = titanic.test)

output<-data.frame(titanic.test$PassengerId, Survived)
colnames(output)<-c('PassengerId', 'Survived')
write.csv(output, file='kaggle_submission.csv', row.names = FALSE)
