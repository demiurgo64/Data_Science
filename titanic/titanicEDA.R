library(tidyverse)
library(ggplot2)
setwd('~/Documentos/R')
titanic.train<-read.csv(file = "input/titanic/train.csv", stringsAsFactors=FALSE, header = TRUE)
titanic.test<-read.csv(file = "input/titanic/test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$Pclass<-as.factor(titanic.train$Pclass)
titanic.train$Sex<-as.factor(titanic.train$Sex)
titanic.train$Survived<-as.factor(titanic.train$Survived)
titanic.train$Embarked<-as.factor(titanic.train$Embarked)

ggplot(titanic.train, aes(x=Survived))+
  geom_bar()

prop.table(table(titanic.train$Survived))

ggplot(titanic.train, aes(x=Survived))+
  theme_bw()+
  geom_bar()+
  labs(y='Passager count',
       title="Titanic survival rate")

#Survived by sex
ggplot(titanic.train, aes(x=Sex, fill=Survived))+
  theme_bw()+
  geom_bar()+
  labs(y='Passager count',
       title="Titanic survival rate by Sex")

#Survived by ticket
ggplot(titanic.train, aes(x=Pclass, fill=Survived))+
  theme_bw()+
  geom_bar()+
  labs(y='Passager count',
       title="Titanic survival rate by Pclass")

#Survived by Sex and Ticket
ggplot(titanic.train, aes(x=Sex, fill=Survived))+
  theme_bw()+
  facet_wrap(~Pclass)+
  geom_bar()+
  labs(y='Passager count',
       title="Titanic survival rate by Sex and Ticket")

#Distribution passager by age
ggplot(titanic.train, aes(x=Age))+
  theme_bw()+
  geom_histogram(binwidth = 5)+
  labs(y='Passager count',
       x='Age (binwindth=5)',
       title="Titanic survival rate by Sex")

#Distribution passager by age and survived
ggplot(titanic.train, aes(x=Age, fill=Survived))+
  theme_bw()+
  geom_histogram(binwidth = 5)+
  labs(y='Passager count',
       x='Age (binwindth=5)',
       title="Titanic survival rate by Sex")

#boxplot
ggplot(titanic.train, aes(x=Survived, y=Age))+
  theme_bw()+
  geom_boxplot()+
  labs(y='Age',
       x='Survived',
       title='Titanic survival by Age')

#Survived by Age, Sex and Ticket
ggplot(titanic.train, aes(x=Age, fill=Survived))+
  theme_bw()+
  facet_wrap(Sex~Pclass)+
  geom_density(alpha=0.5)+
  labs(y='Age',
       x='Survived',
       title='Titanic survival by Age, Pclass and Sex')

#Same with histogram
ggplot(titanic.train, aes(x=Age, fill=Survived))+
  theme_bw()+
  facet_wrap(Sex~Pclass)+
  geom_histogram(binwidth=5)+
  labs(y='Age',
       x='Survived',
       title='Titanic survival by Age, Pclass and Sex')