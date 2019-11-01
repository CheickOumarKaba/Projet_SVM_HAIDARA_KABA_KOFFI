library(shiny)
#install.packages("shinyBS")
#library(shinyBS)
require(shinyBS)
#install.packages("shinydashboard")
#library(shinydashboard)
require(shinydashboard)
#install.packages("shinyjs")
#library(shinyjs)
require(shinyjs)
#install.packages("caret")
#library(caret)
require(caret)
#install.packages("plyr")
#library(plyr)
require(plyr)
require(dplyr)
require(tidyr)
#install.packages("Cairo")
#library(Cairo)
require(Cairo)
#install.packages("raster")
#library(raster)
require(raster)
#install.packages("gstat")
#library(gstat)
require(gstat)
#install.packages("wesanderson")
#library(wesanderson)
require(wesanderson)
#install.packages("nnet")
#library(nnet)
require(nnet)
#install.packages("randomForest")
#library(randomForest)
require(randomForest)
library(DMwR)

### notre base de données d'origine ####


base=read.table("C:/Users/cheic/Desktop/M2 ESA/SVM/Projet detection fraude SVM/creditcard.csv",header=T,sep=",")
dim(base)
summary(base)

is.factor(base$Class)
base$Class=as.factor(base$Class)

### reduction des donnees de la base d'origine ### 

newdata=SMOTE(Class~.,base,perc.over=75,k=5,perc.under=340)
dim(newdata)
print(prop.table(table(newdata$Class)))

is.factor(newdata$Time)
newdata$Time=as.numeric(newdata$Time)

newdata$Class=ifelse(newdata$Class==0,-1,1)
summary(newdata)

is.factor(newdata$Class)
newdata$Class=as.factor(newdata$Class)
summary(newdata)


### exportation de la nouvelle base en format Rdata ###

save(newdata ,file = "C:/Users/cheic/Desktop/M2 ESA/SVM/Projet detection fraude SVM/Appli/newdata.RData")
