install.packages("ggplot2")
install.packages("tidymodels")
install.packages("tidyverse")
install.packages("plotrix")
install.packages("prettydoc")

library(ggplot2)
library(tidymodels)
library(tidyverse)
library(corrplot)
library(cluster) 
library(fpc)
library(readr)
library(dplyr)
library(plotrix)
library(ISLR)

## Predicting the percentage of students based on the number of hours they studied
## Student data

student <- readr::read_csv('https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv')
summary(student)
ggplot(student, aes(Hours, Scores))+
  geom_point(colour= "blue")+
  ggtitle("Student scores by number of hours studied")


## Model to predict the scores of students based on the number of hours they studied

student_split<- initial_split(student)
traindata<- training(student_split)
testdata<- testing(student_split)

LinMod <-lm( Scores ~ Hours ,data = traindata)
summary(LinMod)

scores<- predict(LinMod,testdata)

scores<- as.data.frame(scores)
scores$hours<- c(1,2,3,4,5,6)
lbls<- paste(scores$hours)

pie3D(scores$scores, labels = lbls, explode = 0.1, main= "Percentage of scores by number of hours")

## Predicting scores for student who studies for 9.25 hrs/day

Newhours= data.frame(Hours = 9.5)

predict(LinMod,Newhours)