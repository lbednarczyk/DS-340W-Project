#data from: https://www.kaggle.com/ronitf/heart-disease-uci?select=heart.csv
#code from: https://www.kaggle.com/jarredpriester/heart-disease-predictions-using-a-ml-ensemble-in-r

rm(list=ls())

library(data.table)
library(tidyverse)
library(dplyr)
library(matrixStats)
library(gam)
library(evtree)
library(knitr)

set.seed(8)

##read in UCI Cleveland heart disease dataset
data <- fread('./volume/data/raw/heart.csv')

#----------SUMMARY STATISTICS-------------

##display summary stats
head(data)
dim(data)
str(data)
summary(data)

### cut down data from feature selection 
#data <- subset(data, select = -fbs)
#check for NA
sum(is.na(data) == TRUE) #there are no NAs

#----------EDA-------------
#summary of observations that don't have heart disease (target == 0)
data %>% filter(target == 0) %>% summary()

#summary of observations that do have heart disease (target == 1)
data %>% filter(target == 1) %>% summary()

#----------DATA VISUALIZATION/GRAPHS-------------
#boxplot of all data
boxplot(data) #chol, thalach, and tresbps seem to be the most significant

#scatterplot of resting blood pressure (trestbps) and age (age)
data %>% ggplot(aes(age, trestbps)) + geom_point(aes(color = factor(target))) +
  ggtitle("Resting Blood Pressure and Age") + ylab("Resting BP") + scale_color_manual(name = "Heart Disease", 
                                                                                      labels = c("Negative", "Positive"),
                                                                                      values = c("blue", "red"))

#scatterplot of serum cholesterol and age
data %>% ggplot(aes(age,chol)) +
  geom_point(aes(color = factor(target))) +
  ggtitle("Serum Cholesterol and Age") +
  ylab("Cholesterol") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negative","Positive"),
                     values = c("blue","red"))

#scatterplot of max achieved heart rate and age
data %>% ggplot(aes(age,thalach)) +
  geom_point(aes(color = factor(target)))+
  ggtitle("Maximum Heart Rate Achieved and Age") +
  ylab("Max Heart Rate") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negative","Positive"),
                     values = c("blue","red"))

#scatterplot of max heart rate achieved, age, and sex 0 = female and 1 = male
data %>% ggplot(aes(age,thalach)) +
  geom_point(aes(color = factor(target))) +
  facet_grid(.~sex) +
  ggtitle("Maximum Heart Rate Achieved by Age and Sex") +
  ylab("Max Heart Rate") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negative","Positive"),
                     values = c("blue","red"))

#scatterplot of max heart rate achieved, cholesterol, sex
data %>% ggplot(aes(thalach,chol)) +
  geom_point(aes(col=factor(target))) +
  facet_grid(.~sex) +
  labs(title = "Maximum Heart Rate Achieved, Cholesterol and Sex",
       x = "Max Heart Rate", y = "Cholesterol") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negative","Positive"),
                     values = c("blue","red"))

#bar chart of major vessels 0-3 (colored by flouroscopy) split by heart disease diagnosis
data %>% ggplot(aes(ca)) +
  geom_bar(aes(fill=factor(target))) +
  facet_grid(.~target) +
  ggtitle("Number of Major Vessels (0-3) Colored by Flourosopy During Exam
Split by Heart Disease Diagnosis") +
  xlab("Major Vessels") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negative","Positive"),
                     values = c("blue","red"))


