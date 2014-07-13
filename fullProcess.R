library(rpart)
library(randomForest)
library(party)
library(data.table)
require("featureEng.R")
rm(list=ls())
Titanic.path <- "http://www.kaggle.com/c/titanic-gettingStarted/download/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.raw <- read.csv(train.data.file, na.strings = missing.types) # We store a copy of the raw data
df.train <- train.raw
source
test.raw <- read.csv(test.data.file, na.strings = missing.types)      # We store a copy of the raw data
              
df.infer <- test.raw  

df.infer$Survived <- NA
df.combi <- rbind(df.train, df.infer)
df.combi <- featureEng(df.combi)
df.train <- combi[1:891,]
df.infer <- combi[892:1309,]


