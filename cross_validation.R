library(caret)
library(readr)
library(car)
library(olsrr)
library("GGally")
library(robustHD)
library(moments)
library(MASS)


bodyfatmen <- read_csv("~/År 3/Regression analysis/Project 1/bodyfatmen.csv")

lm.fit <- lm(density ~ ., data = bodyfatmen)
testfinal <- lm(density ~ age+weight+neck+abdomen+hip+thigh+forearm+wrist, data = bodyfatmen)
testfinalvif <- lm(density ~ age+neck+abdomen+hip+thigh+forearm+wrist, data = bodyfatmen)

traindata <- bodyfatmen[,c(2, 5, 7, 8, 9, 13, 14)]
trainclasses <- bodyfatmen[,1]
set.seed(42)
cv <- train(density ~ age+neck+abdomen+hip+thigh+forearm+wrist, data = bodyfatmen, method = 'lm',
            trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE, savePredictions = T))

n <- 224
p <- 8

R_2_adj <- cv$results[3]
R_2 <- R_2_adj * (n-p)/(n-1) - (n-p)/(n-1) + 1

cvt <- train(density ~ age+weight+neck+abdomen+hip+thigh+forearm+wrist, data = bodyfatmen, method = 'lm',
            trControl = trainControl(method = 'cv', number = 10, verboseIter = TRUE, savePredictions = T))

print(cv)
