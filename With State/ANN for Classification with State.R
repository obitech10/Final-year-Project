library(tidyverse)
library(neuralnet)
library(GGally)
library("nnet")
library(caret)

# Importing the dataset
datum= read.csv('Dummy Data With State for ANN Classification.csv')
vars=c("Segment..Dummy.", "Total.Amount", "Unit.Price")

#PARTIONING
set.seed(12345)
datum_Train <- sample_frac(tbl = datum, replace = FALSE, size = 0.60)
datum_Test <- anti_join(datum, datum_Train)

# when y has multiple classes - need to dummify
trainData <- cbind(datum_Train[c(vars)],
                   class.ind(datum_Train$ï..Order.Status..Dummy.))
names(trainData)=c(vars,
                   paste("Order.Status..Dummy_", c(1, 2, 3, 4), sep=""))
validData <- cbind(datum_Test[c(vars)],
                   class.ind(datum_Test$ï..Order.Status..Dummy.))
names(validData)=c(vars,
                   paste("Order.Status..Dummy_", c(1, 2, 3, 4), sep=""))

#run nn with 2 hidden nodes
#use hidden= with a vector of integers specifying number of hidden nodes in each layer
nn <- neuralnet(Order.Status..Dummy_1 + Order.Status..Dummy_2 + 
                  Order.Status..Dummy_3 + Order.Status..Dummy_4 ~
                  Segment..Dummy.+ Total.Amount+ Unit.Price, 
                  data = trainData, hidden = 2)
plot(nn)
training.prediction=compute(nn, trainData)
training.class=apply(training.prediction$net.result,1,which.max)
confusionMatrix(factor(training.class), factor(datum_Train$ï..Order.Status..Dummy., levels = 1:4))

#testing data
validation.prediction=compute(nn, validData)
validation.class=apply(validation.prediction$net.result,1,which.max)
confusionMatrix(factor(validation.class), factor(datum_Test$ï..Order.Status..Dummy., levels = 1:4))
