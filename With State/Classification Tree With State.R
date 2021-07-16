library(rpart)
library(rpart.plot)
library(caret)

df <- read.csv("Dummy Data With State.csv")
selected = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)

# Splitting the dataset into the Training set and Test set
ind = sample(2, nrow(df), replace=TRUE, prob=c(0.6,0.4))
training = df[ind==1,selected]
testing = df[ind==2,selected]

dim(training)
dim(testing)

# classification tree
default.ct <- rpart(Order.Status..Dummy. ~ ., data = training, method = "class")

# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

#CODE FOR TESTING ACCURACY 
dpred.train <- predict(default.ct,training,type = "class")
dpred.test <- predict(default.ct,testing,type = "class")

str(testing$Order.Status..Dummy.)
str(dpred.test)

#GENERATE CONFUSION MATRIX

#default tree: training
confusionMatrix(dpred.train, as.factor(training$Order.Status..Dummy.))

#default tree: testing
confusionMatrix(factor(dpred.test, levels = 1:4), factor(testing$Order.Status..Dummy., levels = 1:4))

#CODE FOR CREATING A DEEPER CLASSIFICATION TREE

deeper.ct <- rpart(Order.Status..Dummy. ~ ., data = training, method = "class", cp = 0, minsplit = 1)

# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])

# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))

#CODE FOR TESTING ACCURACY 
# classify records in the testing data.
#set argument type = "class" in predict() to generate predicted class membership.
deeperpred.train <- predict(deeper.ct,training,type = "class")
deeperpred.test <- predict(deeper.ct,testing,type = "class")

#GENERATE CONFUSION MATRIX

#deeper tree: training
confusionMatrix(deeperpred.train, as.factor(training$Order.Status..Dummy.))

#deeper tree: testing
confusionMatrix(factor(deeperpred.test,levels = 1:4), factor(testing$Order.Status..Dummy., levels = 1:4))
