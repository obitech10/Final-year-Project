library(tidyverse)
library(neuralnet)
library(GGally)

# Importing the dataset
datum= read.csv('Dummy Data With State for ANN Prediction.csv')

#Scatterplot matrix
ggpairs(datum, title = "Scatterplot Matrix of the Features of the Data Set")

# Scale the Data
scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

datum <- datum %>%
  mutate_all(scale01)

set.seed(12345)
datum_Train <- sample_frac(tbl = datum, replace = FALSE, size = 0.60)
datum_Test <- anti_join(datum, datum_Train)

#HIDDEN = 2

set.seed(12321)
train_NN2 <- neuralnet(Total.Amount ~ ., data = datum_Train, hidden=2)

# Predict on test data
pr <- compute(train_NN2, datum_Test)

# Compute mean squared error
pr.nn <- pr$net.result * (max(datum$Total.Amount) - min(datum$Total.Amount)) + min(datum$Total.Amount)
test.r <- (datum_Test$Total.Amount) * (max(datum$Total.Amount) - min(datum$Total.Amount)) + min(datum$Total.Amount)
MSE.nn <- sum((test.r - pr.nn)^2) / nrow(datum_Test)

#mean squared error
MSE.nn

#Root mean squared error
RMSE <- sqrt(MSE.nn)
RMSE

#To view the diagram of the ANN
plot(train_NN2)

# Plot regression line
plot(datum_Test$Total.Amount, pr.nn, col = "red",
     main = 'Real vs Predicted for HIDDEN = 2')
abline(0, 1, lwd = 2)


#HIDDEN = 5

set.seed(12321)
train_NN5 <- neuralnet(Total.Amount ~ ., data = datum_Train, hidden=5)

# Predict on test data
pr5 <- compute(train_NN5, datum_Test)

# Compute mean squared error
pr.nn5 <- pr5$net.result * (max(datum$Total.Amount) - min(datum$Total.Amount)) + min(datum$Total.Amount)
test.r5 <- (datum_Test$Total.Amount) * (max(datum$Total.Amount) - min(datum$Total.Amount)) + min(datum$Total.Amount)
MSE.nn5 <- sum((test.r5 - pr.nn5)^2) / nrow(datum_Test)

#mean squared error
MSE.nn5

#Root mean squared error
RMSE5 <- sqrt(MSE.nn5)
RMSE5

#To view the diagram of the ANN
plot(train_NN5)

# Plot regression line
plot(datum_Test$Total.Amount, pr.nn5, col = "blue",
     main = 'Real vs Predicted for HIDDEN = 5')
abline(0, 1, lwd = 2)
