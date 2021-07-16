df <- read.csv("Dummy Data With State.csv")

# Splitting the dataset into the Training set and Test set
library (caTools)
set.seed(101) 
sample = sample.split(df$Total.Amount, SplitRatio = .60)
train = subset(df, sample == TRUE)
test  = subset(df, sample == FALSE)

# use lm() to run a linear regression of Total Amount on all the predictors 
# in the training set.
model <- lm(Total.Amount ~ ., data = train)

# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)

summary(model)

#CODE FOR PREDICTION AND MEASURING ACCURACY

library(forecast)
# use predict() to make predictions on a new set.
pred <- predict(model, test)

options(scipen=999, digits = 0)

#some.residuals <- test$Total.Amount - pred
#data.frame("Predicted" = pred, "Actual" = test$Total.Amount,
#           "Residual" = some.residuals)

#options(scipen=999, digits = 3)

# use accuracy() to compute common accuracy measures.
accuracy(pred, test$Total.Amount)