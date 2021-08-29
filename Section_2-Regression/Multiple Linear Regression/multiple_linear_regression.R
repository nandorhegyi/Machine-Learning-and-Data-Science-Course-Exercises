#importing the dataset
dataset= read.csv('50_Startups.csv')

#encoding the state (categorical data)
dataset$State= factor(dataset$State,
                      levels= c('New York', 'Florida', 'California'),
                      labels= c(1, 3, 2))

#splitting the dataset into a training and test set
library(caTools)
set.seed(123)
split= sample.split(dataset$Profit, SplitRatio = 0.8)
training_set=subset(dataset, split==TRUE)
test_set= subset(dataset, split==FALSE)

#fitting multiple linear regression to the training set
regressor= lm(formula= Profit~ .,
              data=training_set)
#predicting the test set results
y_pred=predict(regressor, newdata= test_set)

#building the optimal model using backward elimination
regressor= lm(formula= Profit~ R.D.Spend + Administration + Marketing.Spend + State,
              data=dataset)
summary(regressor)
regressor= lm(formula= Profit~ R.D.Spend + Administration + Marketing.Spend,
              data=dataset)
summary(regressor)
regressor= lm(formula= Profit~ R.D.Spend + Marketing.Spend,
              data=dataset)
summary(regressor)
regressor= lm(formula= Profit~ R.D.Spend,
              data=dataset)
summary(regressor)
