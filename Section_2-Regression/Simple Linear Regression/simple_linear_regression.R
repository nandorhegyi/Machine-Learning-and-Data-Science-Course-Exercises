# Simple linear regression

# importing the dataset
dataset= read.csv('Salary_Data.csv')

#splitting the dataset
library(caTools)
set.seed(123)
split= sample.split(dataset$Salary, SplitRatio= 2/3)
training_set=subset(dataset, split==TRUE)
test_set= subset(dataset, split==FALSE)

#scaling the dataset
# training_set= scale(training_set)
# test_set= scale(test_set)

#fitting simple linear regression to the training set
regressor=lm(formula = Salary ~ YearsExperience,
             data = training_set)

#predicting the test set results
y_pred= predict(regressor, newdata= test_set)

#visualising the training set results
#install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x= training_set$YearsExperience, y= training_set$Salary),
             colour= 'red') +
  geom_line(aes(x= training_set$YearsExperience, y=predict(regressor, newdata= training_set)),
            colour='blue') +
  ggtitle('Salary vs Experience (Training Set)') +
  xlab('Years of experience') +
  ylab('Salary')
#visualising the test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x= test_set$YearsExperience, y= test_set$Salary),
             colour= 'red') +
  geom_line(aes(x= training_set$YearsExperience, y=predict(regressor, newdata= training_set)),
            colour='blue') +
  ggtitle('Salary vs Experience (Test Set)') +
  xlab('Years of experience') +
  ylab('Salary')