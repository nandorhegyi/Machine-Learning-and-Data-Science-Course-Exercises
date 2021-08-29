# polynomial regression
#importing the dataset
dataset= read.csv('Position_Salaries.csv')
dataset=dataset[2:3]

#fitting the linear regression to the dataset
lin_reg= lm(formula= Salary~.,
            data= dataset)

#fitting polynomial regression to the dataset
dataset$Level2= dataset$Level^2
dataset$Level3= dataset$Level^3
dataset$Level4= dataset$Level^4
poly_reg= lm(formula=Salary~.,
             data=dataset)

#visualising the linear regression results
library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=dataset$Level, y=predict(lin_reg, newdata=dataset)),
            colour='blue')+
  ggtitle('Truth or bluff (linear regression)')+
  xlab('Level')+
  ylab('Salary')

#visualising the polynomial regression results
ggplot()+
  geom_point(aes(x=dataset$Level, y=dataset$Salary),
             colour='red')+
  geom_line(aes(x=dataset$Level, y=predict(poly_reg, newdata=dataset)),
            colour='blue')+
  ggtitle('Truth or bluff (polynomial regression)')+
  xlab('Level')+
  ylab('Salary')

#predicting a new result with linear regression
y_pred= predict(lin_reg, data.frame(Level=6.5))

#predicting a new result with polynomial regression
y_pred= predict(poly_reg, data.frame(Level=6.5,
                                     Level2=6.5^2,
                                     Level3=6.5^3,
                                     Level4=6.5^4))