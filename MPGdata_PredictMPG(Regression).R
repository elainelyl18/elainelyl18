#Import dataset
auto = read.csv('auto.csv')

#Remove rows with missing values
auto = auto[auto$horsepower != "?",]
sapply(auto, class)
auto$horsepower = as.integer(auto$horsepower)
class(auto$horsepower)

#Split data to train and test set
library(caTools)
set.seed(123)
split = sample.split(auto$MPG, SplitRatio = 2/3)
training_set = subset(auto, split == TRUE)
test_set = subset(auto, split == FALSE)
_________________________________________

#Correlation to MPG
correlation = cor(training_set)

#Fitting simple linear regression model to training set
lin_reg = lm(formula = MPG ~ weight, data = training_set)
summary(lin_reg)

#Predicting the test set results
y_pred = predict(lin_reg, newdata = test_set)

#Visualising training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$weight, y = training_set$MPG),
             colour = 'red') +
  geom_line(aes(x = training_set$weight, y = predict(lin_reg, newdata = training_set)),
            colour = 'blue') +
  ggtitle('MPG vs Weight (Training set - lin_reg)') +
  xlab('Weight') +
  ylab('MPG') +
  xlim(1500,5000) + 
  ylim(5,50)

#Visualising test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$weight, y = test_set$MPG),
             colour = 'red') +
  geom_line(aes(x = training_set$weight, y = predict(lin_reg, newdata = training_set)),
            colour = 'blue') +
  ggtitle('MPG vs Weight (Test set - lin_reg)') +
  xlab('Weight') +
  ylab('MPG') +
  xlim(1500,5000) + 
  ylim(5,50)
_________________________________________

#Fitting multiple linear regression model to training set
multilin_reg = lm(formula = MPG ~., data = training_set)
summary(multilin_reg)

#Backward elimination
multilin_reg = lm(formula = MPG ~ cylinders + displacement + horsepower + weight + acceleration + model.year,
                  data = auto)
summary(multilin_reg)
multilin_reg = lm(formula = MPG ~ cylinders + displacement + weight + acceleration + model.year,
                  data = auto)
summary(multilin_reg)
multilin_reg = lm(formula = MPG ~ displacement + weight + acceleration + model.year,
                  data = auto)
summary(multilin_reg)
multilin_reg = lm(formula = MPG ~ weight + acceleration + model.year,
                  data = auto)
summary(multilin_reg)
multilin_reg = lm(formula = MPG ~ weight + model.year,
                  data = auto)
summary(multilin_reg)
plot(multilin_reg)

#Predicting the test set results
y_pred = predict(multilin_reg, newdata = test_set)
_________________________________________

#Fitting polynomial regression model to training set
training_set$weight2 = training_set$weight^2
training_set$weight3 = training_set$weight^3
poly_reg = lm(formula = MPG ~ (weight + weight2), 
              data = training_set)
summary(poly_reg)
poly_reg2 = lm(formula = MPG ~ (weight + weight2 + weight3), 
              data = training_set)
summary(poly_reg2)
plot(poly_reg)

#Visualising training set results
ggplot() +
  geom_point(aes(x = training_set$weight, y = training_set$MPG),
             colour = 'red') +
  geom_line(aes(x = training_set$weight, y = predict(poly_reg, newdata = training_set)),
            colour = 'blue') +
  ggtitle('MPG vs Weight (Training set - poly_reg)') +
  xlab('Weight') +
  ylab('MPG') +
  xlim(1500,5000) + 
  ylim(5,50)

#Visualising test set results
ggplot() +
  geom_point(aes(x = test_set$weight, y = test_set$MPG),
             colour = 'red') +
  geom_line(aes(x = training_set$weight, y = predict(poly_reg, newdata = training_set)),
            colour = 'blue') +
  ggtitle('MPG vs Weight (Test set - poly_reg)') +
  xlab('Weight') +
  ylab('MPG') +
  xlim(1500,5000) + 
  ylim(5,50)