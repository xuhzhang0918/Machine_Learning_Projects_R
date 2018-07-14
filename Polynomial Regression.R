dataset <- read.csv('Position_Salaries.csv')
library(caTools)
set.seed(123)
dataset <- dataset[, 2:3]

# Fitting Linear Regression to the dataset
lin_reg <- lm(Salary ~ ., data = dataset)
summary(lin_reg)

# Fitting Polynomial Regression to the dataset
dataset$level2 <- dataset$Level^2
dataset$level3 <- dataset$Level^3
dataset$level4 <- dataset$Level^4
poly_reg <- lm(Salary ~., data = dataset)
summary(poly_reg)

# Visualising the Linear Regression Results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)), colour = 'blue') +
  xlab('Position Level') +
  ylab('Salary') +
  ggtitle('Salary vs Position Level')

ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)), colour = 'blue') +
  xlab('Position Level') +
  ylab('Salary') +
  ggtitle('Salary vs Position Level')

# Predicting a new result with Linear Regression
y_pred <- predict(lin_reg, data.frame(Level = 6.5))

# Predicting a new result with Polynomial Regression
y_pred <- predict(lin_reg, data.frame(Level = 6.5,
                                      level2 = 6.5^2,
                                      level3 = 6.5^3,
                                      level4 = 6.5^4))
