dataset <- read.csv('Salary_data.csv')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
regressor = lm(formula = Salary ~ YearsExperience, data = training_set)
summary(regressor)
y_pred <- predict(regressor, newdata = test_set)
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary), colour = 'red') + geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), colour = 'blue') +
  xlab('Years of Experience') +
  ylab('Salary') +
  ggtitle('Salary vs Experience (Training set)')

ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary), colour = 'red') + geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)), colour = 'blue') +
  xlab('Years of Experience') +
  ylab('Salary') +
  ggtitle('Salary vs Experience (Test set)')
