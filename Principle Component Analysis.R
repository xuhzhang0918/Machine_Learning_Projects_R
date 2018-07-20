dataset  = read.csv('wine.csv')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
training_set[, 1:13] = scale(training_set[, 1:13])
test_set[, 1:13] = scale(test_set[, 1:13])

library(caret)
library(e1071)
library(dplyr)
pca = preProcess(x = training_set[-14], method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)
training_set = training_set %>%
  select(PC1, PC2, Customer_Segment)
test_set = predict(pca, test_set)
test_set = test_set %>%
  select(PC1, PC2, Customer_Segment)

classifier = svm(formula = Customer_Segment ~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set[-3])
cm = table(test_set[,3], y_pred)

# Visualsing the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.0075)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.0075)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Classifier (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'Deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[,3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualsing the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.0075)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.0075)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Classifier (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'Deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[,3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))