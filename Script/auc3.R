library(caret)
library(ROCR)
data(iris)


iris <- iris[iris$Species == "virginica" | iris$Species == "versicolor", ]
iris$Species <- factor(iris$Species)  # setosa should be removed from factor

samples <- sample(NROW(iris), NROW(iris) * .7)
data.train <- iris[samples, ]
data.test <- iris[-samples, ]
forest.model <- train(Species ~., data.train)


y <- data.test$Species # logical array of positive / negative cases
predictions <- predict(forest.model, data.test, type="prob") # Prediction

pred <- prediction(predictions, y);

# Recall-Precision curve             
RP.perf <- performance(pred, "prec", "rec");
plot (RP.perf);

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr");
plot (ROC.perf);

# ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)
