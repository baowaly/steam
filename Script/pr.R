## A simple example with data in package ROCR
library(ROCR)
library(DMwR)
library(pROC)
data(ROCR.simple)
pred <- ROCR::prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- ROCR::performance(pred,"prec","rec")
## The plot obtained with the standard ROCR functions
## Not run: 
plot(perf)

## End(Not run)

## Now our Precision/Recall curve avoiding the saw-tooth effect
## Not run: 
PRcurve(ROCR.simple$predictions,ROCR.simple$labels)

## End(Not run)

result.roc <- roc(ROCR.simple$labels, ROCR.simple$predictions) # Draw ROC curve.
plot(result.roc, percent=TRUE, col="#1c61b6", print.auc=TRUE)
