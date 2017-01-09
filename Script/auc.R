#COMPARE XGBOOST with GBM
### Packages Required

library(caret)
library(corrplot)			# plot correlations
library(doParallel)		# parallel processing
library(dplyr)        # Used by caret
library(gbm)				  # GBM Models
library(pROC)				  # plot the ROC curve
library(xgboost)      # Extreme Gradient Boosting

library(ROCR)


### Get the Data
# Load the data and construct indices to divied it into training and test data sets.

data(segmentationData)  	# Load the segmentation data set
dim(segmentationData)
head(segmentationData,2)
#
trainIndex <- createDataPartition(segmentationData$Case,p=.5,list=FALSE)
trainData <- segmentationData[trainIndex,-c(1,2)]
testData  <- segmentationData[-trainIndex,-c(1,2)]
#
trainX <-trainData[,-1]        # Pull out the dependent variable
testX <- testData[,-1]
sapply(trainX,summary) # Look at a summary of the training data

## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)

# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)
#											
set.seed(1951)  # set the seed

# Set up to do parallel processing   
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune <- train(x=trainX,y=trainData$Class,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid,
                  verbose=FALSE)


# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune$bestTune
plot(gbm.tune)  		# Plot the performance of the training models
res <- gbm.tune$results
res

### GBM Model Predictions and Performance
# Make predictions using the test data set
gbm.pred <- predict(gbm.tune,testX)

#Look at the confusion matrix  
confusionMatrix(gbm.pred,testData$Class)   

#Draw the ROC curve 
gbm.probs <- predict(gbm.tune,testX,type="prob")
head(gbm.probs)

gbm.ROC <- roc(predictor=gbm.probs$PS, response=testData$Class, levels=rev(levels(testData$Class)))
gbm.ROC$auc
#Area under the curve: 0.8731
plot(gbm.ROC,main="GBM ROC")

# Plot the propability of poor segmentation
histogram(~gbm.probs$PS|testData$Class,xlab="Probability of Poor Segmentation")

###ROC#############
pred <- prediction(gbm.pred, testData$Class);

# Recall-Precision curve             
RP.perf <- performance(pred, "prec", "rec");

plot (RP.perf);

# ROC curve
ROC.perf <- performance(pred, "tpr", "fpr");
plot (ROC.perf);

# ROC area under the curve
auc.tmp <- performance(pred,"auc");
auc <- as.numeric(auc.tmp@y.values)

