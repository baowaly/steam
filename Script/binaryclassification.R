# load libraries
library(caret)
library(mlbench)
library(ROCR)
library(data.table)

setwd("/home/mmnet/steam/")

# load the dataset
data(PimaIndiansDiabetes)

n.loop <- 2
comb.score <- NULL
for(i in seq(n.loop)){

# create a list of 80% of the rows in the original dataset we can use for training
train_index <- createDataPartition(PimaIndiansDiabetes$diabetes, p=0.80, list=FALSE)
# select 20% of the data for validation
dataTest <- PimaIndiansDiabetes[-train_index,]
# use the remaining 80% of data to training and testing the models
dataTrain <- PimaIndiansDiabetes[train_index,]

# summarize the class distribution
percentage <- prop.table(table(dataTest$diabetes)) * 100
cbind(freq=table(dataTest$diabetes), percentage=percentage)

# prepare resampling method
fitControl = trainControl(method="repeatedcv", # Baowaly changed here, it was boot
                          number=10, 
                          repeats=5,
                          returnData = TRUE,
                          returnResamp = "final",
                          selectionFunction = "best",
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          allowParallel = TRUE)

#set.seed(7)

#Train the model


gbmFit <- train(diabetes~., data=dataTrain, method="gbm", metric="ROC", trControl=fitControl, verbose = FALSE)
svmFit <- train(diabetes~., data=dataTrain, method = "svmRadial", metric="ROC", trControl = fitControl)

gbmScore <- get_eval_score(gbmFit, dataTrain, dataTest, "GBM")
svmScore <- get_eval_score(svmFit, dataTrain, dataTest, "SVM")

#combine score
comb.score <- rbind(comb.score, rbind(gbmScore, svmScore))

}
print(comb.score)
save(comb.score, file="Result/Combine_Score.Rdata")

#stop("hrere")



#Model Evaluation function
get_eval_score = function(model.fit, dataTrain, dataTest, method){
  
  #Evaluation on Training Data
  #train.pred <- predict(model.fit, dataTrain)
  train.pred <- predict(model.fit, type = "raw")
  train.ref <- dataTrain$diabetes
  train.confMatrx <- confusionMatrix(data=train.pred, reference=train.ref, positive="pos", mode = "prec_recall")
  train.accuracy <- train.confMatrx$overall[["Accuracy"]]
  train.f1score <- train.confMatrx$byClass[["F1"]]
  #Training AUC
  pred_vector <- as.numeric(ifelse(train.pred == "pos", 1, 0)) #make numeric
  ref_vector <- as.numeric(ifelse(train.ref == "pos", 1, 0)) #make numeric
  auc.pred <- prediction(predictions = pred_vector, labels = ref_vector)
  auc.tmp <- performance(auc.pred,"auc");
  train.auc <- as.numeric(auc.tmp@y.values)
  
  #Evaluation on Test Data
  test.pred <- predict(model.fit, dataTest)
  test.ref <- dataTest$diabetes
  test.confMatrx <- confusionMatrix(data=test.pred, reference=test.ref, positive="pos", mode = "prec_recall")
  test.precision <- test.confMatrx$byClass[["Precision"]]
  test.recall <- test.confMatrx$byClass[["Recall"]]
  test.sensitivity <- test.confMatrx$byClass[["Sensitivity"]]
  test.specificity <- test.confMatrx$byClass[["Specificity"]]
  test.accuracy <- test.confMatrx$overall[["Accuracy"]]
  test.f1score <- test.confMatrx$byClass[["F1"]]
  #Test AUC
  pred_vector <- as.numeric(ifelse(test.pred == "pos", 1, 0)) #make numeric
  ref_vector <- as.numeric(ifelse(test.ref == "pos", 1, 0)) #make numeric
  auc.pred <- prediction(predictions = pred_vector, labels = ref_vector)
  auc.tmp <- performance(auc.pred,"auc");
  test.auc <- as.numeric(auc.tmp@y.values)
  
  return(data.table(train.accuracy=train.accuracy, 
                    train.f1score=train.f1score, 
                    train.auc=train.auc, 
                    test.precision=test.precision, 
                    test.recall=test.recall, 
                    test.sensitivity=test.sensitivity, 
                    test.specificity=test.specificity, 
                    test.accuracy=test.accuracy, 
                    test.f1score=test.f1score, 
                    test.auc=test.auc, 
                    method=method))
}




