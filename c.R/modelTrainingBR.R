start.time <- Sys.time()

cat("Loading Packages..")

# source("/home/baowaly/steam/c.R/modelTrainingBR.R")
list.of.packages <- c("data.table","parallel","caret", "doMC", "gbm", "e1071", "xgboost", "ROCR", "pROC", "kernlab", "DMwR", "ROSE")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(data.table)
library(parallel)
library(caret)
library(doMC)
library(gbm)
library(e1071)
library(xgboost)
library(ROCR)
library(pROC)
library(kernlab)
library(DMwR)
library(ROSE)

#Register cores
freeCores = max(1, detectCores(logical = FALSE) - 1)
registerDoMC(freeCores)

#Set path
path = "/home/baowaly/steam/"
#path = "/home/yipeitu/steam_helpful_review_20160701/"
setwd(path)

#Model Evaluation function
get_eval_score = function(model.fit, trainX, trainY, testX, testY, method){
  
  positiveClass <- "Yes"
  
  #Evaluation on Training Data
  #train.pred <- predict(model.fit, trainX)
  train.pred <- predict(model.fit, type = "raw")
  train.ref <- trainY#dataTrain$helpful
  train.confMatrx <- confusionMatrix(data=train.pred, reference=train.ref, positive=positiveClass, mode = "prec_recall")
  train.accuracy <- train.confMatrx$overall[["Accuracy"]]
  train.f1score <- train.confMatrx$byClass[["F1"]]
  
  
  #Training AUC
  pred_vector <- as.numeric(ifelse(train.pred == positiveClass, 1, 0)) #make numeric
  ref_vector <- as.numeric(ifelse(train.ref == positiveClass, 1, 0)) #make numeric
  auc.pred <- prediction(predictions = pred_vector, labels = ref_vector)
  auc.tmp <- performance(auc.pred,"auc");
  train.auc <- as.numeric(auc.tmp@y.values)
  
  #Evaluation on Test Data
  test.pred <- predict(model.fit, testX)
  test.ref <- testY#dataTest$helpful
  test.confMatrx <- confusionMatrix(data=test.pred, reference=test.ref, positive=positiveClass, mode = "prec_recall")
  test.precision <- test.confMatrx$byClass[["Precision"]]
  test.recall <- test.confMatrx$byClass[["Recall"]]
  test.sensitivity <- test.confMatrx$byClass[["Sensitivity"]]
  test.specificity <- test.confMatrx$byClass[["Specificity"]]
  test.accuracy <- test.confMatrx$overall[["Accuracy"]]
  test.f1score <- test.confMatrx$byClass[["F1"]]
  #Test AUC
  pred_vector <- as.numeric(ifelse(test.pred == positiveClass, 1, 0)) #make numeric
  ref_vector <- as.numeric(ifelse(test.ref == positiveClass, 1, 0)) #make numeric
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

##########Model Bulding and Evaluation###########

#######Select Some Parameters#########
g <- "All"
score <- 0.80
vote.num <- 50
max.dims <- 712

#Cross Validation Params
n.fold <- 10 
n.repeats <- 5

#Select dataset by vote
cat("\nLoading Dataset..")
dataFile <- paste0("Dataset/reviews_combine_", vote.num, ".Rdata")
if(!file.exists(dataFile)){
  file.create(dataFile)
  
  #load the original dataset
  if(! exists("dg.all")) load("Dataset/reviews_combine_new.Rdata")
  dg.all = d.combine
  
  d.combine.vote <- dg.all[vote.total >= vote.num]
  save(d.combine.vote, file=dataFile)
  
  rm("dg.all", "d.combine")
  
}else{
  load(dataFile)
}

#Ensure no NA value in target column:rating
d.combine.vote <- subset(d.combine.vote, !is.na(rating))

##Add a helpful column
d.combine.vote$helpful <- ifelse(d.combine.vote$rating >= score, "Yes", "No")
d.combine.vote$helpful <- as.factor(d.combine.vote$helpful)

#load Feature File
cat("\nLoading Feature File..")
featureFile <- paste0("Features/BR_50_All_0.8_20.Rdata")
if(!file.exists(featureFile)){
  stop("\nFeature File Not Exists!!")
}
load(featureFile)

#Select dataset with max features
name.dim.x <- feature.list[order(feature.list$Freq)[1:max.dims],]$feature.name
name.dim.y <- "helpful"
dg <- d.combine.vote[, c(name.dim.x,name.dim.y), with=F]

#Delete all rows with NA
dg <- na.omit(dg)
n.rows <- NROW(dg)

#Sample Size for Model Training
n.samplePercent <- 30
n.sampleSize <- ceiling(n.rows * n.samplePercent/100)

cat("\nModel Bulding and Evaluation: Genre:", g, " Vote:", vote.num, " Score:", score, " Sample Size(",n.samplePercent,"%):", n.sampleSize)

#loop control
n.loop <- 1
comb.score <- NULL

cat("\nTraining Model..")

for(l.counter in seq(n.loop)){
  
  cat("\nSample: ", l.counter)
  
  set.seed(vote.num+score+l.counter)
  
  #Random Sample from original dataset
  #sampleIndex <- sample(n.rows, size=n.sampleSize, replace=FALSE)
  #sampleData <- dg[sampleIndex, c(name.dim.x,name.dim.y), with=F]
  #sampleData <- dg[sampleIndex,]
  
  #Random Sample from original dataset by both oversampling & undersampling
  sampleData <- ovun.sample(helpful ~ ., data = dg, method="both", p=0.5, N=n.sampleSize)$data
  
  set.seed(vote.num+score+l.counter)
  #Partition sample dataset into training and testing
  split=0.80
  trainIndex <- as.vector(createDataPartition(y=sampleData$helpful, p=split, list=FALSE))
  
  #n.dim <- 90
  for(n.dim in c(206)){
  
    #selecting features
    name.dim.x <- feature.list[order(feature.list$Freq)[1:n.dim],]$feature.name
    
    dataTrain <- sampleData[trainIndex, c(name.dim.x,name.dim.y), ]
    dataTest <- sampleData[-trainIndex, c(name.dim.x,name.dim.y), ]
    
    #split balanced train data
    dataTrain.balanced <- dataTrain
    trainX <- dataTrain.balanced[, name.dim.x, ]
    trainY <- as.factor(dataTrain.balanced[, name.dim.y, ])
    
    #split test data 
    testX <- dataTest[, name.dim.x, ]
    testY <- as.factor(dataTest[, name.dim.y, ])
    
    fitControl = trainControl(method="repeatedcv",
                              number=n.fold, 
                              repeats=n.repeats,
                              returnResamp = "final",
                              selectionFunction = "best",
                              classProbs=TRUE, 
                              summaryFunction=twoClassSummary,
                              allowParallel = TRUE)
    
    
    #Remove nearZeroVar columns 
    #badCols <- nearZeroVar(trainX)
    #if(length(badCols) > 0)
    #trainX <- trainX[, -badCols]
    
    #Preprocess Training Data
    preObj <- preProcess(trainX, method = c("center", "scale"))
    trainX <- predict(preObj, trainX)
    
    #Preprocess Testing Data
    preObj <- preProcess(testX, method = c("center", "scale"))
    testX <- predict(preObj, testX)
    
    gbmFit <- train(trainX, trainY, method="gbm", metric="ROC", trControl=fitControl, verbose=F)
    #svmFit <- train(trainX, trainY, method="svmRadial", metric="ROC", trControl=fitControl)
    #xgboostFit <- train(trainX, trainY, method="xgbTree", metric="ROC", trControl=fitControl)
    
    #print(gbmFit)
    #print(svmFit)
    #print(xgboostFit)
    
    #print(xgboostFit$bestTune)
    
    #Get Evaluation Score
    gbmScore <- cbind(genre=g, vote=vote.num, rating=score, sample.size=n.sampleSize, features=n.dim, get_eval_score(gbmFit, trainX, trainY, testX, testY, "GBM"))
    #svmScore <- cbind(genre=g, vote=vote.num, rating=score, sample.size=n.sampleSize, features=n.dim, get_eval_score(svmFit, trainX, trainY, testX, testY, "SVM"))
    #xgboostScore <- cbind(genre=g, vote=vote.num, rating=score, sample.size=n.sampleSize, features=n.dim, get_eval_score(xgboostFit, trainX, trainY, testX, testY, "XGBoost"))
    
    #print(gbmScore)
    #print(svmScore)
    #print(xgboostScore)
    
    #combine score
    #tmp.score <- rbind(gbmScore, svmScore, xgboostScore)
    tmp.score <- gbmScore
    cat("\n") 
    print(tmp.score)
    comb.score <- rbind(comb.score, tmp.score)
    
  }#end of dim
  
}#end of loop
outputFile <- paste0("Evaluation/ES_BR_", g, "_V", vote.num, "_R", score, "_F", n.dims, "_S", n.samplePercent, ".Rdata")
outputCSV <- paste0("Evaluation/ES_BR_", g, "_V", vote.num, "_R", score, "_F", n.dims, "_S", n.samplePercent, ".csv")
if(!file.exists(outputFile)){
  file.create(outputFile)  
} 
save(comb.score, file=outputFile)
write.csv(comb.score, file=outputCSV)

cat("\nModel Bulding and Evaluation: Done")
cat("\nSaved file in: ", outputFile, "\n")
#########End of Model Bulding and Evaluation###########

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


