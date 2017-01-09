# load the library
library(mlbench)
library(caret)
library(data.table)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)

name.x <- c("pregnant", "glucose",  "pressure", "triceps",  "insulin",  "mass", "pedigree",  "age")

feature.imp <- cbind(feature=name.x)

for(i in seq(1:5)){
  # train the model
  model <- train(diabetes~., data=PimaIndiansDiabetes, method="gbm", preProcess="scale", trControl=control, verbose=F)
  # estimate variable importance
  gbmImp <- varImp(model, scale=FALSE)
  
  # combine importance features
  impFeatures <- gbmImp$importance
  feature.imp <- cbind(feature.imp, weight=impFeatures$Overall)

}

feature.imp <- as.data.frame(feature.imp, stringsAsFactors=FALSE)
n.cols <- NCOL(feature.imp)
for (column in 2:n.cols) {
  feature.imp[, column] <- as.numeric(feature.imp[, column])
}
feature.imp$total <- rowSums(subset(feature.imp, select=2:n.cols))
#sort
feature.imp <- feature.imp[order(-feature.imp$total),]


x = sample(-100:100,10)

x <- append(x, c(10, 0))

#Normalized Data
normalized = (x-min(x))/(max(x)-min(x))
