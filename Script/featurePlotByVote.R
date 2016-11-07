start.time <- Sys.time()

library(ggplot2)
library(reshape2)


# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"
score.num <- 0.7
vote.num <- 10


# load the meanAccuracy.csv file from the local directory
load("Rdata/Com_All_0.7.Rdata")
dataset <- combine.data

#group by
dataset$sample <- as.factor(dataset$sample)
dataset$dim <- as.factor(dataset$dim)
dataset$method <- as.factor(dataset$method)
meanFeatureScore <- aggregate(cbind(accuracy, f.score)~sample+dim+method, data=subset(dataset, vote == vote.num), FUN = mean)

# 0->gbm, 1->svm
method <- 0     

if(method == 0){
  # Plot 
  accuracyPlot <- ggplot(data=subset(meanFeatureScore, method=="gbm"), aes(x=as.numeric(as.character(dim)), y=f.score, group=sample, colour=sample)) +
    geom_line(aes(linetype=sample), size=0.5) +
    geom_point(aes(shape=sample), size=1) +
    ggtitle(paste0("F1-Score of ",genre," Reviews\nVote: ", vote.num, " Score: ",score.num, " Classifier: GBM")) +     # Set title
    scale_x_continuous(breaks=c(1, seq(10,200,10))) +
    xlab("No. of Features") + ylab("F1-Score") + # Set axis labels
    scale_linetype_manual(values = c("dotted",  "F1",  "dotdash",  "solid", "longdash")) +
    theme(legend.position="top", plot.title = element_text(size = 10))
  
  print(paste0("Image Filename: featureF1score_", vote.num,"_", genre, "_", score.num,"_gbm"))
  print(accuracyPlot)
  
}else{
  
  # Plot 
  accuracyPlot <- ggplot(data=subset(meanFeatureScore, method=="svm"), aes(x=as.numeric(as.character(dim)), y=f.score, group=sample, colour=sample)) +
    geom_line(aes(linetype=sample), size=0.5) +
    geom_point(aes(shape=sample), size=1) +
    ggtitle(paste0("F1-Score of ",genre," Reviews\nVote: ", vote.num, " Score: ",score.num, " Classifier: SVM")) +     # Set title
    scale_x_continuous(breaks=c(1, seq(10,200,10))) +
    xlab("No. of Features") + ylab("F1-Score") + # Set axis labels
    scale_linetype_manual(values = c("dotted",  "F1",  "dotdash",  "solid", "longdash")) +
    theme(legend.position="top", plot.title = element_text(size = 10))
  
  print(paste0("Image Filename: featureF1score_", vote.num,"_", genre, "_", score.num,"_svm"))
  print(accuracyPlot)
}


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

