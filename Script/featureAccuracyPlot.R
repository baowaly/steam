start.time <- Sys.time()

library(ggplot2)
library(reshape2)
library(data.table)


# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"
score.num <- 0.9
sample.size <- 5
n.dim <- 700

#combine files
#file1 <- paste0("Evaluation/ES_All_V50_R0.9_F400_S5.Rdata")
#file1.data <- get(load(file1))
#file1.data <- cbind(sample=sample.size, file1.data)

#file2 <- paste0("Evaluation/ES_All_V50_R0.9_F700_S5.Rdata")
#file2.data <- get(load(file2))
#file2.data <- cbind(sample=sample.size, file2.data)
#combine.data <- rbind(file1.data, file2.data)

#comFile <- paste0("Evaluation/Com_", genre, "_R", score.num, "_F", n.dim, ".Rdata")
#save(combine.data, file=comFile)


# load the meanAccuracy.csv file from the local directory
load("Evaluation/Com_All_R0.9_F700.Rdata")
dataset <- combine.data

#group by
dataset$features <- as.factor(dataset$features)
dataset$method <- as.factor(dataset$method)
meanFeatureScore <- aggregate(cbind(test.auc, test.f1score)~features+method, data=dataset, FUN = mean)

#get max test.f1score
GBMSCORE <- subset(meanFeatureScore, method == "GBM")
GBMSCORE[which.max(GBMSCORE$test.f1score),]

SVMSCORE <- subset(meanFeatureScore, method == "SVM")
SVMSCORE[which.max(SVMSCORE$test.f1score),]


  # Plot 
  accuracyPlot <- ggplot(data=meanFeatureScore, aes(x=as.numeric(as.character(features)), y=test.f1score, group=method, colour=method)) +
    geom_line(aes(linetype=method), size=0.5) +
    geom_point(aes(shape=method), size=1) +
    ggtitle(paste0("Model FScore of ",genre," Reviews\nScore: ",score.num, " Sample Size: ", sample.size)) +     # Set title
    scale_x_continuous(breaks=c(0, seq(50,700,50))) +
    xlab("No. of Features") + ylab("FScore") + # Set axis labels
    theme(legend.position="top", plot.title = element_text(size = 10))
  

  print(paste0("Image Filename: featureAccuracy_", genre, "_", score.num,"_gbm"))
  print(accuracyPlot)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

