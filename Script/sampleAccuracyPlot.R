start.time <- Sys.time()

library(ggplot2)
library(reshape2)
library(data.table)


# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"
score.num <- 0.8
vote.num <- 50
n.dim <- 300

#combine files
#file1 <- paste0("Evaluation/ES_All_V50_R0.8_F300_S20.Rdata")
#file1.data <- get(load(file1))
#combine.data <- cbind(sample=as.integer(file1.data$sample.size/1000), file1.data)

#file2 <- paste0("Evaluation/ES_All_V50_R0.9_F700_S5.Rdata")
#file2.data <- get(load(file2))
#file2.data <- cbind(sample=sample.size, file2.data)
#combine.data <- rbind(file1.data, file2.data)

#comFile <- paste0("Evaluation/Com_", genre, "_R", score.num, "_F", n.dim, ".Rdata")
#save(combine.data, file=comFile)

# load the meanAccuracy.csv file from the local directory
comFile <- paste0("RG_GBM_All_V50_S5.Rdata")
load(comFile)

dataset <- comb.score

#group by
dataset$sample <- as.factor(dataset$sample)
meanSampleAccuracy <- aggregate(cbind(test.rmse, test.cor)~sample, data=dataset, FUN = mean)

#plot training accuracy comparison
TrainAccuracyPlotBySample <- ggplot(data=meanSampleAccuracy, aes(x=as.numeric(as.character(sample)), y=train.f1score, group=Classifier, colour=Classifier)) +
  geom_line(aes(linetype=Classifier), size=0.5) +
  geom_point(aes(shape=Classifier), size=1) +
  ggtitle(paste0("Comparison of Learning Models of ",genre," Reviews\nRating Score: ",score.num, " Vote: ", vote.num, " Features: ", n.dim)) +     # Set title
  scale_x_continuous(breaks=c(1,5,10,15,20)) +
  xlab("Sample Size") + ylab("Training FScore") + # Set axis labels
  theme(legend.position="top", plot.title = element_text(size = 10))
print(TrainAccuracyPlotBySample)

#plot test accuracy comparison
  TestAccuracyPlotBySample <- ggplot(data=meanSampleAccuracy, aes(x=as.numeric(as.character(sample)), y=test.f1score, group=Classifier, colour=Classifier)) +
    geom_line(aes(linetype=Classifier), size=0.5) +
    geom_point(aes(shape=Classifier), size=1) +
    ggtitle(paste0("Comparison of Learning Models of ",genre," Reviews\nRating Score: ",score.num, " Vote: ", vote.num, " Features: ", n.dim)) +     # Set title
    scale_x_continuous(breaks=c(1,5,10,15,20)) +
    xlab("Sample Size") + ylab("Test FScore") + # Set axis labels
    theme(legend.position="top", plot.title = element_text(size = 10))
  print(TestAccuracyPlotBySample)
  
#Model Learning Curve
  method <-"GBM"
  #method <-"SVM" 
  train.score <- cbind(meanSampleAccuracy[meanSampleAccuracy$Classifier == method, c("sample", "train.f1score"), ], Type="Training score")
  setnames(train.score, "train.f1score", "fscore")
  test.score <- cbind(meanSampleAccuracy[meanSampleAccuracy$Classifier == method, c("sample", "test.f1score"), ], Type="Test score")
  setnames(test.score, "test.f1score", "fscore")
  data <- rbind(train.score, test.score)
  
  accuracyPlotByMethod <- ggplot(data, aes(x=as.numeric(as.character(sample)), y=fscore, group=Type, colour=Type)) +
    geom_line(aes(linetype=Type), size=0.5) +
    geom_point(aes(shape=Type), size=1) +
    ggtitle(paste0(method, " Learning Curve of ",genre," Reviews\nRating Score: ",score.num, " Vote: ", vote.num, " Features: ", n.dim)) +     # Set title
    scale_x_continuous(breaks=c(1,5,10,15,20)) +
    xlab("Sample Size") + ylab("F Score") + # Set axis labels
    scale_linetype_manual(values = c("solid", "solid")) +
    theme(legend.title=element_blank(), legend.position="top", plot.title = element_text(size = 10))
  print(accuracyPlotByMethod)
  
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

