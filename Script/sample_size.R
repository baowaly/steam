start.time <- Sys.time()

library(ggplot2)
library(reshape2)
# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"
sample.size <- 1000
vote.num <- 30
score.num <- 0.7

sourceFile <- paste0("Rdata/AN_", vote.num, genre, score.num, "_", sample.size, ".Rdata")
load(sourceFile)
dataset <- di.tmp

# 1 for training and 0 for test
tnAccuracy = 0

if(tnAccuracy){
    #Mean Training Accuracy
    result <- aggregate(x = dataset$accuracy, by = list(dim = dataset$dim, method = dataset$method), FUN = "mean")
    write.csv(result, file = "Result/meanFeatureAccuracy.csv")
    
    # load the meanAccuracy.csv file from the local directory
    meanFeatureAccuracy <- read.csv("Result/meanFeatureAccuracy.csv", header=TRUE)
    
    
    # Plot 
    accuracyPlot <- ggplot(data=meanFeatureAccuracy, aes(x=dim, y=x, group=method, colour=method)) +
      geom_line() +
      geom_point()+
      ggtitle(paste0("Training Accuracy of ",genre," Reviews\nSample Size: ",sample.size," Vote: >=",vote.num," Score: ",score.num)) +     # Set title
      scale_x_continuous(breaks=c(seq(0,200,10))) +
      xlab("No. of Features") + ylab("Accuracy") + # Set axis labels
      scale_color_discrete(name = "Classifier:", labels = c("GBM", "SVM")) + # Set legend title
      theme(legend.position="top", plot.title = element_text(size = 10))
    
    print(paste0("Image Filename: Accuracy_", vote.num, genre, score.num, "_", sample.size))
    print(accuracyPlot)

} else {

    #Mean Testing Accuracy
    result <- aggregate(x = dataset$f.score, by = list(dim = dataset$dim, method = dataset$method), FUN = "mean")
    write.csv(result, file = "Result/meanFeatureF1score.csv")
    
    # load the meanFeatureF1score.csv file from the local directory
    meanFeatureF1score <- read.csv("Result/meanFeatureF1score.csv", header=TRUE)
    
    # Plot 
    f1scorePlot <- ggplot(data=meanFeatureF1score, aes(x=dim, y=x, group=method, colour=method)) +
      geom_line() +
      geom_point()+
      ggtitle(paste0("Test Accuracy of ",genre," Reviews\nSample Size: ",sample.size," Vote: >=",vote.num," Score: ",score.num)) +     # Set title
      scale_x_continuous(breaks=c(seq(0,200,10))) +
      xlab("No. of Features") + ylab("F1-Score") + # Set axis labels
      scale_color_discrete(name = "Classifier:", labels = c("GBM", "SVM")) + # Set legend title
      theme(legend.position="top", plot.title = element_text(size = 10))
    
    print(paste0("Image Filename: F1score_", vote.num, genre, score.num, "_", sample.size))
    print(f1scorePlot)

    f1scoreByMethod<-merge(aggregate(x ~ method, data = meanFeatureF1score, FUN = max), meanFeatureF1score)
    print(f1scoreByMethod)
}



end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

