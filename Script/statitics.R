start.time <- Sys.time()

library(ggplot2)
library(reshape2)
# load the CSV file from the local directory
setwd("~/steam/")
dataset <- read.csv("Result/all_with_newFlow.csv")

#Mean Model Accuracy
result <- aggregate(x = dataset$accuracy, by = list(Method = dataset$method, Vote = dataset$vote), FUN = "mean")
write.csv(result, file = "Result/meanAccuracy.csv")

#stop("stop here")

# load the meanAccuracy.csv file from the local directory
meanAccuracy <- read.csv("Result/meanAccuracy.csv", header=TRUE)
dat1 <- data.frame(
  Classifier = factor(meanAccuracy$Method),
  Vote = meanAccuracy$Vote,
  Model_Accuracy = meanAccuracy$x
)

# Plot Model Accuracy
acimg <- ggplot(data=dat1, aes(x=Vote, y=Model_Accuracy, group=Classifier, colour=Classifier)) +
  geom_line() +
  geom_point()+
  ggtitle("Accuracy Comparison of Classifiers") +     # Set title
  scale_x_continuous(breaks=c(1,5,seq(10,100,10))) +
  xlab("Vote") + ylab("Accuracy") + # Set axis labels
  scale_color_discrete(name = "Classifier:", labels = c("GBM", "SVM")) + # Set legend title
  theme(legend.position="top")

print(acimg)

#Mean F1 Score
result <- aggregate(x = dataset$fscore, by = list(Method = dataset$method, Vote = dataset$vote), FUN = "mean")
write.csv(result, file = "Result/meanF1Score.csv")

# load the meanF1Score.csv file from the local directory
meanF1Score <- read.csv("Result/meanF1Score.csv", header=TRUE)
dat1 <- data.frame(
  Classifier = factor(meanF1Score$Method),
  Vote = meanF1Score$Vote,
  F1_Score = meanF1Score$x
)

# Plot F1-Score
f1img <- ggplot(data=dat1, aes(x=Vote, y=F1_Score, group=Classifier, colour=Classifier)) +
  geom_line() +
  geom_point()+
  ggtitle("F1-Score Comparison of Classifiers") +     # Set title
  scale_x_continuous(breaks=c(1,5,seq(10,100,10))) +
  xlab("Vote") + ylab("F1-Score") + # Set axis labels
  scale_color_discrete(name = "Classifiers:", labels = c("GBM", "SVM")) + # Set legend title
  theme(legend.position="top")

print(f1img)

print("I am in end")

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

