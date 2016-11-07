start.time <- Sys.time()

library(ggplot2)
library(reshape2)
# load the CSV file from the local directory
setwd("~/steam/")
#dataset <- read.csv("Result/Vote1_Score0.7_Ite1.csv", header=TRUE)
load("Rdata/AN_10All0.7.Rdata")

dataset <- di.tmp


# Plot Model Accuracy
acimg <- ggplot(data=dataset, aes(x=dim, y=accuracy, group=method, colour=method)) +
  geom_line() +
  geom_point()+
  ggtitle("Accuracy Comparison for the Reviews with Vote >=10 and Score 0.70") +     # Set title
  scale_x_continuous(breaks=c(seq(0,200,10))) +
  xlab("Feature Dimension") + ylab("Accuracy") + # Set axis labels
  scale_color_discrete(name = "Classifier:", labels = c("GBM", "SVM")) + # Set legend title
  theme(legend.position="top", plot.title = element_text(size = 10))

print(acimg)

# Plot Model Accuracy
acimg <- ggplot(data=dataset, aes(x=dim, y=f.score, group=method, colour=method)) +
  geom_line() +
  geom_point()+
  ggtitle("F1-Score Comparison for the Reviews with Vote >=10 and Score 0.70") +     # Set title
  scale_x_continuous(breaks=c(seq(0,200,10))) +
  xlab("Feature Dimension") + ylab("F1-Score") + # Set axis labels
  scale_color_discrete(name = "Classifier:", labels = c("GBM", "SVM")) + # Set legend title
  theme(legend.position="top", plot.title = element_text(size = 10))

print(acimg)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

