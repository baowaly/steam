start.time <- Sys.time()

#install.packages(new.packages, repos="http://cran.rstudio.com/")

library(ggplot2)
library(reshape2)
library(data.table)


# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"

datafile <- paste0("WSF/WSF_GBM_", genre, "_V50.Rdata")
dataset <- get(load(datafile))


#group by
dataset$features <- as.factor(dataset$features)
dataset$Threshold <- as.factor(dataset$ws.score)
meanFeatureScore <- aggregate(cbind(test.auc, test.f1score)~features+Threshold, data=dataset, FUN = mean)

  # Plot 
  accuracyPlot <- ggplot(data=meanFeatureScore, aes(x=as.numeric(as.character(features)), y=test.f1score, group=Threshold, colour=Threshold)) +
    geom_line(aes(linetype=Threshold), size=0.5) +
    geom_point(aes(shape=Threshold), size=1.5) +
    #ggtitle(paste0("F Score of ", genre ," genre")) +     # Set title
    scale_x_continuous(breaks=c(seq(100,700,100))) +
    xlab("# Features") + ylab("F-score") + # Set axis labels
    #theme(legend.position="right", plot.title = element_text(size = 10))
    scale_colour_manual(values = c("black","black", "blue", "red", "blue", "red")) +
    scale_linetype_manual(values = c("solid", "dotted", "solid", "dotted", "solid", "dotted")) +
    theme_bw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="right", plot.title = element_text(size = 10) )
  
  print(paste0("Image Filename: featureFscore_", genre))
  print(accuracyPlot)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

