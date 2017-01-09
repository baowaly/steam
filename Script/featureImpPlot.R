start.time <- Sys.time()

#install.packages(new.packages, repos="http://cran.rstudio.com/")

library(ggplot2)
library(reshape2)
library(data.table)


# load the CSV file from the local directory
setwd("~/steam/")

genre = "Combine"

# load the meanAccuracy.csv file from the local directory
featureFile <- paste0("FeatureImp/IMP_FT_", genre, "_V50_R0.8.Rdata")
impFeatures <- get(load(featureFile))
rownames(impFeatures) <- NULL

#f.weights <- impFeatures$mean
#impFeatures$mean <- round((f.weights-min(f.weights))/(max(f.weights)-min(f.weights)) * 100, digits = 2)

featureWeights <- impFeatures[1:30, c("feature", "mean"),]

#Turn your 'feature' column into a character vector
featureWeights$feature <- as.character(featureWeights$feature)
#Then turn it back into an ordered factor
featureWeights$feature <- factor(featureWeights$feature, levels=unique(featureWeights$feature))

# Plot 
accuracyPlot <- ggplot(data=featureWeights, aes(x= feature, y= as.numeric(mean) )) +
  geom_bar(stat="identity", position=position_dodge(1), fill="steelblue", width=0.9) +
  #ggtitle(paste0("Feature importance of ", genre ," genre")) +     # Set title
  xlab("Features") + ylab("Feature importance (Relative weight)") + # Set axis labels
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90, hjust=1)
      )

  #theme(legend.position="top", plot.title = element_text(size = 12)) 
  
  print(accuracyPlot)
  
  print(paste0("featureImp_", genre))

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

