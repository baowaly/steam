start.time <- Sys.time()

#install.packages(new.packages, repos="http://cran.rstudio.com/")

library(ggplot2)
library(reshape2)
library(data.table)

setwd("~/steam/")

genres <- c(#"All",
            #"Action",
            "Strategy",
            "RPG",
            "Survival",
            "Simulation",
            "Adventure",
            "FPS",
            "Horror",
            "Racing",
            "Anime"
          )


#find the common features
first.genre <- "Action"

#make a initial list of features
featureFile <- paste0("FeatureImp/IMP_FT_", first.genre, "_V50_R0.8.Rdata")
impFeatures <- get(load(featureFile))
rownames(impFeatures) <- NULL
common.feature <- impFeatures$feature
 
for(genre in genres){
    # load the feature file
    featureFile <- paste0("FeatureImp/IMP_FT_", genre, "_V50_R0.8.Rdata")
    impFeatures <- get(load(featureFile))
    rownames(impFeatures) <- NULL
    f2.list <- impFeatures$feature
    common.feature <- intersect(common.feature, f2.list)
}

#ranking features
comb.feature <- cbind(feature = common.feature)
#genre <- "Strategy"
for(genre in genres){
  # load the feature file
  featureFile <- paste0("FeatureImp/IMP_FT_", genre, "_V50_R0.8.Rdata")
  impFeatures <- get(load(featureFile))
  rownames(impFeatures) <- NULL
  
  f.weights <- impFeatures$mean
  impFeatures$RelativeWeight <- round((f.weights-min(f.weights))/(max(f.weights)-min(f.weights)) * 100, digits = 2)
  
  index <- unlist(lapply(common.feature, function(f) which(f == impFeatures$feature)))
  weight <- impFeatures[index,c("RelativeWeight")]
  comb.feature <- cbind(comb.feature, weight)
  
}

comb.feature <- as.data.frame(comb.feature, stringsAsFactors=FALSE)
n.cols <- NCOL(comb.feature)
for (column in 2:n.cols) {
  comb.feature[, column] <- as.numeric(comb.feature[, column])
}
comb.feature$mean <- round(rowMeans(subset(comb.feature, select=2:n.cols)), digits = 2)
#sort
comb.feature <- comb.feature[order(-comb.feature$mean),]
rownames(comb.feature) <- NULL

#save
outputRdata <- paste0("FeatureImp/IMP_FT_Combine_V50_R0.8.Rdata")
outputCSV <- paste0("FeatureImp/IMP_FT_Combine_V50_R0.8.csv")

save(comb.feature, file=outputRdata)
write.csv(comb.feature, file=outputCSV)


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
