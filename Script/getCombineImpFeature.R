start.time <- Sys.time()

#install.packages(new.packages, repos="http://cran.rstudio.com/")

library(ggplot2)
library(reshape2)
library(data.table)

setwd("~/steam/")

genres <- c(#"All",
            "Action",
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

comb.feature <- NULL
#genre <- "Action"

for(genre in genres){

    #load Feature File
    cat("\nLoading Feature File..", genre)

    # load the meanAccuracy.csv file from the local directory
    featureFile <- paste0("FeatureImp/IMP_FT_", genre, "_V50_R0.8.Rdata")
    impFeatures <- get(load(featureFile))
    rownames(impFeatures) <- NULL
    f.weights <- impFeatures$mean
    impFeatures$RelativeWeight <- round((f.weights-min(f.weights))/(max(f.weights)-min(f.weights)) * 100, digits = 2)
    
    featureWeights <- impFeatures[1:20, c("feature", "RelativeWeight"),]
    
    makeString <-toString(paste0( featureWeights$feature,  "(", featureWeights$RelativeWeight  ,")" ))
    
    #print(makeString)
    
    makerow <- cbind(Genre = genre, Feature = makeString)
    
    comb.feature <- rbind(comb.feature, makerow)

}

outputCSV <- paste0("FeatureImp/COM_IMP_Feature.csv")
if(!file.exists(outputCSV)){
  file.create(outputCSV)  
} 
write.csv(comb.feature, file=outputCSV)




end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
