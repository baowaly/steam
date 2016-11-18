start.time <- Sys.time()

setwd("~/steam/")

genres <- c("All",
            "Action",
            "Strategy",
            "RPG",
            "Survival",
            "Simulation",
            "Adventure",
            "FPS",
            "Horror",
            "Racing",
            "Casual"
          )


scores <- c(0.05, 0.10, 0.8, 0.85, 0.90, 0.95)
vote <- 50
method <- "GBM"


final.result <- NULL 
for(genre in genres){

  for(score in scores){
    
    if(genre == "All")
      sample.size <- "15"
    else
      sample.size <- "100"
    
    sourceFile <- paste0("finalResult/", method, "_", genre ,"_V", vote ,"_R", score, "_S", sample.size,".Rdata")
    
    if(!file.exists(sourceFile)) next
    load(sourceFile)
    dataset <- comb.score
    
    stat <- aggregate(cbind(vote, rating, train.f1score, train.auc, test.f1score, test.auc ) ~ genre, data = dataset, FUN = mean)
    
    final.result <- rbind(final.result, stat)

  }

}

outputFile <- paste0("finalResult/finalResult.Rdata")
outputCSV <- paste0("finalResult/finalResult.csv")
if(!file.exists(outputFile)){
  file.create(outputFile)  
} 
save(final.result, file=outputFile)
write.csv(final.result, file=outputCSV)


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
