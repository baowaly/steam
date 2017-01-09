start.time <- Sys.time()

library(ggplot2)
library(reshape2)
# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"
scores <- c(0.05, 0.10, 0.80, 0.85, 0.90, 0.95)

# if the combined dataset doesn't exist, create it
comFile <- paste0("WSF/WSF_GBM_", genre, "_V50.Rdata")
if(!file.exists(comFile)){
  file.create(comFile)  
} 
combine.wsf <- NULL
for(score in scores){
  # if the merged dataset does exist, append to it
  file <- paste0("WSF/WSF_GBM_", genre,"_V50_R", score,".Rdata")
  if(!file.exists(file)) next

  file.data <- get(load(file))
  combine.wsf <- rbind(combine.wsf, file.data)

}

#save file
save(combine.wsf, file=comFile)
cat("\nSaved file in: ", comFile, "\n")


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

