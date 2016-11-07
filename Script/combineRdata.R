start.time <- Sys.time()

library(ggplot2)
library(reshape2)
# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"
score.num <- 0.8
sample.seq <- c(1, 10, 15, 20)
vote.seq <- c(50)
n.dim <- 500

# if the combined dataset doesn't exist, create it
comFile <- paste0("Evaluation/Com_", genre, "_R", score.num, "_F", n.dim, ".Rdata")

if(!file.exists(comFile)){
  file.create(comFile)  
} 
combine.data <- NULL
for(sample.size in sample.seq){
  for(vote.num in vote.seq){
    # if the merged dataset does exist, append to it
    file <- paste0("Evaluation/ES_", genre, "_V", vote.num, "_R", score.num,"_F", n.dim, "_S", sample.size, ".Rdata")
    if(!file.exists(file)) next

    file.data <- get(load(file))
    file.data <- cbind(sample=sample.size, file.data)
    combine.data <- rbind(combine.data, file.data)

  }
  
}
save(combine.data, file=comFile)
cat("\nSaved file in: ", comFile, "\n")


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

