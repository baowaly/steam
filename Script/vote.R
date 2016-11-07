start.time <- Sys.time()

library(ggplot2)
library(reshape2)
# load the CSV file from the local directory
setwd("~/steam/")

genre <- "All"
sample.size <- 5000
vote.num <- 50
score.num <- 0.7

sourceFile <- paste0("Rdata/AN_", vote.num, genre, score.num, "_", sample.size, ".Rdata")
load(sourceFile)
dataset <- di.tmp

# Accuracy by method
print(paste0("Sample:",sample.size," Vote:",vote.num," Score:",score.num))
stat <- aggregate(cbind(accuracy, f.score) ~ method, data = dataset, FUN = mean)
print(stat)



end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

