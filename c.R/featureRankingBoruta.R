start.time <- Sys.time()

cat("Loading Packages..")

# source("/home/baowaly/steam/c.R/featureRankingAll.R")
list.of.packages <- c("data.table","parallel","caret", "doMC", "gbm", "e1071", "ROCR", "pROC", "kernlab", "Boruta")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(data.table)
library(parallel)
library(caret)
library(doMC)
library(gbm)
library(e1071)
library(ROCR)
library(pROC)
library(kernlab)
library(Boruta)

#Register cores
freeCores = max(1, detectCores(logical = FALSE) - 1)
registerDoMC(freeCores)

#Set path
#path = "/home/baowaly/steam/"
path = "/home/yipeitu/steam_helpful_review_20160701/"
setwd(path)


##########Boruta Feature Selection###########

#######Select Some Parameters#########
g <- "All"
score <- 0.80
vote.num <- 50
n.dims <- 300

#Cross Validation Params
n.fold <- 10 
n.repeats <- 5

#Select dataset by vote
cat("\nLoading Dataset..")
dataFile <- paste0("Dataset/reviews_combine_", vote.num, ".Rdata")
if(!file.exists(dataFile)){
  file.create(dataFile)
  
  #load the original dataset
  if(! exists("dg.all")) load("Dataset/reviews_combine_new.Rdata")
  dg.all = d.combine
  
  d.combine.vote <- dg.all[vote.total >= vote.num]
  save(d.combine.vote, file=dataFile)
  
  rm("dg.all", "d.combine")
  
}else{
  load(dataFile)
}

#Ensure no NA value in response column:rating
dg <- subset(d.combine.vote, !is.na(rating))
n.rows <- NROW(dg)
rm("d.combine.vote")

##Add a helpful column
dg$helpful <- ifelse(dg$rating >= score, "Yes", "No")
dg$helpful <- as.factor(dg$helpful)

#load Feature File
cat("\nLoading Feature File..")
featureFile <- paste0("Features/FT_50_All_0.8_20.Rdata")
if(!file.exists(featureFile)){
  stop("\nFeature File Not Exists!!")
}
load(featureFile)

#Sample Size for Model Training
n.samplePercent <- 20
n.sampleSize <- ceiling(n.rows * n.samplePercent/100)

cat("\nBoruta Feature Selection: Genre:", g, " Vote:", vote.num, " Score:", score, " Sample Size(",n.samplePercent,"%):", n.sampleSize, "\n")


#selecting features
#name.dim.x <- feature.rank[order(feature.rank$total)[1:n.dims],]$name
name.dim.x <- feature.rank[order(feature.rank$total),]$name
name.dim.y <- "helpful"

n.loop <- 10
feature.name <- NULL 
for(i in (seq(n.loop))){
    #Random Sample from original dataset
    sampleIndex <- sample(n.rows, size=n.sampleSize, replace=FALSE)
    sampleData <- dg[sampleIndex, c(name.dim.x,name.dim.y), with=F]
    
    #Now we’ll replace blank cells with NA
    sampleData[sampleData == ""] <- NA
    
    #all rows without NA value 
    sampleData <- sampleData[complete.cases(sampleData),]
    
    #do boruta selection
    boruta.train <- Boruta(helpful~., data = sampleData, doTrace = 2)
    #print(boruta.train)
    
    #Take the final decision of tentative attributes
    final.boruta <- TentativeRoughFix(boruta.train)
    #print(final.boruta)
    confirm.Attrs <- getSelectedAttributes(final.boruta, withTentative = F)
    feature.name <- append(feature.name, confirm.Attrs)
    cat("\nIteration ", i, ": Done\n")
}
#create file
br.featureFile <- paste0("Features/BR_", vote.num, "_", g, "_", score, "_", n.samplePercent, ".Rdata")
if(!file.exists(br.featureFile)){
  file.create(br.featureFile)
}
#Save features to file
feature.freq <- table(feature.name)
feature.list <- as.data.frame(sort(feature.freq, decreasing=TRUE))
save(feature.list, file=br.featureFile)

cat("\nBoruta Feature Selection: Done")
#cat("\nSaved file in: ", outputFile, "\n")
#########End of Model Bulding and Evaluation###########

end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)


