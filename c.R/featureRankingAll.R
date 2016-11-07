#remove all variables
rm(list=ls())

#start time
start.time <- Sys.time()
cat("\nLoading Packages..")

# source("/home/baowaly/steam/c.R/featureRankingAll.R")
list.of.packages <- c("data.table","parallel","caret", "randomForest", "ggplot2", "doMC","DMwR", "ROSE")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(data.table)
library(parallel)
library(randomForest, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(caret)
library(doMC)
library(DMwR)
library(ROSE)

#Register cores
freeCores = max(1, detectCores(logical = FALSE) - 1)
registerDoMC(freeCores)

#Set path
path = "/home/baowaly/steam/"
#path = "/home/yipeitu/steam_helpful_review_20160701/"
setwd(path)

#######Select Some Parameters#########
g <- "All"
score <- 0.95
vote.num <- 50

#Cross Validation
n.fold <- 10 
n.repeats <- 3

#load dataset by vote
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

#Now we’ll replace blank cells with NA
d.combine.vote[d.combine.vote == ""] <- NA

#all rows without NA value 
dg <- d.combine.vote[complete.cases(d.combine.vote),]
n.rows <- NROW(dg)
rm("d.combine.vote")

##Add a helpful column
dg$helpful <- ifelse(dg$rating >= score, "Yes", "No")
dg$helpful <- as.factor(dg$helpful)

#########################
### Feature Selection ###
#########################
cat("\nRanking Features: Executing..")

#Sample Size for feature ranking
n.samplePercent <- 10
n.sampleSize <- ceiling(n.rows * n.samplePercent/100)

#Create feature file
featureFile <- paste0("Features/FT_", g, "_V", vote.num, "_R", score,"_S", n.samplePercent, ".Rdata")
if(!file.exists(featureFile)){
  file.create(featureFile)
}

name.rm = c("app.id.date.order", "appid", "genre", "genre.12", "vote.agree", "vote.total", "rating", "row.i", "helpful") #added helpful here
name.wv = paste0("wv.", seq(1000))
name.bool = c("recommend", "contain.first.cap", "contain.link", "contain.score", "first.1", "first.2", "first.3", "first.4", "first.5", "first.10", "first.20", "first.30", "first.40", "first.50")
# name.int = names(d.clean)[! names(d.clean) %in% c(name.rm, name.bool)]
name.x = names(dg)[! names(dg) %in% name.rm]
name.log = name.x[grep(".log", name.x)]
name.reciprocal = name.x[grep(".reciprocal", name.x)]
name.root = name.x[grep(".root", name.x)]
name.map = name.x[! name.x %in% c(name.rm, name.wv, name.bool, name.log, name.reciprocal, name.root)]
name.y = c("helpful")

df.name = data.table(name=name.x)

cat("\nGenre:", g, " Vote:", vote.num, " Score:", score, " Sample Size(",n.samplePercent,"%):", n.sampleSize)

#Sampling interation
f.loop <- 10
for(i in seq(f.loop)){
  cat( "\nSample: ", i)
  
  set.seed(i)
  
  #Random Sample from training set
  #sample_index <- sample(n.rows, size=n.sampleSize, replace=FALSE)
  
  sampleData <- ovun.sample(helpful ~ ., data = dg, method="both", p=0.5, N=n.sampleSize)$data
  x <- sampleData[, name.x, ]
  y <- as.factor(sampleData[, name.y, ])
  
  rfectrl <- rfeControl(functions=rfFuncs, 
                        rerank = TRUE,
                        method = "cv", 
                        number = n.fold,
                        #repeats = n.repeats,
                        allowParallel =TRUE
                      )
  #trCtrl <- trainControl(classProbs=TRUE, summaryFunction=twoClassSummary)
  
  # compute RFE, use training data
  ptm = proc.time()
  results <- rfe(x = x, y = y, 
                 sizes=length(name.x), 
                 metric="Accuracy",
                 rfeControl=rfectrl
                 #trControl=trCtrl
                 )
  #print(results)
  time.rfe = (proc.time()-ptm)[3]
  cat( "\tDone:\tTime(min):", time.rfe/60)
  
  ord.n <- predictors(results)
  ord <- unlist(lapply(name.x, function(i) which(i == ord.n)))
  df.name <- cbind(df.name, ord)
  
}

#Save features	
df.name[, name:=NULL]
df.name[, total:=rowSums(.SD)]
df.name[, name:=name.x]
features = df.name[order(rank(total))]$name
i.map = unlist(lapply(name.map, function(i) grep(i, features)[1]))
i.bool = which(features %in% name.bool)
i.wv = which(features %in% name.wv)
features = features[sort(c(i.map, i.bool, i.wv))]

feature.rank <- df.name[which(df.name$name %in% features)]
feature.rank <- feature.rank[order(feature.rank$total)]
save(feature.rank, file=featureFile)

cat("\nFeature Extraction: Done")
cat("\nSaved file in: ", featureFile, "\n")
######End of Feature Selecstion########

#estimate time for each vote
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)