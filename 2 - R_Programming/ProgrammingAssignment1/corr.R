corr <- function(directory, threshold=0) {
     source("complete.R")
     sample.dir<-dir(directory, full.names=TRUE)
     numReq<-length(sample.dir)
     # numReq <- 3

     allData <- complete(directory,1:numReq)
     output <- vector(mode="numeric")
     j=1
     for(i in 1:numReq) {
          if(allData[i,"nobs"] > threshold) {
               #nodeData <- lapply(sample.dir[i], read.csv)
               #tmp <- do.call(rbind, nodeData)
               tmp <- data.frame()
               tmp <- rbind(tmp, read.csv(sample.dir[i]))
               
               cleanNode <- tmp[complete.cases(tmp),]
               x<-cleanNode["nitrate"]
               y<-cleanNode["sulfate"]
               output[j] <- cor(x,y)
               # cat(sprintf("J: %s Output[j]: %s\n", j, output[j]))
               j <- j+1
          }
     }
     output
}