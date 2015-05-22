complete <- function(directory, id=1:332) {
     sample.dir<-dir(directory, full.names=TRUE)
     numReq<-length(id)
     ## create empty list vector to hold data and final empty dataframe to fill
     tmp2 <- vector(mode = "list", length=2)
     nobs <- data.frame()
     for(i in 1:numReq) {
          tmp <- data.frame()
          tmp <- rbind(tmp, read.csv(sample.dir[id[i]]))
          tmp2 <- c(id[i], sum(complete.cases(tmp)))
          nobs <- rbind(nobs, tmp2)
          colnames(nobs) <- c("id", "nobs")
     }
     nobs
}