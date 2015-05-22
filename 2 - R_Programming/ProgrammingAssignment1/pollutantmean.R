## pollutantmean("specdata", "sulfate", 1:10)

## This code actually shows two ways we can do this
pollutantmean <- function(directory, pollutant, id=1:332) {
     sample.dir<-dir(directory, full.names=TRUE)
     numReq<-length(id)
     ## create empty list vector to hold the csv filenames
     sample.req <- vector(mode = "list", length=numReq)
     if(numReq > 1) {
          j=1
          for(i in id[1]:id[numReq]) {
              sample.req[j] <- sample.dir[i]
              j<-j+1
               
          }
      }
     else {
          sample.req[1] <- sample.dir[id[1]]
     }
#      str(sample.req)
     
#    This is the original brute force method     
#      ## Creating an empty dataframe to hold our data
#      data <- data.frame()
#
#      ## fill the vector of the files we'll be reading
#      if(numReq > 1) {
#           for(i in 1:numReq) {
#                print(sample.req[[i]])
#                data <- rbind(data, read.csv(sample.req[[i]]))
#            }
#      }
#      else {
#            data <- read.csv(sample.req[[1]])
#      }
#      # str(data)
#      mean(data[, pollutant], na.rm=TRUE)
de
#    This is the second way, using the LAPPLY method, eliminates a loop
     data <- lapply(sample.req, read.csv)
     ## concatentate all the dataframes into a single dataframe
     output <- do.call(rbind, data)
#      str(output)
     mean(output[, pollutant], na.rm=TRUE)
}

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