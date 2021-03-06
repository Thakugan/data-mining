require(data.table)

CollectDat <- function(year) {
  headers <- read.csv("../data/headers.csv", header = TRUE)
  base.url <- paste("https://archive.org/download/opm-federal-employment-data/data/1973-09-to-2014-06/non-dod/status/Status_Non_DoD_", year, sep="")
  url.ext <- if (year < 2014) c("_03.txt", "_06.txt", "_09.txt", "_12.txt") else c("_03.txt", "_06.txt")
  urls <- paste(base.url, url.ext, sep="")
  
  # The data frame where we'll store the year's data
  ConvertFile <- function(url, test = FALSE) {
    if(test == FALSE) {
      dat.raw <- readLines(url)
    } else {
      dat.raw <- readLines(url, n = 5)
    }
    dat.curr <- t(sapply(dat.raw, FUN = function(x) trimws(substring(x, headers[,2], headers[,3]))))
    dimnames(dat.curr) <- NULL
    dat.curr <- as.data.frame(dat.curr)
    colnames(dat.curr) <- headers[,1]
    return(dat.curr)
  }
  
  # Convert all the text files to dataframe
  dat.list <- lapply(urls, ConvertFile)
  dat <- rbindlist(dat.list)
  
  return(dat)
}