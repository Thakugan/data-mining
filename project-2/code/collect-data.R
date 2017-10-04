require(data.table)

CollectDat <- function(year) {
  csv.ready <- FALSE
  
  if(!csv.ready) {
    headers <- read.csv("headers.csv", header = TRUE)
    base.url <- paste("https://archive.org/download/opm-federal-employment-data/data/1973-09-to-2014-06/non-dod/status/Status_Non_DoD_", year, sep="")
    url.ext <- if (year < 2009) c("_03.txt", "_06.txt", "_09.txt", "_12.txt") else c("_03.txt", "_06.txt")
    urls <- paste(base.url, url.ext, sep="")
    
    # The data frame where we'll store the year's data
    convert.file <- function(url, test = FALSE) {
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
  } else {
    file.name <- if (year < 2009) paste("bush_", year, ".csv") else paste("obama_", year, ".csv")
    dat <- fread(file = file.name)
  }
}

save_dat <- function(object) {
  file_name <- paste(deparse(substitute(object)), ".csv")
  fwrite(object, file = file_name)
}