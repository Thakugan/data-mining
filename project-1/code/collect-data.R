require(data.table)

collect_dat <- function(year) {
  csv_ready <- FALSE
  
  if(csv_ready) {
    headers <- read.csv("headers.csv", header = TRUE)
    base_url <- paste("https://archive.org/download/opm-federal-employment-data/data/1973-09-to-2014-06/non-dod/status/Status_Non_DoD_", year.toString, sep="")
    url_ext <- if (year < 2009) c("_03.txt", "_06.txt", "_09.txt", "_12.txt") else c("_03.txt", "_06.txt")
    urls <- paste(base_url, url_ext, sep="")
    
    # The data frame where we'll store the year's data
    convert_file <- function(url, test = TRUE) {
      if(test == FALSE) {
        dat_raw <- readLines(url)
      } else {
        dat_raw <- readLines(url, n = 5)
      }
      dat_curr <- t(sapply(dat_raw, FUN = function(x) trimws(substring(x, headers[,2], headers[,3]))))
      dimnames(dat_curr) <- NULL
      dat_curr <- as.data.frame(dat_curr)
      colnames(dat_curr) <- headers[,1]
      return(dat_curr)
    }
    
    # Convert all the text files to dataframe
    dat_list <- lapply(urls, convert_file)
    dat <- rbindlist(dat_list)
    
    return(dat)
  } else {
    
  }
}

save_dat <- function() {
  
}