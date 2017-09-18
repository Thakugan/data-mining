# Importing code stored in other files
source("collect-data.R")

# Storing the years I will be examining, split up by President
bush_years <- c(2001:2008)
obama_years <- c(2009:2014)

bush_data <- lapply(bush_years, collect)