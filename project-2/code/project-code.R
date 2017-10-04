# Importing code stored in other files
source("collect-data.R")

# Storing the years I will be examining, split up by President
collect_all <- function() {
  bush_2001 <- collect_dat(2001)
  bush_2002 <- collect_dat(2002)
  bush_2003 <- collect_dat(2003)
  bush_2004 <- collect_dat(2004)
  bush_2005 <- collect_dat(2005)
  bush_2006 <- collect_dat(2006)
  bush_2007 <- collect_dat(2007)
  bush_2008 <- collect_dat(2008)
  obama_2009 <- collect_dat(2009)
  obama_2010 <- collect_dat(2010)
  obama_2011 <- collect_dat(2011)
  obama_2012 <- collect_dat(2012)
  obama_2013 <- collect_dat(2013)
  obama_2014 <- collect_dat(2014)
}

save_all <- function() {
  save_dat(bush_2001)
  save_dat(bush_2002)
  save_dat(bush_2003)
  save_dat(bush_2004)
  save_dat(bush_2005)
  save_dat(bush_2006)
  save_dat(bush_2007)
  save_dat(bush_2008)
  save_dat(obama_2009)
  save_dat(obama_2010)
  save_dat(obama_2011)
  save_dat(obama_2012)
  save_dat(obama_2013)
  save_dat(obama_2014)
}
