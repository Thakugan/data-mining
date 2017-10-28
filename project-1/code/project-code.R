# Importing necessary libraries
library(tidyverse)

# Importing code stored in other files
source("collect-data.R")

# Storing the years I will be examining, split up by President
dat.2000 <- CollectDat(2000)
dat.2001 <- CollectDat(2001)
dat.2002 <- CollectDat(2002)
dat.2003 <- CollectDat(2003)
dat.2004 <- CollectDat(2004)
dat.2005 <- CollectDat(2005)
dat.2006 <- CollectDat(2006)
dat.2007 <- CollectDat(2007)
dat.2008 <- CollectDat(2008)
dat.2009 <- CollectDat(2009)
dat.2010 <- CollectDat(2010)
dat.2011 <- CollectDat(2011)
dat.2012 <- CollectDat(2012)
dat.2013 <- CollectDat(2013)
dat.2014 <- CollectDat(2014)

load(file = "../dat.rda")
