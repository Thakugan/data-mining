# Importing necessary libraries
library(tidyverse)
library(arules)
library(arulesViz)

# Importing code stored in other files
source("clean-data.R")

# Save the data for further use
save(list = ls(), file = "../dat.rda")

# Data collection already done in the first project
load(file = "../dat.rda")

# Data Cleaning
dat_2005 <- clean_dat(dat_2005)
dat_2013 <- clean_dat(dat_2013)

for(i in which(sapply(dat_2005, FUN = function(i) is.numeric(i))))
  dat_2005[[i]] <- discretize(dat_2005[[i]], method = "frequency")

for(i in which(sapply(dat_2013, FUN = function(i) is.numeric(i))))
  dat_2013[[i]] <- discretize(dat_2013[[i]], method = "frequency")

edu_dat_2005 <- filter(dat_2005,
                  education == 1 |
                  education == 2)

edu_dat_2013 <- filter(dat_2013,
                       education == 1 |
                       education == 2)

edu_dat_2005 <- filter(edu_dat_2005,
                       pay == 75000 | pay == 100000 | pay == 125000 | pay == 150000)

edu_dat_2013 <- filter(edu_dat_2013,
                       pay == 75000 | pay == 100000 | pay == 125000 | pay == 150000)

los_dat_2005 <- filter(dat_2005,
                  los == "[ 1,10)" )

los_dat_2013 <- filter(dat_2013,
                       los == "[ 1,10)" )

# The transaction datasets I'll be working with
trans_2005 <- as(dat_2005, "transactions")
summary(trans_2005)

trans_2013 <- as(dat_2013, "transactions")
summary(trans_2013)

trans_edu_2005 <- as(edu_dat_2005, "transactions")
summary(trans_edu_2005)

trans_edu_2013 <- as(edu_dat_2013, "transactions")
summary(trans_edu_2013)

trans_los_2005 <- as(los_dat_2005, "transactions")
summary(trans_los_2005)

trans_los_2013 <- as(los_dat_2013, "transactions")
summary(trans_los_2013)

# Getting frequent itemsets
freq_2005 <- apriori(trans_2005, parameter = list(target = "frequent",
                                                  support = .01,
                                                  minlen = 4))
freq_2013 <- apriori(trans_2013, parameter = list(target = "frequent",
                                                  support = .01,
                                                  minlen = 4))
freq_edu_2005 <- apriori(trans_edu_2005, parameter = list(target = "frequent",
                                                          support = .01,
                                                          minlen = 4))
freq_edu_2013 <- apriori(trans_edu_2013, parameter = list(target = "frequent",
                                                          support = .01,
                                                          minlen = 4))
freq_los_2005 <- apriori(trans_los_2005, parameter = list(target = "frequent",
                                                          support = .01,
                                                          minlen = 4))
freq_los_2013 <- apriori(trans_los_2013, parameter = list(target = "frequent",
                                                          support = .01,
                                                          minlen = 4))

# Getting closed itemsets
closed_edu_2005 <- apriori(trans_edu_2005, parameter = list(target = "closed",
                                                          support = .01,
                                                          minlen = 2))
closed_edu_2013 <- apriori(trans_edu_2013, parameter = list(target = "closed",
                                                          support = .01,
                                                          minlen = 2))

# Getting maximal itemsets
maximal_edu_2005 <- apriori(trans_edu_2005, parameter = list(target = "maximal",
                                                            support = .01,
                                                            minlen = 2))
maximal_edu_2013 <- apriori(trans_edu_2013, parameter = list(target = "maximal",
                                                            support = .01,
                                                            minlen = 2))

# Association rules for low education/high pay
pay <- grep("pay=", itemLabels(trans_edu_2005), value = TRUE)
pay

rules_2005 <- apriori(trans_2005, parameter = list(supp = 0.001, conf = 0.9, target = "rules"))
rules_2005 <- sort(rules_2005, by="lift")
rules_2013 <- apriori(trans_2013, parameter = list(supp = 0.001, conf = 0.9, target = "rules"))
rules_2013 <- sort(rules_2013, by="lift")
rules_edu_2005 <- apriori(trans_edu_2005, parameter = list(supp = 0.001, conf = 0.9, target = "rules"))
rules_edu_2005 <- sort(rules_edu_2005, by="lift")
rules_edu_2013 <- apriori(trans_edu_2013, parameter = list(supp = 0.001, conf = 0.9, target = "rules"))
rules_edu_2013 <- sort(rules_edu_2013, by="lift")
