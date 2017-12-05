# Importing necessary libraries
library(tidyverse)
library(GGally)
library(cluster)

# Importing code stored in other files
source("clean-data.R")

# Save the data for further use
save(list = ls(), file = "../dat.rda")

# Data collection already done in the first project
load(file = "../dat.rda")

# Selecting data for FAA
faa_dat_2005 <- filter(dat_2005,
                       Agency == "TD03")
faa_dat_2013 <- filter(dat_2013,
                       Agency == "TD03")

full_dat_2005 <- clean_dat(dat_2005)
full_dat_2013 <- clean_dat(dat_2013)

# Data Cleaning
dat_2005 <- clean_dat(dat_2005)
dat_2005 <- scale(dat_2005)
faa_dat_2005 <- clean_dat(faa_dat_2005)
faa_dat_2005 <- scale(faa_dat_2005)
dat_2013 <- clean_dat(dat_2013)
dat_2013 <- scale(dat_2013)
faa_dat_2013 <- clean_dat(faa_dat_2013)
faa_dat_2013 <- scale(faa_dat_2013)

# K-Means Clustering
set.seed(1000)
km_2005 <- kmeans(dat_2005, centers = 4)
km_2005_min <- kmeans(dat_2005[1:5000,], centers = 4)
km_2013 <- kmeans(dat_2013, centers = 4)
km_faa_2005 <- kmeans(faa_dat_2005, centers = 4)
km_faa_2013 <- kmeans(faa_dat_2013, centers = 4)

# K-Means Visualization
ggparcoord(cbind(data.frame(km_2005$centers), data.frame(id = as.character(1:4))), columns = 1:ncol(km_2005$centers), groupColumn = 'id')
clusplot(dat_2005[1:5000,], km_2005_min$cluster)

# Hierarchical Clustering

agency_data_2005 <- aggregate(cbind(age, education, los, pay, supervisory_status) ~ agency, data = full_dat_2005, FUN = mean)

agency_size_2005 <- as.data.frame(table(full_dat_2005$agency))
colnames(agency_size_2005) <- c("agency", "size")

agency_data_2005 <- merge(agency_data_2005, agency_size_2005)
rownames(agency_data_2005) <- agency_data_2005$agency
agency_data_2005 <- agency_data_2005[,-1]
agency_data_2005 <- subset(agency_data_2005, subset = size > 20000)
head(agency_data_2005)

agency_data_2013 <- aggregate(cbind(age, education, los, pay, supervisory_status) ~ agency, data = full_dat_2013, FUN = mean)
head(agency_data_2013)

agency_size_2013 <- as.data.frame(table(full_dat_2013$agency))
colnames(agency_size_2013) <- c("agency", "size")

agency_data_2013 <- merge(agency_data_2013, agency_size_2013)
rownames(agency_data_2013) <- agency_data_2013$agency
agency_data_2013 <- agency_data_2013[,-1]
agency_data_2013 <- subset(agency_data_2013, subset = size > 20000)
head(agency_data_2013)

# Hierarchical Visualization

d_2005 <- dist(scale(agency_data_2005[,colnames(agency_data_2005) != "size"]))
cl_2005 <- hclust(d_2005, method = "complete")
plot(cl_2005)

d_2013 <- dist(scale(agency_data_2013[,colnames(agency_data_2013) != "size"]))
cl_2013 <- hclust(d_2013, method = "complete")
plot(cl_2013)


pc_2005 <- prcomp(scale(agency_data_2005[,colnames(agency_data_2005) != "size"]))
biplot(pc_2005, col = c("grey", "red"))

pc_2013 <- prcomp(scale(agency_data_2013[,colnames(agency_data_2013) != "size"]))
biplot(pc_2013, col = c("grey", "red"))


# Internal Validation
d_norm_2005 <- dist(dat_2005)
d_norm_2013 <- dist(dat_2013)
d_faa_2005 <- dist(faa_dat_2005)
d_faa_2013 <- dist(faa_dat_2013)

dissplot(d_norm_2005, labels=km_2005$cluster, options=list(main="k-means with k=4"))

ks <- 2:10
WSS <- sapply(ks, FUN=function(k) {
  kmeans(dat_2005, centers=4, nstart=5)$tot.withinss
})
plot(ks, WSS, type="l")
abline(v=4, col="red", lty=2)