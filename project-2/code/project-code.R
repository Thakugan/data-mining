# Importing necessary libraries
library(tidyverse)
library(caret)
library(doParallel)
library(sampling)
library(rpart)
library(rpart.plot)
library(randomForest)

# Importing code stored in other files
source("clean-data.R")

# Save the data for further use
save(list = ls(), file = "../dat.rda")

# Data collection already done in the first project
load(file = "../dat.rda")

# Data Cleaning
dat <- clean_dat(dat)

# Check for class imbalance
by_class <- group_by(dat, pay)
class_stats <- summarize(by_class,
                         count = n())

ggplot(class_stats, aes(x = pay, y = count)) +
  geom_col()

# Rebalancing
id <- strata(dat, stratanames="pay", 
             size=c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000), method="srswr")
dat_balanced <- dat[id$ID_unit, ]
table(dat_balanced$pay)

# Split dataset
index <- createDataPartition(dat_balanced$pay, p=0.8, list=FALSE)
train_set <- dat_balanced[ index,]
test_set <- dat_balanced[-index,]

# Create fixed sampling scheme
train <- createFolds(train_set$pay,k=10)

# Enable multicore processing
registerDoParallel()

# First model: Predicting pay with rpart
rpart_fit <- train(pay ~ .,  data = train_set, method = "rpart",
                tuneLength = 10,
                trControl = trainControl(
                  method = "cv", indexOut = train))
rpart_fit
rpart.plot(rpart_fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
table(rpart_fit$maximize)
