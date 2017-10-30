# Importing necessary libraries
library(tidyverse)
library(caret)
library(doParallel)
library(sampling)
library(rpart)
library(rpart.plot)

# Importing code stored in other files
source("clean-data.R")

# Save the data for further use
save(list = ls(), file = "../dat.rda")

# Data collection already done in the first project
load(file = "../dat.rda")

# Data Cleaning
dat <- clean_dat(dat)
dat_2005 <- clean_dat(dat_2005)
dat_2013 <- clean_dat(dat_2013)

# Find top three agencies
by_agency <- group_by(dat, agency)
agency_stats <- summarize(by_agency, count = n())
top_agencies <- top_n(agency_stats, 3, count)

# Seperate each top agency into its own dataframe
irs_dat <- filter(dat, agency == top_agencies$agency[1])
ssa_dat <- filter(dat, agency == top_agencies$agency[2])
vha_dat <- filter(dat, agency == top_agencies$agency[3])
# Remove agency since they should all be the same
irs_dat <- select(irs_dat, -agency)
ssa_dat <- select(ssa_dat, -agency)
vha_dat <- select(vha_dat, -agency)

# Check for class imbalance
by_class <- group_by(dat, pay)
class_stats <- summarize(by_class,
                         count = n())

ggplot(class_stats, aes(x = pay, y = count)) +
  geom_col()

# Rebalancing all years
id <- strata(dat, stratanames="pay", 
             size=c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000), method="srswr")
dat_balanced <- dat[id$ID_unit, ]
# Split dataset
index <- createDataPartition(dat_balanced$pay, p=0.8, list=FALSE)
train_set <- dat_balanced[ index,]
test_set <- dat_balanced[-index,]

# Rebalancing 2005
id <- strata(dat_2005, stratanames="pay", 
             size=c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000), method="srswr")
dat_balanced_2005 <- dat_2005[id$ID_unit, ]
# Split dataset
index <- createDataPartition(dat_balanced_2005$pay, p=0.8, list=FALSE)
train_set_2005 <- dat_balanced_2005[ index,]
test_set_2005 <- dat_balanced_2005[-index,]

# Rebalancing 2013
id <- strata(dat_2013, stratanames="pay", 
             size=c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000), method="srswr")
dat_balanced_2013 <- dat_2013[id$ID_unit, ]
# Split dataset
index <- createDataPartition(dat_balanced_2013$pay, p=0.8, list=FALSE)
train_set_2013 <- dat_balanced_2013[ index,]
test_set_2013 <- dat_balanced_2013[-index,]

# Split datasets for top three agencies
# Rebalancing IRS data
id <- strata(irs_dat, stratanames="pay", 
             size=c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000), method="srswr")
irs_dat_balanced <- irs_dat[id$ID_unit, ]
# Split dataset
index <- createDataPartition(irs_dat_balanced$pay, p=0.8, list=FALSE)
train_set_irs <- irs_dat_balanced[ index,]
test_set_irs <- irs_dat_balanced[-index,]

# Rebalancing SSA data
id <- strata(ssa_dat, stratanames="pay", 
             size=c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000), method="srswr")
ssa_dat_balanced <- ssa_dat[id$ID_unit, ]
# Split dataset
index <- createDataPartition(ssa_dat_balanced$pay, p=0.8, list=FALSE)
train_set_ssa <- ssa_dat_balanced[ index,]
test_set_ssa <- ssa_dat_balanced[-index,]

# Rebalancing VHA data
id <- strata(vha_dat, stratanames="pay", 
             size=c(10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000, 10000), method="srswr")
vha_dat_balanced <- vha_dat[id$ID_unit, ]
# Split dataset
index <- createDataPartition(vha_dat_balanced$pay, p=0.8, list=FALSE)
train_set_vha <- vha_dat_balanced[ index,]
test_set_vha <- vha_dat_balanced[-index,]

# Rebalancing data for education
id <- strata(mutate(dat, education = as.factor(education)), stratanames="education", 
             size=c(5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000,
                    5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000), method="srswr")
dat_balanced_edu <- mutate(dat, education = as.factor(education))[id$ID_unit, ]
# Split dataset
index <- createDataPartition(dat_balanced_edu$pay, p=0.8, list=FALSE)
train_set_edu <- dat_balanced_edu[ index,]
test_set_edu <- dat_balanced_edu[-index,]

# Rebalancing data for supervisory status
id <- strata(mutate(dat, supervisory_status = as.factor(supervisory_status)), stratanames="supervisory_status", 
             size=c(20000, 20000, 20000, 20000, 20000, 20000), method="srswr")
dat_balanced_ss <- mutate(dat, supervisory_status = as.factor(supervisory_status))[id$ID_unit, ]
# Split dataset
index <- createDataPartition(dat_balanced_ss$pay, p=0.8, list=FALSE)
train_set_ss <- dat_balanced_ss[ index,]
test_set_ss <- dat_balanced_ss[-index,]

# Enable multicore processing
registerDoParallel()

# First model: Predicting pay with CART all years
pay_fit <- train(pay ~ .,  data = train_set, method = "rpart",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneLength = 10)
pay_fit
rpart.plot(pay_fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
pay_fit$results
varImp(pay_fit)

# Testing
pay_pred <- predict(pay_fit, newdata = test_set, na.action = na.pass)
confusionMatrix(data = pay_pred, test_set$pay)

# Second model: Predicting pay with CART 2005
pay_fit_2005 <- train(pay ~ .,  data = train_set_2005, method = "rpart",
                   trControl = trainControl(method = "cv", number = 10),
                   tuneLength = 10)
pay_fit_2005
rpart.plot(pay_fit_2005$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
pay_fit_2005$results
varImp(pay_fit_2005)

# Testing
pay_pred_2005 <- predict(pay_fit_2005, newdata = test_set_2005, na.action = na.pass)
confusionMatrix(data = pay_pred_2005, test_set_2005$pay)

# Third model: Predicting pay with CART 2013
pay_fit_2013 <- train(pay ~ .,  data = train_set_2013, method = "rpart",
                   trControl = trainControl(method = "cv", number = 10),
                   tuneLength = 10)
pay_fit_2013
rpart.plot(pay_fit_2013$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
pay_fit_2013$results
varImp(pay_fit_2013)

# Testing
pay_pred_2013 <- predict(pay_fit_2013, newdata = test_set_2013, na.action = na.pass)
confusionMatrix(data = pay_pred_2013, test_set_2013$pay)

# Fourth model: Predict pay with fewer features
pay_fit_min <- train(pay ~ education+los+category+supervisory_status,  data = train_set, method = "rpart",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneLength = 10)
pay_fit_min
rpart.plot(pay_fit_min$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
pay_fit_min$results
varImp(pay_fit_min)

# Testing
pay_pred_min <- predict(pay_fit_min, newdata = test_set, na.action = na.pass)
confusionMatrix(data = pay_pred_min, test_set$pay)

# Predict pay with CART for top three agencies
# Fifth model: Predict pay for IRS
pay_fit_irs <- train(pay ~ .,  data = train_set_irs, method = "rpart",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneLength = 10)
pay_fit_irs
rpart.plot(pay_fit_irs$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
pay_fit_irs$results
varImp(pay_fit_irs)

# Testing
pay_pred_irs <- predict(pay_fit_irs, newdata = test_set_irs, na.action = na.pass)
confusionMatrix(data = pay_pred_irs, test_set_irs$pay)

# Sixth model: Predict pay for SSA
pay_fit_ssa <- train(pay ~ .,  data = train_set_ssa, method = "rpart",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneLength = 10)
pay_fit_ssa
rpart.plot(pay_fit_ssa$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
pay_fit_ssa$results
varImp(pay_fit_ssa)

# Testing
pay_pred_ssa <- predict(pay_fit_ssa, newdata = test_set_ssa, na.action = na.pass)
confusionMatrix(data = pay_pred_ssa, test_set_ssa$pay)

# Seventh model: Predict pay for VHA
pay_fit_vha <- train(pay ~ .,  data = train_set_vha, method = "rpart",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneLength = 10)
pay_fit_vha
rpart.plot(pay_fit_vha$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
pay_fit_vha$results
varImp(pay_fit_vha)

# Testing
pay_pred_vha <- predict(pay_fit_vha, newdata = test_set_vha, na.action = na.pass)
confusionMatrix(data = pay_pred_vha, test_set_vha$pay)

# Eighth model: Predict education
edu_fit <- train(education ~ .,  data = train_set_edu, method = "rpart",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneLength = 10)
edu_fit
rpart.plot(edu_fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
edu_fit$results
varImp(edu_fit)

# Testing
edu_pred <- predict(edu_fit, newdata = test_set_edu, na.action = na.pass)
confusionMatrix(data = edu_pred, test_set_edu$education)

# Ninth model: Predict supervisory status
ss_fit <- train(supervisory_status ~ .,  data = train_set_ss, method = "rpart",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneLength = 10)
ss_fit
rpart.plot(ss_fit$finalModel, extra = 2, under = TRUE, varlen=0, faclen=0)
ss_fit$results
varImp(ss_fit)

# Testing
ss_pred <- predict(ss_fit, newdata = test_set_ss, na.action = na.pass)
confusionMatrix(data = ss_pred, test_set_ss$supervisory_status)


# Saving data for the report
save(pay_fit, pay_pred, test_set,
     pay_fit_2005, pay_pred_2005, test_set_2005,
     pay_fit_2013, pay_pred_2013, test_set_2013,
     pay_fit_irs, pay_pred_irs, test_set_irs,
     pay_fit_ssa, pay_pred_ssa, test_set_ssa,
     pay_fit_vha, pay_pred_vha, test_set_vha,
     pay_fit_min, pay_pred_min, 
     edu_fit, edu_pred, test_set_edu,
     ss_fit, ss_pred, test_set_ss,
     class_stats,
     file = "../report_dat.rda")
