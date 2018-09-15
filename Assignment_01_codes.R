library(readr)
library(readxl)
library(tidyverse)
library(mice)
library(DataExplorer)
library(visdat)
library(car)
library(caret)
library(MASS)
library(lubridate)
library(DMwR)
library(glmnet)
library(corrplot)
library(dummies)
library(Hmisc)
library(fastDummies)
library(xgboost)
library(forecast)

train <- read_csv("C:/Users/anjal/OneDrive/MMA/assignments/867 - Predictive Analytics/Future Sales Data/sales_train.csv.gz", col_types = cols(date = col_date(format = "%d.%m.%Y")))
test <- read_csv("C:/Users/anjal/OneDrive/MMA/assignments/867 - Predictive Analytics/Future Sales Data/test.csv.gz")
item_categories <- read_csv("C:/Users/anjal/OneDrive/MMA/assignments/867 - Predictive Analytics/Future Sales Data/item_categories.csv")
items <- read_csv("C:/Users/anjal/OneDrive/MMA/assignments/867 - Predictive Analytics/Future Sales Data/items.csv")
shops <- read_csv("C:/Users/anjal/OneDrive/MMA/assignments/867 - Predictive Analytics/Future Sales Data/shops.csv")

train <- train %>% left_join(items, by =c("item_id"))
test <- test %>% left_join(items, by = c("item_id"))
test <- test %>% left_join(item_categories, by = c("item_category_id"))
train <- merge(train, item_categories, all = TRUE)

train%>% filter(item_cnt_day<0) %>% n_distinct()
train %>% filter(item_price<0) %>% n_distinct()


train$item_cnt_day <- abs(train$item_cnt_day)
train$item_price <- abs(train$item_price)

train$category_name <- as.factor(train$category_name)
test$category_name <- as.factor(test$category_name)

train$category_name <- impute(train$category_name,mode)
test$category_name <- impute(test$category_name,mode)

summary(train)
summary(test)

train$date <- NULL
train$date_block_num <- NULL
train$item_price <- NULL

str(train)

train$item_name <- nchar(train$item_name)
test$item_name <- nchar(test$item_name)

#category name
percent_itm_cnt_category <- train %>%
  group_by(train$category_name) %>% summarise(contribution = sum(item_cnt_day)/ sum(train$item_cnt_day)*100)

names(percent_itm_cnt_category)[1]<-"category_name" 
train <- train %>% left_join(percent_itm_cnt_category, by =c("category_name"))
test <- test %>% left_join(percent_itm_cnt_category, by = c("category_name"))
summary(test)

#item_id
percent_itm_cnt_by_id<- train %>%
  group_by(train$item_id) %>% summarise(contribution = sum(item_cnt_day)/ sum(train$item_cnt_day)*100)

names(percent_itm_cnt_by_id)[1]<-"item_id" 
train <- train %>% left_join(percent_itm_cnt_by_id, by =c("item_id"))
test <- test %>% left_join(percent_itm_cnt_by_id, by = c("item_id"))

#shop_id
percent_itm_cnt_shop_id <- train %>%
  group_by(train$shop_id) %>% summarise(contribution = sum(item_cnt_day)/ sum(train$item_cnt_day)*100)

names(percent_itm_cnt_shop_id)[1]<-"shop_id" 
train <- train %>% left_join(percent_itm_cnt_shop_id, by =c("shop_id"))
test <- test %>% left_join(percent_itm_cnt_shop_id, by = c("shop_id"))

#item_category_id
percent_itm_cnt_itm_cat_id <- train %>%
  group_by(train$item_category_id) %>% summarise(contribution = sum(item_cnt_day)/ sum(train$item_cnt_day)*100)

names(percent_itm_cnt_itm_cat_id)[1]<-"item_category_id" 
train <- train %>% left_join(percent_itm_cnt_itm_cat_id, by =c("item_category_id"))
test <- test %>% left_join(percent_itm_cnt_itm_cat_id, by = c("item_category_id"))

names(train)[7] <- "percent_contri_cat_name"
names(test)[7] <- "percent_contri_cat_name"

names(train)[8] <- "percent_contri_item_id"
names(test)[8] <- "percent_contri_item_id"

names(train)[9] <- "percent_contri_shop_id"
names(test)[9] <- "percent_contri_shop_id"

names(train)[10] <- "percent_contri_item_cat_id"
names(test)[10] <- "percent_contri_item_cat_id"

train <- dummy_cols(train, select_columns = 'category_name')
test <- dummy_cols(test, select_columns = 'category_name')

names(train)
names(test)

train$category_name <- NULL
test$category_name <- NULL
test[1]<- NULL

names(train)
names(test)

train <- train[c(2,3,5,1,6,7,8,9,10,11,13,12,4)]
summary(test)

names(train)
names(test)


rm(percent_itm_cnt_by_id,percent_itm_cnt_category, percent_itm_cnt_day, percent_itm_cnt_itm_cat_id, percent_itm_cnt_month, percent_itm_cnt_shop_id, percent_itm_cnt_year, shops, items, item_categories)

### train is not capped
### train.transformed is capped

# capping outliers

qnt <- quantile(train$item_cnt_day, probs=c(.05, .99), na.rm = T)
caps <- quantile(train$item_cnt_day, probs=c(.05, .99), na.rm = T)
H <- 1.5 * IQR(train$item_cnt_day, na.rm = T)

train$item_cnt_day[train$item_cnt_day < (qnt[1] - H)] <- caps[1]
train$item_cnt_day[train$item_cnt_day > (qnt[2] + H)] <- caps[2]

### preproc 

preproc <- preProcess(train[, -13],method = c("center", "scale"))
train.transformed <- predict(preproc, newdata = train[,-13])
train.transformed <- cbind(train.transformed, train[,13])
names(train.transformed)[13]<-"item_cnt_day"


#### Preprocessing the test file based on the preproc values

test <- predict(preproc, test)

summary(train)
train.transformed$percent_contri_item_id <- NULL
train$percent_contri_item_id <- NULL
validation$percent_contri_item_id <- NULL

summary(test)
test$percent_contri_item_id <- NULL
test[,"item_cnt_day"] <- NA

### train-validation split
set.seed(3)
trainIndex <- train.transformed$item_cnt_day %>% createDataPartition( p = .70, list = FALSE)

train <- train.transformed[trainIndex,]
validation  <- train.transformed[-trainIndex,]

####### Visualisation #####

hist(train$item_cnt_day)
hist(train$shop_id)
hist(train$item_id)
hist(train$item_name)
hist(train$item_category_id)
hist(train$percent_contri_cat_name)
hist(train$percent_contri_shop_id)
hist(train$percent_contri_item_cat_id)
hist(train$percent_contri_item_cat_id)
hist(train$category_name_console)
hist(train$category_name_accesories)
hist(train$category_name_cd)
hist(train$category_name_handheld)

###### TRansforming variables #####

model <- lm(item_cnt_day~ shop_id+item_id+(item_name)+item_category_id+percent_contri_cat_name+percent_contri_shop_id+percent_contri_item_cat_id+category_name_console+category_name_cd+category_name_accesories+category_name_handheld, train)

stepAIC(model)

qqp(model)
plot(density(resid(model)))

# Make predictions on the test data
predictions <- model %>% predict(validation[,-12])

data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

predictions <- model %>% predict(test) %>% as.vector()


# write .csv file
write.csv(predictions, file = "preds_transformation.csv")

##### transformation 1 ###

model <- lm(formula = log(item_cnt_day+1) ~ shop_id + item_id + item_name + item_category_id + percent_contri_cat_name + percent_contri_shop_id + percent_contri_item_cat_id + category_name_cd, data = train)

qqp(model)
plot(density(resid(model)))

# Make predictions on the test data
predictions <- model %>% predict(validation[,-12])

data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

predictions <- model %>% predict(test) %>% as.vector()


# write .csv file
write.csv(predictions, file = "preds_transformation.csv")


##### transform 2

model <- lm(formula = sign(item_cnt_day)*log10((abs(item_cnt_day))+1) ~ log(shop_id) + log(item_id) + log(item_name + item_category_id + percent_contri_cat_name + percent_contri_shop_id + percent_contri_item_cat_id + category_name_cd, data = train))

qqp(model)

# Make predictions on the test data
predictions <- model %>% predict(validation[,-12])

data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

predictions <- model %>% predict(test) %>% as.vector()


# write .csv file
write.csv(predictions, file = "preds_transformation.csv")

###### transform 3 ####

model <- lm(formula =(1/(item_cnt_day)^1/2) ~ shop_id + item_id + item_name + item_category_id + percent_contri_cat_name + percent_contri_shop_id + percent_contri_item_cat_id + category_name_cd, data = train)

qqp(model)
plot(density(resid(model)))

#predicting on test.File dataset

# Make predictions on the test data
predictions <- model %>% predict(validation[,-12])
predictions <- (predictions)^1/2

data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

predictions <- model %>% predict(test) %>% as.vector()
predictions <- (predictions)^1/2

# write .csv file
write.csv(predictions, file = "preds_transformation.csv")


#predicting on test.File dataset

# Make predictions on the test data
predictions <- model %>% predict(validation[,-12])

data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

predictions <- model %>% predict(test) %>% as.vector()


# write .csv file
write.csv(predictions, file = "preds_transformation.csv")



#### regression ####

# Build the model using the training set
set.seed(123)
model <- train(
  item_cnt_day ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
model$bestTune

# Coefficient of the final model. 
coef(model$finalModel, model$bestTune$lambda)


predictions <- model %>% predict(validation[,-12]) %>% as.vector()
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

# Make predictions on the test data
predict <- model %>% predict(test) %>% as.vector()

# write .csv file
write.csv(predict, file = "predic_take3_1.csv")


#Setup a grid range of lambda values

lambda <- 10^seq(-3, 3, length = 100)

#Compute ridge regression

# Build the model
set.seed(123)
ridge <- train(
  item_cnt_day ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda)
)
# Model coefficients
coef(ridge$finalModel, ridge$bestTune$lambda)

#plotting ridge model
plot(ridge)

# Make predictions
predictions <- ridge %>% predict(validation[,-12])
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

# Make predictions on the test data
prediction <- ridge %>% predict(test) %>% as.vector()

# write .csv file
write.csv(prediction, file = "ridge_prediction.csv")


#Compute lasso regression:
  
# Build the model
set.seed(123)
lasso <- train(
  item_cnt_day ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda)
)
# Model coefficients
coef(lasso$finalModel, lasso$bestTune$lambda)

#plotting Lasso model
plot(lasso)

# Make predictions
predictions <- lasso %>% predict(validation[,-12])
# Model prediction performance
data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

plot(lasso)

# Make predictions on the test data
pred <- lasso %>% predict(test) %>% as.vector()

# write .csv file
write.csv(pred, file = "lasso_prediction.csv")

#Elastic net regression

set.seed(123)
elastic <- train(
  item_cnt_day ~., data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)

coef(elastic$finalModel, elastic$bestTune$lambda)

predictions <- elastic %>% predict(validation[,-12])

data.frame(
  RMSE = RMSE(predictions, validation$item_cnt_day),
  Rsquare = R2(predictions, validation$item_cnt_day)
)

plot(elastic)

predictions <- elastic %>% predict(test) %>% as.vector()

write.csv(predictions, file = "elastic_prediction.csv")


#Comparing models performance

models <- list(ridge = ridge, lasso = lasso, elastic = elastic)
resamples(models) %>% summary(metric = "RMSE")


#################################

set.seed(123)
## Model parameters trained using xgb.cv function
xgbFit <- xgboost(data = as.matrix(train[, -13]), 
                  nfold = 10,booster = 'dart',
                  label = as.matrix(train$item_cnt_day),
                  nrounds = 1000,gamma = 0.0468, max_depth = 6,
                  min_child_weight = 1.7817,
                  eta = 0.01, subsample = 0.5213, 
                  colsample_bytree = 0.4603)



print(xgbFit)

## Predictions
preds <- predict(xgbFit, newdata = as.matrix(validation[, -13]))
RMSE(validation$item_cnt_day, preds)


preds.test <- predict(xgbFit, newdata = as.matrix(test[, -13]))
write.csv(preds.test, "predxgb_take3.csv")

predxgb <- read_csv("predxgb_take3.csv")
predxgb$x <- predxgb$x*30
write.csv(predxgb, "predxgb_take3.csv")

######## Decision Tree ############

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
dtCart = rpart(item_cnt_day ~.,data=train,method="anova")    
summary(dtCart)

predCartEval=predict(dtCart, newdata=validation, type="vector")


data.frame(
  RMSE = RMSE(predCartEval, validation$item_cnt_day),
  Rsquare = R2(predCartEval, validation$item_cnt_day)
)

bestcp <- dtCart$cptable[which.min(dtCart$cptable[,"xerror"]),"CP"]

dtCart = rpart(item_cnt_day ~.,data=train,method="anova", cp = 0.011)   

predCartEval=predict(dtCart, newdata=validation, type="vector")


data.frame(
  RMSE = RMSE(predCartEval, validation$item_cnt_day),
  Rsquare = R2(predCartEval, validation$item_cnt_day)
)

plotcp(dtCart)

predCartTest=predict(dtCart, newdata=test, type="vector")

write.csv(predCartTest, "predictCart.csv")

predictCart$item_count_day <- predictCart$item_count_day*30
write.csv(predictCart, "predCart.csv")

###### Neural Net ####

train$percent_contri_item_id <- NULL
test$percent_contri_item_id <- NULL

summary(test)
summary(train)

library(neuralnet)

formula = as.formula(paste("item_cnt_day ~", 
                           paste(setdiff(names(train),
                                         "item_cnt_day"), 
                                 collapse = " + ")))

nn = neuralnet(formula, data=train, act.fct = "tanh")

## validation Error
validation_Output <- compute(validation, validation[-13])
validation_SSE <- sum((validation_Output - validation[, 13])^2)/2

## Test Prediction
test_Output <- compute(test, test[-13])
write.csv()


rm(list = ls(all = TRUE))







# #########Time series#####

train <- read_csv("C:/Users/anjal/OneDrive/MMA/assignments/867 - Predictive Analytics/Future Sales Data/sales_train.csv.gz", col_types = cols(date = col_date(format = "%d.%m.%Y")))
test <- read_csv("C:/Users/anjal/OneDrive/MMA/assignments/867 - Predictive Analytics/Future Sales Data/test.csv.gz")

timeseries <- ts(train, frequency=12)
timeseries
par(mfrow=c(1,1))
plot(timeseries)

par(mfrow = c(1, 2))
acf(timeseries)
pacf(timeseries)
