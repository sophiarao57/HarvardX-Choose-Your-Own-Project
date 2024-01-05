## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)


## ----message=FALSE, warning=FALSE---------------------------------------------------
library(tidyverse)
library(caTools)
library(caret)
library(e1071)
library(glmnet)
library(randomForest)
library(xgboost)
library(data.table)
library(lubridate)
library(ggplot2)
library(corrplot)
library(kableExtra)
library(Metrics)


## ----message=FALSE, warning=FALSE---------------------------------------------------
#Read in the training data set
train <- read.csv("train.csv")

#Replace NA with 0- numerics are better for modeling
train <- train %>% mutate_all(~replace(., is.na(.), 0))

#Read in the testing data set
test <- read.csv("test.csv")

#Replace NA with 0- numerics are better for modeling
test <- test %>% mutate_all(~replace(., is.na(.), 0))

#Converting characters to numerics
train$paved[train$Street == "Pave"] <- 1
train$paved[train$Street != "Pave"] <- 0

#Converting characters to numerics
train$regshape[train$LotShape == "Reg"] <- 1
train$regshape[train$LotShape != "Reg"] <- 0


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------
summary(train)


## ----echo=TRUE, message=FALSE, warning=FALSE----------------------------------------
summary(test)


## ----message=FALSE, warning=FALSE, paged.print=FALSE--------------------------------
ggplot(data=train, aes(x=SalePrice)) +
  geom_histogram(fill="red4", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000)) + 
  labs(title="House Sale Prices")


## ----message=FALSE, warning=FALSE---------------------------------------------------
num_vars <- which(sapply(train, is.numeric)) #which colummns from train are numeric
num_vars_colnames <- data.table(names(num_vars)) #column names of the numeric variables

train_num_vars <- train[, num_vars] #new data frame containing only numeric variables from train
cor_num_vars <- cor(train_num_vars, use="pairwise.complete.obs") #correlations of all numeric variables, this is the correlation matrix

#sorting the correlations in decreasing order (largest to smallest)
cor_sorted <- as.matrix(sort(cor_num_vars[,'SalePrice'], decreasing = TRUE))
#select variables with high correlations. corr>.5
high_cor <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
high_cor_colnames <- data.table(high_cor)
  
cor_num_vars <- cor_num_vars[high_cor, high_cor] #this is a matrix that contains only the correlations greater than .5

corrplot(cor_num_vars, type = "upper")


## ----message=FALSE, warning=FALSE---------------------------------------------------
ggplot(data=train[!is.na(train$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
        geom_boxplot(col='blue') + labs(x='OverallQual') +
        scale_y_continuous(breaks= seq(0, 800000, by=100000))+
        labs(title="OverallQual correlation to SalePrice")


## -----------------------------------------------------------------------------------
set.seed(123)

outcome <- train$SalePrice

partition <- createDataPartition(y=outcome,
                                 p=.5,
                                 list=F)
training <- train[partition,]
testing <- train[-partition,]



## ----message=FALSE, warning=FALSE---------------------------------------------------
# Fitting Simple Regression Model to the train set.
set.seed(123)

OQ_effect_model <- lm(SalePrice ~ OverallQual, data = training)

summary(OQ_effect_model)

prediction <- predict(OQ_effect_model, testing, type="response")

prediction_log <- log(prediction)

testing_log <- log(testing$SalePrice)

model_rmse <- RMSE(testing_log, prediction_log)

RMSE_table <- data_frame(Method = "Regression model using OverallQual effect", 
                               RMSE = model_rmse)

RMSE_table


## ----message=FALSE, warning=FALSE---------------------------------------------------

set.seed(57)

top10_effect_model <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars +
                         GarageArea + TotalBsmtSF + X1stFlrSF + FullBath +
                         TotRmsAbvGrd + YearBuilt + YearRemodAdd, data = training)

summary(top10_effect_model)

prediction <- predict(top10_effect_model, testing, type="response")

prediction_log <- log(prediction)

testing_log <- log(testing$SalePrice)

model_rmse <- rmse(testing_log, prediction_log)

RMSE_table <- rbind(RMSE_table,
                    data_frame(Method = "Regression model using top-10-effect", 
                               RMSE = model_rmse))

RMSE_table 


## -----------------------------------------------------------------------------------
set.seed(57)

top10_effect_model <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + YearBuilt + YearRemodAdd, data = training)

summary(top10_effect_model)

prediction <- predict(top10_effect_model, testing, type="response")

prediction_log <- log(prediction)

testing_log <- log(testing$SalePrice)

model_rmse <- rmse(testing_log, prediction_log)

RMSE_table <- rbind(RMSE_table,
                    data_frame(Method = "Regression model using backward elimination", 
                               RMSE = model_rmse))

RMSE_table


## -----------------------------------------------------------------------------------
set.seed(57)
rf_model <- randomForest(SalePrice ~ ., data = training)
prediction <- predict(rf_model, testing)
prediction_log <- log(prediction)
testing_log <- log(testing$SalePrice)
model_rmse <- RMSE(testing_log, prediction_log)
RMSE_table <- rbind(RMSE_table,
 data_frame(Method = "Random Forest regression model",
 RMSE = model_rmse))
RMSE_table


## -----------------------------------------------------------------------------------
RMSE_table

