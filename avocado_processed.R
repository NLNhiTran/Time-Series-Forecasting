setwd("C:/Users/nhiii/OneDrive/Desktop/Data Science/Methodology/Group Assignment")
avocado_raw <- read.csv("avocado.csv", header=TRUE, stringsAsFactors = TRUE)
View(avocado_raw)

library(dplyr)
library(ggplot2)
library(lsr)
library(lubridate)
library(usmap)
library(scales)
library(tidyr)
library(ggpubr)
library(zoo)
library(forecast)
library(rio)
library(tseries)
library(tidyverse)

#######################################################
#Inspect the data set
#######################################################
str(avocado_raw)
summary(avocado_raw)
psych::describe(avocado_raw)
head(avocado_raw, 10)
#######################################################


#######################################################
#Pre-processing 
#######################################################
avocado <- avocado_raw %>%
  mutate(date = ymd(date), week = strftime(date, "%W"), 
         month = as.numeric(strftime(date, "%m")), 
         days_since_first_date = as.numeric(difftime((date), min(date), units = "days"))) 
View(avocado)
str(avocado)
#######################################################

#######################################################
#Set a seed to ensure that the results are always the same 
# and shuffle the rows of your data using first
#######################################################
set.seed(123) # set the random number seed for reproducibility 
avocado_seed <- avocado[sample(nrow(avocado)),]
#######################################################
#Divide your data set into training, validation and test data
#######################################################
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(
  seq(nrow(avocado_seed)), 
  nrow(avocado_seed)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(avocado_seed, g)

train <- res[["train"]]
test <- res[["test"]]
validate <- res[["validate"]]

str(train)
str(test)
str(validate)
#######################################################

#######################################################
#Build three simple linear regression models using the lm() function, (m0, m1, and m2), 
#each model should have each of the predictors that you considered relevant. 
#######################################################
annual_sales <- avocado %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales
####m0####
#subset total annual sales on different train, validate and test set
annual_sales_train <- train %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales_validate <- validate %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales_test <- test %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales_test

#build m0
obs_full_m0 <- as.numeric(unlist(annual_sales[1]))
obs_train_m0 <- as.numeric(unlist(annual_sales_train[1]))
m0_train <- lm(total_sales ~ duration, annual_sales_train)
summary(m0_train)
pred_train_m0 <- predict(m0_train)
m0_test <- lm(total_sales ~ duration, annual_sales_test)
pred_test_m0 <- predict(m0_test)
m0_validate <- lm(total_sales ~ duration, annual_sales_validate)
pred_validate_m0 <-predict(m0_validate)

x <- c(2015,2016,2017,2018,2019,2020)
y <-  as.data.frame(obs_full_m0)
y1 <- as.data.frame(obs_train_m0)
y2 <- as.data.frame(pred_train_m0)
y3 <- as.data.frame(pred_validate_m0)
y4 <- as.data.frame(pred_test_m0)
tm0 <- cbind(x,y, y1, y2, y3, y4)
tm0

tm0_graph <- tm0 %>% ggplot() + geom_line(aes(x=x,y=obs_full_m0,color='Obs. Full dataset')) + 
  geom_line(aes(x=x,y=obs_train_m0,color='Obs. Train set')) + 
  geom_line(aes(x=x,y=pred_train_m0,color='Pred. Train set')) + 
  geom_line(aes(x=x,y=pred_validate_m0,color='Pred. Validate set')) +
  geom_line(aes(x=x,y=pred_test_m0,color='Pred. Test set')) +
  ylab('Sales')+xlab('Time') +
  ggtitle("m0: Total annual sales trend anlaysis and forecast for Hass avocado in the US") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(legend.position="right", legend.box = "vertical") +
  scale_color_discrete(name = "Data")
tm0_graph

price_vs_sales <- avocado %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  ungroup()
price_vs_sales

####m1####
#subset total annual sales on different train, validate and test set
price_vs_sales_train <- train %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_sales_validate <- validate %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_sales_test <- test %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price))%>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()

#build m1
obs_full_m1 <- as.numeric(unlist(price_vs_sales[1]))
obs_train_m1 <- as.numeric(unlist(price_vs_sales_train[1]))
m1_train <- lm(total_sales ~ mean_price, price_vs_sales_train)
summary(m1_train)
pred_train_m1 <- predict(m1_train)
m1_test <- lm(total_sales ~ mean_price, price_vs_sales_test)
pred_test_m1 <- predict(m1_test)
m1_validate <- lm(total_sales ~ mean_price, price_vs_sales_validate)
pred_validate_m1 <-predict(m1_validate)

x <- price_vs_sales$mean_price
y <-  unlist(obs_full_m1)
y1 <- unlist(obs_train_m1)
y2 <- unlist(pred_train_m1)
y3 <- unlist(pred_validate_m1)
y4 <- unlist(pred_test_m1)
tm1 <- cbind(x,y, y1, y2, y3, y4)
tm1

colnames 

tm1_graph <- tm1 %>% ggplot() + geom_line(aes(x=x,y=obs_full_m1,color='Obs. Full dataset')) + 
  geom_line(aes(x=x,y=obs_train_m1,color='Obs. Train set')) + 
  geom_line(aes(x=x,y=pred_train_m1,color='Pred. Train set')) + 
  geom_line(aes(x=x,y=pred_validate_m1,color='Pred. Validate set')) +
  geom_line(aes(x=x,y=pred_test_m1,color='Pred. Test set')) +
  ylab('Sales')+xlab('Average Weekly Price') +
  ggtitle("m1: Weekly sales trend anlaysis in accordance to price ") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(legend.position="right", legend.box = "vertical") +
  scale_color_discrete(name = "Data")
tm1_graph

weekly_sales <- avocado %>% 
  group_by(date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(broom::glance(lm(total_sales ~ date))) %>%
  select(total_sales:date,adj.r.squared,p.value) %>%
  ungroup()

#subset total weekly sales on different train, validate and test set
weekly_sales_train <- train %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(broom::glance(lm(total_sales ~ date))) %>%
  select(total_sales:date,adj.r.squared,p.value) %>%
  ungroup()
weekly_sales_validate <- validate %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(broom::glance(lm(total_sales ~ date))) %>%
  select(total_sales:date,adj.r.squared,p.value) %>%
  ungroup()
weekly_sales_test <- test %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(broom::glance(lm(total_sales ~ date))) %>%
  select(total_sales:date,adj.r.squared,p.value) %>%
  ungroup()

#build m2
obs_full_m2 <- as.numeric(unlist(weekly_sales[1]))
obs_train_m2 <- as.numeric(unlist(weekly_sales_train[1]))
m2_train <- lm(total_sales ~ date, weekly_sales_train)
summary(m2_train)
pred_train_m2 <- predict(m2_train)
m2_test <- lm(total_sales ~ date, weekly_sales_test)
pred_test_m2 <- predict(m2_test)
m2_validate <- lm(total_sales ~ date, weekly_sales_validate)
pred_validate_m2 <-predict(m2_validate)

x <- weekly_sales$date
class(weekly_sales$date)
y <-  as.data.frame(unlist(obs_full_m2))
y1 <- as.data.frame(unlist(obs_train_m2))
y2 <- as.data.frame(unlist(pred_train_m2))
y3 <- as.data.frame(unlist(pred_validate_m2))
y4 <- as.data.frame(unlist(pred_test_m2))
tm2 <- cbind(x,y, y1, y2, y3, y4)
colnames(tm2) <- c("date", "obs_full_m0", "obs_train_m0", "pred_train_m0", "pred_validate_m0", "pred_test_m0")
as.data.frame(tm2)
class(tm2)

tm2_graph <- tm2 %>% ggplot() + geom_line(aes(x=x,y=obs_full_m2,color='Obs. Full dataset')) + 
  geom_line(aes(x=x,y=obs_train_m2,color='Obs. Train set')) + 
  geom_line(aes(x=x,y=pred_train_m2,color='Pred. Train set')) + 
  geom_line(aes(x=x,y=pred_validate_m2,color='Pred. Validate set')) +
  geom_line(aes(x=x,y=pred_test_m2,color='Pred. Test set')) +
  ylab('Sales')+xlab('Week') +
  ggtitle("m2: Weekly sales trend anlaysis and forecast for Hass avocado in the US") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(legend.position="right",legend.box = "vertical") +
  scale_color_discrete(name = "Data")
tm2_graph

#######################################################
#Evaluate three models appropriately
#######################################################

#Create mse function 
mse <- function(obs,pred) { 
  residuals_ = pred - obs
  mean(residuals_^2)
}

#Evaluate performance on m0
mse_train_m0 <- mse(obs_train_m0,pred_train_m0)
obs_validate_m0 <- annual_sales_validate$total_sales
obs_test_m0 <- annual_sales_test$total_sales
mse_validate_m0 <- mse(obs_validate_m0,pred_validate_m0)
mse_test_m0 <- mse(obs_test_m0,pred_test_m0)
mse_m0 <- data.frame(mse_train_m0, mse_test_m0, mse_validate_m0)
mse_m0
library(caret)
postResample(pred_train_m0, obs_train_m0)
postResample(pred_validate_m0, obs_validate_m0)

#Evaluate performance on m1
mse_train_m1 <- mse(obs_train_m1,pred_train_m1)
mse_validate_m1 <- mse(obs_validate_m1,pred_validate_m1)
obs_validate_m1 <- price_vs_sales_validate$total_sales
obs_test_m1 <- annual_sales_test$total_sales
mse_test_m1 <- mse(obs_test_m1,pred_test_m1)
mse_m1 <- data.frame(mse_train_m1, mse_test_m1, mse_validate_m1)
mse_m1
library(caret)
postResample(pred_train_m1, obs_train_m1)
postResample(pred_validate_m1, obs_validate_m1)

#Evaluate performance on m2
mse_train_m2 <- mse(obs_train_m2,pred_train_m2)
mse_validate_m2 <- mse(obs_validate_m2,pred_validate_m2)
obs_validate_m2 <- weekly_sales_validate$total_sales
obs_test_m2 <- annual_sales_test$total_sales
mse_test_m2 <- mse(obs_test_m2,pred_test_m2)
mse_m2 <- data.frame(mse_train_m2, mse_test_m2, mse_validate_m2)
mse_m2
library(caret)
postResample(pred_train_m2, obs_train_m2)
postResample(pred_validate_m2, obs_validate_m2)


#A well-fitting regression model results in predicted values close to the observed data values
#If there were no informative features, 
#we would always predict the average of labels  in the training data (the mean model)
#To evaluate the regression model, we compare it to the mean model by computing the mean square error (MSE): 
#the average of the difference between observation (y) and predictions (y)  
#(this difference is also called the residual error)MSE =1???????????????????=1????????(???????????????????y????????)2
#1. We optimize the classifier for a specific metric with the training data with different 
#hyper parameters (e.g., k in knn) (or different classifiers)
#2. We evaluate them with the validation data.
#3. We report the metric of the best model with the best hyper parameters on the test data.
#. Over fitting means a high performance of the chosen metric on the training data and 
#poor on the validation data.
#. Under fitting means a low performance of the chosen metric
