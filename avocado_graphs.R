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
colnames(avocado_raw)
#######################################################

#######################################################
#Pre-processing 
#######################################################
avocado <- avocado_raw %>%
  mutate(date = ymd(date), week = strftime(date, "%W"), month = as.numeric(strftime(date, "%m")), 
         days_since_first_date = as.numeric(difftime((date), min(date), units = "days"))) 
View(avocado)
str(avocado)
head(avocado, 10)

#######################################################
#Linear regression model for Annual Sales 
#X = year(duration), Y = total_volume)
#######################################################
annual_sales <- avocado %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales
annual_sales %>%
  ggplot(aes(x = duration, y = total_sales, label = total_sales)) + 
  geom_point(size = 2) + 
  geom_line(aes(y = predlm), size = 1)
  geom_smooth(method = "lm", alpha = .15, aes(fill = total_sales)) +
  geom_text(aes(label=scales::comma(total_sales),hjust=-0.2,vjust=0.3)) +
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  xlim(0,7)+
  scale_y_continuous(labels = scales::comma, limits = c(4000000000, 8500000000)) +
  labs(x = "Year(s) since 2015 (duration)", y = "Total Sales Volume (avocado piece)", 
       title = "Total annual sales forecast for Hass avocado in the US")
plot(annual_sales$duration, annual_sales$total_sales, pch = 17, col = "green") 
abline(m0, col = "blue")
annual_sales_forecast <-forecast(annual_sales_arima, h=3)
a <- autoplot(annual_sales_forecast) + autolayer(annual_sales_forecast, showgap = F)
annual_sales$predlm <- predict(m0,list(duration = 1:6))

#Create m0 with lm()function 
m0 <- lm(total_sales ~ duration, annual_sales)
m0
autoplot(m0)
4.312e+09 + 4.085e+08*5
summary(lm(total_sales ~ duration, annual_sales)) 
cor(annual_sales$duration, annual_sales$total_sales) #check for correlation between the two variables
as.numeric(annual_sales$duration)

class(annual_sales$total_sales)
class(annual_sales$duration)
duration_num <-  as.numeric(annual_sales$duration)
class(duration_num)

abline(lm(total_sales ~ duration, annual_sales))
pred <- predict(lm(total_sales ~ duration, annual_sales), list(duration = c(1,12)))
annual_sales$predlm <- pred

annual_sales_ts <-ts(annual_sales$total_sales,freq=1,start=c(2015,1))
plot(annual_sales_ts)
annual_sales_arima <-auto.arima(annual_sales_ts)
summary(annual_sales_arima)
annual_sales_forecast <-forecast(annual_sales_arima, h=5)
plot(annual_sales_forecast, showgap = FALSE)
plot(annual_sales_forecast$residuals)
qqnorm(annual_sales_forecast$residuals)
acf(annual_sales_forecast$residuals)
pacf(annual_sales_forecast$residuals)
#######################################################
#Linear regression model for Annual Sales in accordance with Mean Price
#(X = average_price, Y = total_volume)
#######################################################
price_vs_sales_split <-  avocado %>%
  group_by(type, date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_sales_split

price_vs_sales_split %>%
  ggplot(aes(x = mean_price, y = total_sales, col = "red")) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, col = "turquoise") + 
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 200000000)) + xlim(0,3) +
  facet_wrap(~ type) +
  theme_minimal() +
  labs(x = "Weekly Average Price", y = "Total Sales Volume (avocado piece)", 
       title = "Daily sales volume at different selling prices of Hass avocado - per type")

price_vs_sales_all <-  avocado %>%
  group_by(date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()

price_vs_sales_all
price_vs_sales_all %>%
  ggplot(aes(x = mean_price, y = total_sales)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma, limits = c(0, 200000000)) + xlim(0,3) +
  geom_forecast(showgap = FALSE, show.legend = TRUE) +
  labs(x = "Daily average price (per avocado sold)", y = "Total Sales Volume (avocado piece)", 
       title = "Daily sales volume at different selling prices of Hass avocado")

#Create m1 using lm() function
m1 <- lm(total_sales ~ mean_price, price_vs_sales)
summary(m1) 

#Forecast 
predict(m1, list(mean_price = 1.6)) #use predict function to project next sales volume @next pricing point
price_vs_sales_ts <-ts(price_vs_sales$total_sales,freq=0.2,start=c(0.2,0.1)) #turn total_sales into time series object
plot(price_vs_sales_ts) #
price_vs_sales_arima <-auto.arima(price_vs_sales_ts)
summary(price_vs_sales_arima)
price_vs_sales_forecast <-forecast(price_vs_sales_arima, h=25)
plot(price_vs_sales_forecast)

#Checking for most linear relationship per region
summary(avocado$geography)
price_vs_sales <-  avocado %>%
  group_by(type, geography, year) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup() %>%
  as.data.frame()
subset(price_vs_sales, price_vs_sales$adj.r.squared >0.9)
price_vs_salesGP <- avocado %>%
  filter(geography == "Grand Rapids", type == "organic") %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_salesGP
price_vs_salesGP %>%
  ggplot(aes(x = mean_price, y = total_sales)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_forecast()
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Yearly Average Price", y = "Total Sales Volume", 
       title = "Annual sales volume of Hass avocado in Grand Rapids in relation to selling price")

#######################################################
#Linear regression model for Daily Sales
#(X = Month(duration), Y = total_volume)
  
#######################################################
daily_sales <- avocado %>%
  group_by(days_since_first_date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = days_since_first_date, broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup() %>%
  as.data.frame() 
daily_sales
daily_sales %>%
  ggplot(aes(x = duration, y = total_sales)) + 
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE) + 
  geom_forecast()+
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Days since first date 2015-01-04 (duration)", y = "Total Sales Volume (avocado piece)", 
       title = "Total weekly sales volume of Hass avocado in the US")
#Checking for seasonality and trend
count_ma <- ts(na.omit(avocado$total_volume), frequency = 365)
decomp <- stl(count_ma, "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
avocado2020 <- subset(avocado, avocado$year == 2020)
count_ma <- ts(na.omit(avocado2020$total_volume), frequency = 365)
decomp <- stl(count_ma, "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#Create m2 using lm() function
m2 <- lm(total_sales ~ duration, daily_sales)
summary(m2)
cor() #check correlation 
plot(daily_sales$duration, daily_sales$total_sales) #plot simple graph to check for correlation
#Forecast
predict(m2, list(duration = 100))
abline(lm(total_sales ~ duration, daily_sales))
#######################################################
#Linear regression model for Weekly Sales
#(X = Weekly(ma), Y = total_volume)
#######################################################
weekly_sales <- avocado %>%
  group_by(year, week) %>%
  summarize(total_sales = sum(total_volume)) %>%
  ungroup() %>%
  as.data.frame()
weekly_sales
as.numeric(weekly_sales$week)
str(weekly_sales)
class(as.numeric(weekly_sales$week))
avocado%>%
  ggplot(aes(x = date, y = total_sales)) + 
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "24 weeks", date_labels = "%b") +
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~ year, ncol = 1) +
  labs(x = "Month", y = "Total Sales Volume", title = "Total weekly sales volume of Hass avocado in the US over the period of 6 years (2015 - 2020)")
summary(lm(total_sales ~ week, weekly_sales))

count_ma <- ts(na.omit(weekly_sales$total_sales), frequency = 12)
decomp <- stl(count_ma, "periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
#######################################################

#######################################################
#Set a seed to ensure that the results are always the same 
# and shuffle the rows of your data using first
#######################################################
set.seed(123) # set the random number seed for reproducibility 
avocado_seed <- avocado[sample(nrow(avocado)),]
spec = c(train = .6, test = .2, validate = .2)

#######################################################
#Divide your data set into training, validation and test data
#######################################################
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
#Build three simple linear regression models using the lm() function, (m0, m1, and m2), 
#each model should have each of the predictors that you considered relevant. 
#######################################################
annual_sales_train <- train %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales_train
annual_sales_train %>%
  ggplot(aes(x = duration, y = total_sales, label = total_sales)) + 
  geom_point(size = 2) + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_forecast(forecast = pred_train_m0, showgap=FALSE, show.legend = TRUE) +
  geom_text(aes(label=scales::comma(total_sales),hjust=-0.2,vjust=0.3)) +
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  xlim(0,7)+
  scale_y_continuous(labels = scales::comma, limits = c(2000000000, 4500000000)) +
  labs(x = "Year(s) since 2015 (duration)", y = "Total Sales Volume (avocado piece)", 
       title = "Total annual sales trend anlaysis and forecast for Hass avocado in the US")

m0_train <- lm(total_sales ~ duration, annual_sales_train)
summary(m0_train)


price_vs_sales_train <-  train %>%
  group_by(type, date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_sales_train

price_vs_sales_train %>%
  ggplot(aes(x = mean_price, y = total_sales)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ type) +
  geom_forecast() +
  labs(x = "Yearly Average Price", y = "Total Sales Volume", 
       title = "Annual sales volume of Hass avocado in the US varies with selling price") 
m1_train <- lm(total_sales ~ mean_price, price_vs_sales_train)
summary(m1_train) 

daily_sales_train <- train %>%
  group_by(days_since_first_date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = days_since_first_date, broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup() %>%
  as.data.frame() 
daily_sales_train
daily_sales_train %>%
  ggplot(aes(x = duration, y = total_sales)) + 
  geom_line() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Days", y = "Total Sales Volume", 
       title = "Total daily sales volume of Hass avocado in the US")
#Create m2 using lm() function
m2_train <- lm(total_sales ~ duration, daily_sales_train)
summary(m2_train)

#######################################################
#Evaluate three models appropriately
#######################################################

#mse function 
mse <- function(obs,pred) { 
  residuals_ = pred - obs
  mean(residuals_^2)
}

#Performance on train set
pred_train_m0 <- predict(m0_train)
obs_full_m0 <- as.numeric(unlist(annual_sales[1]))
obs_train_m0 <- as.numeric(unlist(annual_sales_train[1]))
mse_train_m0 <- mse(obs_train_m0,pred_train_m0)
mse_train_m0

m0_train

x <- c(2015,2016,2017,2018,2019,2020)
y <-  as.data.frame(scales::comma(obs_full_m0))
y1 <- as.data.frame(scales::comma(obs_train_m0))
y2 <- as.data.frame(scales::comma(pred_train_m0))
y3 <- as.data.frame(scales::comma(pred_validate_m0))
y4 <- as.data.frame(scales::comma(pred_test_m0))
tm0 <- cbind(x,y, y1, y2, y3, y4)
tm0
str(tm0)
str(validate)
str(train)
table(annual_sales$total_sales)

plot(tm0$x,tm0$y1, type="b", pch=19, col="red", xlab="x", ylab="y")
# Add a line
plot(tm0$x, tm0$y2, pch=10, col="blue", type="b", lty=2, add = TRUE)
plot(tm0$x, tm0$y3, pch=9, col="green", type="b", lty=2, add = TRUE)
# Add a legend
legend(1, 6, legend=c("Observe", "Train", "Validate"),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8)

tm0_graph <- ggplot() + geom_line(aes(x=x,y=obs_full_m0,color='Obs. Full dataset')) + 
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
colnames(tm0) <- c("year", "m0_obs_full","m0_obs_train","m0_pred_train","m0_pred_validate","m0_pred_test")
tm0
pred_train_m1 <- predict(m1)
obs_train_m1 <- as.numeric(unlist(price_vs_sales_train[2]))
mse_train_m1 <- mse(obs_train_m1,pred_train_m1)
mse_train_m1

pred_train_m2 <- predict(m2)
obs_train_m2 <- as.numeric(unlist(daily_sales_train[1]))
mse_train_m2 <- mse(obs_train_m2,pred_train_m2)
mse_train_m2
as.data.frame(mse_train_m0, mse_train_m1, mse_train_m2)

caret::postResample(pred_train_m0, obs_train_m0)

obs_full_m1 <- price_vs_sales[2]
obs_full_m1
obs_train_m1 <- price_vs_sales_train[2]
obs_train_m1
pred_train_m1 <- prediction
x <- c(2015,2016,2017,2018,2019,2021)
y <-  as.data.frame(scales::comma(obs_full_m1))
y1 <- as.data.frame(scales::comma(obs_train_m1))
y2 <- as.data.frame(scales::comma(pred_train_m1))
y3 <- as.data.frame(scales::comma(pred_validate_m1))
y4 <- as.data.frame(scales::comma(pred_test_m1))
tm1 <- cbind(x,y, y1, y2, y3, y4)
tm1
str(tm1)
str(validate)
str(train)
table(annual_sales$total_sales)
tm1_graph <- ggplot() + geom_line(aes(x=x,y=obs_full_m1,color='Obs. Full dataset')) + 
  geom_line(aes(x=x,y=obs_train_m1,color='Obs. Train set')) + 
  geom_line(aes(x=x,y=pred_train_m1,color='Pred. Train set')) + 
  geom_line(aes(x=x,y=pred_validate_m1,color='Pred. Validate set')) +
  geom_line(aes(x=x,y=pred_test_m1,color='Pred. Test set')) +
  ylab('Sales')+xlab('Time') +
  ggtitle("m1: Total annual sales trend anlaysis and forecast for Hass avocado in the US") +
  scale_y_continuous(labels = scales::comma) + 
  theme_minimal() +
  theme(legend.position="right", legend.box = "vertical") +
  scale_color_discrete(name = "Data")
tm1_graph
colnames(tm1) <- c("year", "m1_obs_full","m1_obs_train","m1_pred_train","m1_pred_validate","m1_pred_test")
tm1


#Performance on test set
annual_sales_test <- test %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales_test
price_vs_sales_test <-  test %>%
  group_by(type, date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_sales_test
daily_sales_test <- test %>%
  group_by(days_since_first_date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = days_since_first_date, broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup() %>%
  as.data.frame() 
daily_sales_test

m0_test <- lm(total_sales ~ duration, annual_sales_test)
summary(m0_test)
pred_test_m0 <- predict(m0_test)
obs_test_m0 <- as.numeric(unlist(annual_sales_test[1]))
mse_test_m0 <- mse(obs_test_m0,pred_test_m0)
mse_test_m0

pred_test_m1 <- predict(m1)
obs_test_m1 <- as.numeric(unlist(price_vs_sales_test[2]))
mse_test_m1 <- mse(obs_test_m1,pred_test_m1)
mse_test_m1

pred_test_m2 <- predict(m2)
obs_test_m2 <- as.numeric(unlist(daily_sales_test[1]))
mse_test_m2 <- mse(obs_test_m2,pred_test_m2)
mse_test_m2

#Performance on validate set
annual_sales_validate <- validate %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales_validate
m0_validate <- lm(total_sales ~ duration, annual_sales_validate)
summary(m0_validate)
price_vs_sales_validate <-  validate %>%
  group_by(type, date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_sales_validate
daily_sales_validate <- validate %>%
  group_by(days_since_first_date) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = days_since_first_date, broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup() %>%
  as.data.frame() 
daily_sales_validate

pred_validate_m0 <- predict(m0_validate)
obs_validate_m0 <- as.numeric(unlist(annual_sales_validate[1]))
mse_validate_m0 <- mse(obs_validate_m0,pred_validate_m0)
mse_validate_m0

pred_validate_m1 <- predict(m1)
obs_validate_m1 <- as.numeric(unlist(price_vs_sales_validate[2]))
mse_validate_m1 <- mse(obs_validate_m1,pred_validate_m1)
mse_validate_m1

pred_validate_m2 <- predict(m2)
obs_validate_m2 <- as.numeric(unlist(daily_sales_validate[1]))
mse_validate_m2 <- mse(obs_validate_m2,pred_validate_m2)
mse_validate_m2

#######################################################
#Decide on best model based on the performance of test data
#######################################################


#######################################################
#Evaluate under fitness and over fitness
#######################################################
#Over fitting and Under fitting Based on performance

#Under fitting: MSE is high on both the training and validation sets.

#Over fitting: MSE is very low on the training set but very high on the validation set.

###Draft

price_vs_sales <- avocado %>%
  group_by(year) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  ungroup()
price_vs_sales

year <- c(2015,2016,2017,2018,2019,2020)
sales <- unlist(annual_sales$total_sales)
price <-  as.data.frame(price_vs_sales$mean_price)
tmdraft <- cbind(year, sales, price)
as.data.frame(tmdraft)
class(tmdraft)

cor_sales_price <- tmdraft %>% ggplot() + geom_line(aes(x= year, y = sales))
geom_line(aes(x=year, y=price, color='red'))
