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
#Preprocessing 
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
  ggplot(aes(x = duration, y = total_sales)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_forecast() +
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Year", y = "Total Sales Volume", 
       title = "Total sales volume of Hass avocado in the US")
annual_sales <- avocado %>%
  group_by(month(date)) %>%
  summarize(total_sales = sum(total_volume)) %>%
  mutate(duration = year - first(year), broom::glance(lm(total_sales ~ duration))) %>%
  select(total_sales:duration,adj.r.squared,p.value) %>%
  ungroup()
annual_sales
annual_sales %>%
  ggplot(aes(x = duration, y = total_sales)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  geom_forecast() +
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Year", y = "Total Sales Volume", 
       title = "Total sales volume of Hass avocado in the US")

#Create m0 with lm()function 
m0 <- lm(total_sales ~ duration, annual_sales)
summary(lm(total_sales ~ duration, annual_sales)) 
cor(annual_sales$duration, annual_sales$total_sales) #check for correlation betweem the two variables
plot(annual_sales$duration, annual_sales$total_sales)
abline(lm(total_sales ~ duration, annual_sales))
predict(lm(total_sales ~ duration, annual_sales), list(duration = 6))
annual_sales_ts <-ts(annual_sales$total_sales,freq=1,start=c(2015,1))
plot(annual_sales_ts)
annual_sales_arima <-auto.arima(annual_sales_ts)
summary(annual_sales_arima)
annual_sales_forecast <-forecast(annual_sales_arima, h=5)
plot(annual_sales_forecast)
plot(annual_sales_forecast$residuals)
qqnorm(annual_sales_forecast$residuals)
acf(annual_sales_forecast$residuals)
pacf(annual_sales_forecast$residuals)

#######################################################
#Linear regression model for Annual Sales in accordance with Mean Price
#(X = average_price, Y = total_volume)
#######################################################
price_vs_sales <-  avocado %>%
  group_by(type, date) %>%
  summarize(total_sales = sum(total_volume), mean_price = mean(average_price)) %>%
  mutate(broom::glance(lm(total_sales ~ mean_price))) %>%
  select(total_sales:mean_price,adj.r.squared,p.value) %>%
  ungroup()
price_vs_sales

price_vs_sales %>%
  ggplot(aes(x = mean_price, y = total_sales)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ type) +
  geom_forecast() +
  labs(x = "Yearly Average Price", y = "Total Sales Volume", 
       title = "Annual sales volume of Hass avocado in the US varies with selling price") 

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
  stat_regline_equation(aes(label = ..adj.rr.label..), show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "Days", y = "Total Sales Volume", 
       title = "Total daily sales volume of Hass avocado in the US")
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
weekly_sales%>%
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

#######################################################
#Divide your dataset into training, validation and test data
#######################################################
# get indices for 70% of the data set
intrain <- createDataPartition(y = avocado_seed[,3], p= 0.7)[[1]]

#splitting data into training/testing data using the train Index object
training <- avocado_seed[intrain,]
testing <- avocado[-intrain,]

#######################################################
#Build three simple linear regression models using the lm() function, (m0, m1, and m2), 
#each model should have each of the predictors that you considered relevant. 
#######################################################
summary(m0)
summary(m1)
summary(m2)

#######################################################
#Evaluate three models appropriately
#######################################################

#######################################################
#Decide on best model based on the performance of test data
#######################################################

#######################################################
#Evaluate under fitness and over fitness
#######################################################

