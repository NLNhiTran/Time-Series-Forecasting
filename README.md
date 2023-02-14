# Time series forecasting using R
## Predicting Hass Avocado Sales in the US market for the year 2021 

The avocado dataset was available on Kaggle and has originally been compiled from the Hass Avocado Board (HAB) by Justin Kiggins. The dataset contains 13 variables and 33045 observations with a time span between 04-01-2015 and 29-11-2020. With regards to the completeness of the data: there are no missing values. The variables are elaborated further in question 3. 
Based on literature review and our own judgement three variables have be determined to predict the expected total sales demand of Hass Avocados in the US market in 2021:
- The average price, based on the studies that have examined the correlation between the price and the sales trends over the years.
- The year and date, based on studies that have shown that the time factor has an effect on the total amount of sales, the date and year variable can be used to see which one can predict the total sales demand the best.

The disadvantages of a linear model is to capture the relationship between the variables, and the tendency for the model to overfit. In an ideal situation the model is located between the underfitting and overfitting spot. Despite the fact that this is the ultimate goal, in reality this is difficult to achieve especially when performing only linear function while the relationships are not perfectly linear. There are other multiple possible factors (economic or seasonality factors) that can affect the data trend. The chances of errors in predicting values hence are much higher using simple linear functions. 

An additional check for model m2 was run to assess whether there is seasonal factor within a year for avocado sales in the US. As can be seen from the graph below, there are repeated peaks during May and throughs during December, which affects the model linear trend and the prediction values.

![image](https://user-images.githubusercontent.com/117458345/218734814-bf75c6c7-bc4a-4191-a311-5d4851b82d10.png)

Another point being noticed from model m1 was the differences in price elasticity between different types of avocados. Sales of organic products seem to be less price-sensitive compared to those of conventional avocado’s. However, the model m2 was built based on the total sales of all avocado types and the average price. 

![image](https://user-images.githubusercontent.com/117458345/218734979-536d75aa-96aa-4af1-b790-2bb1089a992f.png)

