# Forecasting future stock prices using historical data from the past 5 years
# Using time series data analysis, measuring one variable (Price) over time
View(`AAPL (Apple)`)
View(`GOOG (Google)`)
View(`META (Facebook)`)

#######################################
# installing package for time series analysis
#######################################
library(fpp2)

#######################################
# Creating variables for each stock using closing price
#######################################
Price_Apple <- ts(data = `AAPL (Apple)`[,6], start = (2017), end = (2022), frequency = 365)
Price_Google <- ts(data = `GOOG (Google)`[,5], start = (2017), end = (2022),frequency = 365)
Price_Meta <- ts(data = `META (Facebook)`[,5], start = (2017), end = (2022),frequency = 365)
rm(Price)


#######################################
#Graphing the price variations for the 3 companies from 2017-2022
#######################################
autoplot(Price_Apple) +
  ggtitle("Apple Price Variation from 2017-2022") +
  ylab("Price")
  
  autoplot(Price_Google) +
    ggtitle("Google Price Variation from 2017-2022") +
    ylab("Price")
  
  autoplot(Price_Meta) +
    ggtitle("Facebook Price Variation from 2017-2022") +
    ylab("Price")
  
########################################
#Daily price difference (PD) to have a more accurate representation of the change
########################################
PDa <- diff(Price_Apple) 
  autoplot(PDa) +
  ggtitle("Apple Daily Price Variation from 2017-2022") +
  ylab("Price")  
  
PDg <- diff(Price_Google)
  autoplot(PDg) +
  ggtitle("Google Daily Price Variation from 2017-2022") +
  ylab("Price")

PDf <- diff(Price_Meta)
  autoplot(PDf) +
  ggtitle("Facebook Daily Price Variation from 2017-2022") +
  ylab("Price")

########################################
#Seasonality 
########################################

ggseasonplot(Price_Apple) +
    ggtitle("Apple: Seasonality of Price per Year") +
    ylab("Price")
  
ggseasonplot(Price_Google) +
    ggtitle("Google: Seasonality of Price per Year") +
    ylab("Price")

ggseasonplot(Price_Meta) +
  ggtitle("Facebook: Seasonality of Price per Year") +
  ylab("Price")

########################################
#Calculating SD
########################################
sd(Price_Apple)
print(summary(sd(Price_Apple)))

sd(Price_Google)
print(summary(sd(Price_Google)))

sd(Price_Meta)
print(summary(sd(Price_Meta)))

########################################
#Histogram to evaluate if there is a normal distribution  
########################################

summary(`AAPL (Apple)`) +
  hist(Price_Apple) 
  
summary(`GOOG (Google)`) +
  hist(Price_Google)

summary(`META (Facebook)`) +
  hist(Price_Meta)

########################################
# Linear relationship
########################################
sd(Price_Apple)
sd(Price_Google)
sd(Price_Meta)

ggplot(`AAPL (Apple)`, aes(x = Date.1, y = Close)) +
  geom_smooth(method = lm, color = "red") +
  geom_point(aes(alpha = 1/20), position = "jitter") +
  theme(legend.position="none") +
  ggtitle("Apple Linear regression: 2017-2022 ") +
  labs(subtitle= "SD = 43.70239") +
  ylab("Price") +
  xlab("Years")
  
ggplot(GOOG.excel, aes(x = Date.1, y = Close)) +
  geom_smooth(method = lm, color = "red") +
  geom_point(aes(alpha = 1/20), position = "jitter") +
  theme(legend.position="none") +
  ggtitle("Google Linear regression: 2017-2022 ") +
  labs(subtitle= "SD = 29.59767") +
  ylab("Price") +
  xlab("Years")

ggplot(META.excel, aes(x = Date.1, y = Close)) +
  geom_smooth(method = lm, color = "red") +
  geom_point(aes(alpha = 1/20), position = "jitter") +
  theme(legend.position="none") +
  ggtitle("Facebook Linear regression: 2017-2022 ") +
  labs(subtitle= "SD = 57.36884") +
  ylab("Price") +
  xlab("Years")

#Summarize data for conclusion
summary(`AAPL (Apple)`)
summary(`GOOG (Google)`)
summary(`META (Facebook)`)

#QQ Plot Apple
qqnorm(Price_Apple, ylab="Residuals", main="" ) +
qqline(Price_Apple) 

#QQ Plot Google
qqnorm(Price_Google, ylab="Residuals", main="" ) +
qqline(Price_Google)

#QQ Plot Facebook
qqnorm(Price_Meta, ylab="Residuals", main="" ) +
qqline(Price_Meta)
