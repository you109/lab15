#~~~~~~~ Step 1 – Install and Load Required Packages

# Install necessary packages (only run if not already installed)
packages <- c("tseries", "forecast", "TTR", "ggplot2")
installed <- packages %in% installed.packages()

if (any(!installed)) {
      install.packages(packages[!installed])
}


# Load the libraries
library(tseries)
library(forecast)
library(TTR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)


#~~~~~~~ Step 2 – Data import
setwd("~/Desktop/AGE333_Lab14")

#### Load the Soybean data

# Filter for the desired date range
soybeans <- read.csv("soybean-prices-historical-chart-data.csv")
head(soybeans)

# Transform the Soybean date column in Date format using the as.Date() function
soybeans$date <- as.Date(soybeans$date, format = "%m/%d/%Y")
str(soybeans)

# calculate the monthly average from your daily soybean price data 
soybeans <- soybeans %>%
      mutate(date = floor_date(date, "month")) %>%        # Set each date to the first of the month
      group_by(date) %>%
      summarize(price = mean(value, na.rm = TRUE)) %>%    # Calculate monthly average
      ungroup()

# Filter the soybean data
soybeans <- soybeans %>% filter(date >= as.Date("01/01/1969", format = "%m/%d/%Y") & date <= as.Date("03/01/2025", format = "%m/%d/%Y"))
head(soybeans)


#### Load the CPI data 

CPI<- read.csv("historical-cpi-u-202503!.csv")

head(CPI)

# Transform CPI data into longer table 
CPI <- CPI %>% pivot_longer(cols = -Year, names_to = 'Month', values_to = 'CPI') 

head(CPI)

# Convert month names 
CPI <- CPI %>% mutate(Month = match(gsub("\\.", "", Month), month.abb))  # Convert month names to numbers

# Create date column
CPI <- CPI %>% mutate(date = as.Date(paste(Year, Month, 1, sep = "-")))

# Format date as m/1/yyyy
CPI <- CPI %>% mutate(date = format(date, "%m/1/%Y"))

# Drop the Year and Month columns and ensure chronological order
CPI <- CPI %>% select(date, CPI) %>% arrange(as.Date(date, format = "%m/%d/%Y")) 

str(CPI)


# Transform the CPI date column in Date format using the as.Date() function
CPI$date <- as.Date(CPI$date, format = "%m/%d/%Y")
str(CPI)

# Filter the CPI data
CPI <- CPI %>% filter(date >= as.Date("01/01/1969", format = "%m/%d/%Y") & date <= as.Date("03/01/2025", format = "%m/%d/%Y"))
head(CPI, 20)

# Preview the data
str(soybeans)
str(CPI)




#~~~~~~~ Step 3 – Create date variables and merge data frames


# Merge datasets
soybeans <- merge(soybeans, CPI, by="date")
head(soybeans)
tail(soybeans)



#~~~~~~~ Step 4 – Convert nominal price to real price

soybeans <- soybeans %>% mutate(price_real = price / (CPI / 100))

head(soybeans)
tail(soybeans)


#~~~~~~~ Step 5 – Visualize soybeans prices over time
ggplot() + 
      geom_line(data = soybeans, aes(x = date, y = price, color = 'Nominal')) +
      geom_line(data = soybeans, aes( x = date, y = price_real, color = 'Real')) + 
      scale_color_manual(name = "Price Type", values= c("Nominal" = 'green', "Real" = 'red'))

str(soybeans)

soybeans$date <- as.Date(soybeans$date, "%m/%d/%Y")

ggplot() + 
      geom_line(data = soybeans, aes(x = date, y = price, color = 'Nominal')) +
      geom_line(data = soybeans, aes(x = date, y = price_real, color = 'Real')) +
      scale_color_manual(name = "Type", values = c('Nominal' = 'green','Real' = 'red' ))


# Explain the colors: https://r-graph-gallery.com/ggplot2-color.html
# Explain the themes: https://ggplot2.tidyverse.org/reference/ggtheme.html
ggplot(soybeans, aes(x = date)) +
      geom_line(aes(y = price, color = "Nominal"), linewidth = 0.8) +
      geom_line(aes(y = price_real, color = "Real"), linewidth = 0.8) +
      scale_color_manual(name = "Price Type", 
                         values = c("Nominal" = 622, "Real" = 47))+
      labs(title = 'Nominal vs Real Soybean Prices', x = 'Date', y = 'Price (USD)') + 
      theme_light()

# https://stackoverflow.com/questions/63064848/how-to-plot-several-columns-on-the-same-line-chart-in-r
library(tidyr) # Pivot
soybeans_long <- soybeans %>% pivot_longer(cols = -c(date, CPI)) %>% 
      mutate(name = recode(name, price = 'Nominal', price_real = 'Real'))

ggplot(soybeans_long, aes(x = date, y = value, color = name)) +
      geom_line(linewidth = 0.5) + 
      scale_color_manual(name = "Price Type", 
                         values = c("Nominal" = "#E69F00", "Real" = 622))+
      labs(title = 'Nominal vs Real Soybean Prices', x = 'Date', y = 'Price (USD)') + theme_light()
  

# https://stackoverflow.com/questions/9531904/plot-multiple-columns-on-the-same-graph-in-r/46229347
install.packages("reshape2")

library(reshape2) # Melt
soybeans_filter <- soybeans %>% select(date, price, price_real)
soybeans_long2 <- melt(soybeans_filter, id.vars = 'date')


#~~~~~~~ Step 6 – Establishing a time-series

# Your task: Set nominal prices as a time series. Replace the placeholders (including the brackets) with the appropriate arguments.
price.ts <- ts(soybeans$price, start = c(1969, 1), end = c(2025, 3), frequency = 12)


#~~~~~~~ Step 7 – Decompose the time-series 

# Decompose nominal price time-series
price_components <- decompose(price.ts, type="additive")
plot(price_components)
price_components$figure

#~~~~~~~ Step 8 – Data smoothing

# Create a seasonally adjusted time-series
price_sa.ts <- price.ts - price_components$seasonal


#############
## Ask them to findout how to plot time series:
# https://atsa-es.github.io/atsa-labs/sec-tslab-time-series-plots.html
#############
# Your task: Plot the seasonally adjusted time-series using `plot.ts()`.
# The `plot.ts()` function is specifically for time-series objects and works similarly to the `plot()` function.
plot.ts(price.ts, main="Soybeans Prices (Seasonally Adjusted) Over Time", xlab="", ylab="Nominal Price ($)")


###############
## Documentation: https://www.geeksforgeeks.org/moving-averages-in-r/
##############
# Your task: Take a 3-, 6-, and 12-month simple moving average of price using the `SMA()` function below (arguments in brackets).
price_sma3 <- TTR::SMA(price.ts, n=3) # 3-month simple moving average
price_sma12 <- SMA(price.ts, n=12)    # 12-month simple moving average
price_sma48 <- SMA(price.ts, n=48)    # 48-month simple moving average




# Graph each moving average series in a separate graph using `plot.ts()`. You can use the `par(mfrow=c([# of rows], [# of columns]))` command to tell R to group the three graphs together.
par(mfrow=c(3, 1))  # Arrange plots in 3 rows
# Example: Plot the 3-month moving average
plot.ts(price_sma3, main="Soybeans Price - 3 Month SMA", xlab="", ylab="SMA Price ($)")
# Your task: Plot the 12-month moving average
plot.ts(price_sma12, main="Soybeans Price - 12 Month SMA", xlab="", ylab="SMA Price ($)")
# Your task: Plot the 48-month moving average
plot.ts(price_sma48, main="Soybeans Price - 48 Month SMA", xlab="", ylab="SMA Price ($)")

par(mfrow=c(1, 1))  # Reset to default layout

###########
# Saving a plot: https://www.datamentor.io/r-programming/saving-plot
#########
jpeg(filename = 'Plot.jpeg')
dev.off() # Close the opened jpeg() graphical device 


#~~~~~~~ Step 9 – Stationarity

#########
# https://www.r-bloggers.com/2023/06/mastering-the-power-of-rs-diff-function-a-programmers-guide/
#########
# Take the first difference of the monthly soybeans price time-series using the `diff()` function.
price_diff.ts <- diff(price.ts, differences=1)

plot.ts(price_diff.ts, main = 'Month-over-Mont Change in Soybeans Prices Over Time', xlab = '', ylab='Nominal Price Change')


# Your task: Log-transform the soybeans price time-series using the `log()` function.
# This helps to stabilize variance and make the data more suitable for analysis.
log_price.ts <- log(price.ts)

# Your task: Take the first difference of the log-transformed soybeans price time-series using the `diff()` function.
# This essentially calculates the month-over-month percentage change. (*Why is this the case?*)
log_price_diff.ts <- diff(price.ts, differences=1)

# Your task: Plot the log differenced time-series using the `plot.ts()` function. Change the title to "Month-over-Month % Change in Soybeans Prices Over Time", x axis lable to "", and y axis lable to "% Price Change"

plot.ts(price_diff.ts, main = 'Month-over-Mont % Change in Soybeans Prices Over Time', xlab = '', ylab='% Price Change')


#~~~~~~~ Step 10 – Auto-regressive model

# Run auto-regressive model with 3 lags (AR(3))
ar <- ar(log_price_diff.ts, order.max=5)
ar
checkresiduals(ar, lag.max=60)


#~~~~~~~ Step 11 – Forecasting

# Forecast prices 6 months into the future based on the AR(3) model
forecast <- forecast(ar, h=6)
forecast
autoplot(forecast, include=36, xlab="", ylab="Soybeans price - monthly % returns")





