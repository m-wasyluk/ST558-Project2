#this is the start file to test out code as specified in "prepare for your app"

library(readr)
library(dplyr)
library(ggplot2)

bike_raw <-  read_csv("./Data/SeoulBikeData.csv", col_names = TRUE, locale = locale(encoding = 'latin1'))

#create contingency tables so we can get an idea of which days are "functioning days"
table(bike_raw$`Functioning Day`)
table(bike_raw$`Functioning Day`, bike_raw$Holiday)
table(bike_raw$`Functioning Day`, bike_raw$Seasons)

#create numerical summaries
bike_summaries_by_holiday <- bike_raw |> 
  group_by(Holiday) |>
  summarise(across(where(is.numeric), 
                   list("mean" = mean, "median" = median, "sd" = sd)))

bike_summaries_by_season <- bike_raw |> 
  group_by(Seasons) |>
  summarise(across(where(is.numeric), 
                   list("mean" = mean, "median" = median, "sd" = sd)))

#create plots
bike_raw |> 
  ggplot(aes(x = `Rented Bike Count`)) + 
  geom_histogram() +
  ggtitle("Histogram of Rented Bike Count") +
  xlab("Number of bikes rented") +
  ylab("Count of days with n bikes rented")

bike_raw |> 
  ggplot(aes(x = `Rented Bike Count`)) + 
  geom_histogram(alpha = .5, aes(fill = Seasons)) +
  ggtitle("Histogram of Rented Bike Count by Season") +
  xlab("Number of bikes rented") +
  ylab("Count of days with n bikes rented")

bike_raw |> 
  ggplot(aes(x = `Temperature(°C)`, y = `Humidity(%)`)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("Humidity vs. Temperature") +
  xlab("Temperature (°C)") +
  ylab("Humidity (Relative %)")

bike_raw |> 
  ggplot(aes(x = `Temperature(°C)`, y = `Humidity(%)`)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap("Seasons") +
  ggtitle("Humidity vs. Temperature by Season") +
  xlab("Temperature (°C)") +
  ylab("Humidity (Relative %)")
