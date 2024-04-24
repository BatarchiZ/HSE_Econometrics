library(sandwich)
library(lmtest)
library(stargazer)
library(readr)
library(dplyr)

# Exclude the columns 'room', 'inverse_metro_dist', 'X', 'lng', 'lat', 'attr_index', 'rest_index', 'metro_dist_squared'
# data_modified <- data %>% select(-room, -inverse_metro_dist, -X, -lng, -lat, -attr_index, -rest_index, -metro_dist_squared)
data_modified <- read_csv("paris_weekends.csv")

# Create table with the modified data
columns <- colnames(data_modified)
t1 <- stargazer(
  data_modified[,columns], type = "text",
  summary.stat = c("min", "p25", "median", "p75", "max", "sd"),
  digits = 2  # Set the number of decimal places to 3
)

# To create the HTML table
sink("my_table.html")
stargazer(data_modified[,columns], type = "html", 
          summary.stat = c("min", "p25", "median", "p75", "max", "sd"), 
          digits = 2)
sink()


data$room_private <- as.integer(as.logical(data$room_private))
data$room_shared <- as.integer(as.logical(data$room_shared))
data$host_is_superhost <- as.integer(as.logical(data$host_is_superhost))

# Regression model
model <- lm(log(realSum) ~ room_shared + room_private + 
              person_capacity + host_is_superhost + 
              multi + biz + cleanliness_rating + 
              guest_satisfaction_overall + bedrooms + 
              attr_index_norm + rest_index_norm +
              metro_dist + dist + room_shared*person_capacity +room_private*person_capacity,
            data = data)
# Robust Errors
coeftest(model, vcov. = vcovHC(model))


# Export as html
sink("model_summary.html")
stargazer(model, type = "html", title = "My model")
sink()


data$room_private <- as.integer(as.logical(data$room_private))
data$room_shared <- as.integer(as.logical(data$room_shared))
data$host_is_superhost <- as.integer(as.logical(data$host_is_superhost))


model <- lm(log(realSum) ~ room_shared + room_private + 
              person_capacity + host_is_superhost + 
              multi + biz + cleanliness_rating + 
              guest_satisfaction_overall + bedrooms + 
              attr_index_norm + rest_index_norm +
              metro_dist + dist + room_shared*person_capacity +room_private*person_capacity,
            data = data)
coeftest(model, vcov. = vcovHC(model))

# !!! Manually changed results in model_summary to display robust errors !!!
# !!! Manually changed results in model_summary to display robust errors !!!
# !!! Manually changed results in model_summary to display robust errors !!!
