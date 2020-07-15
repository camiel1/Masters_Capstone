
# load library ------------------------------------------------------------

library(rEDM)
library(dplyr)
library(fpp3)
library(ggplot2)

# Import Data -------------------------------------------------------------

# import data from csv
buoy_data <- read.csv("buoy_data.csv", stringsAsFactors=FALSE)

# remove row numbers column
buoy_data$X <- NULL

# Data Wrangling ----------------------------------------------------------

# format time field as month day year hour
df_multiyear_hour <- buoy_data %>% 
  mutate(dt = ymd_h(
    paste(substr(x = buoy_data$time, start = 1, 10),
          substr(x = buoy_data$time, start = 12, 13),
          sep = " ")
  ))

# fill implicit gaps in hourly data and convert to daily data and *tsibble* object
df_multiyear_day <- df_multiyear_hour %>% 
  filter(!are_duplicated(df_multiyear_hour, key = buoy_id, index = dt)) %>% 
  select(dt, everything(), -time, -dewpt_temperature, -visibility, -water_level, -lat, -lon) %>% 
  select(buoy_id, dt, everything()) %>% 
  as_tsibble(key = buoy_id, index = dt) %>% 
  tsibble::fill_gaps() %>% 
  fill(wind_dir:sea_surface_temperature, .direction = "down") %>% 
  group_by(buoy_id) %>% 
  index_by(date = as_date(dt)) %>% 
  summarise_at(vars(wind_dir:sea_surface_temperature), mean) %>% 
  dplyr::slice(-1)

# creat data frame for rEDM
dat <- df_multiyear_day %>% 
  filter(buoy_id == 46221) %>% # toogle for new buoy
  select(date, wave_height)

# plot data
plot(dat$date, dat$wave_height, type = "l",
     xlab = "day", ylab = "wave height")

# determine optimap embedding dimension E
n <- NROW(dat)

# create train and test regions
# lib <- c(1, floor(2/3 * n))
# pred <- c(floor(2/3 * n) + 1, n)

lib <- c(1, floor(8/10 * n))
pred <- c(floor(8/10 * n) + 1, n)

# # alternaitve train and test
# lib <- c(1, n - 10)
# pred <- c((n - 10) + 1, n)

# determine optimal E
output <- simplex(dat, 
                 lib = lib, pred = lib,
                 E = 1:10) # after run the E with highest rho is 3

plot(output$E, output$rho, type = "l", 
     xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)",
     main = "Santa Monica Bay (46221)") # toggle for new buoy San Clemente Basin (46086)

# remodel using E = 3
output <- simplex(dat, 
                  lib = lib, pred = pred,
                  E = 5, # toogle for new buoy E = 3
                  stats_only = FALSE)

# create predictions
predictions <- na.omit(output$model_output[[1]])

# plot actuals
plot(dat$date, dat$wave_height, type = "l",
     xlab = "day", ylab = "wave height",
     main = "Santa Monica Bay (46221)") # toggle for new buoy San Clemente Basin (46086)

# plot predcitionss
lines(predictions$date, predictions$Predictions, col = "blue", lty = 2)

# plot prediction intervals
polygon(c(predictions$date, rev(predictions$date)),
        c(predictions$Predictions - sqrt(predictions$Pred_Variance),
        rev(predictions$Predictions + sqrt(predictions$Pred_Variance))),
        col = rgb(0,0,1,0.5), border = NA)

# view stats
output$stats


