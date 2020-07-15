
# Load Libraries ----------------------------------------------------------

library(forecastML)
library(glmnet)
library(dplyr)
library(lubridate)
library(xgboost)
library(ggplot2)

# Import/Create Data ------------------------------------------------------

# import from csv when rnooa does not work on work computer
df_multiyear_hour <- read.csv("buoy_data.csv", 
                              stringsAsFactors=FALSE)
#drop row label column
df_multiyear_hour$X <- NULL

# Data Wrangling ----------------------------------------------------------

# # Run after Buoy_TimeSeries which does data Prep

# # format time field as month day year hour
# df_multiyear_hour <- df_multiyear_hour %>%
#   mutate(dt = ymd_h(
#     paste(substr(x = df_multiyear_hour$time, start = 1, 10),
#           substr(x = df_multiyear_hour$time, start = 12, 13),
#           sep = " ")
#   ))
# 
# # fill implicit gaps in hourly data and convert to daily data and *tsibble* object
# df_multiyear_day <- df_multiyear_hour %>%
#   filter(!are_duplicated(df_multiyear_hour, key = buoy_id, index = dt)) %>% # filter any duplicate key, index pairs
#   select(dt, everything(), -time, -dewpt_temperature, -visibility, -water_level, -lat, -lon) %>%
#   select(buoy_id, dt, everything()) %>%
#   as_tsibble(key = buoy_id, index = dt) %>% # coerce to tsibble to find,count, and fill implicit gaps
#   tsibble::fill_gaps() %>%
#   fill(wind_dir:sea_surface_temperature, .direction = "down") %>%
#   group_by(buoy_id) %>%
#   index_by(date = as_date(dt)) %>%
#   summarise_at(vars(wind_dir:sea_surface_temperature), mean) %>%
#   dplyr::slice(-1) # remove prior record from min year

# create forecastML data
data <- df_multiyear_day %>% 
  as.data.frame() %>% 
  mutate(day = mday(date),
         year = year(date)) %>% 
  mutate(lat = ifelse(buoy_id == 46086, 32.499, 33.855),
         lon = ifelse(buoy_id == 46086, -118.052, -118.634)) %>% 
  select(date, wave_height, buoy_id, lat, lon, day, year, wind_spd, sea_surface_temperature)

# Model Training with Nested CV -------------------------------------------

# set up buoy_id as factor (may not be correct way to set up)
data$buoy_id <- as.numeric(factor(data$buoy_id))

## create training data set ##

outcome_col <- 1  # The column position of our 'wave_height' outcome (after removing the 'date' column).

horizons <- c(1, 10)  # Forecast 1, 1:10 days into the future.

lookback <- c(1:30, 360:370)  # Features from 1 to 30 days in the past and annually.

dates <- data$date  # Grouped time series forecasting requires dates.
data$date <- NULL  # Dates, however, don't need to be in the input data.

frequency <- "1 day"  # A string that works in base::seq(..., by = "frequency").

dynamic_features <- c("day", "year")  # Features that change through time but which will not be lagged.

groups <- "buoy_id"  # 1 forecast for each group or buoy.

static_features <- c("lat", "lon")  # Features that do not change through time.

type <- "train"  # Create a model-training dataset.

data_train <- forecastML::create_lagged_df(data, type = type, outcome_col = outcome_col,
                                           horizons = horizons, lookback = lookback,
                                           dates = dates, frequency = frequency,
                                           dynamic_features = dynamic_features,
                                           groups = groups, static_features = static_features, 
                                           use_future = FALSE)

# plot feature map
plot(data_train) + geom_tile()

# CV Set Up ---------------------------------------------------------------

# set up windows
windows <- forecastML::create_windows(data_train, window_length = 365, skip = 730,
                                      include_partial_window = FALSE)

# plot windows
plot(windows, data_train) + theme(legend.position = "bottom")

# User Defined Modeling Function ------------------------------------------

# The value of outcome_col can also be set in train_model() with train_model(outcome_col = 1).
model_function <- function(data, outcome_col = 1) {
  
  # xgboost cannot handle missing outcomes data.
  data <- data[!is.na(data[, outcome_col]), ]
  
  indices <- 1:nrow(data)
  
  set.seed(224)
  train_indices <- sample(1:nrow(data), ceiling(nrow(data) * .8), replace = FALSE)
  test_indices <- indices[!(indices %in% train_indices)]
  
  data_train <- xgboost::xgb.DMatrix(data = as.matrix(data[train_indices, 
                                                           -(outcome_col), drop = FALSE]),
                                     label = as.matrix(data[train_indices, 
                                                            outcome_col, drop = FALSE]))
  
  data_test <- xgboost::xgb.DMatrix(data = as.matrix(data[test_indices, 
                                                          -(outcome_col), drop = FALSE]),
                                    label = as.matrix(data[test_indices, 
                                                           outcome_col, drop = FALSE]))
  
  params <- list("objective" = "reg:linear")
  watchlist <- list(train = data_train, test = data_test)
  
  set.seed(224)
  model <- xgboost::xgb.train(data = data_train, params = params, 
                              max.depth = 8, nthread = 2, nrounds = 30,
                              metrics = "rmse", verbose = 0, 
                              early_stopping_rounds = 5, 
                              watchlist = watchlist)
  
  return(model)
}

# Model Training ----------------------------------------------------------

#future::plan(future::multiprocess)  # Multi-core or multi-session parallel training.

# train model with user defined model function *not in parallel*
model_results_cv <- forecastML::train_model(lagged_df = data_train,
                                            windows = windows,
                                            model_name = "xgboost",
                                            model_function = model_function, 
                                            use_future = FALSE) # Uncomment to train in parallel

# 1-step-ahead model summary for the first validation window which is 2015.
summary(model_results_cv$horizon_1$window_1$model)

# Forecasting with Nested Models ------------------------------------------

## User Defined Prediction Function ## 

# If 'model' is passed as a named list, the prediction model would be accessed with model$model or model["model"].
prediction_function <- function(model, data_features) {
  x <- xgboost::xgb.DMatrix(data = as.matrix(data_features))
  data_pred <- data.frame("y_pred" = predict(model, x),
                          "y_pred_lower" = predict(model, x) - 1,  # Optional; in practice, forecast bounds are not hard coded.
                          "y_pred_upper" = predict(model, x) + 1)  # Optional; in practice, forecast bounds are not hard coded.
  return(data_pred)
}

# Historical Model Fit ----------------------------------------------------

# Here, we're predicting on our 2 validation datasets.
data_pred_cv <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_train)

# plot for closer inspection for each time horizon eg 1 and 10 days ahead
plot(data_pred_cv) + theme(legend.position = "bottom")

# facet plot by specific buoy factor Id with horizons 1 and 10 as colored lines
plot(data_pred_cv, facet = group ~ model, group_filter = "buoy_id %in% c(1, 2)", windows = 1) 

# facet by forecast horizon and buoy ID
plot(data_pred_cv, facet = group ~ horizon, group_filter = "buoy_id %in% c(1, 2)", windows = 1) 

# plot residulas
# plot(data_pred_cv, type = c("residual"))

# Historical Prediction Error ---------------------------------------------

# return model errors
data_error <- forecastML::return_error(data_pred_cv)

# plot MAE **DOES NOT WORK NEED TO MANUALLY PLOT**
# plot(data_error, type = "time", group_filter = "buoy_id %in% c(1, 2)", metric = "mae")
# 
# plot(data_error, type = "horizon", group_filter = "buoy_id %in% c(1, 2)", metric = "mae")
# 
# plot(data_error, type = "global", group_filter = "buoy_id %in% c(1, 2)", metric = "mae")

data_error$error_by_window %>% 
  mutate(buoy_id = factor(buoy_id)) %>% 
  ggplot(aes(x = window_midpoint, y = mae, color = buoy_id)) + 
  geom_line() +
  geom_point() + 
  facet_grid(model_forecast_horizon ~ model) + 
  scale_color_viridis_d() +
  theme_bw() + 
  ylab(paste0("Forecast error metric (mae)")) + 
  xlab("Dataset index") + 
  ggtitle(label = "Forecast Error by Validation Window")

data_error$error_by_horizon %>% 
  mutate(buoy_id = factor(buoy_id)) %>%
  ggplot(aes(x = buoy_id, y = mae, fill = buoy_id)) + 
  geom_bar(stat = "identity") + 
  facet_grid(model_forecast_horizon ~ model) + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  theme(axis.text.x = element_blank()) +
  ylab(paste0("Forecast error metric (mae)")) + 
  xlab("buoy_id") + 
  ggtitle(label = "Forecast Error by Forecast Horizon")

data_error$error_global %>% 
  mutate(buoy_id = factor(buoy_id),
         horizon = "All") %>%
  ggplot(aes(x = buoy_id, y = mae, fill = buoy_id)) + 
  geom_bar(stat = "identity") + 
  facet_grid(horizon ~ model) + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  theme(axis.text.x = element_blank()) +
  ylab(paste0("Forecast error metric (mae)")) + 
  xlab("buoy_id") + 
  ggtitle(label = "Forecast Error Accross Validation Windows and Horizons")

# write errors to files
data_error$error_by_window %>% tibble() %>% write.csv("ml_window_train_error.csv")
data_error$error_by_horizon %>% tibble() %>% write.csv("ml_horizon_train_error.csv")
data_error$error_global %>% tibble() %>% write.csv("ml_global_train_error.csv")

# Forecasting with Multiple Models from Nested CV -------------------------

type <- "forecast"  # Create a forecasting dataset for our predict() function.

data_forecast <- forecastML::create_lagged_df(data, type = type, outcome_col = outcome_col,
                                              horizons = horizons, lookback = lookback,
                                              dates = dates, frequency = frequency,
                                              dynamic_features = dynamic_features,
                                              groups = groups, static_features = static_features, 
                                              use_future = FALSE)

# Create Dynamic Features Step Ahead --------------------------------------

# create dynamic fatures indexes
for (i in seq_along(data_forecast)) {
  data_forecast[[i]]$day <- lubridate::mday(data_forecast[[i]]$index)  # When dates are given, the 'index` is date-based.
  data_forecast[[i]]$year <- lubridate::year(data_forecast[[i]]$index)
}

# Forecast ----------------------------------------------------------------

# forecast for each time horizon 1 & 10 day
data_forecasts <- predict(model_results_cv, prediction_function = list(prediction_function), data = data_forecast)

# Plots for each model-just xgboost here-and direct forecast horizon 
plot(data_forecasts)

# each buoy plot for two xgboost models
plot(data_forecasts, facet = group ~ ., group_filter = "buoy_id %in% 1:2")

# Model Training with All Data --------------------------------------------

# create single window to train on all data
windows <- forecastML::create_windows(data_train, window_length = 0)

# plot single window
plot(windows, data_train) + theme(legend.position = "none")

#future::plan(future::multiprocess)  # Multi-core or multi-session parallel training.
model_results_no_cv <- forecastML::train_model(lagged_df = data_train, 
                                               windows = windows,
                                               model_name = "xgboost",
                                               model_function = model_function,
                                               use_future = FALSE)

# Historical Model Fit ----------------------------------------------------

# Here, we're predicting on our 1 validation dataset
data_forecasts <- predict(model_results_no_cv, prediction_function = list(prediction_function), data = data_forecast)

# inspect data forecasts
head(data_forecasts)

# Forecast Combination ----------------------------------------------------

# combine multiple direct-horizon forecast models with combine_forecasts() to produce a single h-step-ahead forecast.
data_combined <- forecastML::combine_forecasts(data_forecasts)

# Plot a background dataset of actuals using the most recent data.
data_actual <- data[dates >= as.Date("2018-11-01"), ]
actual_indices <- dates[dates >= as.Date("2018-11-01")]

# Plot all final forecasts plus historical data.
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices)

# actuals vs forecast split into individual buoys on seprate graphs
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices, 
     facet = group ~ ., group_filter = "buoy_id %in% c(1, 2)")

# Plot final forecasts for a single buoy plus historical data.
plot(data_combined, data_actual = data_actual, actual_indices = actual_indices,
     group_filter = "buoy_id == 1")

