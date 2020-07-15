
# Load Libraries ----------------------------------------------------------

library(tidyquant)
library(tidyverse)
library(timetk)
library(xgboost)
library(ggplot2)
library(parsnip)
library(yardstick)
library(fpp3)
library(ggforce)

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

# create time series dataframe
buoy_tbl <- df_multiyear_day %>% 
  filter(buoy_id == 46221) %>% # toggle for different buoys
  select(date, wave_height) %>% 
  as_tibble() %>% 
  rename(value = wave_height)

# Train and Test ----------------------------------------------------------

# create train and test regions
buoy_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2015-01-01")), # 2018-07-01
            xmax = as.numeric(ymd("2018-03-14")), # 2018-12-31
            ymin = 0, ymax = 6,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2016-10-01"), y = 4.75,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2018-10-01"), y = 4.5,
           color = palette_light()[[1]], label = "Test Region") +
  # geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_line() + 
  labs(title = "Buoy Data Set: 46086", x = "") +
  theme_tq()

# Split into training and test sets
# train_tbl <- buoy_tbl %>% filter(date < ymd("2018-07-01"))
# test_tbl  <- buoy_tbl %>% filter(date >= ymd("2018-07-01"))

# toggle to adjust train and test regions here 80/20 split
train_tbl <- buoy_tbl %>% filter(date < ymd("2018-03-14")) # 2018-12-22
test_tbl  <- buoy_tbl %>% filter(date >= ymd("2018-03-14")) # 2018-12-22

# add time series signature
train_tbl <- train_tbl %>%
  tk_augment_timeseries_signature() %>% 
  select(-c(hour, minute, second, hour12, am.pm)) # add to remove unecessary features

# Model Build -------------------------------------------------------------

# build xgboost model

set.seed(123)
model_xgboost <- boost_tree(
  mode = "regression",
  mtry = 20,
  trees = 5000, # 500
  min_n = 3,
  tree_depth = 8, # 8
  learn_rate = 0.01,
  loss_reduction = 0.01) %>% 
  set_engine("xgboost") %>% 
  fit.model_spec(value ~ ., data = train_tbl %>% select(-date, -diff))

# add time features to the testing data
test_tbl <- test_tbl %>%
  tk_augment_timeseries_signature() %>% 
  select(-c(hour, minute, second, hour12, am.pm)) # add to remove unecessary features

# predict on testing table
prediction_tbl <- predict(model_xgboost, new_data = test_tbl) %>% 
  bind_cols(test_tbl) %>% 
  select(.pred, date, value)

# visualize predictions
ggplot(aes(x = date), data = buoy_tbl) +
  geom_rect(xmin = as.numeric(ymd("2015-01-01")), # 2018-07-01
            xmax = as.numeric(ymd("2018-03-14")), # 2018-12-31
            ymin = 0, ymax = 6,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2016-10-01"), y = 4.75,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2018-10-01"), y = 4.5,
           color = palette_light()[[1]], label = "Test Region") +
  geom_line(aes(x = date, y = value)) + 
  # Add predictions
  geom_line(aes(x = date, y = .pred), color = "red", data = prediction_tbl) +
  theme_tq()

# alternative visual !!!!!!!!!!!USE THIS FOR PRESENTATION OF TRAIN VS TEST!!!!!!!!!!!!!!!!!!
prediction_tbl %>% 
  select(date, .pred) %>% 
  right_join(buoy_tbl, by = c("date")) %>% 
  #mutate(type = ifelse(is.na(.pred), "original", "forecast")) %>%
  ggplot(aes(x = date, y = value, col = "original")) +
  geom_line() +
  geom_line(aes(x = date, y = .pred, col = "forecast")) + 
  labs(color = "type") + 
  facet_zoom(xy = date > "2018-03-01" & date <= "2019-01-10", horizontal = FALSE) + # first date: 2018-12-25
  ggtitle(label = "Historical Significant Wave Height & Forecast vs Actual", 
          subtitle = "San Clemente Basin (46086)") + 
  xlab("") + ylab("Wave Height\n") + 
  theme(legend.position = "bottom")

# Calculating forecast error
prediction_tbl %>% metrics(value, .pred) %>% write.csv("xgb_test_error.csv")

# visualize residuals of test set
prediction_tbl %>%
  ggplot(aes(x = date, y = value - .pred)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth() +
  theme_tq() +
  labs(title = "Test Set: XGBoost Model Residuals", x = "") +
  scale_y_continuous(limits = c(-5, 5))

# Make Predictions --------------------------------------------------------

# create a table with vlaues of days into the future
future_tbl <- buoy_tbl %>% 
  tk_index() %>% 
  # tk_make_future_timeseries(n_future = 121) %>% !!! changed to 10 days !!!
  tk_make_future_timeseries(n_future = 10) %>% 
  tk_get_timeseries_signature()

# predict on future time 
future_predictions_tbl <- predict(model_xgboost, new_data = future_tbl)

# combine future table with future predictions
future_predictions_tbl <- future_predictions_tbl %>% 
  bind_cols(future_tbl) %>% 
  select(index, .pred) %>% 
  rename(date = index, value = .pred) # !!!!!!!change ".pred" in first visual below or comment out "value = .pred!!!!!

# visualize predictions
buoy_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2015-01-01")), # 2018-07-01
            xmax = as.numeric(ymd("2018-03-14")), # 2018-12-31
            ymin = 0, ymax = 6,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2019-01-01")),
            xmax = as.numeric(ymd("2019-05-01")),
            ymin = 0, ymax = 6,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2017-10-01"), y = 4.75,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2018-10-01"), y = 4.5,
           color = palette_light()[[1]], label = "Test Region") +
  annotate("text", x = ymd("2019-2-15"), y = 4,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_line() + 
  # future data
  geom_line(aes(x = date, y = value), data = future_predictions_tbl, color = "red") +
  labs(title = "Buoy 46086: 10 day forecast", x = "") +
  theme_tq()

# Alternative visuals !!!!!!!!!!!!!!! USE THIS FOR 10 DAY FORECAST !!!!!!!!!!!!!!

buoy_tbl %>% 
  select(date, value) %>% 
  mutate(type = "original") %>% 
  bind_rows(future_predictions_tbl) %>% 
  mutate(type = ifelse(is.na(type), "forecast", type)) %>% 
  ggplot(aes(x = date, y = value, col = type)) + 
  geom_line() + 
  facet_zoom(xy = date > "2018-12-25" & date <= "2019-01-10", horizontal = FALSE) + 
  ggtitle(label = "Historical Significant Wave Height & 10 day Forecast", 
          subtitle = "Santa Monica Bay (46221)") + # toggle for differnt buoys
  xlab("") + ylab("Wave Height\n") + # removed date xlab
  theme(legend.position = "bottom")

