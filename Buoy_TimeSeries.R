
# Import Libraries --------------------------------------------------------

# import noaa buoy data
library(rnoaa)
library(ncdf4)
# plotting and data wrangling
library(dplyr)
library(ggplot2)
library(lubridate)
# forecasting
library(fpp3)
# advanced visuals
library(gridExtra)
library(ggridges)
library(forcats)
library(clifro)
library(seasonal)
library(zoo)
library(xts)
library(marmap)
library(ggforce)
library(lattice)
library(repr)

# Data Import -------------------------------------------------------------

# create empty dataframe
df_multiyear_hour <- data.frame()

# start loop for multi buoy multi year extract
for (b in c(46221,46086)) { # extract multiple buoys Santa Monica Bay & San Clemente Basin Respectively
  # outer loop
  for (i in 2015:2018) { # extract multiple years 
    # extract yearly buoy data
    dat <- buoy(dataset = "stdmet", buoyid = b, year = i)$data
    # label each data set
    dat$buoy_id <- b
    # append each buoy/years to data frame
    df_multiyear_hour <- bind_rows(df_multiyear_hour, dat)
  }
} # end

# Data Import w File ------------------------------------------------------

# # import from csv if needed
# df_multiyear_hour <- read.csv("buoy_data.csv", 
#                               stringsAsFactors=FALSE)
# #drop row label column
# df_multiyear_hour$X <- NULL

# Data Wrangling ----------------------------------------------------------

# format time field as month day year hour
df_multiyear_hour <- df_multiyear_hour %>% 
  mutate(dt = ymd_h(
    paste(substr(x = df_multiyear_hour$time, start = 1, 10),
          substr(x = df_multiyear_hour$time, start = 12, 13), 
          sep = " ")
  ))

# visualize hourly missing values in both buoys *750 width*
df_multiyear_hour %>% 
  select(buoy_id, dt, time, wave_height) %>%
  filter(!are_duplicated(df_multiyear_hour, key = buoy_id, index = dt)) %>% # filter any duplicate key, index pairs
  mutate(buoy_id = as.factor(buoy_id)) %>% 
  as_tsibble(key = buoy_id, index = dt) %>% 
  count_gaps(.full = TRUE) %>%
  ggplot(aes(x = buoy_id, colour = buoy_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from)) +
  geom_point(aes(y = .to)) +
  coord_flip() +
  theme(legend.position = "bottom") + 
  ggtitle(label = "Implicit Gaps in Hourly Historical Significant Wave Height",
          subtitle = "Santa Monica Bay (46221)\nSan Clemente Basin (46086)")

# fill implicit gaps in hourly data and convert to daily data and *tsibble* object
df_multiyear_day <- df_multiyear_hour %>%
  filter(!are_duplicated(df_multiyear_hour, key = buoy_id, index = dt)) %>% # filter any duplicate key, index pairs
  select(dt, everything(), -time, -dewpt_temperature, -visibility, -water_level, -lat, -lon) %>%
  select(buoy_id, dt, everything()) %>% 
  as_tsibble(key = buoy_id, index = dt) %>% # coerce to tsibble to find,count, and fill implicit gaps
  tsibble::fill_gaps() %>% 
  fill(wind_dir:sea_surface_temperature, .direction = "down") %>% 
  group_by(buoy_id) %>% 
  index_by(date = as_date(dt)) %>% 
  summarise_at(vars(wind_dir:sea_surface_temperature), mean) %>% 
  dplyr::slice(-1) # remove prior record from min year

# Time Series Visualization -----------------------------------------------

# visualize daily time series with smoothed trend *750 Width*
df_multiyear_day %>% 
  mutate(date = ymd(date),
         year = year(date)) %>% 
  ggplot(aes(x = date, y = wave_height)) +
  geom_line(size = 1) + 
  geom_smooth() + 
  facet_wrap(~ buoy_id, nrow = 2) + 
  scale_color_discrete(name = "Year") +
  labs(x = "", y = "wave height (m)") + 
  ggtitle(label = "Daily Mean Wave Heights: 2015 - 2018", 
          subtitle = "San Clemente Basin (46086)\nSanta Monica Bay (46221)" )

# visualize daily time series with smoothed trend per year
df_multiyear_day %>% 
  mutate(date = ymd(date),
         year = year(date)) %>% 
  ggplot(aes(x = date, y = wave_height, color = as.character(year))) + 
  geom_line(size = 1) + 
  geom_smooth(aes(group = as.character(year)), color = "black", size=0.5) + 
  facet_wrap(~ buoy_id, nrow = 2) + 
  scale_color_discrete(name = "Year") +
  labs(x = "", y = "wave height (m)") + 
  ggtitle(label = "Daily Mean Wave Heights: 2015 - 2018", 
          subtitle = "Santa Monica Bay (46086)\nSan Clemente Basin (46221)" ) + 
  theme(legend.position = 'bottom') # changed legend to bottom

# visualize month seasonality
df_multiyear_day %>% 
  as_tibble() %>% # convert to tibble from tsibble
  mutate(date = ymd(date),
         year = year(date),
         month = month(date, label = TRUE, abbr = TRUE)) %>% 
  group_by(buoy_id, year, month) %>% 
  summarise(month_avg = mean(wave_height)) %>% 
  ggplot() + 
  geom_line(aes(x = month, y = month_avg, 
                group = as.character(year),
                color = as.character(year)), size = 2) + 
  facet_wrap( ~ buoy_id, nrow = 2) +
  scale_color_discrete(name = "Year") + 
  labs(x = "", y = "wave height (m)") + 
  ggtitle(label = "Mean Wave Heights per Month", 
          subtitle = "San Clemente Basin (46086)\nSanta Monica Bay (46221)" ) + 
  theme(legend.position = 'bottom') # changed legend to bottom

# ggridges plot to display wave height distribution for monthly season for years 15,16,17,18
df_multiyear_day %>% 
  as_tibble() %>% 
  mutate(month = month(date, label = TRUE, abbr = FALSE),
         month = fct_relevel(month, 
                             "December", "November", "October", "September",
                             "August", "July", "June", "May",
                             "April", "March", "February", "January")) %>%   #%>% str()
  ggplot(aes(x = wave_height, y = month, fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) + 
  facet_wrap( ~ buoy_id, ncol = 2) +
  scale_fill_viridis_c(name = "Wave Height", option = "C") + 
  ggtitle(label = "Daily Mean Wave Heights by Month 2015-18", 
          subtitle = "San Clemente Basin (46086)\nSanta Monica Bay (46221)") + 
  labs(x = "wave height (m)", y = "") + 
  theme(legend.position = "bottom")

# build windrose chart faceted by month to view monthly seasonal wave direction and period
df_multiyear_day %>% 
  as_tibble() %>% 
  with(windrose(average_wpd, mean_wave_dir, as.character(buoy_id), n_col = 2, legend_title = "Wave Period") + 
         ggtitle(label = "Daily Mean Wave Period & Directions", 
                 subtitle = "San Clemente Basin (46086)\nSanta Monica Bay (46221)"))

# build windrose chart faceted by month to view monthly seasonal wave direction and period
df_multiyear_day %>% 
  as_tibble() %>% 
  filter(buoy_id == 46086) %>% # san clemente
  mutate(month = month(date, label = TRUE)) %>% 
  with(windrose(average_wpd, mean_wave_dir, month, n_col = 3, legend_title = "Wave Period") + 
         ggtitle(label = "Daily Mean Wave Period & Directions", 
                 subtitle = "San Clemente Basin (46086)"))

# build windrose chart faceted by month to view monthly seasonal wave direction and period
df_multiyear_day %>% 
  as_tibble() %>% 
  filter(buoy_id == 46221) %>% # santa monica bay
  mutate(month = month(date, label = TRUE)) %>% 
  with(windrose(average_wpd, mean_wave_dir, month, n_col = 3, legend_title = "Wave Period") + 
         ggtitle(label = "Daily Mean Wave Period & Directions", 
                 subtitle = "Santa Monica Bay (46221)"))

# wave period, direction, and height facet plot
df_multiyear_day %>% 
  as_tibble() %>% 
  filter(buoy_id == 46086) %>% 
  select(date, wave_height, mean_wave_dir, average_wpd) %>% 
  pivot_longer(-date, names_to = "type", values_to = "measure") %>% 
  ggplot(aes(x = date, y = measure, col = type)) + 
  geom_line() + 
  facet_grid(type ~ ., scales = "free") + 
  labs(x = "", y = "wave_height(m)    /    mean_wave_dir(deg)    /    average_wpd(sec)") + 
  ggtitle(label = "Wave Period, Direction, & Height: 2015 -2018", subtitle = "San Clemente Basin (46086)")

df_multiyear_day %>% 
  as_tibble() %>% 
  filter(buoy_id == 46221) %>% 
  select(date, wave_height, mean_wave_dir, average_wpd) %>% 
  pivot_longer(-date, names_to = "type", values_to = "measure") %>% 
  ggplot(aes(x = date, y = measure, col = type)) + 
  geom_line() + 
  facet_grid(type ~ ., scales = "free") + 
  labs(x = "", y = "wave_height(m)    /    mean_wave_dir(deg)    /    average_wpd(sec)") + 
  ggtitle(label = "Wave Period, Direction, & Height: 2015 -2018", subtitle = "Santa Monica Bay (46221)")

# autocorrelation
df_multiyear_day %>% 
  ACF(wave_height, lag_max = 365) %>% 
  autoplot() + 
  scale_x_continuous(breaks = seq(0,365,30)) + 
  ggtitle(label = "Autocorrelation w/ 365 Day Lag Window",
          subtitle = "San Clemente Basin (46086)\nSanta Monica Bay (46221)")

## Anomalies ##

# build anomaly wave height plot (from flowing data tutorial)
avg_multiyear <- df_multiyear_day %>% 
  as_tibble() %>% 
  filter(buoy_id == 46086) %>% # san clemente buoy
  filter(!is.na(wave_height)) %>% 
  mutate(doy = yday(date)) %>% 
  group_by(doy) %>% 
  summarise(doytavg = mean(wave_height)) #change to doywavg after

# created smoothed curve
avg_multiyear$rollavg20 <- rollmean(avg_multiyear$doytavg, 20, fill = c(NA,NA,NA))

# create a smoothed line
avg_multiyear <- mutate(avg_multiyear, doy.2 = ifelse(doy > 345, doy -366, doy)) %>% 
  arrange(doy.2) %>% 
  mutate(rollavg20.2 = rollmean(doytavg, 20, fill = c(NA,NA,NA))) %>% 
  mutate(rollavg20 = ifelse(is.na(rollavg20), rollavg20.2, rollavg20)) %>% 
  select(-doy.2, -rollavg20.2)

# average with daily plotting dataframe
avgt1518 <- df_multiyear_day %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  left_join(avg_multiyear, by = "doy") %>% 
  mutate(avgtdiff = wave_height - rollavg20)

# create data to add month labels
midmonths <- seq(ymd("2019-01-16"), ymd("2019-12-16"), by = "months")
midmonths <- yday(midmonths)

# create data to add month bands
bands.start <- seq(ymd("2019-02-01"), ymd("2019-12-01"), by = "2 months")
bands.start <- yday(bands.start)
bands.end <- seq(ymd("2019-03-01"), ymd("2020-01-01"), by = "2 months") - 1
bands.end <- yday(bands.end)
bands <- cbind.data.frame(bands.start, bands.end)

# plot results
ggplot(avgt1518) + 
  theme_minimal() +
  geom_rect(data = bands, 
            aes(xmin = bands.start, xmax = bands.end),
            ymin = 0, ymax = 5,
            fill = "#cccccc", alpha = 0.5) + 
  geom_rect(aes(xmin = doy - 0.5, 
                xmax = doy + 0.5, 
                ymin = rollavg20, 
                ymax = wave_height, 
                fill = avgtdiff)) + 
  geom_line(aes(x = doy, y = rollavg20), color = "#777777") + 
  scale_fill_gradient2(low = "blue", high = "red", name = "Wave Height Anomaly") + 
  facet_wrap(~ year, ncol = 1) + 
  scale_x_continuous(breaks = midmonths, 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  ylab("") + xlab("") + 
  scale_y_continuous(position = "right") + 
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(hjust = .5, face = "bold")
  ) + 
  ggtitle(label = "Daily Mean Wave Anomalies", 
          subtitle = "San Clemente Basin (46086)")


# plot results as polar plot for only 2018
avgt1518 %>% 
  filter(year(date) == 2018) %>% 
  ggplot() + 
  theme_minimal() +
  geom_rect(data = bands, 
            aes(xmin = bands.start, xmax = bands.end),
            ymin = 0, ymax = 5,
            fill = "#cccccc", alpha = 0.5) + 
  geom_rect(aes(xmin = doy - 0.5, 
                xmax = doy + 0.5, 
                ymin = rollavg20, 
                ymax = wave_height, 
                fill = avgtdiff)) + 
  geom_line(aes(x = doy, y = rollavg20), color = "#777777") + 
  scale_fill_gradient2(low = "blue", high = "red", name = "Wave Height Anomaly") + 
  facet_wrap(~ year, ncol = 2) + 
  scale_x_continuous(breaks = midmonths, 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  ylab("") + xlab("") + 
  scale_y_continuous(position = "right") + 
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold")
  ) + 
  coord_polar()

# build anomaly wave height plot (from flowing data tutorial)
avg_multiyear <- df_multiyear_day %>% 
  as_tibble() %>% 
  filter(buoy_id == 46221) %>% # san clemente buoy
  filter(!is.na(wave_height)) %>% 
  mutate(doy = yday(date)) %>% 
  group_by(doy) %>% 
  summarise(doytavg = mean(wave_height)) #change to doywavg after

# created smoothed curve
avg_multiyear$rollavg20 <- rollmean(avg_multiyear$doytavg, 20, fill = c(NA,NA,NA))

# create a smoothed line
avg_multiyear <- mutate(avg_multiyear, doy.2 = ifelse(doy > 345, doy -366, doy)) %>% 
  arrange(doy.2) %>% 
  mutate(rollavg20.2 = rollmean(doytavg, 20, fill = c(NA,NA,NA))) %>% 
  mutate(rollavg20 = ifelse(is.na(rollavg20), rollavg20.2, rollavg20)) %>% 
  select(-doy.2, -rollavg20.2)

# daily plotting dataframe
# avgt1518 <- z %>% 
#   fortify.zoo(z) %>% 
#   rename(dt = "Index", mean_wave_height = ".") %>% 
#   filter(dt > "2014-12-31") %>% 
avgt1518 <- df_multiyear_day %>% 
  mutate(doy = yday(date),
         year = year(date)) %>% 
  left_join(avg_multiyear, by = "doy") %>% 
  mutate(avgtdiff = wave_height - rollavg20)

# create data to add month labels
midmonths <- seq(ymd("2019-01-16"), ymd("2019-12-16"), by = "months")
midmonths <- yday(midmonths)

# create data to add month bands
bands.start <- seq(ymd("2019-02-01"), ymd("2019-12-01"), by = "2 months")
bands.start <- yday(bands.start)
bands.end <- seq(ymd("2019-03-01"), ymd("2020-01-01"), by = "2 months") - 1
bands.end <- yday(bands.end)
bands <- cbind.data.frame(bands.start, bands.end)

# plot results
ggplot(avgt1518) + 
  theme_minimal() +
  geom_rect(data = bands, 
            aes(xmin = bands.start, xmax = bands.end),
            ymin = 0, ymax = 5,
            fill = "#cccccc", alpha = 0.5) + 
  geom_rect(aes(xmin = doy - 0.5, 
                xmax = doy + 0.5, 
                ymin = rollavg20, 
                ymax = wave_height, 
                fill = avgtdiff)) + 
  geom_line(aes(x = doy, y = rollavg20), color = "#777777") + 
  scale_fill_gradient2(low = "blue", high = "red", name = "Wave Height Anomaly") + 
  facet_wrap(~ year, ncol = 1) + 
  scale_x_continuous(breaks = midmonths, 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  ylab("") + xlab("") + 
  scale_y_continuous(position = "right") + 
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(hjust = .5, face = "bold")
  ) + 
  ggtitle(label = "Daily Mean Wave Period & Directions", 
          subtitle = "Santa Monica Bay (46221)")

# plot results as polar plot for only 2018
avgt1518 %>% 
  filter(year(date) == 2018) %>% 
  ggplot() + 
  theme_minimal() +
  geom_rect(data = bands, 
            aes(xmin = bands.start, xmax = bands.end),
            ymin = 0, ymax = 5,
            fill = "#cccccc", alpha = 0.5) + 
  geom_rect(aes(xmin = doy - 0.5, 
                xmax = doy + 0.5, 
                ymin = rollavg20, 
                ymax = wave_height, 
                fill = avgtdiff)) + 
  geom_line(aes(x = doy, y = rollavg20), color = "#777777") + 
  scale_fill_gradient2(low = "blue", high = "red", name = "Wave Height Anomaly") + 
  facet_wrap(~ year, ncol = 2) + 
  scale_x_continuous(breaks = midmonths, 
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  ylab("") + xlab("") + 
  scale_y_continuous(position = "right") + 
  theme(
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold")
  ) + 
  coord_polar()

# Time Series Forecasting -------------------------------------------------

# plot yearly sesonality
df_multiyear_day %>%
  gg_season(wave_height) +
  ggtitle(label = "Historical Significant Wave Height",
          subtitle = "San Clemente Basin (46086)\nSanta Monia Bay (46221)")

# plot relationship between varialbes (confirm windspeed to wave height relationship)
df_multiyear_day %>%
  filter(buoy_id == 46086) %>%
  GGally::ggpairs(columns = c("wind_spd", "wave_height", "dominant_wpd", "air_temperature")) + 
  ggtitle(label = "Historical Significant Wave Height",
          subtitle = "Combined:\nSan Clemente Basin (46086)\nSanta Monica Bay (46221)")

# time series model building for both buoys
ts_fits <- df_multiyear_day %>% 
  model(
    ets = ETS(wave_height),
    arima = ARIMA(wave_height),
    snaive = SNAIVE(wave_height),
    nnetar = NNETAR(wave_height)
  )

# view accuracy of training
fabletools::accuracy(ts_fits) %>% write.csv("ts_train_error.csv", row.names = FALSE)

# visualize model fits for both buoys
ts_fits %>% 
  augment() %>% 
  ggplot() + 
  geom_line(aes(x = date, y = wave_height)) + 
  geom_line(aes(x = date, y = .fitted, col = .model), size = .20) + 
  facet_grid(.model ~ buoy_id) + 
  ggtitle(label = "Historical Significant Wave Height & Fitted Values", 
          subtitle = "San Clemente Basin (46086)\nSanta Monica Bay (46221)") + 
  theme(legend.position = "bottom")

# produce forecast 10 days ahead
ts_fc <- ts_fits %>% 
  forecast(h = 10)

# visualize actuals with 10 day future forecast *change forecast type filter*
obs <- ts_fits %>% 
  augment() %>% 
  filter(.model == "nnetar") %>% # currently neural net
  as_tibble() %>% 
  select(buoy_id, date, wave_height) %>% 
  mutate(type = "original")

new <- ts_fc %>% 
  as_tibble() %>% 
  filter(.model == "nnetar") %>% # currently neural net
  select(buoy_id, date, wave_height) %>% 
  mutate(type = "forecast")

rbind(obs, new) %>% 
  filter(buoy_id == 46086) %>% # view san clemente basin only
  ggplot(aes(x = date, y = wave_height, col = type)) + 
  geom_line() + 
  facet_zoom(xy = date > "2018-12-25" & date <= "2019-01-10", horizontal = FALSE) + 
  ggtitle(label = "Historical Significant Wave Height & 10 day Forecast", 
          subtitle = "San Clemente Basin (46086)") + 
  xlab("\nDate") + ylab("Wave Height\n") + 
  theme(legend.position = "bottom")

# visualize actuals with 10 day future forecast *change forecast type filter*
rbind(obs, new) %>% 
  filter(buoy_id == 46221) %>% # view santa monica bay only
  ggplot(aes(x = date, y = wave_height, col = type)) + 
  geom_line() + 
  facet_zoom(xy = date > "2018-12-25" & date <= "2019-01-10", horizontal = FALSE) + 
  ggtitle(label = "Historical Significant Wave Height & 10 day Forecast", 
          subtitle = "Santa Monica Bay (46221)") + 
  xlab("") + ylab("Wave Height\n") + # removed date xlab
  theme(legend.position = "bottom")

## forecast accuracy calculations: https://robjhyndman.com/hyndsight/fable/ ##

# create training data up to December 21 2018 to forecast last 10 days
ts_train <- df_multiyear_day %>% 
  filter(date <= "2018-12-21")

# model on training set and introduce ensemble forecast
ts_train_fits <- ts_train %>% 
  model(
    ets = ETS(wave_height),
    arima = ARIMA(wave_height),
    snaive = SNAIVE(wave_height),
    nnetar = NNETAR(wave_height)
  ) %>% 
  mutate(mixed = (ets + arima + snaive) / 3) # ensemble

# forecast using training set trained models *10 day forecast*
ts_train_fc <- ts_train_fits %>% forecast(h = 10)

# visualize forecast vs actuals zoom in on December
ts_train_fc %>% 
  autoplot(filter(df_multiyear_day, date > "2018-12-01"), level = NULL) + 
  ggtitle(label = "Historical Significant Wave Height & 10 day Forecast vs Actual", 
          subtitle = "San Clemente Basin (46086)\nSanta Monica Bay (46221)") + 
  theme(legend.position = "bottom")

# check accuracy stats for test e.g. MAE 
fabletools::accuracy(ts_train_fc, df_multiyear_day) %>% write.csv("ts_test_error.csv")

