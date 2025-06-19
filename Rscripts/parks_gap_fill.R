# install.packages("weathercan",
#                  repos = c("https://ropensci.r-universe.dev",
#                            "https://cloud.r-project.org"))

rm(list=ls())

#______________________________----
# Libraries ----
library(tidyverse)    # Tidy data
library(splines)      # Spline-fitting
library(psych)        # Pairs plot
library(lubridate)    # Wrangling dates
library(dplyr)        # Tidy notation
library(scales)       # Rescaling data and alpha
library(forecast)     # tsclean()
library(purrr)        # Functional programming tools
library(weathercan)   # Get weather station details from Environment Canada
library(patchwork)    # Tidy organizing of plots
library(wesanderson)  # Nice color palettes
library(modelr)       # Tidy modeling
library(broom)

#______________________________----
# Constants ----
col_pal <- wes_palette("Darjeeling1")
setwd("~/Desktop/Workspace")
time_zone <- "Canada/Central"
time_unit <- "hour"
weather_station <- "CHURCHILL"
data_output <- "./Parks/"
location <- "parks"
p <- c("parks_EnvCan_20050926_20241005")

#______________________________----
# Functions----
source("./Earthwatch/R_functions/R_functions.R")

#______________________________----
# Read in station data ----
df <- read.csv("./Parks/data/parks_microclimate.csv", header = TRUE)

# Converts to date format (lubridate)
df$date <- ymd(df$date)

p1 <- df %>% 
  ggplot(aes(x = date, y = mlk_temp_150_avg)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = mlk_temp_0_avg), color = col_pal[3]) +
  geom_line(aes(y = mlk_temp_80_avg), color = col_pal[2])

p2 <- df %>% 
  ggplot(aes(x = date, y = rlk_temp_150_avg)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = rlk_temp_0_avg), color = col_pal[3]) +
  geom_line(aes(y = rlk_temp_80_avg), color = col_pal[2])

p1 / p2 

#______________________________----
# 👾 Step 1: Environment Canada data ----
# If recent data are needed, download here.
# Otherwise, read in the existing data below

# # Get the correct station ID, make a df using that ID and the time period of interest, and download
# temp_df <- tibble(
#   date = as_date(c(min(df$date), max(df$date)))
# )
# weather_df <- weather_download(temp_df, weather_station, time_zone, time_unit, location)

# If already downloaded:
weather_df <- read_most_recent_weather(data_output, p)

weather_df %>% ggplot(aes(x = as_datetime(datetime), y = station_temp)) + geom_line()

df <- weather_join(weather_df, df)

df %>% ggplot(aes(x = as_datetime(date), y = mean.150)) + geom_line()

#______________________________----
# 🫶 Step 2: Fill missing air temperatures ----

# Now can fill each month using EnvCan
# Select columns that match the criteria
air_t_cols <- df %>%
  select(matches("150"), -matches("neg150"), -matches("hum")) %>%
  names()

df %>%
  mutate(month = as.integer(month(date))) %>%
  group_by(month) %>%
  nest() %>%
  mutate(has_fill_var = map_lgl(data, ~ "mean.150" %in% colnames(.x))) %>%
  select(month, has_fill_var)

df <- df %>%
  mutate(mean.150 = ifelse(is.na(mean.150), mean(mean.150, na.rm = TRUE), mean.150)) %>% 
  mutate(mean.150 = as.numeric(mean.150))

# Fill missing values using linear models
df <- df %>%
  mutate(month = as.integer(month(date)),
         mean.150 = as.numeric(mean.150)) %>%  # Ensure numeric type
  group_by(month) %>%
  nest() %>%
  mutate(filled = map(data, ~ fill_missing_with_lm(.x, vars = air_t_cols, fill_var = "mean.150"))) %>%
  select(month, filled) %>%
  unnest(cols = c(filled)) %>%
  arrange(date) %>%
  ungroup()

df %>% ggplot(aes(x = date, y = !!sym(air_t_cols[1]))) + geom_line()
df %>% ggplot(aes(x = date, y = !!sym(air_t_cols[2]))) + geom_line()
df %>% ggplot(aes(x = date, y = !!sym(air_t_cols[3]))) + geom_line()
df %>% ggplot(aes(x = date, y = !!sym(air_t_cols[4]))) + geom_line()
df %>% ggplot(aes(x = date, y = tsclean(!!sym(air_t_cols[5])))) + geom_line()
df %>% mutate(rlk_temp_150_min = ifelse(rlk_temp_150_min < -40, -40, rlk_temp_150_min)) %>% 
  ggplot(aes(x = date, y = tsclean(!!sym(air_t_cols[6])))) + geom_line()

df <- df %>% 
  mutate(
    rlk_temp_150_max = tsclean(rlk_temp_150_max),
    rlk_temp_150_min = tsclean(rlk_temp_150_min),
    rlk_temp_150_min = ifelse(rlk_temp_150_min < -40, -40, rlk_temp_150_min)
    )

# Create the initial ggplot object with the x-aesthetic
p <- ggplot(df, aes(x = date))

# Use lapply to create a list of geom_line layers for each selected column
line_layers <- lapply(1:length(air_t_cols), function(i) {
  geom_line(aes(y = .data[[air_t_cols[i]]]), color = i)  # Using i as a placeholder for color
})

# Add all the geom_line layers to the ggplot object
p + line_layers

df <- df %>% arrange(date)

#______________________________----
# 🤠 Step 3: Fill the missing ground surface temperatures ----

# Select columns that end with a zero and do not contain other numbers
ground_surface_cols <- df %>%
  select(matches("_0(\\b|_)")) %>%
  colnames()

#_________----
## Mary Lake - avg ground surface T ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_0_avg)) %>% 
  do(model = lm(mlk_temp_0_avg ~ mlk_temp_150_avg + rlk_temp_150_avg + mean.150, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_0_avg = ifelse(is.na(mlk_temp_0_avg), pred, mlk_temp_0_avg))
mp %>% ggplot(aes(x = date, y = .data[[ground_surface_cols[1]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[ground_surface_cols[1]]]), color = "red")
df[[ground_surface_cols[1]]] <- mp[[ground_surface_cols[1]]]

#_________----
## Mary Lake - max ground surface T ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_0_max)) %>% 
  do(model = lm(mlk_temp_0_max ~ mlk_temp_150_max + rlk_temp_150_max + mean.150, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_0_max = ifelse(is.na(mlk_temp_0_max), pred, mlk_temp_0_max))
mp %>% ggplot(aes(x = date, y = .data[[ground_surface_cols[2]]])) + geom_line() +
  geom_line(data = df, aes(y = tsclean(.data[[ground_surface_cols[2]]])), color = "red")
df[[ground_surface_cols[2]]] <- tsclean(mp[[ground_surface_cols[2]]])

#_________----
## Mary Lake - min ground surface T ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_0_min)) %>% 
  do(model = lm(mlk_temp_0_min ~ mlk_temp_150_min + rlk_temp_150_min + mean.150, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_0_min = ifelse(is.na(mlk_temp_0_min), pred, mlk_temp_0_min))
mp %>% ggplot(aes(x = date, y = .data[[ground_surface_cols[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[ground_surface_cols[3]]]), color = "red")
df[[ground_surface_cols[3]]] <- tsclean(mp[[ground_surface_cols[3]]])

#_________----
## Roberge Lake - avg ground surface T ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_0_avg)) %>% 
  do(model = lm(rlk_temp_0_avg ~ mlk_temp_150_avg + rlk_temp_150_avg + mean.150 + mlk_temp_0_avg, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_0_avg = ifelse(is.na(rlk_temp_0_avg), pred, rlk_temp_0_avg))
mp %>% ggplot(aes(x = date, y = .data[[ground_surface_cols[4]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[ground_surface_cols[4]]]), color = "red")
df[[ground_surface_cols[4]]] <- mp[[ground_surface_cols[4]]]

#_________----
## Roberge Lake - max ground surface T ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_0_max)) %>% 
  do(model = lm(rlk_temp_0_max ~ mlk_temp_150_max + rlk_temp_150_max + mean.150 + mlk_temp_0_max, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_0_max = ifelse(is.na(rlk_temp_0_max), pred, rlk_temp_0_max))
mp %>% ggplot(aes(x = date, y = .data[[ground_surface_cols[5]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[ground_surface_cols[5]]]), color = "red")
df[[ground_surface_cols[5]]] <- mp[[ground_surface_cols[5]]]

#_________----
## Roberge Lake - min ground surface T ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_0_min)) %>% 
  do(model = lm(rlk_temp_0_min ~ mlk_temp_150_min + rlk_temp_150_min + mean.150 + mlk_temp_0_min, data = .)) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_0_min = ifelse(is.na(rlk_temp_0_min), pred, rlk_temp_0_min))
mp %>% ggplot(aes(x = date, y = .data[[ground_surface_cols[6]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[ground_surface_cols[6]]]), color = "red")
df[[ground_surface_cols[6]]] <- mp[[ground_surface_cols[6]]]

#______________________________----
# 👀 Step 4: Fill the missing -2.5 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_1 <- df %>%
  select(matches("_2p5_")) %>%
  colnames()

#_________----
## Roberge Lake - avg -2.5 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_2p5_avg)) %>% 
  do(model = step(lm(rlk_temp_2p5_avg ~ rlk_temp_0_avg + rlk_temp_150_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_2p5_avg = ifelse(is.na(rlk_temp_2p5_avg), pred, rlk_temp_2p5_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_1[1]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_1[1]]]), color = "red")
df[[sub_surface_cols_1[1]]] <- mp[[sub_surface_cols_1[1]]]

#_________----
## Roberge Lake - max -2.5 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_2p5_max)) %>% 
  do(model = step(lm(rlk_temp_2p5_max ~ rlk_temp_0_max + rlk_temp_150_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_2p5_max = ifelse(is.na(rlk_temp_2p5_max), pred, rlk_temp_2p5_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_1[2]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_1[2]]]), color = "red")
df[[sub_surface_cols_1[2]]] <- mp[[sub_surface_cols_1[2]]]


#_________----
## Roberge Lake - min -2.5 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_2p5_min)) %>% 
  do(model = step(lm(rlk_temp_2p5_min ~ rlk_temp_0_min + rlk_temp_150_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_2p5_min = ifelse(is.na(rlk_temp_2p5_min), pred, rlk_temp_2p5_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_1[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_1[3]]]), color = "red")
df[[sub_surface_cols_1[3]]] <- mp[[sub_surface_cols_1[3]]]

#______________________________----
# 👀 Step 5: Fill the missing -5 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_2 <- df %>%
  select(matches("_5_")) %>%
  colnames()

#_________----
## Roberge Lake - avg -5 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_5_avg)) %>% 
  do(model = step(lm(rlk_temp_5_avg ~ rlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_150_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_5_avg = ifelse(is.na(rlk_temp_5_avg), pred, rlk_temp_5_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_2[1]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_2[1]]]), color = "red")
df[[sub_surface_cols_2[1]]] <- mp[[sub_surface_cols_2[1]]]

#_________----
## Roberge Lake - max -5 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_5_max)) %>% 
  do(model = step(lm(rlk_temp_5_max ~ rlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_150_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_5_max = ifelse(is.na(rlk_temp_5_max), pred, rlk_temp_5_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_2[2]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_2[2]]]), color = "red")
df[[sub_surface_cols_2[2]]] <- mp[[sub_surface_cols_2[2]]]

#_________----
## Roberge Lake - min -5 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_5_min)) %>% 
  do(model = step(lm(rlk_temp_5_min ~ rlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_150_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_5_min = ifelse(is.na(rlk_temp_5_min), pred, rlk_temp_5_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_2[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_2[3]]]), color = "red")
df[[sub_surface_cols_2[3]]] <- mp[[sub_surface_cols_2[3]]]

#______________________________----
# 👀 Step 6: Fill the missing -10 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_3 <- df %>%
  select(matches("_10_")) %>%
  colnames()

#_________----
## Roberge Lake - avg -10 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_10_avg)) %>% 
  do(model = step(lm(rlk_temp_10_avg ~ rlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_5_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_10_avg = ifelse(is.na(rlk_temp_10_avg), pred, rlk_temp_10_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_3[1]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_3[1]]]), color = "red")
df[[sub_surface_cols_3[1]]] <- mp[[sub_surface_cols_3[1]]]

#_________----
## Roberge Lake - max -10 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_10_max)) %>% 
  do(model = step(lm(rlk_temp_10_max ~ rlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_5_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_10_max = ifelse(is.na(rlk_temp_10_max), pred, rlk_temp_10_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_3[2]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_3[2]]]), color = "red")
df[[sub_surface_cols_3[2]]] <- mp[[sub_surface_cols_3[2]]]

#_________----
## Roberge Lake - min -10 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_10_min)) %>% 
  do(model = step(lm(rlk_temp_10_min ~ rlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_5_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_10_min = ifelse(is.na(rlk_temp_10_min), pred, rlk_temp_10_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_3[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_3[3]]]), color = "red")
df[[sub_surface_cols_3[3]]] <- mp[[sub_surface_cols_3[3]]]

#______________________________----
# 👀 Step 7: Fill the missing -15 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_3a <- df %>%
  select(matches("_15_")) %>%
  colnames()

#_________----
## Roberge Lake - avg -15 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_15_avg)) %>% 
  do(model = step(lm(rlk_temp_15_avg ~ rlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_5_avg + rlk_temp_10_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_15_avg = ifelse(is.na(rlk_temp_15_avg), pred, rlk_temp_15_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_3a[1]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_3a[1]]]), color = "red")
df[[sub_surface_cols_3a[1]]] <- mp[[sub_surface_cols_3a[1]]]

#_________----
## Roberge Lake - max -15 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_15_max)) %>% 
  do(model = step(lm(rlk_temp_15_max ~ rlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_5_max + rlk_temp_10_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_15_max = ifelse(is.na(rlk_temp_15_max), pred, rlk_temp_15_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_3a[2]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_3a[2]]]), color = "red")
df[[sub_surface_cols_3a[2]]] <- mp[[sub_surface_cols_3a[2]]]

#_________----
## Roberge Lake - min -15 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_15_min)) %>% 
  do(model = step(lm(rlk_temp_15_min ~ rlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_5_min + rlk_temp_10_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_15_min = ifelse(is.na(rlk_temp_15_min), pred, rlk_temp_15_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_3a[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_3a[3]]]), color = "red")
df[[sub_surface_cols_3a[3]]] <- mp[[sub_surface_cols_3a[3]]]

#______________________________----
# 👀 Step 8: Fill the missing -20 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_4 <- df %>%
  select(matches("_20_")) %>%
  colnames()

#_________----
## Roberge Lake - avg -20 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_20_avg)) %>% 
  do(model = step(lm(rlk_temp_20_avg ~ rlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_5_avg + rlk_temp_10_avg + rlk_temp_15_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_20_avg = ifelse(is.na(rlk_temp_20_avg), pred, rlk_temp_20_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_4[4]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_4[4]]]), color = "red")
df[[sub_surface_cols_4[4]]] <- mp[[sub_surface_cols_4[4]]]

#_________----
## Roberge Lake - max -20 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_20_max)) %>% 
  do(model = step(lm(rlk_temp_20_max ~ rlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_5_max + rlk_temp_10_max + rlk_temp_15_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_20_max = ifelse(is.na(rlk_temp_20_max), pred, rlk_temp_20_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_4[5]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_4[5]]]), color = "red")
df[[sub_surface_cols_4[5]]] <- mp[[sub_surface_cols_4[5]]]

#_________----
## Roberge Lake - min -20 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_20_min)) %>% 
  do(model = step(lm(rlk_temp_20_min ~ rlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_5_min + rlk_temp_10_min + rlk_temp_15_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_20_min = ifelse(is.na(rlk_temp_20_min), pred, rlk_temp_20_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_4[6]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_4[6]]]), color = "red")
df[[sub_surface_cols_4[6]]] <- mp[[sub_surface_cols_4[6]]]

#______________________________----
# 👀 Step 9: Fill the missing -20 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_5 <- df %>%
  select(matches("_20_")) %>%
  colnames()

#_________----
## Mary Lake - avg -20 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_20_avg)) %>% 
  do(model = step(lm(mlk_temp_20_avg ~ mlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_5_avg + rlk_temp_10_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_20_avg = ifelse(is.na(mlk_temp_20_avg), pred, mlk_temp_20_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[1]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[1]]]), color = "red")
df[[sub_surface_cols_5[1]]] <- mp[[sub_surface_cols_5[1]]]

#_________----
## Mary Lake - max -20 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_20_max)) %>% 
  do(model = step(lm(mlk_temp_20_max ~ mlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_5_max + rlk_temp_10_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_20_max = ifelse(is.na(mlk_temp_20_max), pred, mlk_temp_20_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[2]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[2]]]), color = "red")
df[[sub_surface_cols_5[2]]] <- mp[[sub_surface_cols_5[2]]]

#_________----
## Mary Lake - min -20 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_20_min)) %>% 
  do(model = step(lm(mlk_temp_20_min ~ mlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_5_min + rlk_temp_10_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_20_min = ifelse(is.na(mlk_temp_20_min), pred, mlk_temp_20_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[3]]]), color = "red")
df[[sub_surface_cols_5[3]]] <- mp[[sub_surface_cols_5[3]]]

#______________________________----
# 👀 Step 10: Fill the missing -40 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_5 <- df %>%
  select(matches("_40_")) %>%
  colnames()

#_________----
## Roberge Lake - avg -40 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_40_avg)) %>% 
  do(model = step(lm(rlk_temp_40_avg ~ rlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_5_avg + rlk_temp_10_avg + rlk_temp_20_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_40_avg = ifelse(is.na(rlk_temp_40_avg), pred, rlk_temp_40_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[4]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[4]]]), color = "red")
df[[sub_surface_cols_5[4]]] <- mp[[sub_surface_cols_5[4]]]

#_________----
## Roberge Lake - max -40 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_40_max)) %>% 
  do(model = step(lm(rlk_temp_40_max ~ rlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_5_max + rlk_temp_10_max + rlk_temp_20_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_40_max = ifelse(is.na(rlk_temp_40_max), pred, rlk_temp_40_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[5]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[5]]]), color = "red")
df[[sub_surface_cols_5[5]]] <- mp[[sub_surface_cols_5[5]]]

#_________----
## Roberge Lake - min -40 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_40_min)) %>% 
  do(model = step(lm(rlk_temp_40_min ~ rlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_5_min + rlk_temp_10_min + rlk_temp_20_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_40_min = ifelse(is.na(rlk_temp_40_min), pred, rlk_temp_40_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[6]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[6]]]), color = "red")
df[[sub_surface_cols_5[6]]] <- mp[[sub_surface_cols_5[6]]]

#______________________________----
# 👀 Step 11: Fill the missing -40 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_5 <- df %>%
  select(matches("_40_")) %>%
  colnames()

#_________----
## Mary Lake - avg -40 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_40_avg)) %>% 
  do(model = step(lm(mlk_temp_40_avg ~ mlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_5_avg + rlk_temp_10_avg + mlk_temp_20_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_40_avg = ifelse(is.na(mlk_temp_40_avg), pred, mlk_temp_40_avg))
mp %>% ggplot(aes(x = date, y = tsclean(.data[[sub_surface_cols_5[1]]]))) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[1]]]), color = "red")
df[[sub_surface_cols_5[1]]] <- tsclean(mp[[sub_surface_cols_5[1]]])

df %>% mutate(mlk_temp_40_avg = ifelse(date < "2007-01-01" & mlk_temp_40_avg > 7, 7, mlk_temp_40_avg)) %>% 
  ggplot(aes(x = date, y = mlk_temp_40_avg)) + geom_line()

df <- df %>% mutate(mlk_temp_40_avg = ifelse(date < "2007-01-01" & mlk_temp_40_avg > 7, 7, mlk_temp_40_avg))

#_________----
## Mary Lake - max -40 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_40_max)) %>% 
  do(model = step(lm(mlk_temp_40_max ~ mlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_5_max + rlk_temp_10_max + mlk_temp_20_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_40_max = ifelse(is.na(mlk_temp_40_max), pred, mlk_temp_40_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[2]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[2]]]), color = "red")
df[[sub_surface_cols_5[2]]] <- mp[[sub_surface_cols_5[2]]]

df %>% 
  mutate(mlk_temp_40_max = ifelse(date < "2012-01-01" & mlk_temp_40_max > 7, 7, mlk_temp_40_max)) %>% 
  mutate(mlk_temp_40_max = ifelse(date < "2012-01-01" & mlk_temp_40_max < -8.5, -8.5, mlk_temp_40_max)) %>% 
  ggplot(aes(x = date, y = mlk_temp_40_max)) + geom_line()

df <- df %>% 
  mutate(mlk_temp_40_max = ifelse(date < "2012-01-01" & mlk_temp_40_max > 7, 7, mlk_temp_40_max)) %>% 
  mutate(mlk_temp_40_max = ifelse(date < "2012-01-01" & mlk_temp_40_max < -8.5, -8.5, mlk_temp_40_max))

#_________----
## Mary Lake - min -40 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_40_min)) %>% 
  do(model = step(lm(mlk_temp_40_min ~ mlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_5_min + rlk_temp_10_min + mlk_temp_20_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_40_min = ifelse(is.na(mlk_temp_40_min), pred, mlk_temp_40_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_5[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_5[3]]]), color = "red")
df[[sub_surface_cols_5[3]]] <- mp[[sub_surface_cols_5[3]]]

#______________________________----
# 👀 Step 12: Fill the missing -80 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_6 <- df %>%
  select(matches("_80_")) %>%
  colnames()

#_________----
## Roberge Lake - avg -80 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_80_avg)) %>% 
  do(model = step(lm(rlk_temp_80_avg ~ rlk_temp_0_avg + rlk_temp_2p5_avg + rlk_temp_5_avg + rlk_temp_10_avg + rlk_temp_20_avg + rlk_temp_40_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_80_avg = ifelse(is.na(rlk_temp_80_avg), pred, rlk_temp_80_avg))
mp %>% ggplot(aes(x = date, y = tsclean(.data[[sub_surface_cols_6[4]]]))) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_6[4]]]), color = "red")
df[[sub_surface_cols_6[4]]] <- mp[[sub_surface_cols_6[4]]]

df %>% 
  mutate(rlk_temp_80_avg = ifelse(date < "2007-01-01" & rlk_temp_80_avg > 1, 1, rlk_temp_80_avg)) %>% 
  ggplot(aes(x = date, y = rlk_temp_80_avg)) + geom_line()

df <- df %>% 
  mutate(rlk_temp_80_avg = ifelse(date < "2007-01-01" & rlk_temp_80_avg > 1, 1, rlk_temp_80_avg))

#_________----
## Roberge Lake - max -80 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_80_max)) %>% 
  do(model = step(lm(rlk_temp_80_max ~ rlk_temp_0_max + rlk_temp_2p5_max + rlk_temp_5_max + rlk_temp_10_max + rlk_temp_20_max + rlk_temp_40_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_80_max = ifelse(is.na(rlk_temp_80_max), pred, rlk_temp_80_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_6[5]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_6[5]]]), color = "red")
df[[sub_surface_cols_6[5]]] <- mp[[sub_surface_cols_6[5]]]

#_________----
## Roberge Lake - min -80 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(rlk_temp_80_min)) %>% 
  do(model = step(lm(rlk_temp_80_min ~ rlk_temp_0_min + rlk_temp_2p5_min + rlk_temp_5_min + rlk_temp_10_min + rlk_temp_20_min + rlk_temp_40_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(rlk_temp_80_min = ifelse(is.na(rlk_temp_80_min), pred, rlk_temp_80_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_6[6]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_6[6]]]), color = "red")
df[[sub_surface_cols_6[6]]] <- mp[[sub_surface_cols_6[6]]]

#______________________________----
# 👀 Step 13: Fill the missing -80 cm temperatures ----

# Select columns that end with neg150
sub_surface_cols_6 <- df %>%
  select(matches("_80_")) %>%
  colnames()

#_________----
## Mary Lake - avg -80 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_80_avg)) %>% 
  do(model = step(lm(mlk_temp_80_avg ~ mlk_temp_0_avg + mlk_temp_20_avg + mlk_temp_40_avg, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_80_avg = ifelse(is.na(mlk_temp_80_avg), pred, mlk_temp_80_avg))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_6[1]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_6[1]]]), color = "red")
df[[sub_surface_cols_6[1]]] <- mp[[sub_surface_cols_6[1]]]

#_________----
## Mary Lake - max -80 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_80_max)) %>% 
  do(model = step(lm(mlk_temp_80_max ~ mlk_temp_0_max + mlk_temp_20_max + mlk_temp_40_max, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_80_max = ifelse(is.na(mlk_temp_80_max), pred, mlk_temp_80_max))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_6[2]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_6[2]]]), color = "red")
df[[sub_surface_cols_6[2]]] <- mp[[sub_surface_cols_6[2]]]

df %>% 
  mutate(mlk_temp_80_max = ifelse(date < "2009-01-01" & mlk_temp_80_max > 4, 4, mlk_temp_80_max)) %>% 
  mutate(mlk_temp_80_max = ifelse(date < "2009-01-01" & mlk_temp_80_max < -6, -6, mlk_temp_80_max)) %>% 
  ggplot(aes(x = date, y = mlk_temp_80_max)) + geom_line()

df <- df %>% 
  mutate(mlk_temp_80_max = ifelse(date < "2009-01-01" & mlk_temp_80_max > 4, 4, mlk_temp_80_max)) %>% 
  mutate(mlk_temp_80_max = ifelse(date < "2009-01-01" & mlk_temp_80_max < -6, -6, mlk_temp_80_max)) 

#_________----
## Mary Lake - min -80 cm ----

# Construct linear model based on non-NA pairs
models <- df %>% 
  group_by(month) %>% 
  filter(!is.na(mlk_temp_80_min)) %>% 
  do(model = step(lm(mlk_temp_80_min ~ mlk_temp_0_min + mlk_temp_20_min + mlk_temp_40_min, data = .))) %>%
  ungroup()
mp <- left_join(as_tibble(df), models, by = "month")

# generate the extra column
mp <- mp %>%
  group_by(month) %>%
  do(modelr::add_predictions(., dplyr::first(.$model))) %>% 
  mutate(model = NULL) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  mutate(mlk_temp_80_min = ifelse(is.na(mlk_temp_80_min), pred, mlk_temp_80_min))
mp %>% ggplot(aes(x = date, y = .data[[sub_surface_cols_6[3]]])) + geom_line() +
  geom_line(data = df, aes(y = .data[[sub_surface_cols_6[3]]]), color = "red")
df[[sub_surface_cols_6[3]]] <- mp[[sub_surface_cols_6[3]]]

df %>% 
  mutate(mlk_temp_80_min = ifelse(date < "2009-01-01" & mlk_temp_80_min > 4, 4, mlk_temp_80_min)) %>% 
  mutate(mlk_temp_80_min = ifelse(date < "2009-01-01" & mlk_temp_80_min < -6.5, -6.5, mlk_temp_80_min)) %>%
  ggplot(aes(x = date, y = mlk_temp_80_min)) + geom_line()

df <- df %>% 
  mutate(mlk_temp_80_min = ifelse(date < "2009-01-01" & mlk_temp_80_min > 4, 4, mlk_temp_80_min)) %>% 
  mutate(mlk_temp_80_min = ifelse(date < "2009-01-01" & mlk_temp_80_min < -6.5, -6.5, mlk_temp_80_min)) 

df %>% 
  ggplot(aes(x = date, y = mlk_temp_150_avg)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = mlk_temp_0_avg), color = col_pal[2]) +
  geom_line(aes(y = mlk_temp_20_avg), color = col_pal[3]) +
  geom_line(aes(y = mlk_temp_40_avg), color = col_pal[4]) +
  geom_line(aes(y = mlk_temp_80_avg), color = col_pal[5])

df %>% 
  ggplot(aes(x = date, y = rlk_temp_150_avg)) + geom_line(color = col_pal[1]) + 
  geom_line(aes(y = rlk_temp_0_avg), color = col_pal[2]) +
  geom_line(aes(y = rlk_temp_2p5_avg), color = col_pal[3]) +
  geom_line(aes(y = rlk_temp_5_avg), color = col_pal[4]) +
  geom_line(aes(y = rlk_temp_10_avg), color = col_pal[5]) +
  geom_line(aes(y = rlk_temp_40_avg), color = col_pal[6]) +
  geom_line(aes(y = rlk_temp_80_avg), color = col_pal[7])

## Generate the date range
min_date <- min(df$date)
min_year <- year(min_date)
min_month <- sprintf("%02d", month(min_date))
min_day <- sprintf("%02d", day(min_date))
max_date <- max(df$date)
max_year <- year(max_date)
max_month <- sprintf("%02d", month(max_date))
max_day <- sprintf("%02d", day(max_date))

## Generate the file name and path and export
save_path <- sprintf("./parks/data/parks_microclimate_%s%s%s_%s%s%s_filled.csv",
                     min_year, min_month, min_day,
                     max_year, max_month, max_day)
write.csv(df, save_path, row.names=FALSE)

df_sum <- df %>% 
  select(YEAR, contains("temp"), -station_temp) %>% 
  group_by(YEAR) %>% 
  summarise(across(mlk_temp_150_avg:rlk_temp_2p5_min, ~mean(.x, na.rm = T))) %>% 
  slice(2:(n()-1))# %>% 
  # ggplot(aes(x = YEAR, y = rlk_temp_150_avg)) + geom_line()

# Function to extract slope and standard error from lm
extract_slope <- function(df, var) {
  model <- lm(reformulate("YEAR", response = "value"), data = df)  # Use "value" as response
  coef_summary <- summary(model)$coefficients
  tibble(
    variable = var,  # Store actual variable name
    slope = coef_summary["YEAR", "Estimate"],
    se = coef_summary["YEAR", "Std. Error"]
  )
}

# Compute slopes for each column starting from mlk_temp_150_avg
slopes_df <- df_sum %>%
  select(YEAR, starts_with("mlk_temp_150_avg"):last_col()) %>%  # Adjust range as needed
  pivot_longer(-YEAR, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  group_split() %>%  # Split data into list of tibbles
  map_dfr(~ extract_slope(.x, unique(.x$variable))) %>%  # Apply function to each tibble
  mutate(
    site = substr(variable,1,3),  # Extract site name
    depth = as.numeric(gsub("p",".",str_extract(variable, "(?<=_)(\\d+p?5?)(?=_)"))),  # Extract depth
    type = str_extract(variable, "[^_]+$")  # Extract last part (type)
  ) %>% 
  mutate(
    depth = ifelse(depth %in% c(0,150), depth, depth*-1), # Adjust sign for depth
    ci_lower = slope - 1.96 * se,  # Approx. 95% CI (assuming large sample)
    ci_upper = slope + 1.96 * se,
    significant = ifelse(ci_lower > 0 | ci_upper < 0, "Yes", "No")
    ) 

# slopes_df %>% 
#   filter(site == "mlk", type == "avg") %>% 
#   ggplot(aes(x = depth, y = slope, color = site, fill = site)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +  # Shaded confidence interval
#   coord_flip() +
#   theme_minimal() +
#   labs(title = "Slope vs. Depth with Confidence Intervals",
#        x = "Depth (cm)",
#        y = "Slope (°C per year)",
#        color = "Site",
#        fill = "Site") +
#   theme(legend.position = "top")

slopes_df %>% 
  filter(site == "mlk", type == "avg") %>% 
  ggplot(aes(x = depth, y = slope, ymin = ci_lower, ymax = ci_upper, color = site)) +
  geom_pointrange() +  # Adds points with error bars for CI
  geom_line() +  # Connects points to show trends
  coord_flip() +  # Flip for conventional depth representation
  theme_bw() +
  labs(title = "Slope vs. Depth",
       x = "Depth (cm)",
       y = "Slope (°C per year)",
       color = "Site") +
  theme(legend.position = "top")
  
# Define custom colors
cust_cols <- wes_palette("Darjeeling1")

custom_colors <- c("Mary" = cust_cols[4], "Roberge" = cust_cols[5])  # Blue & Red

slopes_df %>% 
  filter(type == "avg") %>% 
  mutate(site = ifelse(site == "rlk", "Roberge", "Mary")) %>% 
  ggplot(aes(x = depth, y = slope, ymin = ci_lower, ymax = ci_upper, color = site)) +
  geom_pointrange(alpha = 0.65) +  # Adds points with error bars for CI
  geom_line(alpha = 0.65) +  # Connects points to show trends
  geom_hline(yintercept = 0, linetype = 2, color = "gray40") +
  geom_vline(xintercept = 0, linetype = 2, color = "gray40") +
  coord_flip() +  # Flip for conventional depth representation
  theme_bw() +
  labs(
    # title = "Slope vs. Depth",
    x = "Depth (cm)",
    y = "Temperature change (°C per year)",
    color = "") +
  theme(
    legend.position = c(0.2, 0.2),  # Move legend inside the plot
    legend.background = element_rect(fill = "white", color = NULL)  # White background for contrast
  ) +
  scale_color_manual(values = custom_colors)  # Apply custom colors
ggsave("./Parks/figures/temp_trends_vs_depth.png", height = 5, width = 5, units = "in", dpi = 300)

df %>% 
  select(YEAR, contains("rlk") & contains("temp") & contains("max")) %>% 
  group_by(YEAR) %>% 
  summarise(across(rlk_temp_150_max:last_col(), ~max(.x, na.rm = T)))

seasonal_avg <- df %>% 
  mutate(
    season = case_when(
      month %in% c(6:9) ~ "warm",
      month %in% c(1:5,10:12) ~ "cool",
      .default = NA
    ),
    season_year = ifelse(month %in% 10:12, YEAR + 1, YEAR)
  ) %>% 
  group_by(season_year, season) %>%
  summarise(
    mlk_air = mean(mlk_temp_150_avg, na.rm = TRUE),
    rlk_air = mean(rlk_temp_150_avg, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)
library(dplyr)
library(broom)

# Function to compute regression trends per site and season
compute_trend <- function(df, y_var) {
  df %>%
    group_by(season) %>%
    group_map(~ {
      model <- lm(reformulate("season_year", y_var), data = .x)
      summary_model <- summary(model)
      p_value <- coef(summary_model)["season_year", "Pr(>|t|)"]
      
      # Print p-values for debugging
      message(paste("Season:", .x$season[1], "| Site:", y_var, "| p-value:", round(p_value, 4)))
      
      if (p_value < 0.05) {
        augment(model) %>% 
          mutate(season = .x$season[1], site = y_var)  # Retain season correctly
      } else {
        return(NULL)  # Return NULL if not significant
      }
    }) %>%
    bind_rows()  # Combine all valid results
}

# Compute trends for both sites
trend_rlk <- compute_trend(seasonal_avg %>% filter(season_year < 2024), "rlk_air")
trend_mlk <- compute_trend(seasonal_avg %>% filter(season_year < 2024), "mlk_air")

# Combine trend data for plotting
trend_df <- bind_rows(trend_rlk, trend_mlk)

# Check if any significant trends exist before plotting
if (nrow(trend_df) == 0) {
  message("No significant trends found (p ≥ 0.05 for all regressions).")
} else {
  # Now plot with regression lines where significant
  p <- seasonal_avg %>%
    filter(season_year < 2024) %>%
    pivot_longer(cols = c(mlk_air, rlk_air), names_to = "site", values_to = "temp") %>%
    ggplot(aes(x = season_year, y = temp, color = season)) +
    geom_line() + geom_point() +
    theme_bw() +
    ylim(-20,25) + 
    labs(x = "Year", y = "Temperature (°C)", color = "Season") +
    theme(legend.position = "top") +
    facet_wrap(~site, scales = "free_y")
  
  # Add regression lines only for significant trends
  # p <- 
    p + geom_line(data = trend_df, aes(x = season_year, y = .fitted), linetype = "dashed", linewidth = 1)
  
  # Print plot
  print(p)
}
