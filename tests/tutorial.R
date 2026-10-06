# ============================================================
# 🐦 Adventure Tutorial: Bird Nests Meet Weather!
# ============================================================
# Today, we’re going on a mini data adventure:
# we have our own dataset about bird nests in the Veluwe, and grab weather info through
# the Data Nature Cube
# and see if temperature affects when birds lay their eggs.
#
# Along the way, you’ll:
# 1. Load and peek at our nest data
# 2. Use a Shiny app to grab weather data
# 3. Clean, filter, and merge datasets
# 4. Make a plot of lay dates vs temperature
#
# Let’s go! Run step by step (Ctrl + Enter) and have fun! 
# ============================================================


# ============================================================
# 1. Load libraries
# ============================================================
library(tidyverse)   
library(lubridate)  
library(dplyr)       


# ============================================================
# 2. Load our collected Bird Nest data, (supplied by Joseph Burant, PhD)
# ============================================================
# This CSV contains:
# - lay_date: when eggs were laid
# - species
# - clutch_size: number of eggs
# - year

# (the CSV is not part of this repository: put it in a `data/` folder first)
data <- read.csv("data/first_nests_HV.csv")

# ============================================================
# 3. Convert lay_date to a date format
# ============================================================
# Sometimes R thinks dates are just text. Let’s fix that.

data$lay_date <- as.Date(data$lay_date)

# ============================================================
# 4. Filter the nest data
# ============================================================
# Let’s keep only:
# - nests from 2024 onwards
# - nests with at least one egg
# - nests from March → July (peak breeding season)

data <- data %>%
  filter(
    year >= 2024,
    clutch_size > 0,
    month(lay_date) %in% 3:7
  )

# 🐣 Mini challenge (optional): How many nests remain after filtering?
# Hint: use nrow(data)


# ============================================================
# 5. Open the Shiny app and select weather data
# ============================================================
# Time for some interactive fun! The app lets you explore different datasets,
# but we’re focusing on weather today.
#
# Once you finish, the data will be stored in `weather_data`.
#
# Requirements:
# - the `rNDC` package (remotes::install_github("LTER-LIFE/rNDC", ref = "texel26"))
# - your tokens in this R session, e.g. Sys.setenv(NDC_TOKEN = "...", ADC_TOKEN = "...")
# - run this from the root of the rNDC-Shiny repository, in an interactive R
#   session (the "Return data to R" button is only shown in interactive sessions)

weather_data <- shiny::runApp("inst/shiny/naturedatacube_app")

# Shiny app steps:
# 1. Select project: Nestboxes
# 2. Zoom to Arnhem and select the polygon north of A12 ("De Hoge Veluwe")
# 3. Pick "Weather" under available datasets
# 4. Select the period: 2024-01-01 → 2025-01-01
# 5. Click "Add to overview"
# 6. Click "Return data to R (close app)"
#
# Now your weather data lives in `weather_data`! 🎉

# 🐣 Mini challenge: Explore weather_data$datasets to see what type of weather data is available


# ============================================================
# 6. Extract daily mean temperature 
# ============================================================
# We’ll select just the date and mean_temperature columns and rename them.

mean_temp <- weather_data$datasets$Weather_1 %>%
  select(
    date = datum,
    temp = mean_temperature
  )

# ============================================================
# 7. Make sure weather dates are Date objects
# ============================================================
mean_temp$date <- as.Date(mean_temp$date)


# ============================================================
# 8. Merge nest data with temperature data
# ============================================================
# Each nest now gets the mean temperature on its lay date.

plot_data <- data %>%
  left_join(mean_temp, by = c("lay_date" = "date"))


# ============================================================
# 9. Keep only nests from 2024
# ============================================================
# This keeps the plot focused on a single breeding season.

plot_data <- plot_data %>%
  filter(year(lay_date) == 2024)


# ============================================================
# 10. Make a plot!
# ============================================================
# What we’ll see:
# - X-axis: lay date
# - Y-axis: mean daily temperature
# - Point size: clutch size
# - Point color: species

ggplot(plot_data, aes(y = lay_date, x = temp)) +
  geom_point(
    aes(size = clutch_size, colour = species),
    alpha = 0.5
  ) +
  scale_size_continuous(
    name = "Clutch size",
    range = c(1, 15)
  ) +
  labs(
    y = "Lay date",
    x = "Mean daily temperature (°C)",
    colour = "Species"
  ) +
  theme_minimal()

# 🎉 Congratulations! You’ve now linked bird nests with weather and
# visualized how temperature might influence egg-laying. 


# 🐣 Optional exploration challenge:
# - Can you add a smooth trend line to see the temperature effect?
