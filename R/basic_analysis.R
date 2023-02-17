# rvest run -------------------------------------------------------------------
library(rvest)
library(lubridate)
library(ggplot2)
library(leaflet)
library(here)

# read in activity
activity <- read_html(here::here('data', "run_outdoor.gpx"))

#extractmetrics: heart rate, timepoints, and coordinates
hr <- as.numeric(activity %>% html_elements('hr')
                 %>% html_text2())

times <- lubridate::as_datetime(activity %>% 
                                  html_elements('time') %>% html_text2())

lat <- activity %>% html_nodes('trkpt') %>%
  html_attr('lat')
long <- activity %>% html_nodes('trkpt') %>% 
  html_attr('lon')

# prepare the data for plotting
plot_data <- data.frame(times = times[-1],
                        hr = hr)

coords <- data.frame(lon = long,
                     lat = lat)

# plots
ggplot(plot_data, aes(x = times, y = hr)) +
  geom_line()

coords %>% 
  as.matrix() %>%
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines( ) %>%
  setView(lng = mean(as.numeric(range(coords$lon))),
          lat = mean(as.numeric(range(coords$lat))), 
          zoom = 16)

# FITfileR run -----------------------------------------------

library(FITfileR)
library(dplyr)
library(ggplot2)
library(leaflet)
library(here)

# import activity
activity <- readFitFile(here::here('data', "run_outdoor.fit"))

# get the activity data
activity_data <- getMessagesByType(activity, 'record')

# get units
attributes(activity_data$record_2$distance)

attributes(activity_data$record_2$speed)

# plots
ggplot(activity_data$record_2, aes(x = timestamp, y = heart_rate)) +
  geom_line()

m <- activity_data$record_2 %>%
  select(position_long, position_lat) %>%
  as.matrix() %>%
  leaflet(  ) %>%
  addTiles() %>%
  addPolylines( ) %>%
  setView(lng = mean(as.numeric(range(activity_data$record_2$position_long))),
          lat = mean(as.numeric(range(activity_data$record_2$position_lat))), 
          zoom = 16)

m

# add split markers to the map
laps <- getMessagesByType(activity, 'lap')

markers <- activity_data$record_2 %>% 
  filter(timestamp %in% laps$start_time)

m <- m %>%
  addMarkers(markers$position_long, markers$position_lat)

m

# FITfileR tri analysis ------------------------------------------------------------

library(FITfileR)
library(dplyr)
library(here)
library(ggplot2)
library(leaflet)

# read in the data
tri <- readFitFile(here::here('data', 'tri.fit'))
tri_data <- getMessagesByType(tri, 'record')

tri_laps <- getMessagesByType(tri, 'lap') %>%
  select(timestamp, lap_trigger, sport)

# plot a leaflet map for FITfileR data
get_map_plot <- function(df){
  m <- df %>% 
    select(position_long, position_lat) %>%
    as.matrix() %>%
    leaflet(  ) %>%
    addTiles() %>%
    addPolylines( )
  
  return(m)
}

# calculate swim or run pace for FIT fileR data. 
# Swim velocity is expressed as min/100m.
# Run velocity is expressed as min/km
# Note that we assume that the distance passed in in meters 
get_run_swim_pace <- function(timestamp, distance, factor_unit = 1){
  rsp <- c(NA, (as.numeric(diff(timestamp))/60/diff(distance))*factor_unit)
  
  return(rsp)
}

# calculate the cycling speed in km/h. We assume that the distance 
# is in meters
get_cycle_speed <- function(timestamp, distance){
  cs <- c(NA, (diff(distance/1000)/as.numeric(diff(timestamp)))*3600)
  
  return(cs)
}

# do a ggplot for FITfileR data. Always use distance for the x axis.
# The user selects the y axis.
get_plot <- function(df, y_axis){
  p <- ggplot(df, aes(distance, .data[[y_axis]])) +
    geom_step() +
    ylab(y_axis)
  
  return(p)
}

# extract swim data
swim_data <- tri_data$record_5 %>%
  select(timestamp, position_long, position_lat, distance) %>%
  filter(timestamp >= tri_laps$timestamp[1] & 
           timestamp <= tri_laps$timestamp[3]) %>%
  mutate(pace = get_run_swim_pace(timestamp, distance, 100))

# plot swim data
swim_plot <- get_plot(swim_data, 'pace')
swim_map <- get_map_plot(swim_data)

swim_plot
swim_map

# extract cycle data
cycle_data <- tri_data$record_4 %>% 
  select(timestamp, position_long, position_lat, distance) %>%
  filter(timestamp >= tri_laps$timestamp[4] & timestamp <= tri_laps$timestamp[9]) %>%
  mutate(speed = get_cycle_speed(timestamp, distance))

# plot cycle data
cycle_plot <- get_plot(cycle_data, 'speed')
cycle_map <- get_map_plot(cycle_data)

cycle_plot
cycle_map

# extract run data
run_data <- tri_data$record_3 %>%
  select(timestamp, position_long, position_lat, distance) %>%
  filter(timestamp >= tri_laps$timestamp[10] & timestamp <= tri_laps$timestamp[15]) %>%
  mutate(pace = get_run_swim_pace(timestamp, distance, 1000))

# plot run data
run_plot <- get_plot(run_data, 'pace')
run_map <- get_map_plot(run_data)

run_plot
run_map
