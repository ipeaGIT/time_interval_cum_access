library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(Hmisc)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')


gtfs <- "../../data-raw/avaliacao_intervencoes/for/gtfs/gtfs_for_etufor_2019-10.zip"
gtfs <- "../../data-raw/gtfs/for/2019/gtfs_for_etufor_2019-06.zip"
gtfs <- "../../data-raw/gtfs/for/2019/gtfs_for_etufor_2019-10.zip"
# lines to be changed frequency
gtfs <- read_gtfs(gtfs)


# get trips
trips <- gtfs$trips
# ATENCAO: para o caso de Fortaleza ETUFOR, a informacao de direction_id teve que ser
# extraida a partir do trip_id
trips[, direction_id := fifelse(stringr::str_sub(trip_id, -1, -1) == "I", 0, 1)]
# get stoptimes
stop_times <- gtfs$stop_times

# bring route_id to stop_times
stop_times <- merge(stop_times, 
                    trips[, .(trip_id, route_id, direction_id, service_id)], 
                    by = "trip_id",
                    sort = FALSE)


# filter service
stop_times_service <- stop_times[service_id %in% "U"]

# get start of each trip
stop_times_starts <- stop_times_service[, .(arrival_time = arrival_time[1],
                                            n_stops = .N,
                                            ttime_trip = (last(as.ITime(arrival_time)) - first(as.ITime(arrival_time)))/60), 
                                        by = .(route_id, trip_id, direction_id)]

setorder(stop_times_starts, route_id, direction_id, arrival_time)
stop_times_starts[, arrival_time := as.ITime(arrival_time)]

stop_times_starts_pico <- stop_times_starts[between(arrival_time, as.ITime("06:00:00"), as.ITime("08:00:00"))]

stop_times_starts_pico[, headway := arrival_time - lag(arrival_time),
                       by = .(route_id, direction_id)]

stop_times_starts_pico[, headway := as.integer(headway) / 60]

# calculate mean headway by direction
headways_pico <- stop_times_starts_pico[, .(headway_mean = mean(headway, na.rm = TRUE)),
                                        by = .(route_id, direction_id)]

# export
readr::write_rds(headways_pico, "../../data/avaliacao_intervencoes/for/headway_routes_for_2019-04.rds")