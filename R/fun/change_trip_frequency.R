# 
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(Hmisc)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')
# 
# 
# 
# 1) Fortaleza - metro linha leste ex-ante ---------------------------------------------------

# gtfs <- "../../data-raw/avaliacao_intervencoes/for/gtfs_for_metrofor_2021-01.zip"
gtfs <- "../../data-raw/avaliacao_intervencoes/for/gtfs/gtfs_for_etufor_2019-10.zip"
# lines to be changed frequency
gtfs <- read_gtfs(gtfs)

# identify lines to change frequency
# routes_metrofor <- gtfs$routes %>% select(route_id, route_long_name)

# # table with new frequency
# metrofor_new_frequency <- data.frame(
#   route_id = c(8, 6, 7),
#   frequency = c(7.5, 6, 20)
# )

# open data with the new frequencies
# linhas_mudancas <- fread("../../data/avaliacao_intervencoes/for/etufor_linhas_mudancas.csv",
#                          colClasses = "character")
# pegar somente linhas que nao vao ser excluidas
# table(linhas_mudancas$parecer, useNA = 'always')
# linhas_mudancas_stay <- linhas_mudancas %>% filter(parecer != "Eliminada")
# filter: removed 95 rows (31%), 212 rows remaining
# linhas_mudancas_stay <- linhas_mudancas_stay %>% filter(!is.na(intervalo_entre_partidas_min_9))
# linhas_mudancas_stay <- linhas_mudancas_stay %>% filter(intervalo_entre_partidas_min_9 != "")
# filter: removed 6 rows (3%), 206 rows remaining
# mudar frequencia





change_trip_frequency <- function(gtfs, routes, new_freqs, services) {
  
  # routes <- c("026", "045"); new_freqs <- c(4.6, 5.5); services <- "U"
  # routes = linhas_mudancas_stay$cod_linha; new_freqs = linhas_mudancas_stay$intervalo_entre_partidas_min_9
  
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
  
  
  change_trip_frequency_route <- function(route_id1, new_freq) {
    
    # route_id1 <- 8
    # new_freq <- 7.5
    # route_id1 <- "026"; new_freq <- 4.6; services <- "U"
    # route_id1 <- routes[93]; new_freq <- new_freqs[93]; services <- "U"
    # route_id1 <- routes[1]; new_freq <- new_freqs[1]
    
    new_freq_sec <- as.numeric(new_freq) * 60
    
    # filter route_id
    stop_times_route <- stop_times[route_id == route_id1]
    trips_route <- trips[route_id == route_id1]
    
    # filter service
    stop_times_route_service <- stop_times_route[service_id %in% services]
    
    # get start of each trip
    stop_times_starts <- stop_times_route_service[, .(arrival_time = arrival_time[1],
                                                      n_stops = .N,
                                                      ttime_trip = (last(as.ITime(arrival_time)) - first(as.ITime(arrival_time)))/60), 
                                                  by = .(trip_id, direction_id)]
    
    setorder(stop_times_starts, direction_id, arrival_time)
    
    # select the first trip after 6am (correction is made only after morning peak)
    # stop_times_starts[arrival_time := as.ITime(arrival_time)]
    stop_times_starts <- stop_times_starts[arrival_time >= "06:00:00"]
    
    # get the first trip for inbound and outbound
    trips_starts <- stop_times_starts[, .(arrival_time = arrival_time[1], 
                                          trip_id = trip_id[1],
                                          n_stops = n_stops[1]), 
                                      by = direction_id]
    trips_starts[, arrival_time := as.ITime(arrival_time)]
    
    # replicate this by the new trip frequency
    trips_starts_new <- trips_starts[, .(
      arrival_time = seq(
        from = arrival_time, 
        to = as.ITime("10:00:00"),
        by = as.ITime(new_freq_sec)),
      n_stops = n_stops[1]
      # trip_id = trip_id[1]
    ),
    by = direction_id]
    
    # identify which trips we will replicate to get the travel times from
    stop_times_starts[, arrival_time := as.ITime(arrival_time)]
    setnames(stop_times_starts, "arrival_time", "arrival_time_reference")
    
    setkey(trips_starts_new, direction_id, arrival_time)
    setkey(stop_times_starts, direction_id, arrival_time_reference)
    
    # get the closest start from the 
    trips_starts_new <- stop_times_starts[trips_starts_new, roll = "nearest"]
    
    # bring the entire set of stop_sequence and arrival_time from the stop_times
    # filter only trips of reference
    stop_times_reference <- stop_times_route_service[trip_id %in% trips_starts_new$trip_id]
    stop_times_reference[, arrival_time := ifelse(arrival_time == "", NA, arrival_time)]
    stop_times_reference[, arrival_time := as.ITime(arrival_time)]
    stop_times_reference[, arrival_time_reference := arrival_time[1], by = .(trip_id, direction_id)]
    # interpolate NAs
    stop_times_reference[, arrival_time := zoo::na.approx(arrival_time), by = .(trip_id, direction_id)]
    stop_times_reference[, arrival_time := as.ITime(arrival_time)]
    stop_times_reference[, ttime := arrival_time - lag(arrival_time), by = .(trip_id, direction_id)]
    stop_times_reference[, ttime := ifelse(is.na(ttime), 0, ttime)]
    stop_times_reference[, ttime := cumsum(ttime), by = .(trip_id, direction_id)]
    
    b <- merge(trips_starts_new,
               stop_times_reference,
               by = c("trip_id", "direction_id"),
               allow.cartesian = TRUE,
               sort = FALSE)
    
    
    
    # create trip_id
    b[, trip_id := paste0(trip_id, "_", rleid(arrival_time_reference.x)),
      by = .(trip_id, direction_id)]
    
    # cumsum to get new arrival_time
    b[, arrival_time := arrival_time_reference.x + as.ITime(ttime)]
    
    # organize
    stop_times_new_ttime <- b %>%
      # mutate(arrival_time = arrival_time2) %>%
      mutate(arrival_time = as.character(arrival_time)) %>%
      mutate(departure_time = arrival_time) %>%
      dplyr::select(trip_id, direction_id, arrival_time, departure_time, stop_sequence, stop_id, route_id, service_id)
    
    
    # output: route's stop_times with the new frequency
    # to bind them together, fist delete trips of the routes between 06h and 10h
    stop_times_starts1 <- stop_times_starts[arrival_time_reference %between% c(as.ITime("06:00:00"), as.ITime("10:00:00"))]
    stop_times_bind <- stop_times_route[trip_id %nin% stop_times_starts1$trip_id]
    
    
    # bind!!!!!!!!!!!!!!!!!
    stop_times_end <- rbind(stop_times_bind,
                            stop_times_new_ttime,
                            fill = TRUE)
    
    
    # identify first departure of each trip
    stop_times_end[, arrival_time_first := arrival_time[1], by = trip_id ]
    
    setorder(stop_times_end, direction_id, arrival_time_first)
    
    
    # stop_times_end <- stop_times_end %>% 
    # dplyr::select(-route_id, -arrival_time_first, -direction_id)
    
    
    
    
    # setup new trips ---------------------------------------------------------
    trips_new <- distinct( stop_times_new_ttime, trip_id, direction_id, route_id, service_id)
    
    
    
    # get other vars
    trips_vars <- distinct(trips, route_id, direction_id, .keep_all = TRUE) %>% dplyr::select(-trip_id, -service_id)
    trips_new <- left_join(trips_new,
                           trips_vars,
                           by = c("route_id", "direction_id"))
    
    trips_end <- rbind(
      trips_route[trip_id %nin% stop_times_starts1$trip_id],
      trips_new,
      fill = TRUE
      
    )
    
    
    # output
    frequency_new <- list(
      stop_times = stop_times_end,
      # stop_times = list(stop_times_end, nrow(stop_times), nrow(stop_times_end)),
      trips = trips_end
    )
    
    
  }
  
  # apply fun
  library(purrr)
  go <- purrr::map2(routes, new_freqs, possibly(change_trip_frequency_route, otherwise = "erro"))
  
  go <- go[map_lgl(go, is.list)]
  
  # right now, we have one list for each route, and each route has a "stop_times" and a "trips" file
  # we need to transpose list, so we have one list for "stop_times" and one list for "trips"
  ui <- purrr::transpose(go)
  
  # apply rbindlist inside both "stop_times" and "trips"
  ui <- lapply(ui, rbindlist)
  
  # get routes that were sucessufly updated
  routes_ok <- unique(ui$stop_times$route_id)
  
  # bring new files to the full ones ----------------------------
  stop_times_complete <- rbind(
    # get the original stop_times and delete trips from the modified routes
    stop_times[route_id %nin% routes_ok],
    ui$stop_times,
    fill = TRUE
  )
  
  trips_complete <- rbind(
    # get the original trips and delete trips from the modified routes
    trips[route_id %nin% routes_ok],
    ui$trips,
    fill = TRUE
  )
  
  
  
  # format to columns -------------------------------------------------------
  # these columns were inserted only for reference, but the dont belong to stop_times,
  # so we must delete them
  
  stop_times_complete <- stop_times_complete %>% 
    dplyr::select(-route_id, -direction_id, -service_id)
  
  # combine new gtfs files ----------------------------------------------------------------------
  
  gtfs$stop_times <- NULL
  gtfs$trips <- NULL
  
  # create a named list with the new gtfs files
  gtfs_new <- c(gtfs,
                list(stop_times = stop_times_complete,
                     trips      = trips_complete))
  
  # we need to identify it as 'dt_gtfs' class so it can match with the original gtfs
  class(gtfs_new) <- c('dt_gtfs', 'gtfs')
  
  return(gtfs_new)
  
  
  
}