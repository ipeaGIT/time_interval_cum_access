# headways_df <- headways_df
# ttime_df <- ttime_df
# stops_df <- stops_df
# routes_df <- routes_df
# line_shape <- line_shape_routes
# agency_id = "guess"
# service_id = c("weekdays", "weekends")
# stops_crs = 4326



create_merge_gtfs <- function(gtfs, 
                        headways_df, ttime_df, stops_df, routes_df,
                        line_shape, 
                        agency_id = "guess",
                        service_id = c("weekdays", "weekends"),
                        stops_crs = 4326) {
  
  
  # # open original gtfs
  # gtfs_original <- gtfstools::read_gtfs(gtfs)
  # 
  # # check if gtfs is detailed or frequencies
  # frequencies <- names(gtfs_original)[names(gtfs_original) %like% "frequencies"]
  
  gtfs_original <- gtfstools::read_gtfs(gtfs)
  
  # select service_id based on the calendar choice
  if (service_id == "weekdays") {
    
    # filter only weekdays
    service_id1 <- gtfs_original$calendar[monday == 1 & tuesday == 1 & wednesday == 1 & thursday == 1 & friday == 1]
    
    # get service with most date intervals
    service_id1[, interval := paste0(start_date, "-", end_date)]
    service_id_n <- service_id1[, .N, by = interval]
    setorder(service_id_n, -N)
    # selecte the firs obs - interval
    interval1 <- service_id_n[1,1]
    # now filter the interval
    service_id1 <- service_id1[interval == interval1]$service_id[1]
    
    if (length(service_id1) == 0) {
      
      stop("There is no service_id for all weekday trips/nIf you want one, we advise you to create manually")
      
    }
    
    # get the first one
    service_id1 <- unique(service_id1)[1]
    
  } else if (calendar_days == "weekends") {
    
    # filter only weekends
    service_id1 <- gtfs_original$calendar[saturday == 1 & sunday == 1]$service_id
    
    if (length(service_id1) == 0) {
      
      stop("There is no service_id for weekends trips/nIf you want one, we advise you to create manually")
      
    }
    
  }
  
  
  # check if all stops located in the ttime_df are in the stops_df
  stops_check <- setdiff(c(ttime_df$stop_id_start, ttime_df$stop_id_end), stops_df$stop_id)
  
  if(length(stops_check) > 0) {
    
    stop("The following stop_id are located in your ttime_df but are not listed in your stops_df:\n",
         paste0(stops_check, collapse = ", "), "\n",
         "Please add these stop_id to your stops_df")
    
  }
  
  # check if all shapes located in the headways_df and ttime_df are in the line_shape
  shapes_headways_check <- setdiff(c(paste0(headways_df$route_id, "_", headways_df$direction_id)), 
                                   paste0(line_shape$route_id, '_', line_shape$direction_id))
  
  if(length(shapes_headways_check) > 0) {
    
    stop("The following shape_id are located in your headways_df but are not listed in your line_shape:\n",
         paste0(shapes_headways_check, collapse = ", "), "\n",
         "Please add these shape_id to your line_shape")
    
  }
  
  
  
  shapes_ttime_check <- setdiff(c(paste0(ttime_df$route_id, "_", ttime_df$direction_id)), 
                                paste0(line_shape$route_id, '_', line_shape$direction_id))
  
  if(length(shapes_ttime_check) > 0) {
    
    stop("The following shape_id are located in your ttime_df but are not listed in your line_shape:\n",
         paste0(shapes_ttime_check, collapse = ", "), "\n",
         "Please add these shape_id to your line_shape")
    
  }
  
  
  # if (frequencies) {
  #   
  #   
  #   
  # }
  
  # 1. Calculate number of trips each hour ------------------------------------------------------
  headways_df[, n_trips := floor((hour_end-hour_start)*60/headway), by = .(route_id, direction_id, service)]
  # change start_time to ITime
  headways_df[, start_trip := as.ITime(paste0(hour_start, ":00:00"))]
  
  
  # 2. Create dummy df with each trip start --------------------------------------------------------  
  
  # create temporary df with the trips based on its start hour of frequencies
  df <- data.table(
    # trip_id = rep(headways_df$trip_id, headways_df$n_trips),
    route_id     = rep(headways_df$route_id,     headways_df$n_trips),
    direction_id = rep(headways_df$direction_id, headways_df$n_trips),
    start_trip   = rep(headways_df$start_trip,   headways_df$n_trips),
    headway      = rep(headways_df$headway * 60, headways_df$n_trips),
    service      = rep(headways_df$service,      headways_df$n_trips)
    )
  
  # tag each interval, which are the trips with unique headways
  df[, interval := rleid(start_trip)]
  
  # cum sum of headway of each interval of unique headways
  df[, headway_cum := cumsum(c(0, headway[-.N])), by = .(route_id, interval, direction_id, service)]
  
  # apply cumulative headways to each start trip - now we have corrects start trips
  df[, start_trip := start_trip + headway_cum]
  df[, start_trip := as.ITime(start_trip)]
  
  # identify trip_id
  df[, trip_id := paste0(route_id, "-", direction_id, ".", service, "-", rleid(start_trip)),
     by = .(route_id, direction_id, service)]
  
  
  
  # 3. Generate new stop_times based on the information created on the dummy df --------------------
  
  # transform from wide to long stop_times style
  ttime_df_long <-  ttime_df[, .(stop_id = rep(unique(c(stop_id_start, stop_id_end)), each = 1),
                                 ttime = c(0, ttime)
  ),
  by = .(route_id, direction_id, service) ]
  
  # add stop_sequence
  ttime_df_long[, stop_sequence := 1:.N, by = .(route_id, direction_id, service)]
  
  # bring nstops to df
  # this merge will automactcicly replicate each trip start by the number of stops of that route-direction pair
  df_v1 <- merge(df, ttime_df_long,
                 by = c("route_id", "direction_id", "service"),
                 sort = FALSE,
                 # this argument will allow the df to multiply by the number of stops
                 allow.cartesian=TRUE)
  
  
  # cum sum of headway between each stop, by the trip_id
  # this sum will create the cumulative travel time of each trip
  df_v1[, ttime_cum := cumsum(ttime), by = trip_id]
  
  # sum cumulative ttime to create a corrected arrival_time/departure_time
  df_v1[, arrival_time := as.numeric(start_trip) + ttime_cum]
  
  # round arrival_time
  df_v1[, arrival_time := round(arrival_time, 0)]
  
  # convert arrival_time to hms::hms
  # prefrebly we would convert it to ITime (faster), but ITime convert
  # times past 00:00:00 to 24h format, and we dont want that
  df_v1[, arrival_time := hms::hms(arrival_time)]
  df_v1[, departure_time := arrival_time]
  
  # convert to character
  df_v1[, ':='(arrival_time = as.character(arrival_time),
                    departure_time = as.character(departure_time))]
  
  # select columns
  stop_times <- df_v1[, .(trip_id, arrival_time, departure_time, stop_id, stop_sequence)]
  
  
  
  
  
  
  
  # columns: shape_id, shape_pt_lat, shape_pt_lon, shape_pt_sequence, shape_dist_traveled
  # source("R/fun/teste_linha_leste_shapes.R")
  shapes <- line_to_shapes(line_shape)
  
  
  
  
  
  
  
  # genarate new stops --------------------------------------------------------------------------
  # columns: stop_id, stop_code, stop_name, stop_lat, stop_lon, location_type
  
  stops <- stops_df %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = stops_crs) %>%
    st_transform(crs = 4326) %>%
    sfc_as_cols() %>% setDT()
  
  # select variables
  if ("location_type" %in% colnames(stops))  {
    
    
    stops <- stops[, .(stop_id, stop_name, stop_lat = lat, stop_lon = lon, location_type, parent_station)]
    
  } else {
    
    stops <- stops[, .(stop_id, stop_name, stop_lat = lat, stop_lon = lon)]
    
  }
 
  
  
  
  
  # generate new routes -------------------------------------------------------------------------
  # columns: route_id, agency_id, route_short_name, route_long_name, route_type
  
  # if the user desires an agency_id, it will select it. otherwise, it will get the first agency_id that
  # is located in the agency file
  # if (agency_id == "guess") agency_id1 <- unique(gtfs_original$agency$agency_id)[1] else agency_id1 <- agency_id
  
  # identify agency_id
  # routes <- routes_df[, agency_id := agency_id1]
  routes <- routes_df
  
  
  
  
  
  
  # generate new trips --------------------------------------------------------------------------
  # columns: route_id, service_id, trip_id, direction_id, shape_id
  
  # first, select only unique trips
  trips <- distinct(df_v1, trip_id, route_id, direction_id, .keep_all = TRUE) %>% setDT()
  # bring service_id
  trips[, service_id := service_id1]
  # identify shape_id
  trips[, shape_id := paste0(route_id, "_", direction_id)]
  # select vars
  trips <- trips[, .(route_id, service_id, trip_id, direction_id, shape_id)]
  
  
  
  
  # combine new gtfs files ----------------------------------------------------------------------
  
  # create a named list with the new gtfs files
  gtfs_new <- list(stop_times = stop_times, 
                   stops = stops, 
                   routes = routes, 
                   trips = trips, 
                   shapes = shapes)
  
  # we need to identify it as 'dt_gtfs' class so it can match with the original gtfs
  class(gtfs_new) <- c('dt_gtfs', 'gtfs')
  
  # merge the new gtfs with the original gtfs
  gtfs_merge <- gtfstools::merge_gtfs(
    gtfs_original,
    gtfs_new
  )
  
  class(gtfs_merge) <- c('dt_gtfs', 'gtfs')
  return(gtfs_merge)
  
}
