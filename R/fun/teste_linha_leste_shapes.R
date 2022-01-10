# line_shape <- st_read("C:/Users/kaue/Documents/linha_leste_kaue_gearth.gpkg")

sfc_as_cols <- function(x, names = c("lon","lat")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  ui <- dplyr::bind_cols(x,ret)
  st_set_geometry(ui, NULL)
}

get.dist <- function(lon, lat) geosphere::distHaversine(tail(cbind(lon,lat),-1), head(cbind(lon,lat),-1))


# line_shape <- line_shape_routes


line_to_shapes <- function(line_shape) {
  
  # shape_sf_temp2 <- sf::st_segmentize(line_shape, units::set_units(50, "m") ) %>%
  #   sf::st_cast("LINESTRING") %>%
  #   sf::st_cast("POINT")
  # 
  # shape_sf_temp2 <- st_zm(shape_sf_temp2, drop = TRUE)
  # 
  # mapview(shape_sf_temp2)
  # 
  # # coordinats to x, y
  # shape_sf_temp2 <- sfc_as_cols(shape_sf_temp2)
  # setDT(shape_sf_temp2)
  # 
  # # calculate shape_Dist_traveled
  # shape_sf_temp2[, shape_dist_traveled := c(0, get.dist(as.numeric(lon), as.numeric(lat)))]
  
  
  
  
  # Transform to metric
  shape_sf_temp1 <- st_transform(line_shape, 3857)
  
  # delete z coordinates if they exist
  shape_sf_temp1 <- st_zm(shape_sf_temp1, drop = TRUE)
  
  # Get samples at every 50 meters
  shape_sf_temp2 <- st_line_sample(shape_sf_temp1, density = 1/50)
  
  # identify the route_id and direction_id of each one
  names(shape_sf_temp2) <- paste0(shape_sf_temp1$route_id, "_", shape_sf_temp1$direction_id)
  
  
  # Transform back to a sf data.frame
  shape_sf_temp2 <- lapply(shape_sf_temp2, function(x) data.table(geometry = st_geometry(x))) %>%
    lapply(as.data.table) %>% rbindlist(idcol = "shape_id") %>% st_sf() %>%
    st_cast("POINT") %>%
    st_set_crs(3857) %>%
    st_transform(4326)
  
  # mapview(shape_sf_temp2[1:50,]) + line_shape
  # mapview(line_shape) + stops_sf
  
  # coordinats to x, y
  shape_sf_temp2 <- sfc_as_cols(shape_sf_temp2,
                                names = c("shape_pt_lon","shape_pt_lat"))
  setDT(shape_sf_temp2) 
  
  # # unfortunatelly the function 'st_line_sample' doesnt return the first and the last point of the linetrings
  # # so we must extrac them
  # line_first_last <- line_shape %>% st_zm(drop = TRUE) %>% st_cast("POINT") %>% slice(1, n()) %>%
  #   sfc_as_cols() %>% setDT()
  # 
  # # index the first and last point
  # line_first_last[, shape_pt_sequence := c(1, nrow(shape_sf_temp2)+2)]
  # line_first_last <- line_first_last[, .(lon, lat, shape_pt_sequence)]
  # 
  # # identify shape sequence
  # shape_sf_temp2[, shape_pt_sequence := c(2:(.N+1))]
  # 
  # # bind them!
  # shape_sf_temp2 <- rbind(shape_sf_temp2, line_first_last)
  # setorder(shape_sf_temp2, shape_pt_sequence)
  # 
  
  
  # calculate shape_sequence
  shape_sf_temp2[, shape_pt_sequence := 1:.N, by = shape_id]
  
  # calculate shape_dist_traveled
  shape_sf_temp2[, shape_dist_traveled := c(0, cumsum(get.dist(as.numeric(shape_pt_lon), as.numeric(shape_pt_lat)))),
                 by = shape_id]
  
  
  
}



# mapview(shape_sf_temp2)
