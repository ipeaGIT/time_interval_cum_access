# city <- tar_read(both_cities)[1]
# grid_path <- tar_read(grid_path)[1]
create_points_r5 <- function(city, grid_path) {
  
  grid <- setDT(readRDS(grid_path))
  
  # keep only cells that have either population > 0 or opportunities > 0
  
  grid <- grid[
    pop_total > 0 |
      renda_total > 0 |
      empregos_total > 0 |
      saude_total > 0 |
      edu_total > 0
  ]
  
  # get centroids and save it as csv
  
  centroids <- grid[, .(id_hex, geometry)]
  centroids <- st_sf(centroids)
  centroids <- st_centroid(centroids)
  
  coords <- as.data.table(st_coordinates(centroids))
  
  centroids <- cbind(centroids, coords)
  centroids$geometry <- NULL
  
  # save object and return path
  
  file_path <- paste0(
    "../../data/avaliacao_intervencoes/r5/points/points_",
    city,
    "_09_2019.csv"
  )
  fwrite(centroids, file_path)
  
  return(file_path)

}


# city <- tar_read(both_cities)[1]
# scenario <- tar_read(before_after)[1]
# graph <- tar_read(graph)[1]
# points_path <- tar_read(points_path)[1]
transit_ttm <- function(city, scenario, graph, points_path) {
  
  r5r_core <- setup_r5(graph, use_elevation = TRUE)
  
  points <- fread(points_path)
  setnames(points, c("id", "lon", "lat"))
  
  departure <- ifelse(
    city == "for",
    "02-03-2020 06:00:00",
    "02-10-2019 06:00:00"
  )
  
  ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = points,
    mode = c("WALK", "TRANSIT"),
    departure_datetime = as.POSIXct(departure, format = "%d-%m-%Y %H:%M:%S"),
    time_window = 120L,
    max_trip_duration = 180L,
    max_walk_dist = 1000,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_transit.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}


# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# graph <- tar_read(graph)[1]
# points_path <- tar_read(points_path)[1]
bike_ttm <- function(city, scenario, graph, points_path) {
  
  r5r_core <- setup_r5(graph, use_elevation = TRUE)
  
  points <- fread(points_path)
  setnames(points, c("id", "lon", "lat"))
  
  bike_speed <- 12
  bike_max_distance <- 5
  
  ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = points,
    mode = "BICYCLE",
    departure_datetime = as.POSIXct("02-03-2020 06:00:00", format = "%d-%m-%Y %H:%M:%S"),
    time_window = 1L,
    max_trip_duration = bike_max_distance / bike_speed * 60,
    bike_speed = bike_speed,
    max_lts = 2,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_bike.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}


# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# graph <- tar_read(graph)[1]
# points_path <- tar_read(points_path)[1]
# bike_parks_path <- tar_read(bike_parks_path)[1]
bfm_ttm <- function(city, scenario, graph, points_path, bike_parks_path) {
  
  r5r_core <- setup_r5(graph, use_elevation = TRUE)
  
  points <- fread(points_path)
  setnames(points, c("id", "lon", "lat"))
  
  bike_parks <- fread(bike_parks_path)
  bike_speed <- 12
  bike_max_distance <- 5
  
  first_mile_ttm <- travel_time_matrix(
    r5r_core,
    origins = points,
    destinations = bike_parks,
    mode = "BICYCLE",
    departure_datetime = as.POSIXct("02-03-2020 06:00:00", format = "%d-%m-%Y %H:%M:%S"),
    time_window = 1L,
    max_trip_duration = bike_max_distance / bike_speed * 60,
    bike_speed = bike_speed,
    max_lts = 2,
    n_threads = getOption("R5R_THREADS"),
    verbose = FALSE
  )
  
  # the first mile leg can take anywhere from 0 to 25 minutes in this case.
  # we then have to calculate 26 different remaining matrices, each one of them
  # with a different departure time (so 6:00, 6:01, ..., 6:24, 6:25).
  # we also need to subtract the first mile travel time from the max trip
  # duration and from the time window to keep their consistency.
  # finally, when merging the first mile and the remaining matrix we need to use
  # both the ids and the first mile duration as our indices - so we join not
  # only the right ids, but the trips that start at the correct time.
  
  remaining_ttm <- lapply(
    0:25,
    function(i) {
      departure_datetime <- as.POSIXct(
        "02-03-2020 06:00:00",
        format = "%d-%m-%Y %H:%M:%S"
      )
      departure_datetime <- departure_datetime + 60 * i
      max_trip_duration <- 180L - i
      
      travel_time_matrix(
        r5r_core,
        origins = bike_parks,
        destinations = points,
        mode = c("WALK", "TRANSIT"),
        departure_datetime = departure_datetime,
        time_window = 1L,
        max_trip_duration = max_trip_duration,
        max_walk_dist = 1000,
        n_threads = getOption("R5R_THREADS"),
        verbose = FALSE
      )
    }
  )
  names(remaining_ttm) <- 0:25
  remaining_ttm <- rbindlist(remaining_ttm, idcol = "departure_minute")
  remaining_ttm[, departure_minute := as.integer(departure_minute)]
  
  # join both tables together, calculate total travel time and keep only the
  # fastest trip between two points
  
  ttm <- merge(
    first_mile_ttm,
    remaining_ttm,
    by.x = c("toId", "travel_time"),
    by.y = c("fromId", "departure_minute"),
    allow.cartesian = TRUE
  )
  
  setnames(
    ttm,
    old = c("toId", "travel_time", "fromId", "toId.y", "travel_time.y"),
    new = c("intermediateId", "first_mile_time", "fromId", "toId", "remn_time")
  )
  
  ttm[, travel_time := first_mile_time + remn_time]
  ttm <- ttm[
    ttm[, .I[travel_time == min(travel_time)], by = .(fromId, toId)]$V1
  ]
  
  # there may be many trips between the same two points whose travel time equals
  # to the minimum travel time (e.g. imagine that you can get from point A to
  # point B using 3 stops as intermediate bike-first-mile stations and two of
  # those trips have the same total travel time, which is lower than the third).
  # therefore we need to filter 'ttm' to keep only one entry for each pair,
  # otherwise we will double (triple, quadruple, ...) count the opportunities
  # when estimating the accessibility.
  
  ttm <- ttm[ttm[, .I[1], by = .(fromId, toId)]$V1]
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_bike_first_mile.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}


# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# bike_matrix_path <- tar_read(bike_matrix)[1]
# transit_matrix_path <- tar_read(transit_matrix)[1]
# bfm_matrix_path <- tar_read(bike_first_mile_matrix)[1]
# points_path <- tar_read(points_path)[1]
join_ttms <- function(city,
                      scenario,
                      bike_matrix_path,
                      transit_matrix_path,
                      bfm_matrix_path,
                      points_path) {
  
  bike_matrix <- readRDS(bike_matrix_path)
  transit_matrix <- readRDS(transit_matrix_path)
  bfm_matrix <- readRDS(bfm_matrix_path)
  
  points <- fread(points_path, select = "id_hex")
  setnames(points, "id")
  
  ttm <- setDT(expand.grid(toId = points$id, fromId = points$id))
  
  ttm[bike_matrix, on = c("toId", "fromId"), bike_time := i.travel_time]
  ttm[transit_matrix, on = c("toId", "fromId"), transit_time := i.travel_time]
  ttm[bfm_matrix, on = c("toId", "fromId"), bfm_time := i.travel_time]
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "ttmatrix", scenario
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- file.path(dir_path, "ttmatrix_full.rds")
  saveRDS(ttm, file_path)
  
  return(file_path)
  
}


# city <- tar_read(only_for)
# ttm_path <- tar_read(full_matrix)[1]
# scenario <- tar_read(scenarios)[1]
# bike_parks_path <- tar_read(bike_parks_path)[1]
# grid_path <- tar_read(grid_path)[1]
# exploratory_skeleton <- tar_read(exploratory_skeleton)
exploratory_report <- function(city,
                               ttm_path,
                               scenario,
                               bike_parks_path,
                               grid_path,
                               exploratory_skeleton) {
  
  ttm <- readRDS(ttm_path)
  bike_parks <- fread(bike_parks_path)
  
  random_points <- data.table(
    hex_name = c("aldeota", "parangaba", "luciano_cavalcanti", "carlito_pamplona"),#, "ponto_estranho", "ponto_ao_lado", "ponto_abaixo"),
    lon = c(-38.499719, -38.562038, -38.487955, -38.558178),# -38.54842, -38.55137, -38.54847),
    lat = c(-3.740666, -3.777242, -3.776920, -3.717195)#, -3.708078, -3.706314, -3.711516)
  )
  random_points <- st_as_sf(random_points, coords = c("lon", "lat"), crs = 4326)
  
  grid <- readRDS(grid_path)
  
  # suppress "attribute variables are assumed constant" warning
  suppressWarnings(
    relevant_ids <- st_intersection(grid, random_points)$id_hex
  )
  
  setDT(random_points)[, hex_id := relevant_ids]
  
  ttm <- ttm[fromId %in% relevant_ids]
  ttm[setDT(random_points), on = c(fromId = "hex_id"), hex_name := i.hex_name]
  
  # save exploratory analysis report to new folder
  
  report_dir <- file.path("../../data/avaliacao_intervencoes", city, "reports")
  if (!dir.exists(report_dir)) dir.create(report_dir)
  
  filename <- normalizePath(
    file.path(report_dir, paste0("exploratory_analysis_", scenario, ".html"))
  )
  
  rmarkdown::render(
    exploratory_skeleton,
    output_file = filename,
    params = list(
      ttm = ttm,
      scenario = scenario,
      bike_parks = bike_parks,
      grid = grid,
      random_points = random_points
    ),
    quiet = TRUE
  )
  
}