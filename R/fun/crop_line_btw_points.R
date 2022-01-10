
# points <- stops_df %>%
#   filter(stop_id %in% c("T001", "ENS19B")) %>%
#   st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)
# 
# line <- st_read("../../data-raw/avaliacao_intervencoes/goi/Corredor_BRT.gpkg")


crop_line_btw_points <- function(points, line) {
  
  # break line in multiple points
  source("R/fun/break_line_points.R")
  line_points <- break_line_points(line, 25)
  
  a <- st_distance(points,
                   line_points) %>%
    as.data.frame()
  
  point1_min <- which.min(apply(a[1,],MARGIN=2,min)) %>% as.integer()
  point2_min <- which.min(apply(a[2,],MARGIN=2,min)) %>% as.integer()
  
  # define points that will stay
  points_stay <- seq(from = min(point1_min, point2_min), 
                     to = max(point1_min, point2_min),
                     by = 1)
  
  # select these points on our points sf
  line_points_select <- line_points %>%
    slice(min(point1_min, point2_min):max(point1_min, point2_min))
  
  # get geometry column name
  geom_name <- attr(line_points_select, "sf_column")
  
  # back to line
  line_select <- line_points_select %>%
    group_by(across(c(-geom_name))) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
}




