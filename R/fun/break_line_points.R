
# line1 <- line %>%
#   st_cast("POINT") %>%
#   slice(1:152) %>%
#   group_by(Name) %>%
#   summarise(do_union = FALSE) %>%
#   st_cast("LINESTRING")


break_line_points <- function(line, res) {
  
  
  line_points <- st_segmentize(line, res) %>%
    st_cast("POINT")
  
}



# mapview(line_points)
