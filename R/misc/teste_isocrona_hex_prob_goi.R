library(r5r)


id_hex <- c("89a8c0ccd5bffff")
id_hex <- c("89a8c0cc897ffff")

# get isco

ttmatrix <- read_rds("../../data/avaliacao_intervencoes/goi/ttmatrix/ttmatrix_goi_walk.rds")
ttmatrix_hex <- ttmatrix[ttime_antes <= 60 | ttime_depois <= 60]

# get hex
ttmatrix_hex <- ttmatrix_hex[origin %in% id_hex]

ttmatrix_hex %>% 
  filter(ttime_antes <= 60 & ttime_depois > 60) %>% nrow()

ttmatrix_hex %>% 
  filter(ttime_antes > 60 & ttime_depois <= 60) %>% nrow()


# abrir hex
hex <- read_rds("../../data/acesso_oport/hex_municipio/2017/hex_goi_09_2017.rds") %>% dplyr::select(id = id_hex)

hex_go <- hex %>% filter(id %in% c("89a8c0ccd5bffff", "89a8c0cc897ffff")) %>% st_centroid() %>% sfc_as_cols() %>%
  setDT()

# hex_go <- hex %>% filter(id %in% c("89a8c0ccd5bffff", "89a8c0ccd43ffff")) %>% st_centroid() %>% sfc_as_cols() %>%
#   setDT()

# from one to another
r5r_core <- setup_r5(data_path = sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_depois", "goi"), verbose = TRUE)
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 3000   # meters
max_trip_duration <- 180 # minutes
departure_datetime <- as.POSIXct("02-10-2019 06:00:00", format = "%d-%m-%Y %H:%M:%S")


ttm2 <- detailed_itineraries(r5r_core = r5r_core,
                             origins = hex_go[2,],
                             destinations = hex_go[1,],
                             mode = mode,
                             departure_datetime = departure_datetime)

mapview(ttm2, zcol = "mode")

# trazer paradas
gtfs <- read_gtfs("../../data/avaliacao_intervencoes/goi/gtfs_goi_rmtc_2019-10_depois.zip")$stops
# gtfs <- read_gtfs("../../data-raw/gtfs/goi/2019/gtfs_goi_rmtc_2019-10.zip")$stops

stops <- gtfs[stop_id %like% "ENS"] %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

# stops <- gtfs %>%
#   st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

mapview(go, zcol = "dif_rel", col.regions = RColorBrewer::brewer.pal(10, "RdBu"), col = NULL) + stops


go %>% filter(origin %in% c("89a8c0ccd5bffff", "89a8c0cc897ffff")) %>% View()
