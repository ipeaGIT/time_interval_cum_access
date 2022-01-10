source("../acesso_oport/R/fun/setup.R")
library(ggnewscale)


gtfs_antes <- gtfstools::read_gtfs("../../data-raw/gtfs/goi/2019/gtfs_goi_rmtc_2019-10.zip")
gtfs_depois <- gtfstools::read_gtfs("../../data/avaliacao_intervencoes/goi/gtfs_goi_rmtc_2019-10_depois.zip")

# pegar servico principal
service <- gtfs_antes$calendar %>%
  filter(monday == 1 & tuesday == 1 &  wednesday == 1 &  thursday == 1 &  friday == 1)

# extrair linhas
routes_antes <- gtfs_antes$routes[route_id %like% "006|007|013|107|600|611"]$route_id # thenew lines
routes_depois <- gtfs_depois$routes[route_id %like% "BRT_006|BRT_007|BRT_013"]$route_id # thenew lines

# pegar shapes
shapes_antes <- gtfs_antes$trips[route_id %in% routes_antes]$ shape_id %>% unique()
shapes_antes <- gtfstools::convert_shapes_to_sf(gtfs_antes) %>% filter(shape_id %in% shapes_antes) %>%
  mutate(route_id = substr(shape_id, 1, 3)) %>%
  group_by(route_id) %>%
  summarise() %>%
  st_cast("MULTILINESTRING")
shapes_depois <- gtfs_depois$trips[route_id %in% routes_depois]$ shape_id %>% unique()
shapes_depois <- gtfstools::convert_shapes_to_sf(gtfs_depois) %>% filter(shape_id %in% shapes_depois) %>%
  mutate(route_id = substr(shape_id, 1, 7)) %>%
  group_by(route_id) %>%
  summarise() %>%
  st_cast("MULTILINESTRING")

map_tiles <- read_rds(sprintf("../../data/acesso_oport/maptiles_crop/2017/mapbox/maptile_crop_mapbox_%s_2017.rds", 'goi'))
# abrir city limits
city_shape <- geobr::read_municipality(5208707)

# transform map elements to UTM
linhas_shape_antes <- st_transform(shapes_antes, 3857)
linhas_shape_depois <- st_transform(shapes_depois, 3857)
city_shape_map <- st_transform(city_shape, 3857)

linhas_excluidas <- ggplot()+
  geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = linhas_shape_antes,aes(color = route_id), size = 0.8, alpha = 0.5)+
  # geom_sf(data = linhas_shape, aes(color = route_long_name1), size = 0.5, alpha = 0.7)+
  geom_sf(data= city_shape_map, fill = NA)+
  labs(color = "Linha")+
  theme_mapa()

linhas_incluidas <- ggplot()+
  geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = linhas_shape_depois,aes(color = route_id), size = 0.8, alpha = 0.5)+
  # geom_sf(data = linhas_shape, aes(color = route_long_name1), size = 0.5, alpha = 0.7)+
  geom_sf(data= city_shape_map, fill = NA)+
  labs(color = "Linha")+
  theme_mapa()


ggsave(plot = linhas_excluidas, 
       filename = "figures/goi/maps_intervencoes/map_intervencoes_antes.png",
       height = 10, width = 10,
       # height = 14, width = 16,
       units = "cm", device = "png", dpi = 300)

ggsave(plot = linhas_incluidas, 
       filename = "figures/goi/maps_intervencoes/map_intervencoes_depois.png",
       height = 10, width = 10,
       # height = 14, width = 16,
       units = "cm", device = "png", dpi = 300)
