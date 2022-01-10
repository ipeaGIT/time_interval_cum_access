source("../acesso_oport/R/fun/setup.R")
library(ggnewscale)


theme_for_CMA <- function(base_size, ...) {
  
  # theme_void(base_family="Roboto Condensed") %+replace%
  theme_void() %+replace%
    
    theme(
      legend.position = "bottom",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.width=unit(2,"line"),
      legend.key.height = unit(0.2,"cm"),
      legend.text=element_text(size=rel(0.5)),
      legend.title=element_text(size=rel(0.5)),
      # plot.title = element_text(hjust = 0, vjust = 4),
      ...
      
      
    )
}


# abrir linhas
linha_leste <- read_sf("linhas_metrofor_fortaleza.gpkg") %>%
  filter(route_long_name == "Metro Linha Leste")

# abrir paradas
paradas_leste <- read_sf("paradas_metrofor_fortaleza.gpkg") %>%
  filter(linha == "Linha Leste") %>%
  filter(stop_name %in% c("Xico da Silva", "Colegio Militar",
                          "Nunes Valente", "Papicu"))

mapview(linha_leste) + paradas_leste




mapboxapi::mb_access_token("pk.eyJ1Ijoia2F1ZWJyYWdhIiwiYSI6ImNqa2JoN3VodDMxa2YzcHFxMzM2YWw1bmYifQ.XAhHAgbe0LcDqKYyqKYIIQ", install = TRUE)



isocronas_leste <- mapboxapi::mb_isochrone(paradas_leste,
                                           profile = "walking",
                                           time = 15,
                                           id_column = "stop_name")

mapview(isocronas_leste) + linha_leste + paradas_leste

map_tiles <- read_rds("../../data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_for_2019.rds")

# calcular bbox da linha leste para viz
centroide_linha_leste <- linha_leste %>%
  st_transform(3857) %>%
  st_bbox()

ggplot()+
  geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(data = st_transform(linha_leste, 3857))+
  geom_sf(data = st_transform(paradas_leste, 3857))+
  geom_sf(data = st_transform(isocronas_leste, 3857), aes(fill = id), alpha = 0.5)+
  labs(fill = "Estações")+
  coord_sf(expand = FALSE,
           xlim = c(centroide_linha_leste$xmin - 2000, 
                    centroide_linha_leste$xmax + 2000),
           ylim = c(centroide_linha_leste$ymin - 2000, 
                    centroide_linha_leste$ymax + 2000)) +
  theme_aop_map()



ggsave("figures/for/map_carac_for.png", 
       height = 10, width = 16,
       units = "cm", device = "png", dpi = 300)

# calcular populacao nas isocronas
hex_for <- read_rds("../../data/acesso_oport/hex_agregados/2019/hex_agregado_for_09_2019.rds") %>% select(pop_total, quintil, decil)

# intersecao
isocronas_leste_pop <- isocronas_leste %>%
  st_join(hex_for %>% filter(decil != 0)) %>%
  st_set_geometry(NULL) %>%
  group_by(decil) %>%
  summarise(pop_sum = sum(pop_total, na.rm = TRUE))


ks <- function (x) { scales::label_number(accuracy = 1,
                                          scale = 1/1000,
                                          suffix = "k",
                                          big.mark = ",")(x)
  
}

ggplot()+
  geom_col(data = isocronas_leste_pop, aes(x = as.factor(decil), y = pop_sum))+
  labs(x = "Decil de renda", y = "Populacao") +
  scale_y_continuous(labels = ks)+
  coord_flip()+
  theme_ipsum(grid = "X")+
  theme( 
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 7),
    axis.title.y = element_text(size = 7, face="bold"),
    legend.title = element_text(size = 7),
    plot.margin=unit(c(0,0,0,0),"mm")
    
  )


ggsave("figures/for/graph_carac_for.png", 
       height = 10, width = 8,
       units = "cm", device = "png", dpi = 300)
