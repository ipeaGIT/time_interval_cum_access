library(sf)
library(dplyr)
library(readr)

"../../git_kaue/acesso_oport/R/fun/setup.R"


paradas <- st_read("paradas_metrofor_fortaleza.gpkg") %>%
  mutate(tipo_local = "estacao") %>%
  select(nome = stop_name, tipo_local)



# extrair terminais
terminais <- read_rds("terminais.rds") %>%
  st_centroid() %>%
  mutate(tipo_local = "terminal") %>%
  select(nome = terminal, tipo_local)

rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}


paradas <- rename_geometry(paradas, "geom")
terminais <- rename_geometry(terminais, "geom")

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


estacionamento_bibicleta <- rbind(paradas, terminais) %>%
  sfc_as_cols(.) %>%
  select(id = tipo_local, lon, lat)

# save
data.table::fwrite(estacionamento_bibicleta, "../../data/avaliacao_intervencoes/r5/points/bike_parks_for.csv")
