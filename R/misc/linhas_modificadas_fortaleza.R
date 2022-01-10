library(readxl)
library(dplyr)
library(gtfstools)

# linhas que mudaram em fortaleza
linhas_planilha <- read_xlsx("../../data-raw/avaliacao_intervencoes/for/REDETRANSPORTECOLETIVOPASFOR2025.xlsx", skip = 2)

linhas_planilha <- janitor::clean_names(linhas_planilha)


linhas_planilha <- select(linhas_planilha,
                          cod_linha = x1, parecer = x2, nome_linha = x3, intervalo_entre_partidas_min_9)

linhas_planilha <- linhas_planilha %>%
  mutate(cod_linha = stringr::str_pad(cod_linha, width = 3, pad = 0))


# check theses route_id in gtfs
gtfs <- gtfstools::read_gtfs("../../data-raw/avaliacao_intervencoes/for/gtfs_for_etufor_2019-10.zip")

routes <- gtfs$routes


# a <- setdiff(linhas_planilha$cod_linha, routes$route_id)

linhas_faltantes <- linhas_planilha %>%
  filter(cod_linha %in% routes$route_id)


# save
fwrite(linhas_faltantes, "../../data/avaliacao_intervencoes/for/etufor_linhas_mudancas.csv")


