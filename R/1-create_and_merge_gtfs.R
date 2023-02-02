library(dplyr)
library(data.table)
library(tidyr)
library(sf)
library(mapview)
library(googlesheets4) # install.packages("googlesheets4")
library(gtfstools) # install.packages('gtfstools')
library(Hmisc)
source("R/fun/break_line_points.R")
source("R/fun/crop_line_btw_points.R")



# 1) Fortaleza - metro linha leste ex-ante ---------------------------------------------------

gtfs <- "../../data-raw/avaliacao_intervencoes/for/gtfs_for_metrofor_2021-01.zip"


# open tables - they are in a spreadsheet
headways_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                          sheet = "headways_df", skip = 11) %>% setDT()



ttime_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "ttime_df", skip = 11) %>% setDT()


stops_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                       sheet = "stops_df", skip = 11) %>% setDT()


routes_df <- read_sheet("https://docs.google.com/spreadsheets/d/1vhz_cV1rRj6aKPpROmZCe-4fyepp2E3-b_91pBsZ73I/edit?usp=sharing",
                        sheet = "routes_df", skip = 11) %>% setDT()


# open lines
line_shape <- st_read("../../data-raw/avaliacao_intervencoes/for/linha_leste_kaue_gearth.gpkg") %>%
  # identify route and direction
  mutate(route_id = "LL", 
         direction_id = 0) %>%
  select(route_id, direction_id)

# a linha leste so tem um shape - o de ida
# para pegar o shape de volta, eh necessario reverter a ordem do shape para que os pontos resultantes
# estejam no sentido certo
# depois, juntar os arquivos e colocar o sentido
line_shape <- rbind(line_shape, st_reverse(line_shape)) %>%
  mutate(direction_id = c(0, 1))


# stops_sf <- st_as_sf(stops_df, coords = c("stop_lon", "stop_lat"),
#                      # crs SIRGAS 2000 / UTM zone 24S
#                      crs = 31984
# ) %>%
#   st_transform(4326)
# 
# mapview(stops_sf)



# apply function to create the new gtfs and merge to the original one
purrr::walk(list.files("R/fun", full.names = TRUE), source)

a <- create_merge_gtfs(gtfs = gtfs,
                       headways_df = headways_df,
                       ttime_df = ttime_df,
                       stops_df = stops_df,
                       routes_df = routes_df,
                       line_shape = line_shape,
                       service_id = "weekdays",
                       stops_crs = 31984
)


# change frequency of lines - metrofor ------------------

source("R/fun/change_trip_frequency.R")

# table with new frequency
metrofor_new_frequency <- data.frame(
  route_id = c(8, 6, 7),
  frequency = c(7.5, 6, 20)
)

# lines to be changed frequency
gtfs_metrofor_new <- change_trip_frequency(a, 
                                           routes = metrofor_new_frequency$route_id, 
                                           new_freqs = metrofor_new_frequency$frequency,
                                           services = "")


# export gtfs
gtfstools::write_gtfs(a, path = "../../data/avaliacao_intervencoes/for/gtfs/gtfs_for_metrofor_2021-01_depois.zip")



# change frequency of lines - etufor ------------------

# Open gtfs
gtfs_etufor <- read_gtfs("../../data-raw/avaliacao_intervencoes/for/gtfs/gtfs_for_etufor_2019-10.zip")

# open data with the new frequencies
linhas_mudancas <- fread("../../data/avaliacao_intervencoes/for/etufor_linhas_mudancas.csv",
                         colClasses = "character")
# pegar somente linhas que nao vao ser excluidas
table(linhas_mudancas$parecer, useNA = 'always')
linhas_mudancas_stay <- linhas_mudancas %>% filter(parecer != "Eliminada")
# filter: removed 95 rows (31%), 212 rows remaining
linhas_mudancas_stay <- linhas_mudancas_stay %>% filter(!is.na(intervalo_entre_partidas_min_9))
linhas_mudancas_stay <- linhas_mudancas_stay %>% filter(intervalo_entre_partidas_min_9 != "")
# filter: removed 6 rows (3%), 206 rows remaining
# mudar frequencia
gtfs_etufor_new <- change_trip_frequency(gtfs_etufor, 
                                         routes = linhas_mudancas_stay$cod_linha, 
                                         new_freqs = linhas_mudancas_stay$intervalo_entre_partidas_min_9,
                                         services = "U")
# deu ruim para a linha 348, que nao tem viagens pela manha


# delete lines - etufor ------------------
linhas_mudancas_delete <- linhas_mudancas %>% filter(parecer == "Eliminada")
# filter: removed 212 rows (69%), 95 rows remaining
gtfs_etufor_new <- gtfstools::filter_route_id(gtfs_etufor_new,
                                              route_ids = linhas_mudancas_delete$cod_linha,
                                              keep = FALSE)



# export gtfs
gtfstools::write_gtfs(gtfs_etufor_new, path = "../../data/avaliacao_intervencoes/for/gtfs/gtfs_for_etufor_2019-10_depois.zip")




