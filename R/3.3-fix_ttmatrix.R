library(readr)
library(data.table)
library(sf)
library(Hmisc)


# scenario <- "antes"

identificar_e_corrigir_extremos_acess_muni <- function(scenario) {
  
  
  ttmatrix_path <- sprintf("../../data/avaliacao_intervencoes/for/ttmatrix/%s/ttmatrix_transit.rds", scenario)
  ttmatrix_allmodes <- read_rds(ttmatrix_path)
  ttmatrix_allmodes <- ttmatrix_allmodes[, .(origin = fromId, destination = toId, travel_time)]
  
  # ttmatrix_allmodes <- read_rds("../../data/avaliacao_intervencoes/for/ttmatrix/antes/ttmatrix_bike.rds")
  
  # pegar so bike
  # ttmatrix_teste <- ttmatrix_allmodes[mode == "bike"]
  # ttmatrix_teste <- ttmatrix_allmodes[mode == "transit" & pico == 1]
  ttmatrix_teste <- copy(ttmatrix_allmodes)
  
  # abrir os pontos da resolucao 09 ~~~~
  points_file <-  paste0(
    "../../data/avaliacao_intervencoes/r5/points/points_",
    "for",
    "_09_2019.csv"
  )
  # points_file <- "../../data/avaliacao_intervencoes/r5/points/points_for_09_2019.csv"
  points <- fread(points_file)
  
  
  # 1) Identificar quais pontos nao foram roteados --------------------
  
  # checar os pontos na matrix ~~~~
  origem_matrix <- unique(ttmatrix_teste$origin)
  # origem_matrix <- unique(ttmatrix_teste$fromId)
  destino_matrix <- unique(ttmatrix_teste$destination)
  # destino_matrix <- unique(ttmatrix_teste$toId)
  
  ## quais origens e destinos ficaram fora? ~~~~
  origem_fora <- setdiff(points$id_hex, origem_matrix) 
  destino_fora <- setdiff(points$id_hex, destino_matrix)
  
  # quais pontos ficaram fora completamente? tanto a origem como o destino ~~
  # para pt
  hex_problematicos_etapa1 <- intersect(origem_fora, destino_fora)
  
  
  
  # 2) Identificar hexagonos de acessibilidade extrema --------------------
  
  # Criar variavel de 1 oportunidade para calculo da "acessibilidade"
  ttmatrix_teste[, var := 1]
  
  # Calcular acess para 90 minutos
  acess_origin <- ttmatrix_teste[,
                                 .(acess = (sum(var[which(travel_time <= 90)], na.rm = T))),
                                 by=.(origin)]
  # rename id column
  setnames(acess_origin, "origin", "id_hex")
  
  acess_dest <- ttmatrix_teste[,
                               .(acess = (sum(var[which(travel_time <= 90)], na.rm = T))),
                               by=.(destination)]
  setnames(acess_dest, "destination", "id_hex")
  
  
  # extrair hexagonos que nao consigam acessar mais que 20 hexagonos
  acess_prob <- rbind(acess_origin[acess < 20],
                      acess_dest[acess < 20]) %>%
    dplyr::distinct(id_hex, .keep_all = TRUE)
  
  hex_problematicos_etapa2 <- acess_prob$id_hex
  ttmatrix_teste$var <- NULL
  
  
  
  # 3) Juntar os hexagonos da etapa 1 e 2 fazer o teste de distancia --------------------
  
  # pegar todos os hexagonos problematicos
  points_prob <- points %>%
    dplyr::filter(id_hex %in% c(hex_problematicos_etapa1, hex_problematicos_etapa2))
  
  # 4) Corrigir os hexagonos problematicos nas matrizes originais de tempo de viagem ---------------
  
  
  # # # separar na base os problematicos e nao problematicos
  # # esses sao os hex que vao ser utilizados para a correcao dos problematicos
  # ttmatrix_allmodes_blueprint <- setDT(ttmatrix_allmodes)[origin %in%  points_corrigidos$hex_blueprint |
  #                                                           destination %in% points_corrigidos$hex_blueprint]
  # 
  # esses sao os hex que NAO vao ser utilizados para a correcao dos problematicos
  ttmatrix_allmodes_nprob <- setDT(ttmatrix_allmodes)[!(origin %in%  points_prob$id_hex |
                                                          destination %in% points_prob$id_hex)]
  
  
  # funcao para corrigir os hexagonos nas ttmatrix 
  
  corrigir_hex_ttmatrix <- function(hex_prob) {
    
    # hex_prob <- "89818a593cfffff" # nat
    # hex_prob <- "8980104e90bffff" # for
    # hex_prob <- "89801048d37ffff" # for
    # hex_prob <- "8980104e973ffff" # for 8980104e973ffff
    # for 8980104e90fffff nao foi corrigido
    
    # ver quais sao os vizinhos desse hexagono
    # ring_size = 3 vai me trazer todos os hex vizinhos ate o nivel 3, isso da 36 vizinhos
    
    hex_prob_vizinhos <- h3jsr::get_kring(hex_prob, ring_size = 3)[[1]][-1]
    
    # garantir que os vizinhos nao sao problematicos
    hex_prob_vizinhos <- hex_prob_vizinhos[hex_prob_vizinhos %nin% points_prob$id_hex]
    
    
    
    # garantir que os vizinhos estejam dentro da matriz de tempo de viagem da cidade
    # podem acontecer dois casos em que o(s) hex(s) vizinho(s) esteja(m) fora da matriz:
    # 1) o hex problematico ser de borda/perto mar e tem varios vizinhos na cidade vizinha
    # ou no oceano
    # 2) o hex vizinho nao ter pop/atividade e nao ter entrado pro r5r
    hex_prob_vizinhos <- hex_prob_vizinhos[hex_prob_vizinhos %in% points$id_hex]
    
    # pegar so os 6 primeiros vizinhos
    hex_prob_vizinhos <- hex_prob_vizinhos[1:6] %>% na.omit()
    
    # a <- points[id_hex %in% hex_prob_vizinhos] %>% st_as_sf(coords = c("X", "Y"), crs = 4326)
    # b <- points[id_hex %in% hex_prob] %>% st_as_sf(coords = c("X", "Y"), crs = 4326)
    # mapview(a) + b
    
    # ATENCAO: eh necessario corrigir o hex tanto pra quando ele eh origem na matriz
    # como pra quando ele eh destino na matrix
    
    # para a origem:
    # filtrar entao somente a matrix para os hexagonos vizinhos que nao sao problematicos - origem
    ttmatrix_allmodes_blueprint_origin <- ttmatrix_allmodes[origin %in% hex_prob_vizinhos]
    
    # essses hexagonos de origem agora serao representados somente por um hex, o problematico - origem
    ttmatrix_allmodes_blueprint_origin$origin <- hex_prob
    
    # calcular entao a media do tempo de viagem, que vai representar o novo tt do hex prob - origem
    ttmatrix_allmodes_blueprint_origin_mean <- ttmatrix_allmodes_blueprint_origin[, .(travel_time = mean(travel_time, na.rm = TRUE)),
                                                                                  by = .(origin, destination)]
    
    # para o destino:
    # filtrar entao somente a matrix para os hexagonos vizinhos que nao sao problematicos - destino
    ttmatrix_allmodes_blueprint_dest <- ttmatrix_allmodes[destination %in% hex_prob_vizinhos]
    
    # essses hexagonos de destino agora serao representados somente por um hex, o problematico - destino
    ttmatrix_allmodes_blueprint_dest$destination <- hex_prob
    
    # calcular entao a media do tempo de viagem, que vai representar o novo tt do hex prob - destino
    ttmatrix_allmodes_blueprint_dest_mean <- ttmatrix_allmodes_blueprint_dest[, .(travel_time = mean(travel_time, na.rm = TRUE)),
                                                                              by = .(origin, destination)]
    
    
    # juntar as matrizes de quando eh origem e de quando eh destino
    ttmatrix_allmodes_blueprint_fim <- rbind(ttmatrix_allmodes_blueprint_origin_mean, ttmatrix_allmodes_blueprint_dest_mean)
    
    # nrow(ttmatrix_allmodes_blueprint_fim)
    # nrow(distinct(ttmatrix_allmodes_blueprint_fim, origin, destination, mode, pico))
    
    # # qual id correto correspondente
    # hex_correto <- subset(points_corrigidos, hex_problema==hex_prob)$hex_blueprint
    # 
    # # subset de todas origem e destino com id correto
    # temp_tt <- setDT(ttmatrix_allmodes_blueprint)[ origin == hex_correto | destination==hex_correto]
    # 
    # # Substituir id correto pelo id q deu errado
    # temp_tt[, origin := fifelse(origin==hex_correto, hex_prob, origin) ]
    # temp_tt[, destination := fifelse(destination==hex_correto, hex_prob, destination) ]
    
    return(ttmatrix_allmodes_blueprint_fim)
    
  }  
  
  
  # aplicar funcao para cada hex problematico
  ttmatrix_hex_prob_corrigidos <- lapply(points_prob$id_hex, corrigir_hex_ttmatrix) %>%
    rbindlist()
  
  
  # juntar com a base original
  # excluir os pares OD dos corrigidos na base original e juntar
  ttmatrix_hex_fim <- rbind(ttmatrix_allmodes_nprob,
                            ttmatrix_hex_prob_corrigidos)
  
  
  
  # corrigir tempo de viagem para mesma origem/destino ----------------------
  
  
  # Se a origem e o destino forem o mesmo, adotar o tempo de viagem como:
  # transit / walk: 350s equivale ao tempo necessario para cruzar um hexagono a de pe (~1 metro/sec = ~3.6 km/h)
  # bike: 110s equivale ao tempo necessario para cruzar um hexagono a bicicleta (~3.3 metros/sec = ~12 km/h)
  ttmatrix_hex_fim[, travel_time := as.numeric(travel_time)]
  ttmatrix_hex_fim[, travel_time := fifelse(origin == destination, 5.8, travel_time)]
  ttmatrix_hex_fim <- ttmatrix_hex_fim[, .(fromId = origin, toId = destination, travel_time)]
  
  ttmatrix_path_out <- sprintf("../../data/avaliacao_intervencoes/for/ttmatrix/%s/ttmatrix_transit_corrigido.rds", scenario)
  
  
  
  # salvar output corrigido
  saveRDS(ttmatrix_hex_fim, ttmatrix_path_out)
  
  
  
  rm(ttmatrix_allmodes)
  rm(ttmatrix_allmodes_nprob)
  rm(ttmatrix_hex_fim)
  gc(TRUE)
  
}

identificar_e_corrigir_extremos_acess_muni("antes")
identificar_e_corrigir_extremos_acess_muni("depois")
identificar_e_corrigir_extremos_acess_muni("contrafactual")
