rm(list = ls())


library("readxl")
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(purrr)
library(data.table)
library(tidyr)
library(readr)
library(sf)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(scales)


# 00 - abreviacoes --------------------------------------------------------

# acc - acessibility 
# ti - time interval 
# co - cutoff 
# sum - summary 

set.seed(100)

options(scipen = 9999)

hexagonos <- st_read(dsn = 'R/data/hex_agregado_for_09_2019.gpkg')

map_tiles <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_", "for","_2019.rds")) 

#  time interval -  -------------------------------------------------------------------

acc_current <- read_rds('R/data/transit_access_Current.rds') %>%  mutate(situ = "current")
acc_diff <- read_rds('R/data/transit_access_diff_v2.rds') %>% mutate(situ = "diff") %>% select(-scenario,-type)

acc_data <- rbind(acc_current,acc_diff)

vector.janela <- seq(0,120,5) # a resolucao minima da janela eh 5 min 

# possiveis combinacoes de janela 

combinations <- expand.grid(inicio = vector.janela,fim = vector.janela) %>% 
  filter(fim != 0) %>% 
  filter(fim > inicio) %>% 
  filter(inicio > 20) %>% 
  filter(inicio < 100) %>% 
  filter(fim - inicio >= 10)


# para teste da funcao 
inicio = 30
fim = 60
situ.teste = "current"

# calcula a acessibildiade para um hex a partir da escolha de uma janela 

calculate_acc_timeinterval_hexs <- function(inicio,fim,situ){
  
  vamo <- setDT(acc_data)[situ == situ.teste,] # escolhe a situacao
  vamo1 <- vamo[travel_time > inicio & travel_time < fim] # filtra a janela 
  vamo2 <- vamo1[,.(acc = mean(only_transit_CMATT,na.rm = T)), by = .(fromId)] # resume a acessibilidade por hex
  vamo3 <- vamo2[,c("janela","tamanho_janela") := .(paste(inicio,fim,sep = "|"),fim-inicio)] # adiciona informacao da janela utilizada
  
}


# valores de acessibilidade para todos os hexagonos a depender da janela 

resumo_acc_timeinterval_hex_current <- purrr::map2_df(.x = combinations$inicio,
                                                      .y = combinations$fim,
                                                      .f = calculate_acc_timeinterval_hexs,
                                                      situ = "current")

resumo_acc_timeinterval_hex_diff <- purrr::map2_df(.x = combinations$inicio,
                                                      .y = combinations$fim,
                                                      .f = calculate_acc_timeinterval_hexs,
                                                      situ = "diff")


combinacoes_janelas <- resumo_acc_timeinterval_hex_current %>% distinct(janela,tamanho_janela)

num.simu <- 1
tamanho_janela.t = "NULL"
tamanho_amostra = 4
tamanho_janela.t = 20 
tamanho_janela = 15
t_janela = 10

simula_sd_current_hex_ti <- function(num.simu,t_janela,tamanho_amostra){
  
  
  if(is.null(t_janela) == TRUE) {
    
    # sorteia as janelas 
   
    janelas_sorteadas <- setDT(combinacoes_janelas)[sample(.N,tamanho_amostra)]
    
    # resumo 
    
    vamo <- setDT(resumo_acc_timeinterval_hex_current)[janela %in% janelas_sorteadas$janela,]
    vamo1 <- vamo[,num.simu := num.simu]
    vamo1[,janelas_utilizadas := paste(unique(vamo1$janela),collapse = ",")]
    vamo1[,estrategia := "random intervals"]
    
    
    
    return(vamo1)
  }
  
  else {
    
    janelas_sorteadas2 <- setDT(combinacoes_janelas)[tamanho_janela == t_janela]
    janelas_sorteadas3 <- janelas_sorteadas2[sample(.N,tamanho_amostra)]
    
    vamo <- setDT(resumo_acc_timeinterval_hex_current)[janela %in% janelas_sorteadas3$janela,]
    vamo1 <- vamo[,num.simu := num.simu]
    vamo1[,janelas_utilizadas := paste(unique(vamo1$janela),collapse = ",")]
    vamo1[,estrategia := paste0("interval width -",t_janela," min")]
    
    return(vamo1)
    
  }

  
}

simula_sd_diff_hex_ti <- function(num.simu,t_janela,tamanho_amostra){
  
  
  if(is.null(t_janela) == TRUE) {
    
    # sorteia as janelas 
    
    janelas_sorteadas <- setDT(combinacoes_janelas)[sample(.N,tamanho_amostra)]
    
    # resumo 
    
    vamo <- setDT(resumo_acc_timeinterval_hex_diff)[janela %in% janelas_sorteadas$janela,]
    vamo1 <- vamo[,num.simu := num.simu]
    vamo1[,janelas_utilizadas := paste(unique(vamo1$janela),collapse = ",")]
    vamo1[,estrategia := "random intervals"]
    
    return(vamo1)
  }
  
  else {
    
    janelas_sorteadas2 <- setDT(combinacoes_janelas)[tamanho_janela == t_janela]
    
    janelas_sorteadas3 <- janelas_sorteadas2[sample(.N,tamanho_amostra)]
    
    vamo <- setDT(resumo_acc_timeinterval_hex_diff)[janela %in% janelas_sorteadas3$janela,]
    vamo1 <- vamo[,num.simu := num.simu]
    vamo1[,janelas_utilizadas := paste(unique(vamo1$janela),collapse = ",")]
    vamo1[,estrategia := paste0("interval width -",t_janela," min")]
    
    return(vamo1)
    
  }
  
  
}

teste <- simula_sd_diff_hex_ti(num.simu = 1,t_janela = 10,tamanho_amostra = 4) # ok

# resumo simulacao current scenario 

tictoc::tic()
summary_simulation_current_scenario_ti <- map_df(.x = 1:10000,simula_sd_current_hex_ti, t_janela = NULL,
                                              tamanho_amostra = 4)
tictoc::toc()

head(summary_simulation_current_scenario_ti)

hist(summary_simulation_current_scenario_ti$tamanho_janela)

# write_rds(summary_simulation_current_scenario,'R/data/output/summary_simulation_current_scenario.rds')

# resumo simulacao diff scenario 

tictoc::tic()
summary_simulation_diff_scenario_ti <- map_df(.x = 1:10000,simula_sd_diff_hex_ti, t_janela = NULL,
                                              tamanho_amostra = 4)
tictoc::toc()

head(summary_simulation_diff_scenario_ti)

# write_rds(summary_simulation_diff_scenario,'R/data/output/summary_simulation_diff_scenario.rds')

# resumo para tamanho fixo de janela 


summary_simulation_current_scenario_ti_janela_fixa_10 <- map_df(.x = 1:10000,
                                                                 t_janela = 10,
                                                                 simula_sd_current_hex_ti,
                                                                 tamanho_amostra = 4)

summary_simulation_current_scenario_ti_janela_fixa_20 <- map_df(.x = 1:10000,
                                                                 t_janela = 20,
                                                                 simula_sd_current_hex_ti,
                                                                 tamanho_amostra = 4)

summary_simulation_current_scenario_ti_janela_fixa_30 <- map_df(.x = 1:10000,
                                                                 t_janela = 30,
                                                                 simula_sd_current_hex_ti,
                                                                 tamanho_amostra = 4)



# cutoff ------------------------------------------------------------------

cutoff_hexs <- acc_data  %>% 
  filter(travel_time %in% seq(0,120,1)) %>% 
  filter(travel_time > 20 & travel_time <= 100) %>% 
  group_by(fromId) %>% 
  mutate(acc = only_transit_CMATT) %>% 
  select(-only_transit_CMATT)

combinacoes_co <- data.frame(id = unique(cutoff_hexs$travel_time))

situ.teste = "current"

simula_sd_co <- function(num.simu, sit, tamanho_amostra){
  
  co_sort <- combinacoes_co %>% sample_n(tamanho_amostra)
  
  vamo <- setDT(cutoff_hexs)[situ == sit]
  
  vamo1 <- vamo[travel_time %in% co_sort$id]
  
  unique(vamo1$travel_time)
  
  vamo2 <- vamo1[,num.simu := num.simu]
  
  vamo2[,cutoffs_utilizados := paste(unique(vamo2$travel_time),collapse = ",")]
  vamo3 <- vamo2[, !c("situ")]
  
  length(unique(vamo3$fromId))
  
  return(vamo3)

  
}

teste <- simula_sd_co(num.simu = 1,sit = "current",tamanho_amostra = 5)

# resumo cutoff current
tictoc::tic()
summary_simulation_current_scenario_co <- map_df(.x = 1:10000,
                                                 simula_sd_co,
                                                 sit = "current",
                                                 tamanho_amostra = 4)
tictoc::toc()


# resumo cutoff diff

summary_simulation_diff_scenario_co <- map_df(.x = 1:10000,
                                                 simula_sd_co,
                                                 sit = "diff",
                                                 tamanho_amostra = 4)



# Figuras  ----------------------------------------------------------------



x_j <- setDT(summary_simulation_current_scenario_hexs)[,
                                                       by = .(num.simu,fromId),
                                                       .(sd = sd(ACC),ACC = mean(ACC))]

x_j1 <- x_j[, by = .(fromId),
            .(sd = mean(sd),ACC = mean(ACC))]


x1j <- hexagonos %>% select(id_hex,decil) %>% st_as_sf() %>% left_join(x_j1, by = c("id_hex"="fromId"))

# definir limites

library(ggnewscale)

hist(x1j$sd)

ti_current <- ggplot()+
  geom_raster(data = map_tiles, aes(x, y, fill = hex)) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  # geom_sf(data = , aes(fill =empregos_total), color = NA) +
  geom_sf(data = st_transform(x1j, 3857),aes(fill = sd), color = NA) + theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,210000)) +
  labs(fill = 'Standard Deviation') + guides(fill = guide_colorbar(title.position = "top"))
  # theme(legend.position="none",axis.title = element_blank())




# 01 - current cutoff -----------------------------------------------------

cutoff_hexs <- acc_current  %>% 
  filter(travel_time %in% seq(0,120,1)) %>% 
  filter(travel_time > 20 & travel_time <= 100) %>% 
  group_by(fromId) %>% 
  mutate(am = only_transit_CMATT) 

# cutoff_hexs_medio <- cutoff_hexs %>% 
#   group_by(fromId) %>% summarise(am = mean(only_transit_CMATT))

# %>% 
#   group_by(fromId) %>% summarise(sd = sd(am, na.rm = T))  

combinacoes_ids <- data.frame(id = unique(cutoff_hexs$travel_time))


simula_sd_current_hex_co <- function(num.simu){
  
  ids_sort <- combinacoes_ids %>% sample_n(4)
  
  vamo <- cutoff_hexs %>% filter(travel_time %in% ids_sort$id) %>%
    # group_by(fromId) %>% summarise(sd = sd(am,na.rm = T),am = mean(am, na.rm = TRUE)) %>% 
    mutate(num.simu)
  
}

summary_simulation_current_scenario_hexs_cutoff <- map_df(.x = 1:1000,simula_sd_current_hex_co)

write_rds(summary_simulation_current_scenario_hexs_cutoff,'summary_simulation_current_scenario_hexs_cutoff_v2.rds')

summary_simulation_current_scenario_hexs_cutoff <- read_rds('summary_simulation_current_scenario_hexs_cutoff_v2.rds')

head(summary_simulation_current_scenario_hexs_cutoff)

# falta rodar isso aqui 

x_j_co <- setDT(summary_simulation_current_scenario_hexs_cutoff)[,
                                                       by = .(num.simu,fromId),
                                                       .(sd = sd(am),ACC = mean(am))]

x_j1_co <- x_j_co[, by = .(fromId),
            .(sd = mean(sd),ACC = mean(ACC))]


x1j_co <- hexagonos %>% select(id_hex,decil) %>% st_as_sf() %>% left_join(x_j1_co, by = c("id_hex"="fromId"))

hist(x1j_co$sd)

cutoff_current <-  ggplot()+
  geom_raster(data = map_tiles, aes(x, y, fill = hex)) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  # geom_sf(data = , aes(fill =empregos_total), color = NA) +
 geom_sf(data = st_transform(x1j_co %>% na.omit(), 3857),aes(fill = sd), color = NA) + theme_bw() + theme_map() +
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,210000)) +
  labs(fill = 'Standard Deviation') + guides(fill = guide_colorbar(title.position = "top"))
  # theme(legend.position="none",axis.title = element_blank()) 


# graficos_current --------------------------------------------------------

library(cowplot)
library(patchwork)

current <- ti_current + cutoff_current + plot_layout(guides = "collect") & 
  # plot_annotation(tag_levels = 'A') & 
  theme(legend.position = "bottom",legend.direction = 'horizontal',
        text = element_text(size = 12),
        legend.key.size = unit(.8, 'cm'),legend.key.height = unit(.3, 'cm'),
        legend.text = element_text(size=12),
        legend.justification = "center",legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))


cenario_current <- plot_grid(
  ti_current + theme(legend.position="none",axis.title = element_blank()),
  cutoff_current + theme(legend.position="none",axis.title = element_blank()),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,vjust = 11,
  nrow = 1
)



# 02 - difference time interval  ------------------------------------------

acc_diff <- read_rds('transit_access_diff_v2.rds')

max(acc_diff$travel_time) # os tempos so vao até 60m 

vector.janela <- seq(0,120,5)

combinations <- expand.grid(inicio = vector.janela,fim = vector.janela) %>% 
  filter(fim != 0) %>% 
  filter(fim > inicio) %>% 
  filter(inicio > 20) %>% 
  filter(inicio < 100) %>% 
  filter(fim - inicio > 20)

inicio = 30
fim = 60

calculate_acc_ti_hexs_j_diff <- function(inicio,fim){
  
  vamo <- acc_diff %>% filter(travel_time >= inicio) %>% filter(travel_time <= fim) %>% 
    arrange(fromId) %>% 
    # group_by(fromId)
    # filter(scenario == scenario.teste) %>% 
    # filter(opportunities_factor == 'Jobs') %>% 
    # summarise(ACC = mean(only_transit_CMATT,na.rm = T)) %>% 
    mutate(inicio = inicio, fim = fim) %>% 
    group_by(fromId,inicio,fim) %>% 
    summarise(ACC = mean(only_transit_CMATT,na.rm = T))
  
  
}

resumo_acc_ti_hex_diff <- purrr::map2_df(.x = combinations$inicio,.y = combinations$fim,.f = calculate_acc_ti_hexs_j_diff)

resumo_acc_ti_hex_diff_1 <- resumo_acc_ti_hex_diff %>% mutate(id = paste0(inicio,fim)) 

combinacoes_ids <- data.frame(id = unique(resumo_acc_ti_hex_diff_1$id))

num.simu <- 1

simula_sd_diff_hex_ti <- function(num.simu){
  
  ids_sort <- combinacoes_ids %>% sample_n(4)
  
  vamo <- resumo_acc_cutoff_hex_diff_1 %>% filter(id %in% ids_sort$id) %>% 
    # group_by(fromId) %>% summarise(sd = sd(ACC,na.rm = T), ACC = mean(ACC, na.rm = TRUE)) %>% 
    mutate(num.simu)
  
}

# rodar isso aqui 
summary_simulation_diff_scenario_hexs <- map_df(.x = 1:1000,simula_sd_diff_hex_ti)

write_rds(summary_simulation_diff_scenario_hexs,'summary_simulation_diff_scenario_hexs_v0.rds')


x_j_diff <- setDT(summary_simulation_diff_scenario_hexs)[,
                                                                 by = .(num.simu,fromId),
                                                                 .(sd = sd(ACC),ACC = mean(ACC))]

x_j1_diff <- x_j_diff[, by = .(fromId),
                  .(sd = mean(sd),ACC = mean(ACC))]


x1j_diff <- hexagonos %>% select(id_hex,decil) %>% st_as_sf() %>% left_join(x_j1_diff, by = c("id_hex"="fromId"))

# rodar daqui 

# summary_simulation_diff_scenario_hexs <- read_rds('summary_simulation_diff_scenario_hexs_v0.rds')
# 
# x_j <-  summary_simulation_diff_scenario_hexs %>% group_by(fromId) %>% summarise(sd = mean(sd), ACC = mean(ACC)) 
# 
# x1j <- hexagonos %>% select(id_hex,decil) %>% st_as_sf() %>% left_join(x_j, by = c("id_hex"="fromId"))
# 
# # definir limites 
# 

ti_diff <- ggplot()+
  geom_raster(data = map_tiles, aes(x, y, fill = hex)) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  # geom_sf(data = , aes(fill =empregos_total), color = NA) +
  geom_sf(data = st_transform(x1j_diff %>% na.omit(), 3857),aes(fill = sd), color = NA) + theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,40000)) +
  labs(fill = 'Standard Deviation') + guides(fill = guide_colorbar(title.position = "top"))
# theme(legend.position="none",axis.title = element_blank())

ggplot(x1j_diff %>% na.omit()) + geom_sf(aes(fill = sd), color = NA) + theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,210000)) +
  theme(legend.position="none",axis.title = element_blank())


# 03 - diff cutoff --------------------------------------------------------

cutoff_hexs_diff <- acc_diff  %>% 
  filter(travel_time %in% seq(0,120,1)) %>% 
  filter(travel_time > 20 & travel_time <= 100) %>% 
  group_by(fromId) %>% 
  mutate(am = only_transit_CMATT) 

cutoff_hexs_medio_diff <- cutoff_hexs_diff %>% 
  group_by(fromId) %>% summarise(am = mean(only_transit_CMATT))

# %>% 
#   group_by(fromId) %>% summarise(sd = sd(am, na.rm = T))  

combinacoes_ids <- data.frame(id = unique(cutoff_hexs_diff$travel_time))

simula_sd_diff_hex_co <- function(num.simu){
  
  ids_sort <- combinacoes_ids %>% sample_n(4)
  
  vamo <- cutoff_hexs_diff %>% filter(travel_time %in% ids_sort$id) %>%
    group_by(fromId) %>% summarise(sd = sd(am,na.rm = T),am = mean(am, na.rm = TRUE)) %>% mutate(num.simu)
  
}

summary_simulation_diff_scenario_hexs_cutoff <- map_df(.x = 1:1000,simula_sd_diff_hex_co)

write_rds(summary_simulation_diff_scenario_hexs_cutoff,'summary_simulation_diff_scenario_hexs_cutoff_v2.rds')

summary_simulation_diff_scenario_hexs_cutoff <- read_rds('summary_simulation_diff_scenario_hexs_cutoff_v2.rds')

# falta rodar isso aqui 

x_j_diff_co <- setDT(summary_simulation_diff_scenario_hexs_cutoff)[,
                                                         by = .(num.simu,fromId),
                                                         .(sd = sd(am),ACC = mean(am))]

x_j1_diff_co <- x_j_diff_co[, by = .(fromId),
                      .(sd = mean(sd),ACC = mean(ACC))]


x1j_diff_co <- hexagonos %>% select(id_hex,decil) %>% st_as_sf() %>% left_join(x_j1_diff, by = c("id_hex"="fromId"))


# x_j_co <-  summary_simulation_diff_scenario_hexs_cutoff %>% group_by(fromId) %>% summarise(sd = mean(sd), am = mean(am)) 
# 
# x2j <- hexagonos %>% select(id_hex,decil) %>% st_as_sf() %>% left_join(x_j_co, by = c("id_hex"="fromId"))
# 
# library(scales)
# 
# # ajustar limite e basemap
# 

cutoff_diff <- ggplot()+
  geom_raster(data = map_tiles, aes(x, y, fill = hex)) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  # geom_sf(data = , aes(fill =empregos_total), color = NA) +
  geom_sf(data = st_transform(x1j_diff_co %>% na.omit(), 3857),aes(fill = sd), color = NA) + theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,40000)) +
  labs(fill = 'Standard Deviation') + guides(fill = guide_colorbar(title.position = "top"))


diff <- ti_diff + cutoff_diff + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",legend.direction = 'horizontal',
        text = element_text(size = 12),
        legend.key.size = unit(.8, 'cm'),legend.key.height = unit(.3, 'cm'),
        legend.text = element_text(size=12),
        legend.justification = "center",legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))


plot.total <- plot_grid(
  current,
  diff,
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,vjust = 11,
  nrow = 2
)
