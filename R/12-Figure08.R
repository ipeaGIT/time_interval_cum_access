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
library(tictoc)
library(future)
library(furrr)
library(patchwork)
library(ggnewscale)

setwd("//storage6/usuarios/Proj_acess_oport/git_jlucas/paper_thresholdsensitivity/R/data")

# 00 - abreviacoes --------------------------------------------------------

set.seed(100)

options(scipen = 9999)

hexagonos <- st_read(dsn = 'hex_agregado_for_09_2019.gpkg')

city_shape <- geobr::read_municipality(code_muni = 2304400)
state_shape <- geobr::read_municipality(code_muni = "CE")

city_bbox <- sf::st_bbox(city_shape)
xlim <- c(city_bbox$xmin, city_bbox$xmax)
ylim <- c(city_bbox$ymin, city_bbox$ymax)

#  time interval -  -------------------------------------------------------------------

acc_current <- read_rds('transit_access_Current.rds') %>%  mutate(situ = "current")
acc_diff <- read_rds('transit_access_diff_v2.rds') %>% mutate(situ = "diff") %>% select(-scenario,-type)

acc_data <- rbind(acc_current,acc_diff) %>% na.omit()

vector.janela <- seq(0,90,2) # a resolucao minima da janela eh 5 min 

# possiveis combinacoes de janela 

combinations <- expand.grid(inicio = vector.janela,fim = vector.janela) %>% 
  filter(fim != 0) %>% 
  filter(fim > inicio) %>% 
  filter(inicio > 20) %>% 
  filter(fim <= 90) %>% 
  filter(fim - inicio >= 10)

combinations


# para teste da funcao 
inicio = 30
fim = 60
situ.teste = "current"
situacao = "current"


# calcula a acessibildiade para um hex a partir da escolha de uma janela 

calculate_acc_timeinterval_hexs <- function(inicio,fim,situacao){
  
  vamo <- setDT(acc_data)[situ == situacao,] # escolhe a situacao
  vamo1 <- vamo[travel_time > inicio & travel_time < fim] # filtra a janela 
  vamo2 <- vamo1[,.(acc = mean(only_transit_CMATT,na.rm = T)), by = .(fromId)] # resume a acessibilidade por hex
  vamo2[,c("janela","tamanho_janela") := .(paste(inicio,fim,sep = "|"),fim-inicio)] # adiciona informacao da janela utilizada
  
  return(vamo2)
}


# valores de acessibilidade para todos os hexagonos a depender da janela 

future::plan(strategy = "multicore")
options(future.globals.onReference = NULL)


tic()
resumo_acc_timeinterval_hex_current <- furrr::future_map2_dfr(.x = combinations$inicio,
                                                              .y = combinations$fim,
                                                              .f = calculate_acc_timeinterval_hexs,
                                                              situ = "current",.progress = T)
toc()

tic()
resumo_acc_timeinterval_hex_diff <- furrr::future_map2_dfr(.x = combinations$inicio,
                                                           .y = combinations$fim,
                                                           .f = calculate_acc_timeinterval_hexs,
                                                           situ = "diff")
toc()


combinacoes_janelas <- setDT(resumo_acc_timeinterval_hex_current %>% distinct(janela,tamanho_janela))

num.simu <- 1
tamanho_janela.t = "NULL"
tamanho_amostra = 4
tamanho_janela.t = 20 
tamanho_janela = 15
t_janela = 10

# monte carlo simulation 

simula_sd_current_hex_ti <- function(num.simu,t_janela,tamanho_amostra){
  
  if(is.null(t_janela) == TRUE) {
    
    # sorteia as janelas 
    
    janelas_sorteadas <- combinacoes_janelas[sample(.N,tamanho_amostra)]
    
    # resumo 
    
    vamo <- setDT(resumo_acc_timeinterval_hex_current)[janela %in% janelas_sorteadas$janela,]
    vamo1 <- vamo[,num.simu := num.simu]
    vamo1[,janelas_utilizadas := paste(unique(vamo1$janela),collapse = ",")]
    vamo1[,estrategia := "random intervals"]
    # vamo1[,sd := "random intervals"]
    
    
    return(vamo1)
  }
  
  else {
    
    janelas_sorteadas2 <-combinacoes_janelas[tamanho_janela == t_janela]
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
    
    janelas_sorteadas <- combinacoes_janelas[sample(.N,tamanho_amostra)]
    
    # resumo 
    
    vamo <- setDT(resumo_acc_timeinterval_hex_diff)[janela %in% janelas_sorteadas$janela,]
    vamo1 <- vamo[,num.simu := num.simu]
    vamo1[,janelas_utilizadas := paste(unique(vamo1$janela),collapse = ",")]
    vamo1[,estrategia := "random intervals"]
    
    return(vamo1)
  }
  
  else {
    
    janelas_sorteadas2 <- combinacoes_janelas[tamanho_janela == t_janela]
    
    janelas_sorteadas3 <- janelas_sorteadas2[sample(.N,tamanho_amostra)]
    
    vamo <- setDT(resumo_acc_timeinterval_hex_diff)[janela %in% janelas_sorteadas3$janela,]
    vamo1 <- vamo[,num.simu := num.simu]
    vamo1[,janelas_utilizadas := paste(unique(vamo1$janela),collapse = ",")]
    vamo1[,estrategia := paste0("interval width -",t_janela," min")]
    
    return(vamo1)
    
  }
  
  
}

teste <- simula_sd_current_hex_ti(num.simu = 1,t_janela = NULL,tamanho_amostra = 4) # ok

# resumo simulacao current scenario 

tictoc::tic()
summary_simulation_current_scenario_ti <- future_map_dfr(.x = 1:10000,simula_sd_current_hex_ti, t_janela = NULL,
                                                         tamanho_amostra = 4,.progress = TRUE,
                                                         .options = furrr_options(seed = 100))
tictoc::toc()

tictoc::tic()
summary_simulation_diff_scenario_ti <- future_map_dfr(.x = 1:10000,simula_sd_diff_hex_ti, t_janela = NULL,
                                                      tamanho_amostra = 4,.progress = TRUE,
                                                      .options = furrr_options(seed = 100))
tictoc::toc()

head(summary_simulation_diff_scenario_ti)

# resumo para tamanho fixo de janela current 


cutoff_hexs <- acc_data  %>% 
  filter(travel_time %in% seq(0,90,1)) %>% 
  filter(travel_time > 20 & travel_time <= 90) %>% 
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
  vamo2[, situ:= NULL]
  
  length(unique(vamo2$fromId))
  
  return(vamo2)
  
}




simula_sd_co <- function(num.simu, sit, tamanho_amostra){
  
  co_sort <- combinacoes_co %>% sample_n(tamanho_amostra)
  
  vamo <- setDT(cutoff_hexs)[situ == sit]
  
  vamo1 <- vamo[travel_time %in% co_sort$id]
  
  unique(vamo1$travel_time)
  
  vamo2 <- vamo1[,num.simu := num.simu]
  
  vamo2[,cutoffs_utilizados := paste(unique(vamo2$travel_time),collapse = ",")]
  vamo2[, situ:= NULL]
  
  length(unique(vamo2$fromId))
  
  return(vamo2)
  
}

teste <- simula_sd_co(num.simu = 1,sit = "current",tamanho_amostra = 4)

tictoc::tic()
summary_simulation_current_scenario_co <- future_map_dfr(.x = 1:10000,
                                                         simula_sd_co,
                                                         sit = "current",
                                                         tamanho_amostra = 4,
                                                         .progress = TRUE,
                                                         .options = furrr_options(seed = 100))
tictoc::toc()

head(summary_simulation_current_scenario_co)


# resumo cutoff diff

tictoc::tic()
summary_simulation_diff_scenario_co <- future_map_dfr(.x = 1:10000,
                                                      simula_sd_co,
                                                      sit = "diff",
                                                      tamanho_amostra = 4,
                                                      .progress = TRUE,
                                                      .options = furrr_options(seed = 100))
tictoc::toc()



head(summary_simulation_diff_scenario_co)

# Figura 11  --------------------------------------------------------------

resumo_hexagono_ti_current <- summary_simulation_current_scenario_ti[,.(sd = sd(acc), acc = mean(acc)), by = .(num.simu,estrategia,fromId)]

resumo_hexagono_ti_current <- resumo_hexagono_ti_current[,.(sd_medio = mean(sd),acc = mean(acc)), by = .(fromId,estrategia)] %>% 
  left_join(hexagonos %>% select(decil,id_hex), by = c("fromId"="id_hex")) %>% st_as_sf()


resumo_hexagono_ti_diff <- summary_simulation_diff_scenario_ti[,.(sd = sd(acc),acc = mean(acc)), by = .(num.simu,estrategia,fromId)]

resumo_hexagono_ti_diff <- resumo_hexagono_ti_diff[,.(sd_medio = mean(sd),acc = mean(acc)), by = .(fromId,estrategia)] %>% 
  left_join(hexagonos %>% select(decil,id_hex), by = c("fromId"="id_hex")) %>% st_as_sf()


resumo_hexagono_co_current <- summary_simulation_current_scenario_co[,.(sd = sd(acc),acc = mean(acc)), by = .(num.simu,fromId)]

resumo_hexagono_co_current <- resumo_hexagono_co_current[,.(sd_medio = mean(sd),acc = mean(acc)), by = .(fromId)] %>% 
  left_join(hexagonos %>% select(decil,id_hex), by = c("fromId"="id_hex")) %>% st_as_sf()


resumo_hexagono_co_diff <- summary_simulation_diff_scenario_co[,.(sd = sd(acc),acc = mean(acc)), by = .(num.simu,fromId)]

resumo_hexagono_co_diff <- resumo_hexagono_co_diff[,.(sd_medio = mean(sd),acc = mean(acc)), by = .(fromId)] %>% 
  left_join(hexagonos %>% select(decil,id_hex), by = c("fromId"="id_hex")) %>% st_as_sf()


ti_current <- ggplot()+
  geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
  geom_sf(data = city_shape, fill = NA) +
  geom_sf(data = st_transform(resumo_hexagono_ti_current, 3857),aes(fill = sd_medio), color = NA) + 
  coord_sf(xlim, ylim) +
  theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,210000)) +
  labs(fill = 'Average Standard Deviation') + guides(fill = guide_colorbar(title.position = "top")) +
  theme(panel.background = element_rect(fill = "#aadaff", color = NA))+ 
  ggtitle('Time interval')+
  theme(plot.title=element_text(hjust=0.5, face = "plain"))

ti_current <- ti_current + 
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey30", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(3, "in"), pad_y = unit(2.3, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_width = 1,
      line_col= "black",
      fill = c("white","black")
    ))

co_current <- ggplot()+
  geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
  geom_sf(data = city_shape, fill = NA) +
  geom_sf(data = st_transform(resumo_hexagono_co_current, 3857),aes(fill = sd_medio), color = NA) + 
  coord_sf(xlim, ylim) +
  theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,210000)) +
  labs(fill = 'Average Standard Deviation') + guides(fill = guide_colorbar(title.position = "top")) +
  theme(panel.background = element_rect(fill = "#aadaff", color = NA))+
  ggtitle('Cutoff')+
  theme(plot.title=element_text(hjust=0.5, face = "plain"))


co_current <- co_current+ 
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey30", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(3, "in"), pad_y = unit(2.3, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_width = 1,
      line_col= "black",
      fill = c("white","black")
    ))

ti_diff <- ggplot()+
  geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
  geom_sf(data = city_shape, fill = NA) +
  geom_sf(data = st_transform(resumo_hexagono_ti_diff, 3857),aes(fill = sd_medio), color = NA) +
  coord_sf(xlim, ylim) +
  theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,40000)) +
  labs(fill = 'Average Standard Deviation') + guides(fill = guide_colorbar(title.position = "top"))+
  theme(panel.background = element_rect(fill = "#aadaff", color = NA))

ti_diff <- ti_diff + 
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey30", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(3, "in"), pad_y = unit(2.3, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_width = 1,
      line_col= "black",
      fill = c("white","black")
    ))

co_diff <- ggplot()+
  geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
  geom_sf(data = city_shape, fill = NA) +
  geom_sf(data = st_transform(resumo_hexagono_co_diff, 3857),aes(fill = sd_medio), color = NA) + 
  coord_sf(xlim, ylim) +
  theme_bw() + theme_map()+
  scale_fill_viridis(option = 'inferno',direction = 1,label = label_number(suffix = "k", scale = 1e-3),limits = c (0,40000)) +
  labs(fill = 'Average Standard Deviation') + guides(fill = guide_colorbar(title.position = "top"))+
  theme(panel.background = element_rect(fill = "#aadaff", color = NA))

co_diff <- co_diff+ 
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey30", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(3, "in"), pad_y = unit(2.3, "in"),
    style = ggspatial::north_arrow_fancy_orienteering(
      line_width = 1,
      line_col= "black",
      fill = c("white","black")
    ))

# cenario current 

current <- co_current + ti_current + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",legend.direction = 'horizontal',
        text = element_text(size = 10),
        legend.key.size = unit(.8, 'cm'),legend.key.height = unit(.2, 'cm'),
        legend.text = element_text(size=10),
        legend.justification = "center",legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

diff <-   co_diff + ti_diff + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",legend.direction = 'horizontal',
        text = element_text(size = 10),
        legend.key.size = unit(.8, 'cm'),legend.key.height = unit(.2, 'cm'),
        legend.text = element_text(size=10),
        legend.justification = "center",legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))

plot.total <- plot_grid(
  current,
  diff,
  align = 'vh',
  labels = c("A", "B"),label_x = .18,label_y = 0.97,
  nrow = 2
)

plot.total

setwd("~")
ggsave("Fig_8_temp.png", width = 30, height = 22, units = "cm", dpi = 300)

