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

############################ ################################### #######################




summary_simulation_current_scenario_ti_janela_fixa_10 <- future_map_dfr(.x = 1:10000,
                                                                        t_janela = 10,
                                                                        simula_sd_current_hex_ti,
                                                                        tamanho_amostra = 4,
                                                                        .progress = TRUE,
                                                                        .options = furrr_options(seed = 100))

summary_simulation_current_scenario_ti_janela_fixa_20 <- future_map_dfr(.x = 1:10000,
                                                                        t_janela = 20,
                                                                        simula_sd_current_hex_ti,
                                                                        tamanho_amostra = 4,
                                                                        .progress = TRUE,
                                                                        .options = furrr_options(seed = 100))

summary_simulation_current_scenario_ti_janela_fixa_30 <- future_map_dfr(.x = 1:10000,
                                                                        t_janela = 30,
                                                                        simula_sd_current_hex_ti,
                                                                        tamanho_amostra = 4,
                                                                        .progress = TRUE,
                                                                        .options = furrr_options(seed = 100))

ss_current_janelas_fixas <- rbind(summary_simulation_current_scenario_ti_janela_fixa_10,
                                  summary_simulation_current_scenario_ti_janela_fixa_20,
                                  summary_simulation_current_scenario_ti_janela_fixa_30)

# resumo para tamanho fixo de janela diff

summary_simulation_diff_scenario_ti_janela_fixa_10 <- future_map_dfr(.x = 1:10000,
                                                                     t_janela = 10,
                                                                     simula_sd_diff_hex_ti,
                                                                     tamanho_amostra = 4,
                                                                     .progress = TRUE,
                                                                     .options = furrr_options(seed = 100))

summary_simulation_diff_scenario_ti_janela_fixa_20 <- future_map_dfr(.x = 1:10000,
                                                                     t_janela = 20,
                                                                     simula_sd_diff_hex_ti,
                                                                     tamanho_amostra = 4,
                                                                     .progress = TRUE,
                                                                     .options = furrr_options(seed = 100))

summary_simulation_diff_scenario_ti_janela_fixa_30 <- future_map_dfr(.x = 1:10000,
                                                                     t_janela = 30,
                                                                     simula_sd_diff_hex_ti,
                                                                     tamanho_amostra = 4,
                                                                     .progress = TRUE,
                                                                     .options = furrr_options(seed = 100))

ss_diff_janelas_fixas <- rbind(summary_simulation_diff_scenario_ti_janela_fixa_10,
                               summary_simulation_diff_scenario_ti_janela_fixa_20,
                               summary_simulation_diff_scenario_ti_janela_fixa_30)




resumo_cidade_co_current <- summary_simulation_current_scenario_co[,.(acc = mean(acc)), by = .(travel_time, num.simu)]

resumo_cidade_co_current <- resumo_cidade_co_current[, .(sd_medio = sd(acc)), by = num.simu]

resumo_cidade_co_current[,estrategia := 'Cutoff']

head(resumo_cidade_co_current)

#hist(resumo_cidade_co_diff$sd)

# current TI 

# resumo_cidade_ti_current <- summary_simulation_current_scenario_ti[,.(acc = mean(acc,na.rm = TRUE),sd = sd(acc)),
#                                                                    by = .(num.simu,estrategia)]

head(summary_simulation_current_scenario_ti)

resumo_cidade_ti_current <- summary_simulation_current_scenario_ti[,.(acc = mean(acc)), by = .(janela, num.simu,estrategia)]

resumo_cidade_ti_current <- resumo_cidade_ti_current[, .(sd_medio = sd(acc)), by = .(num.simu,estrategia)]

head(resumo_cidade_ti_current)


# current diferentes janelas 

resumo_cidade_dj_current <- ss_current_janelas_fixas[,.(acc = mean(acc)), by = .(janela, num.simu,estrategia)]

resumo_cidade_dj_current <- resumo_cidade_dj_current[, .(sd_medio = sd(acc)), by = .(num.simu,estrategia)]

# diff diferentes janelas 

resumo_cidade_dj_diff <- ss_diff_janelas_fixas[,.(acc = mean(acc)), by = .(janela, num.simu,estrategia)]

resumo_cidade_dj_diff <- resumo_cidade_dj_diff[, .(sd_medio = sd(acc)), by = .(num.simu,estrategia)]


# diff co 

resumo_cidade_co_diff <- summary_simulation_diff_scenario_co[,.(acc = mean(acc)), by = .(travel_time, num.simu)]

resumo_cidade_co_diff <- resumo_cidade_co_diff[, .(sd_medio = sd(acc)), by = num.simu]

resumo_cidade_co_diff[,estrategia := 'Cutoff']

head(resumo_cidade_co_diff)

# diff TI 

resumo_cidade_ti_diff <- summary_simulation_diff_scenario_ti[,.(acc = mean(acc)), by = .(janela, num.simu,estrategia)]

resumo_cidade_ti_diff <- resumo_cidade_ti_diff[, .(sd_medio = sd(acc)), by = .(num.simu,estrategia)]


hist(resumo_cidade_ti_diff$sd)



# mapas 

resumo_hexagono_ti_current <- summary_simulation_current_scenario_ti[,.(acc = mean(acc,na.rm = TRUE),sd = sd(acc)),
                                                                     by = .(fromId,estrategia)]

# histograma 

resumo_cidade_ti_current <- summary_simulation_current_scenario_ti[,.(acc = mean(acc,na.rm = TRUE),sd = sd(acc)),
                                                                   by = .(num.simu,estrategia)]


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

# resumo_hexagono_co_current[,estrategia := "Cutoff"]

resumo_hexagono_co_diff <- summary_simulation_diff_scenario_co[,.(sd = sd(acc),acc = mean(acc)), by = .(num.simu,fromId)]

resumo_hexagono_co_diff <- resumo_hexagono_co_diff[,.(sd_medio = mean(sd),acc = mean(acc)), by = .(fromId)] %>% 
  left_join(hexagonos %>% select(decil,id_hex), by = c("fromId"="id_hex")) %>% st_as_sf()
# resumo_hexagono_co_diff[,estrategia := "Cutoff"]


dispersao_co_current <- ggplot(resumo_hexagono_co_current %>% na.omit()) + 
  geom_point(aes(y = sd_medio, x = acc, color = factor(as.numeric(decil))), alpha = .4, size = 2) + 
  theme_bw() +
  scale_y_continuous(breaks = seq(0,200000,50000),minor_breaks = NULL,limits = c(0,220000)) + 
  scale_x_continuous(minor_breaks = NULL,limits = c(0,500000)) + 
  xlab('Average Accessibility') +
  ylab('Standard deviation (sd)') +
  # scale_colour_viridis_d(labels=c('D1 poor', paste0('D', c(2:9)), 'D10 rich'),
  #                        name='Income decile')+
  scale_colour_brewer(palette = "RdBu", labels=c('D1 poor', paste0('D', c(2:9)), 'D10 rich'), name='Income decile') + 
  theme( 
    axis.text.y  = element_text(face="bold"),
    axis.text.x  = element_text(face="bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15, face ="bold"),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
    # , axis.text.x = element_blank()
    
  )

dispersao_ti_current <- ggplot(resumo_hexagono_ti_current %>% na.omit()) + 
  geom_point(aes(y = sd_medio, x = acc, color = factor(as.numeric(decil))), alpha = .4, size = 2) + 
  theme_bw() +
  scale_y_continuous(breaks = seq(0,200000,50000),minor_breaks = NULL,limits = c(0,220000)) + 
  scale_x_continuous(minor_breaks = NULL,limits = c(0,500000)) + 
  xlab('Average Accessibility') +
  ylab('Standard deviation (sd)') +
  # scale_colour_viridis_d(labels=c('D1 poor', paste0('D', c(2:9)), 'D10 rich'),
  #                        name='Income decile')+
  scale_colour_brewer(palette = "RdBu",
                      labels=c('D1 poor', paste0('D', c(2:9)), 'D10 rich'),
                      name='Income decile') +
  theme( 
    axis.text.y  = element_text(face="bold"),
    axis.text.x  = element_text(face="bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 15, face ="bold"),
    legend.text = element_text(size = 15),
    legend.position = "bottom"
    # , axis.text.x = element_blank()
    
  )

dispersao_ti_current 

prow2 <- plot_grid(
  dispersao_co_current + theme(legend.position="none",axis.title.x = element_blank(),axis.title.y = element_text(size = 15)),
  dispersao_ti_current + theme(legend.position="none",axis.title = element_blank()),
  align = 'vh',
  labels = c("A", "B"),
  hjust = -1,vjust = 3,
  nrow = 1
)

prow2

legenda <- get_legend(
  dispersao_co_current + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = c(.5,.4),
          text = element_text(size = 15),
          legend.key.size = unit(.08, 'cm'),
          legend.text = element_text(size=15),legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
)


G <- plot_grid(prow2, legenda,nrow = 2,rel_heights = c(1,.2),greedy = TRUE)

G

G1 <- ggdraw(add_sub(G , "Average Acessibility", vpadding=grid::unit(1,"lines"), # With `0.8` I add some space in the X margin.
                     y=3.1, x=.5, vjust=1,size = 15, # With `y` and `x` I can indicate the coordinates for my desired text
                     angle=0)
             # With this I can change the orientation of the text
)

G1

ggsave("Fig_9.png", width = 30, height = 15, units = "cm", dpi = 300)



