
library(sf)
library(data.table)
library(dplyr)
library(JABmisc)
library(ggplot2)
setwd("~")

b <- readRDS("//storage6//usuarios//Proj_acess_oport//data//acesso_oport//hex_agregados/2019/hex_agregado_for_09_2019.rds")

acc <- function(cutoff, tmin,tmax){
  access <- readRDS("transit_access_Current.rds") %>%
    filter(travel_time == cutoff) %>%
    mutate(cenario = "cut")%>%
    select(fromId, only_transit_CMATT, cenario)
  
  access_mean <- readRDS("transit_access_Current.rds") %>%
    filter(travel_time >= tmin & travel_time <=tmax) %>%
    group_by(fromId) %>%
    summarise(only_transit_CMATT = mean(only_transit_CMATT))%>%
    mutate(cenario = "interval")
  
  breaks <- if(max(access$only_transit_CMATT, na.rm = T)>max(access_mean$only_transit_CMATT, na.rm = T)){
    seq(0, max(access$only_transit_CMATT, na.rm = T), ifelse(max(access$only_transit_CMATT, na.rm = T)<250000,100000,200000))} else{seq(0, max(access_mean$only_transit_CMATT, na.rm = T),ifelse(max(access_mean$only_transit_CMATT, na.rm = T)<250000,100000,200000))}
    
  
  max_break <- if(max(access$only_transit_CMATT, na.rm = T)>max(access_mean$only_transit_CMATT, na.rm = T)){
    max(access$only_transit_CMATT, na.rm = T)} else{max(access_mean$only_transit_CMATT, na.rm = T)}
  
  access <- rbind(access,access_mean)
  
  access <- dplyr::left_join(b,access,by=c("id_hex"="fromId")) %>%
    filter(!is.na(cenario))
  
  city_shape <- geobr::read_municipality(code_muni = 2304400)
  state_shape <- geobr::read_municipality(code_muni = "CE")
  
  city_bbox <- sf::st_bbox(city_shape)
  xlim <- c(city_bbox$xmin, city_bbox$xmax)
  ylim <- c(city_bbox$ymin, city_bbox$ymax)
  
  gtfs <-  gtfstools::read_gtfs(
    "gtfs_for_metrofor_2021-01_depois.zip"
  )
  desired_trips <- gtfs$trips[
    gtfs$routes,
    on = "route_id",
    `:=`(route_id = i.route_id, route_long_name = i.route_long_name)
  ]
  desired_trips <- desired_trips[
    desired_trips[, .I[1], by = route_long_name]$V1
  ]
  desired_trips <- desired_trips$trip_id
  
  transit_shapes <- gtfstools::get_trip_geometry(
    gtfs,
    trip_id = desired_trips
  )
  transit_shapes <- setDT(transit_shapes)[
    !(trip_id == "LL-0.1-1" & origin_file == "stop_times")
  ]
  
  n_trips <- length(unique(transit_shapes$trip_id))
  
  transit_shapes <- rbind(
    transit_shapes,
    transit_shapes,
    transit_shapes[origin_file != "shapes"]
  )
  transit_shapes[
    ,
    scenario := factor(
      c(
        rep("Depois (Alternativo)", n_trips),
        rep("Depois (Previsto)", n_trips),
        rep("Antes", n_trips - 1)
      ),
      levels = c("Antes", "Depois (Previsto)", "Depois (Alternativo)")
    )
  ]
  
  label <- scales::label_number(
    accuracy = 1,
    scale = 1/1000,
    suffix = "k",
    big.mark = ","
  )
  
  cut1 <- ifelse(cutoff==30,30,ifelse(cutoff==50,50,70))
  label1 <- if(cutoff==30){c("A","B")}
  else if(cutoff==50){c("C","D")}
  else{c("E","F")}
  
  min1 <- ifelse(cutoff==30,20,ifelse(cutoff==50,40,60))
  max1 <- ifelse(cutoff==30,40,ifelse(cutoff==50,60,80))
  
  aa <- paste0("(", label1[1],") Cutoff ",cut1, " Minutes")
  bb <- paste0("(", label1[2], ") Interval ", min1, "-", max1, " Minutes")
  
  
  
  plot <- ggplot() +
    geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
    geom_sf(
      data = st_sf(access),
      aes(fill = get("only_transit_CMATT")),
      color = NA
    ) +
    geom_sf(
      data = st_sf(transit_shapes),
      size = 0.5,
      alpha = 0.7
    ) +
    geom_sf(data = city_shape, fill = NA) +
    #facet_wrap(~ cenario, nrow = 1, ncol = 2,labeller = labeller(cenario = c("cut" = "(E) Cutoff 70 Minutes","interval" = "(F) Interval 60-80 Minutes"))) +
    facet_wrap(~ cenario, nrow = 1, ncol = 2,labeller = labeller(cenario = c("cut" = aa, "interval" = bb))) +
    coord_sf(xlim, ylim) +
    scale_fill_viridis_c(
      name = "Accessibility",
      option = "cividis",
      label = label,
      breaks = breaks,
      limit = c(0, max_break)
    ) +
    labs(title = "")+
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(size = 11),
      strip.text.x = element_text(hjust = 0),
      panel.background = element_rect(fill = "#aadaff", color = NA)
    )
}    

####### Impacto Acessibilidade Futuro - atual ######

acc_diff <- function(cutoff, tmin,tmax){
  a <- readRDS("transit_access_diff_v2.rds") %>%
    filter(travel_time == cutoff,
           scenario == "contrafactual",
           type == "abs")%>%
    mutate(cenario ="cut")%>%
    select(fromId, only_transit_CMATT, cenario)
  
  
  access_dif_mean <- readRDS("transit_access_diff_v2.rds") %>%
    filter(scenario == "contrafactual",
           type == "abs",
           travel_time >= tmin & travel_time <=tmax) %>%
    group_by(fromId) %>%
    summarise(only_transit_CMATT = mean(only_transit_CMATT))%>%
    mutate(cenario ="interval")
  
  
  
  breaks <- if(max(a$only_transit_CMATT, na.rm = T)>max(access_dif_mean$only_transit_CMATT, na.rm = T)){
    seq(0, max(a$only_transit_CMATT, na.rm = T),30000)} else{seq(0, max(access_dif_mean$only_transit_CMATT, na.rm = T),30000)}
  
  max_break <- if(max(a$only_transit_CMATT, na.rm = T)>max(access_dif_mean$only_transit_CMATT, na.rm = T)){
    max(a$only_transit_CMATT, na.rm = T)} else{max(access_dif_mean$only_transit_CMATT, na.rm = T)}
  
  
  a <- rbind(a, access_dif_mean)
  
  access_dif <- dplyr::left_join(b,a,by=c("id_hex"="fromId")) %>%
    filter(!is.na(cenario))
  
  city_shape <- geobr::read_municipality(code_muni = 2304400)
  state_shape <- geobr::read_municipality(code_muni = "CE")
  
  city_bbox <- sf::st_bbox(city_shape)
  xlim <- c(city_bbox$xmin, city_bbox$xmax)
  ylim <- c(city_bbox$ymin, city_bbox$ymax)
  
  gtfs <-  gtfstools::read_gtfs(
    "gtfs_for_metrofor_2021-01_depois.zip"
  )
  desired_trips <- gtfs$trips[
    gtfs$routes,
    on = "route_id",
    `:=`(route_id = i.route_id, route_long_name = i.route_long_name)
  ]
  desired_trips <- desired_trips[
    desired_trips[, .I[1], by = route_long_name]$V1
  ]
  desired_trips <- desired_trips$trip_id
  
  transit_shapes <- gtfstools::get_trip_geometry(
    gtfs,
    trip_id = desired_trips
  )
  transit_shapes <- setDT(transit_shapes)[
    !(trip_id == "LL-0.1-1" & origin_file == "stop_times")
  ]
  
  n_trips <- length(unique(transit_shapes$trip_id))
  
  transit_shapes <- rbind(
    transit_shapes,
    transit_shapes,
    transit_shapes[origin_file != "shapes"]
  )
  transit_shapes[
    ,
    scenario := factor(
      c(
        rep("Depois (Alternativo)", n_trips),
        rep("Depois (Previsto)", n_trips),
        rep("Antes", n_trips - 1)
      ),
      levels = c("Antes", "Depois (Previsto)", "Depois (Alternativo)")
    )
  ]
  
  label <- scales::label_number(
    accuracy = 1,
    scale = 1/1000,
    suffix = "k",
    big.mark = ","
  )
  
  cut1 <- ifelse(cutoff==30,30,ifelse(cutoff==50,50,70))
  label1 <- if(cutoff==30){c("A","B")}
            else if(cutoff==50){c("C","D")}
            else{c("E","F")}
  
  min1 <- ifelse(cutoff==30,20,ifelse(cutoff==50,40,60))
  max1 <- ifelse(cutoff==30,40,ifelse(cutoff==50,60,80))
  
  aa <- paste0("(", label1[1],") Cutoff ",cut1, " Minutes")
  bb <- paste0("(", label1[2], ") Interval ", min1, "-", max1, " Minutes")
  
  #print(aa)
  #print(bb)
  
  plot_dif <- ggplot() +
    geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
    geom_sf(
      data = st_sf(access_dif),
      aes(fill = get("only_transit_CMATT")),
      color = NA
    ) +
    geom_sf(
      data = st_sf(transit_shapes),
      size = 0.5,
      alpha = 0.7
    ) +
    geom_sf(data = city_shape, fill = NA) +
    facet_wrap(~ cenario, nrow = 1, ncol = 2,labeller = labeller(cenario = c("cut" = aa, "interval" = bb))) +
    
    #labeller(cenario = c("cut" = "(E) Cutoff 70 Minutes","interval" = "(F) Interval 60-80 Minutes"))) +
    
    coord_sf(xlim, ylim) +
    scale_fill_viridis_c(
      name = "Accessibility\ngain",
      option = "inferno",
      label = label,
      breaks = breaks,
      limit = c(0, max_break)
    ) +
    labs(title = "")+
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      strip.text = element_text(size = 11),
      strip.text.x = element_text(hjust = 0),
      panel.background = element_rect(fill = "#aadaff", color = NA)
    )

}


################################ PLOT RESULTADOS #######

plot_cut_30 <- acc(30,20,40)
plot_cut_50 <- acc(50,40,60)
plot_cut_70 <- acc(70,60,80)


library(cowplot)
ggdraw(xlim = c(0,10), ylim = c(0,30))+
  draw_plot(plot_cut_30+theme(legend.position = 'none'), x=0, y=20, width = 10, height = 10)+
  draw_plot(plot_cut_50+theme(legend.position = 'none'), x=0, y=12, width = 10, height = 10)+
  draw_plot(plot_cut_70+theme(legend.position = 'bottom'), x=0, y=1.6, width = 10, height = 13)
  


ggsave2(filename="Figura_8_test2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 20, height = 33, units = "cm")




plot_mean_30 <- acc_diff(30,20,40)
plot_mean_50 <- acc_diff(50,40,60)
plot_mean_70 <- acc_diff(70,60,80)


library(cowplot)
ggdraw(xlim = c(0,10), ylim = c(0,30))+
  draw_plot(plot_mean_30+theme(legend.position = 'none'), x=0, y=20, width = 10, height = 10)+
  draw_plot(plot_mean_50+theme(legend.position = 'none'), x=0, y=12, width = 10, height = 10)+
  draw_plot(plot_mean_70+theme(legend.position = 'bottom'), x=0, y=1.6, width = 10, height = 13)
 
 
 
ggsave2(filename="Figura_9_test2.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 20, height = 33, units = "cm")
 
