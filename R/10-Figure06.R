
library(sf)
library(data.table)
library(dplyr)
library(ggplot2)
setwd("~")

b <- readRDS("//storage6//usuarios//Proj_acess_oport//data//acesso_oport//hex_agregados/2019/hex_agregado_for_09_2019.rds")

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
  
  
  access_cutoff <- dplyr::left_join(b,a,by=c("id_hex"="fromId")) %>%
    filter(!is.na(cenario))
  
  access_interval <- dplyr::left_join(b,access_dif_mean,by=c("id_hex"="fromId")) %>%
    filter(!is.na(cenario))
  
  access <- rbind(access_cutoff,access_interval)
  
  city_shape <- geobr::read_municipality(code_muni = 2304400)
  state_shape <- geobr::read_municipality(code_muni = "CE")
  
  city_bbox <- sf::st_bbox(city_shape)
  xlim <- c(city_bbox$xmin, city_bbox$xmax)
  ylim <- c(city_bbox$ymin, city_bbox$ymax)
  
  gtfs <- read_sf("gtfs.shp")
  
  label <- scales::label_number(
    accuracy = 1,
    scale = 1/1000,
    suffix = "k",
    big.mark = ","
  )
  
  cut1 <- ifelse(cutoff==30,30,ifelse(cutoff==50,50,70))
  label1 <- if(cutoff==30){c("A","B")} else if(cutoff==50){c("C","D")} else{c("E","F")}
  
  min1 <- ifelse(cutoff==30,20,ifelse(cutoff==50,40,60))
  max1 <- ifelse(cutoff==30,40,ifelse(cutoff==50,60,80))
  
  aa <- paste0("(", label1[1],") Cutoff ",cut1, " Minutes")
  bb <- paste0("(", label1[2], ") Interval ", min1, "-", max1, " Minutes")
  
  
  plot_dif <- ggplot() +
    geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
    geom_sf(
      data = st_sf(access),
      aes(fill = get("only_transit_CMATT")),
      color = NA
    ) +
    geom_sf(data = gtfs,
            size = 0.5,
            alpha = 0.7
    ) +
    geom_sf(data = city_shape, fill = NA) +
    facet_wrap(~ cenario, nrow = 1, ncol = 2,labeller = labeller(cenario = c("cut" = aa, "interval" = bb))) +
    
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
    )+ 
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
      )
    )
  
}


################################ PLOT RESULTADOS #######

plot_mean_30 <- acc_diff(30,20,40)
plot_mean_50 <- acc_diff(50,40,60)
plot_mean_70 <- acc_diff(70,60,80)


library(cowplot)
ggdraw(xlim = c(0,10), ylim = c(0,30))+
  draw_plot(plot_mean_30+theme(legend.position = 'none'), x=0, y=20, width = 10, height = 10)+
  draw_plot(plot_mean_50+theme(legend.position = 'none'), x=0, y=12, width = 10, height = 10)+
  draw_plot(plot_mean_70+theme(legend.position = 'bottom'), x=0, y=1.6, width = 10, height = 13)



ggsave2(filename="Fig_6.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 20, height = 33, units = "cm")

