##################################################################################################################
############################# SCRIPT PARA CRIAR MAPAS DE FORTALEZA E GOIÂNIA #####################################
##################################################################################################################

library(data.table)
library(dplyr)
library(ggplot2)
library(sf)
library(ggalt)
library(hrbrthemes)
library(ggnewscale)
library(cowplot)
library(purrr)
library(ggsn)
library(BAMMtools) 
library(stringi)
library(ggspatial)


setwd("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/hex_agregados/2019")

df <- readRDS("hex_agregado_for_09_2019.rds") %>%
  st_as_sf()%>%
  st_set_crs(4326) %>%
  st_transform(3857)

############## Natural breaks Empregos

classes <- getJenksBreaks(subset(df$empregos_total, df$empregos_total > 0), 11)

df <- df %>%
  mutate(empregos_total_jenks = cut(empregos_total, classes, include.lowest = T, dig.lab = 10)) 

ncolour.jenks <- sort(unique(df$empregos_total_jenks))
colourCount = length(unique(df$empregos_total_jenks))

col <- rev(hcl.colors(10,"inferno", rev = TRUE))
pal.jenks <- colorRampPalette(col)(colourCount)
label <- ncolour.jenks
label <- stri_replace_all_regex(label, "[,]", " - ")
label <- stri_replace_all_regex(label, "\\[|\\(|\\]", "")

###############################

map_tiles_for <- readRDS(paste0("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/maptiles_crop/2019/mapbox/maptile_crop_mapbox_for_2019.rds"))

ggplot()+
  geom_raster(data = map_tiles_for, aes(x, y, fill = hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() + 
  geom_sf(data=subset(df,df$empregos_total >0), aes(fill=empregos_total_jenks), color=NA, alpha=1) +
  scale_fill_manual(name = "Total Jobs", values=pal.jenks, labels=label)+
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                        pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                       style = north_arrow_fancy_orienteering) +
  coord_sf(datum=NA) +
  theme(legend.title = element_text(size = 16, face="bold"), plot.title = element_text(hjust=0.5), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), rect = element_blank(), axis.title.y=element_blank(),axis.title.x=element_blank())


ggsave2(filename="Fig_4.png", plot=ggplot2::last_plot(),
        dpi = 300, width = 20, height = 10, units = "cm")



