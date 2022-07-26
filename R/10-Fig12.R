
library(data.table)
library(dplyr)
library(ggplot2)
library(sf)

setwd("~")

#### Current scenario ######
current <- readRDS("data/output_access/transit_access_Current.rds") %>%
  select(fromId, travel_time, only_transit_CMATT) %>%
  filter(!is.na(only_transit_CMATT))%>%
  mutate(scenario = "current")


#### Alternative scenario ######
alternative <- readRDS("data/output_access/transit_access_Future.rds") %>%
  select(fromId, travel_time, only_transit_CMATT) %>%
  filter(!is.na(only_transit_CMATT)) %>%
  mutate(scenario = "future")


##### bind of results #####
resultado <- rbind(current, alternative)

###### calculate Palma ratio ######
for_hex <- readRDS("//storage6/usuarios/Proj_acess_oport/data/acesso_oport/hex_agregados/2019/hex_agregado_for_09_2019.rds") %>% 
  st_drop_geometry() %>%
  filter(pop_total > 0,
         renda_decil %in% c(1,2,3,4,10))%>%
  select(id_hex,renda_decil)

resultado_join <- dplyr::left_join(resultado, for_hex, by=c("fromId"="id_hex")) %>%
  filter(!is.na(renda_decil))

media_acc_time_decil_1_4 <- resultado_join %>%
  filter(renda_decil %in% c(1,2,3,4)) %>%
  group_by(travel_time, scenario) %>%
  summarise(mean_acc_jobs_decil_1_4 = mean(only_transit_CMATT))

media_acc_time_decil_10 <- resultado_join %>%
  filter(renda_decil == 10) %>%
  group_by(travel_time, scenario) %>%
  summarise(mean_acc_jobs_decil_10 = mean(only_transit_CMATT))

palma <- cbind(media_acc_time_decil_1_4,media_acc_time_decil_10)%>%
  select(travel_time...1, scenario...2, mean_acc_jobs_decil_1_4, mean_acc_jobs_decil_10) %>%
  rename(travel_time = "travel_time...1",
         scenario = "scenario...2") %>%
  mutate(palma_ratio = mean_acc_jobs_decil_10/mean_acc_jobs_decil_1_4)

h_line <- 1

##### plot results #####
options(scipen = 10000)
ggplot()+
  geom_line(data=subset(palma, scenario == "current"), aes(x=travel_time, y=palma_ratio, colour="Current"))+
  geom_line(data=subset(palma, scenario == "future"), aes(x=travel_time, y=palma_ratio, colour="Future"))+
  geom_hline(aes(yintercept = h_line), color = "red") +
  geom_text(aes(8, h_line, label = paste0("Palma ratio = ",h_line), vjust = -1))+
  ylab("Palma ratio")+
  xlab("Travel Time (minutes)")+
  scale_colour_manual(values = c("Current" = "black", "Future" = "#619CFF"), name="Scenario",
                      guide = guide_legend(override.aes = list(linetype = c("solid", "solid"),
                                                               shape= c(NA,NA))))+
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(size = 18),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray93"),
        panel.grid.major.x = element_line(color = "gray93"))

ggsave("Fig_12.png", width = 15, height = 10, units = "cm", dpi = 300)


