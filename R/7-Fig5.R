
library(data.table)
library(dplyr)
library(ggplot2)

setwd("~")

#### Current scenario ######

current <- readRDS("data/output_access/transit_access_Current.rds") %>%
  filter(!is.na(only_transit_CMATT))%>%
  group_by(travel_time) %>%
  summarise(median_CMATT = median(only_transit_CMATT)) %>%
  mutate(scenario = "current")


current_CMATT <- current %>%
  dplyr::select(travel_time, median_CMATT, scenario) %>%
  rename(median_acc = "median_CMATT") %>%
  mutate(measure = "Jobs")

current_final <- current_CMATT

#### Alternative scenario ######

alternative <- readRDS("data/output_access/transit_access_Future.rds") %>%
  filter(!is.na(only_transit_CMATT)) %>%
  group_by(travel_time) %>%
  summarise(median_CMATT = median(only_transit_CMATT)) %>%
  mutate(scenario = "alternative")

alternative_CMATT <- alternative %>%
  dplyr::select(travel_time, median_CMATT, scenario) %>%
  rename(median_acc = "median_CMATT") %>%
  mutate(measure = "Jobs")

alternative_final <- alternative_CMATT

##### bind of results #####

resultado <- rbind(current_final, alternative_final)

##### plot results #####
options(scipen = 10000)
ggplot()+
  geom_line(data=subset(resultado, scenario == "current"), aes(x=travel_time, y=median_acc, colour="Current"))+
  geom_line(data=subset(resultado, scenario == "alternative"), aes(x=travel_time, y=median_acc, colour="Future"))+
  ylab("Median Accessibility to jobs (number of opportunities)")+
  xlab("Travel Time (minutes)")+
  scale_colour_manual(values = c("Current" = "black", "Future" = "#619CFF"), name="Scenario",
                      guide = guide_legend(override.aes = list(linetype = c("solid", "solid"),
                                                               shape= c(NA,NA))))+
  theme_minimal() +
  theme(legend.position = "bottom", strip.text = element_text(size = 18),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "gray93"),
        panel.grid.major.x = element_line(color = "gray93"))

ggsave("Fig_5.png", width = 15, height = 10, units = "cm", dpi = 300)


