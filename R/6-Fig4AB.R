library(data.table)
library(ggplot2)
library(sf)
library(dplyr)


###############################
############ PLOT A ###########
###############################

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
plot_A <- ggplot()+
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
        panel.grid.major.x = element_line(color = "gray93"))+
  ggtitle("A")



#############################################
################## PLOT B ###################
#############################################

setwd("~")

a <- c("data//output_access//transit_access_diff.rds")
b <- c("//storage6//usuarios//Proj_acess_oport//data//acesso_oport//hex_agregados/2019/hex_agregado_for_09_2019.rds")


grid <- setDT(readRDS(b))
access_diff <- readRDS(a)
access_diff <- access_diff[type == "abs"]
access_diff[
  grid,
  on = c(fromId = "id_hex"),
  `:=`(pop = i.pop_total)
]
access_diff[
  ,
  scenario := factor(
    scenario,
    levels = c("contrafactual"),
    labels = c("depois")
  )
]

measures <- c("CMATT")
relevant_vars <- paste0("only_transit_", measures)

# melt the data to create a faceted chart

access_diff <- melt(
  access_diff,
  measure.vars = relevant_vars,
  variable.name = "opportunities",
  value.name = "access_gain"
)
access_diff[, opportunities := gsub("only_transit_", "", opportunities)]

# convert the opportunities' names to factor

access_diff[
  ,
  opportunities_factor := factor(
    opportunities,
    levels = measures,
    labels = c("Jobs")
  )
]

# calculate the weighted mean of each scenario by travel time threshold and
# measure

access_diff_summary <- access_diff[
  ,
  .(
    weighted_median = matrixStats::weightedMedian(
      access_gain,
      w = pop,
      na.rm = TRUE
    ),
    q1 = quantile(access_gain, na.rm = TRUE)[["25%"]],
    q3 = quantile(access_gain, na.rm = TRUE)[["75%"]]
  ),
  by = .(scenario, travel_time, opportunities, opportunities_factor)
]

# plot settings - first a single chart including all three opportunities

access_diff_summary <- access_diff_summary %>%
  filter(scenario == "depois")

tempos_janelas <- c(
  "20min" = 20,
  "40min" = 40,
  "60min" = 60,
  "80min" = 80
)
oportunidades <- c("jobs","jobs","jobs","jobs")
access <- c(0,70,6299,4794)
access_janelas <- data.frame(oportunidades,access)

janelas_verticais_jobs <- data.table(
  x = tempos_janelas,
  xend = tempos_janelas,
  y = 0,
  yend = subset(access_janelas$access, oportunidades=="jobs")
)
janelas_horizontais_jobs <- data.table(
  x = 0,
  xend = tempos_janelas,
  y = subset(access_janelas$access, oportunidades=="jobs"),
  yend = subset(access_janelas$access, oportunidades=="jobs")
)

plot_theme <- theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray95"),
    strip.text = element_text(size = 11),
    legend.position = "none"
  )


plot_jobs <- ggplot(subset(access_diff_summary, opportunities_factor == "jobs")) +
  geom_segment(
    aes(
      x = 0.0, y = 0,
      xend = 120, yend = 0
    ),
    color = "gray75"
  ) +
  
  geom_line(aes(travel_time, weighted_median, color = scenario)) +
  geom_ribbon(
    aes(travel_time, ymin = q1, ymax = q3, fill = scenario),
    alpha = 0.5
  ) +
  geom_segment(
    data = janelas_verticais_jobs,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dashed",
    color = rep(c("black", "black", "black", "black"))
  ) +
  geom_segment(
    data = janelas_horizontais_jobs,
    aes(x = x, xend = xend, y = y, yend = yend),
    linetype = "dotted",
    color = rep(c("black", "black", "black", "black")),
    alpha = 0.5
  ) +
  
  scale_y_continuous(name = "Accessibility gain") +
  scale_x_continuous(name = "Travel time threshold") +
  scale_color_discrete(name = "Scenario") +
  scale_fill_discrete(name = "Scenario") +
  plot_theme +
  theme(legend.position = "bottom")


filtered_access_diff <- access_diff_summary[opportunities == measures]
env <- environment()

plot_B <- ggplot(filtered_access_diff) +
  geom_segment(
    aes(
      x = 0.0, y = 0,
      xend = 120, yend = 0
    ),
    color = "gray75"
  ) +
  geom_line(aes(travel_time, weighted_median, color = scenario)) +
  geom_ribbon(
    aes(travel_time, ymin = q1, ymax = q3, fill = scenario),
    alpha = 0.5
  ) +
  scale_y_continuous(name = "Accessibility gain") +
  scale_x_continuous(name = "Travel time threshold") +
  scale_color_discrete(name = "Scenario") +
  scale_fill_discrete(name = "Scenario") +
  plot_theme+
  ggtitle("B")

plot_A + plot_B

ggsave("Fig_4AB.png", width = 30, height = 12, units = "cm", dpi = 300)



