library(data.table)
library(ggplot2)
library(sf)
library(dplyr)

compare_gains <- function(city, access_diff_path, grid_path) {
  
  grid <- setDT(readRDS(grid_path))
  access_diff <- readRDS(access_diff_path)
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
      strip.text = element_text(size = 11)
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
  
  
  # save the result and store the path
  
  setwd("~")
  
  dir_path <- file.path("figures")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  dir_path <- file.path(dir_path, "access_gains_comparison")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  # individual plots for each measure
  
  individual_paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      filtered_access_diff <- access_diff_summary[opportunities == measure]
      env <- environment()
      
      plot <- ggplot(filtered_access_diff) +
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
        plot_theme
      
      # save and return the result so the target follows the file
      
      file_path <- file.path(dir_path, paste0(measure, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = 6,
        units = "cm"
      )
    }
  )
  
  #all_paths <- c(faceted_path, individual_paths)
  all_paths <- c(individual_paths)
  return(all_paths)
  
}



setwd("~")

a <- c("data//output_access//transit_access_diff.rds")
b <- c("//storage6//usuarios//Proj_acess_oport//data//acesso_oport//hex_agregados/2019/hex_agregado_for_09_2019.rds")
 
compare_gains("for", a, b)



