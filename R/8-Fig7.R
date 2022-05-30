library(data.table)
library(ggplot2)
# city <- tar_read(both_cities)[1]
# access_diff_path <- tar_read(transit_access_diff)[1]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
# travel_time <- 60
create_boxplots <- function(city,
                            access_diff_path,
                            grid_path,
                            travel_time) {
  
  access_diff <- readRDS(access_diff_path)
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  access_diff <- access_diff[type == "abs"]
  access_diff <- access_diff[
    ,
    scenario := factor(
      scenario,
      levels = c("depois", "contrafactual"),
      labels = c("Previsto", "Alternativo")
    )
  ]
  access_diff <- access_diff[travel_time == get("travel_time", envir = env)]
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(pop = i.pop_total, decil = i.decil)
  ]
  access_diff <- access_diff[decil > 0]
  
  measures <- c("CMATT", "CMAET", "CMASB")
  
  # create a plot for each measure and return the file paths
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      limit <- fcase(
        measure == "CMATT" && city == "for", 100000,
        measure == "CMAET" && city == "for", 80,
        measure == "CMASB" && city == "for", 30,
        measure == "CMATT" && city == "goi", 50000,
        measure == "CMAET" && city == "goi", 8,
        measure == "CMASB" && city == "goi", 5
      )
      
      # plot settings
      
      plot <- ggplot(access_diff) +
        geom_segment(
          aes(
            x = 0.5, y = 0,
            xend = 10.5, yend = 0
          ),
          color = "gray85"
        ) +
        geom_boxplot(
          aes(
            as.factor(decil),
            get(relevant_var),
            weight = pop,
            color = as.factor(decil)
          ),
          outlier.size = 1.5,
          outlier.alpha = 0.5
        ) +
        facet_wrap(~ scenario, nrow = 1) +
        scale_color_viridis_d(
          option = "cividis",
          labels = c("1\nmais pobres", 2:9, "10\nmais ricos"),
          name = "Decil de renda"
        ) +
        scale_y_continuous(labels = scales::number) +
        scale_x_discrete(limits = factor(1:10)) +
        guides(color = guide_legend(nrow = 1, label.position = "bottom")) +
        labs(y = "Diferença de acessibilidade", x = NULL) +
        coord_cartesian(ylim = c(-limit, limit)) +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray93"),
          panel.grid.major.x = element_blank(),
          plot.subtitle = element_markdown(),
          legend.position = "bottom",
          legend.text.align = 0.5,
          strip.text = element_text(size = 11)
        )
      
      # return the result so the target follows the file
      
      dir_path <- file.path("figures")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "boxplot_difference")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      # ggsave() raises a warning in the CMAET case related to the non
      # uniqueness of the quantile regression solution. nothing to really worry
      # about
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = 9,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


# city <- "for"
# access_paths <- tar_read(access_metadata)$access_file[1:3]
# scenarios <- tar_read(access_metadata)$scenario[1:3]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
# travel_time <- 60
create_dist_maps <- function(city,
                             access_paths,
                             scenarios,
                             grid_path,
                             travel_time) {
  
  access <- lapply(access_paths, readRDS)
  names(access) <- scenarios
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  # join accessibility difference datasets to create a faceted chart and filter
  # to keep only relevant travel_time
  
  access <- rbindlist(access, idcol = "scenario")
  access <- access[travel_time == get("travel_time", envir = env)]
  access[
    ,
    scenario := factor(
      scenario,
      levels = c("antes", "depois", "contrafactual"),
      labels = c("Antes", "Depois (Previsto)", "Depois (Alternativo)")
    )
  ]
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil)
  ]
  
  # download city and state shapes
  
  city_code <- ifelse(city == "for", 2304400, 5208707)
  state_code <- ifelse(city == "for", "CE", "GO")
  
  city_shape <- geobr::read_municipality(city_code)
  state_shape <- geobr::read_municipality(state_code)
  
  city_bbox <- sf::st_bbox(city_shape)
  xlim <- c(city_bbox$xmin, city_bbox$xmax)
  ylim <- c(city_bbox$ymin, city_bbox$ymax)
  
  # read transit routes shapes
  
  if (city == "for") {
    
    gtfs <-  gtfstools::read_gtfs(
      #"data-raw/gtfs/gtfs_for_metrofor_2021-01_depois.zip"
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
    
    # the 'before' scenario must not include the new subway line.
    # bind 'transit_shapes' into itself and add column to make sure the lines
    # are adequately faceted
    
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
    
  }
  
  # generate figures for each of our desired variables
  
  measures <- c("CMATT", "CMAET", "CMASB")
  
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      label <- if (grepl("TT", measure)) {
        scales::label_number(
          accuracy = 1,
          scale = 1/1000,
          suffix = "k",
          big.mark = ","
        )
      } else {
        scales::label_number()
      }
      
      break_precision <- ifelse(grepl("TT", measure), 50000, 10)
      max_break <- round_up(
        max(access[[relevant_var]], na.rm = TRUE), 
        break_precision
      )
      breaks <- seq(0, max_break, length.out = 3)
      
      plot <- ggplot() +
        geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
        geom_sf(
          data = st_sf(access),
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        facet_wrap(~ scenario) +
        coord_sf(xlim, ylim) +
        scale_fill_viridis_c(
          name = "Acessibilidade por\ntransporte publico",
          option = "inferno",
          label = label,
          breaks = breaks,
          limit = c(0, max_break)
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 11),
          panel.background = element_rect(fill = "#aadaff", color = NA)
        )
      
      dir_path <- file.path("figures")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "map_distribution")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      if (city == "for") {
        plot_height <- 7.5
      } else {
        plot_height <- 8.5
      }
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = plot_height,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


# city <- "for"
# access_diff_path <- tar_read(transit_access_diff)[1]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
# travel_time <- 60
# # create_diff_maps(city, access_diff_path, grid_path, travel_time)
create_diff_maps <- function(city,
                             access_diff_path,
                             grid_path,
                             travel_time) {
  
  access_diff <- readRDS(access_diff_path)
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  access_diff <- access_diff[type == "abs"]
  access_diff <- access_diff[travel_time == get("travel_time", envir = env)]
  access_diff[
    ,
    scenario := factor(
      scenario,
      #levels = c("depois", "contrafactual"),
      levels = c("contrafactual"),
      #labels = c("Previsto", "Alternativo")
      labels = c("Alternativo")
    )
  ]
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil)
  ]
  
  # download city and state shapes
  
  city_code <- ifelse(city == "for", 2304400, 5208707)
  state_code <- ifelse(city == "for", "CE", "GO")
  
  city_shape <- geobr::read_municipality(city_code)
  state_shape <- geobr::read_municipality(state_code)
  
  city_bbox <- sf::st_bbox(city_shape)
  xlim <- c(city_bbox$xmin, city_bbox$xmax)
  ylim <- c(city_bbox$ymin, city_bbox$ymax)
  
  # read transit routes shapes
  
  if (city == "for") {

    gtfs <-  gtfstools::read_gtfs(
      #"data-raw/gtfs/gtfs_for_metrofor_2021-01_depois.zip"
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

  }
  
  # generate figures depending on the variable
  
  measures <- c("CMATT", "CMAET")
  library(dplyr)
  access_diff <- access_diff %>% filter(!is.na(scenario))
  paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("only_transit_", measure)
      
      # truncate difference values, based on each measure's value
      
      max_value <- fcase(
        measure == "CMATT", 100000,
        measure == "CMAET", 60
        #measure == "CMASB", 20
      )
      
      access_diff[
        ,
        eval(relevant_var) := fifelse(
          get(relevant_var) > max_value,
          max_value,
          fifelse(
            #get(relevant_var) < -max_value,
            #-max_value,
            get(relevant_var) < 0,
            0,
            get(relevant_var)
          )
        )
      ]
      
      # legend-guide related objects
      
      #breaks <- c(-max_value, 0, max_value)
      breaks <- c(0, max_value)
      labels <- if (grepl("TT", measure)) {
        #c(paste0("< ", breaks[1]/1000, "k"), 0, paste0("> ", breaks[3]/1000, "k"))
        c(0, paste0("> ", breaks[2]/1000, "k"))
      } else {
        #c(paste0("< ", breaks[1]), 0, paste0("> ", breaks[3]))
        c(0, paste0("> ", breaks[2]))
      }
      
      # plot settings
      
      plot <- ggplot() +
        geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
        geom_sf(
          data = st_sf(access_diff),
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        facet_wrap(~ scenario) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA, color = "gray60") +
        scale_fill_distiller(
          name = "Accessibility\ngain",
          palette = "Purples", ####ALTEREI
          direction = 1,
          n.breaks = 4,
          #limits = c(-max_value, max_value),
          limits = c(0, max_value),
          breaks = breaks,
          labels = labels
        ) +
        coord_sf(xlim, ylim) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_text(vjust = 1),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 11),
          panel.background = element_rect(fill = "#aadaff", color = NA)
        )
      
      # save the result and return the path
      
      dir_path <- file.path("figures")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "map_difference")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      if (city == "for") {
        plot_height <- 9
      } else {
        plot_height <- 10
      }
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = plot_height,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


# city <- tar_read(only_for)
# scenarios <- tar_read(scenarios)
# access_paths <- tar_read(full_access)
# access_diff_path <- tar_read(full_access_diff)
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
# travel_time <- 60
# # plot_summary(city, scenarios, access_paths, access_diff_path, grid_path, travel_time)
plot_summary <- function(city,
                         scenarios,
                         access_paths,
                         access_diff_path,
                         grid_path,
                         travel_time) {
  
  names(access_paths) <- scenarios
  access <- lapply(access_paths, readRDS)
  access_diff <- readRDS(access_diff_path)
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  access <- rbindlist(access, idcol = "scenario")
  access <- access[travel_time == get("travel_time", envir = env)]
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil, pop = i.pop_total)
  ]
  access[
    ,
    scenario := factor(
      scenario,
      levels = c("antes", "depois", "contrafactual"),
      labels = c("Antes", "Depois (Previsto)", "Depois (Alternativo)")
    )
  ]
  
  access_diff <- access_diff[type == "abs"]
  access_diff <- access_diff[travel_time == get("travel_time", envir = env)]
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(geometry = i.geometry, decil = i.decil, pop = i.pop_total)
  ]
  access_diff[
    ,
    scenario := factor(
      scenario,
      levels = c("depois", "contrafactual"),
      labels = c("Previsto", "Alternativo")
    )
  ]
  
  # download city and state shapes
  
  city_code <- ifelse(city == "for", 2304400, 5208707)
  state_code <- ifelse(city == "for", "CE", "GO")
  
  city_shape <- geobr::read_municipality(city_code)
  state_shape <- geobr::read_municipality(state_code)
  
  city_bbox <- sf::st_bbox(city_shape)
  xlim <- c(city_bbox$xmin, city_bbox$xmax)
  ylim <- c(city_bbox$ymin, city_bbox$ymax)
  
  # read transit routes shapes
  
  gtfs <-  gtfstools::read_gtfs(
    "data-raw/gtfs/gtfs_for_metrofor_2021-01_depois.zip"
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
  
  # the 'before' scenario must not include the new subway line.
  # bind 'transit_shapes' into itself and add column to make sure the lines
  # are adequately faceted
  
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
  
  # create dataframe holding the palma ratio of the distribution of access to
  # each kind of opportunity
  
  measures <- c("CMATT", "CMAET", "CMASB")
  relevant_vars <- paste0("only_transit_", measures)
  
  access_palma <- access[
    ,
    .(dist = list(.SD)),
    by = scenario,
    .SDcols = c(relevant_vars, "pop", "decil")
  ]
  
  # calculate the palma ratio for each relevant_var
  
  palma_expr <- paste0(
    measures,
    "=",
    "vapply(dist, function(dt) calculate_palma(dt,'", relevant_vars,
    "'), numeric(1))",
    collapse = ", "
  )
  palma_expr <- paste0("`:=`(", palma_expr, ")")
  
  access_palma[, eval(parse(text = palma_expr))]
  
  # melt the data to create a faceted chart
  
  access_palma <- melt(
    access_palma,
    id.vars = "scenario",
    measure.vars = measures,
    variable.name = "opportunities",
    value.name = "palma"
  )
  
  # individual plots for each measure
  
  vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      relevant_var <- paste0("all_modes_", measure)
      
      label_func <- if (grepl("TT", measure)) {
        scales::label_number(
          accuracy = 1,
          scale = 1/1000,
          suffix = "k",
          big.mark = ","
        )
      } else {
        scales::label_number()
      }
      
      # first row - accessibility distribution
      
      map_dist <- ggplot() +
        geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
        geom_sf(
          data = st_sf(access),
          aes(fill = get(relevant_var)),
          color = NA
        ) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        facet_wrap(~ scenario) +
        scale_fill_viridis_c(
          name = "Acessibilidade",
          option = "inferno",
          label = label_func,
          n.breaks = 4
        ) +
        coord_sf(xlim, ylim) +
        labs(y = "Distribuição de\nacessibilidade") +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 11),
          legend.justification = "left",
          legend.title = element_text(size = 10),
          panel.background = element_rect(fill = "#aadaff", color = NA)
        )
      
      # fourth row - palma ratio bars
      # first create a data.table to position label on top of each bar
      
      filtered_palma <- access_palma[opportunities == measure]
      filtered_palma <- filtered_palma[
        ,
        `:=`(
          y_pos = palma + max(palma) * 0.05,
          palma_text = format(palma, digits = 2, nsmall = 2)
        )
      ]
      
      palma_bars <- ggplot(filtered_palma) +
        geom_col(aes(scenario, palma, fill = scenario)) +
        geom_text(
          aes(x = scenario, y = y_pos, label = palma_text),
          color = "gray20",
          vjust = 0,
          size = 6
        ) +
        geom_segment(
          aes(
            x = 0.5, y = 1,
            xend = 3.5, yend = 1
          ),
          color = "gray40",
          linetype = "longdash"
        ) +
        coord_cartesian(ylim = c(0, max(filtered_palma$palma) * 1.17)) +
        scale_y_continuous(name = "Razão de Palma") +
        scale_x_discrete(name = "Cenário") +
        scale_fill_discrete(
          name = "Cenário",
          type = c("gray50", "#F8766D", "#00BFC4")
        ) +
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray93"),
          panel.grid.major.x = element_blank(),
          legend.position = "none"
        )
      
      # third row - accessibility differences distribution map
      # truncate values for each measure
      # remove 'scenario' column from 'transit_shapes' so it doesnt create extra
      # facets
      
      transit_shapes <- transit_shapes[scenario == "Depois"]
      transit_shapes$scenario <- NULL
      
      max_value <- fcase(
        measure == "CMATT", 100000,
        measure == "CMAET", 60,
        measure == "CMASB", 20
      )
      
      access_diff[
        ,
        truncated_value := fifelse(
          get(relevant_var) > max_value,
          max_value,
          fifelse(
            get(relevant_var) < -max_value,
            -max_value,
            get(relevant_var)
          )
        )
      ]
      
      breaks <- c(-max_value, -max_value / 2, 0, max_value / 2, max_value)
      labels <- breaks
      if (grepl("TT", measure)) {
        labels <- paste0(labels / 1000, "k")
        labels[3] <- 0
      }
      labels[1] <- paste0("< ", labels[1])
      labels[5] <- paste0("> ", labels[5])
      
      map_diff <- ggplot() +
        geom_sf(data = state_shape, fill = "#efeeec", color = "gray75") +
        geom_sf(
          data = st_sf(access_diff),
          aes(fill = truncated_value),
          color = NA
        ) +
        facet_wrap(~ scenario) +
        geom_sf(
          data = st_sf(transit_shapes),
          size = 0.5,
          alpha = 0.7
        ) +
        geom_sf(data = city_shape, fill = NA) +
        scale_fill_distiller(
          name = "Diferença",
          palette = "RdBu",
          direction = 1,
          limits = c(-max_value, max_value),
          breaks = breaks,
          labels = labels
        ) +
        labs(y = "Distribuição da dif.\nde acessibilidade") +
        coord_sf(xlim, ylim) +
        theme_minimal() +
        theme(
          axis.text = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 11),
          legend.justification = "left",
          legend.title = element_text(size = 10),
          panel.background = element_rect(fill = "#aadaff", color = NA)
        )
      
      # fourth row - accessibility differences distribution boxplot
      
      access_diff <- access_diff[decil > 0, ]
      
      limit <- fcase(
        measure == "CMATT", 100000,
        measure == "CMAET", 80,
        measure == "CMASB", 30
      )
      
      boxplot_diff <- ggplot(access_diff) +
        geom_segment(
          aes(
            x = 0.5, y = 0,
            xend = 10.5, yend = 0
          ),
          color = "gray85"
        ) +
        geom_boxplot(
          aes(
            as.factor(decil),
            get(relevant_var),
            weight = pop,
            color = as.factor(decil)
          ),
          outlier.size = 1.5,
          outlier.alpha = 0.5,
          show.legend = TRUE
        ) +
        facet_wrap(~ scenario, nrow = 1) +
        scale_color_viridis_d(option = "cividis", name = "Decil de renda") +
        scale_y_continuous(labels = label_func) +
        scale_x_discrete(limits = factor(1:10)) +
        labs(y = "Diferença de\nacessibilidade") +
        coord_cartesian(ylim = c(-limit, limit)) +
        guides(color = guide_legend(ncol = 2)) +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray93"),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(size = 11),
          legend.justification = "left",
          legend.title = element_text(size = 10)
        )
      
      # join all four rows rows in a single plot
      
      final_plot <- cowplot::plot_grid(
        map_dist,
        palma_bars,
        map_diff,
        boxplot_diff,
        ncol = 1,
        labels = c("A)", "B)", "C)", "D)"),
        rel_heights = c(1.01, 1, 1.41, 1),
        align = "v",
        axis = "lr"
        
      )
      
      # save the result and return the path
      
      dir_path <- file.path("figures")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "summary_bfm_bike_transit")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
      ggsave(
        file_path,
        final_plot,
        width = 18,
        height = 20,
        units = "cm"
      )
      
      return(file_path)
      
    }
  )
  
}


# city <- tar_read(both_cities)[1]
# access_paths <- tar_read(access_metadata)$access_file[1:3]
# scenarios <- tar_read(access_metadata)$scenario[1:3]
# grid_path <- tar_read(grid_path)[1]
# travel_time <- tar_rea
# measure <- "CMATT"
# travel_time <- 60
create_palma_bars <- function(city,
                              access_paths,
                              scenarios,
                              grid_path,
                              travel_time) {
  
  grid <- setDT(readRDS(grid_path))
  env <- environment()
  
  names(access_paths) <- scenarios
  access <- lapply(access_paths, readRDS)
  access <- rbindlist(access, idcol = "scenario")
  access <- access[travel_time == get("travel_time", envir = env)]
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(pop = i.pop_total, decil = i.decil)
  ]
  access <- access[decil > 0]
  access[
    ,
    scenario := factor(
      scenario,
      levels = c("antes", "depois", "contrafactual"),
      labels = c("Antes", "Depois (Previsto)", "Depois (Alternativo)")
    )
  ]
  
  measures <- c("CMATT", "CMAET", "CMASB")
  relevant_vars <- paste0("only_transit_", measures)
  
  # nest dataframes of accessibility distribution for each scenario to calculate
  # the palma ratio
  
  access <- access[
    ,
    .(dist = list(.SD)),
    by = scenario,
    .SDcols = c(relevant_vars, "pop", "decil")
  ]
  
  # calculate the palma ratio for each relevant_var
  
  palma_expr <- paste0(
    measures,
    "=",
    "vapply(dist, function(dt) calculate_palma(dt,'", relevant_vars,
    "'), numeric(1))",
    collapse = ", "
  )
  palma_expr <- paste0("`:=`(", palma_expr, ")")
  
  access[, eval(parse(text = palma_expr))]
  
  # melt the data to create a faceted chart
  
  access <- melt(
    access,
    id.vars = "scenario",
    measure.vars = measures,
    variable.name = "opportunities",
    value.name = "palma"
  )
  
  # individual plots for each measure
  
  individual_paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      filtered_access <- access[opportunities == measure]
      env <- environment()
      
      # create data.table to position label on top of each bar
      
      label_pos <- copy(filtered_access)[
        ,
        `:=`(
          y_pos = palma + max(palma) * 0.05,
          palma_text = format(palma, digits = 2, nsmall = 2)
        )
      ]
      
      plot <- ggplot(filtered_access) +
        geom_col(aes(scenario, palma, fill = scenario)) +
        geom_text(
          data = label_pos,
          aes(x = scenario, y = y_pos, label = palma_text),
          color = "gray20",
          vjust = 0,
          size = 6
        ) +
        geom_segment(
          aes(
            x = 0.5, y = 1,
            xend = 3.5, yend = 1
          ),
          color = "gray40",
          linetype = "longdash"
        ) +
        scale_y_continuous(name = "Razão de Palma") +
        scale_x_discrete(name = "Cenário") +
        scale_fill_discrete(
          name = "Cenário",
          type = c("gray50", "#F8766D", "#00BFC4")
        ) +
        coord_cartesian(ylim = c(0, max(filtered_access$palma) * 1.12)) +
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray95"),
          panel.grid.major.x = element_blank(),
          legend.position = "none"
        )
      
      # save and return the result
      
      dir_path <- file.path("figures")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      dir_path <- file.path(dir_path, "columns_palma")
      if (!dir.exists(dir_path)) dir.create(dir_path)
      
      file_path <- file.path(dir_path, paste0(measure, travel_time, ".png"))
      ggsave(
        file_path,
        plot,
        width = 16,
        height = 6,
        units = "cm"
      )
    }
  )
  
}


# city <- tar_read(both_cities)[1]
# access_paths <- tar_read(access_metadata)$access_file[1:3]
# scenarios <- tar_read(access_metadata)$scenario[1:3]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
compare_palma <- function(city, access_paths, scenarios, grid_path) {
  
  grid <- setDT(readRDS(grid_path))
  
  names(access_paths) <- scenarios
  access <- lapply(access_paths, readRDS)
  access <- rbindlist(access, idcol = "scenario")
  access[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(pop = i.pop_total, decil = i.decil)
  ]
  access <- access[decil > 0]
  access[
    ,
    scenario := factor(
      scenario,
      #levels = c("antes", "depois", "contrafactual"),
      levels = c("antes", "depois"),
      labels = c("Antes", "Depois (Alternativo)")
    )
  ]
  
  measures <- c("CMATT", "CMAET", "CMASB")
  relevant_vars <- paste0("only_transit_", measures)
  
  # nest dataframes of accessibility distribution for each scenario and travel
  # time to calculate the palma ratio of each case
  
  access <- access[
    ,
    .(dist = list(.SD)),
    by = .(scenario, travel_time),
    .SDcols = c(relevant_vars, "pop", "decil")
  ]
  
  # calculate the palma ratio for each relevant_var
  
  palma_expr <- paste0(
    measures,
    "=",
    "vapply(dist, function(dt) calculate_palma(dt,'", relevant_vars,
    "'), numeric(1))",
    collapse = ", "
  )
  palma_expr <- paste0("`:=`(", palma_expr, ")")
  
  access[, eval(parse(text = palma_expr))]
  
  # melt the data to create a faceted chart
  
  access <- melt(
    access,
    id.vars = c("scenario", "travel_time"),
    measure.vars = measures,
    variable.name = "opportunities",
    value.name = "palma"
  )
  
  # convert the opportunities' names to factor
  
  access[
    ,
    opportunities_factor := factor(
      opportunities,
      levels = measures,
      labels = c("Emprego", "Educação", "Saúde")
    )
  ]
  
  # dataframe for positioning label above segment
  
  label_pos <- access[
    ,
    .(max_palma = max(palma, na.rm = TRUE)),
    by = opportunities_factor
  ]
  label_pos[, ggplot_range := max_palma * 1.05 + 0.0125]
  label_pos[, y_pos := 1 + 0.01 * ggplot_range]
  label_pos[, opportunities := measures]
  
  # plot settings - first a single chart including all three opportunities
  
  plot_theme <- theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      strip.text = element_text(size = 11)
    )
  
  plot <- ggplot(access) +
    geom_segment(
      aes(
        x = 0.0, y = 1,
        xend = 60, yend = 1
      ),
      color = "gray75"
    ) +
    geom_text(
      data = label_pos,
      aes(x = 0, y = y_pos, label = "R.P. = 1"),
      color = "gray75",
      hjust = 0,
      vjust = 0
    ) +
    geom_line(aes(travel_time, palma, color = scenario)) +
    facet_wrap(~ opportunities_factor, scales = "free") +
    scale_y_continuous(
      name = "Razão de Palma",
      expand = expansion(mult = c(0.0125, 0))
    ) +
    scale_x_continuous(name = "Limite de tempo de viagem") +
    scale_color_discrete(
      name = "Cenário",
      type = c("gray50", "#F8766D", "#00BFC4")
    ) +
    expand_limits(y = 0) +
    plot_theme +
    theme(legend.position = "bottom")
  
  # save the result and store the path
  
  dir_path <- file.path("figures")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  dir_path <- file.path(dir_path, "palma_comparison")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  faceted_path <- file.path(dir_path, paste0("faceted.png"))
  ggsave(
    faceted_path,
    plot,
    width = 16,
    height = 6,
    units = "cm"
  )
  
  # individual plots for each measure
  
  individual_paths <- vapply(
    measures,
    FUN.VALUE = character(1),
    FUN = function(measure) {
      filtered_access <- access[opportunities == measure]
      env <- environment()
      
      plot <- ggplot(filtered_access) +
        geom_segment(
          aes(
            x = 0.0, y = 1,
            xend = 60, yend = 1
          ),
          color = "gray75"
        ) +
        geom_text(
          aes(
            x = 0,
            y = label_pos[opportunities == get("measure", envir = env)]$y_pos,
            label = "Razão de Palma = 1"
          ),
          color = "gray75",
          hjust = 0,
          vjust = 0
        ) +
        geom_line(aes(travel_time, palma, color = scenario)) +
        scale_y_continuous(
          name = "Razão de Palma",
          expand = expansion(mult = c(0.0125, 0))
        ) +
        scale_x_continuous(name = "Limite de tempo de viagem") +
        scale_color_discrete(
          name = "Cenário",
          type = c("gray50", "#F8766D", "#00BFC4")
        ) +
        expand_limits(y = 0) +
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
  
  all_paths <- c(faceted_path, individual_paths)
  return(all_paths)
  
}


# city <- tar_read(both_cities)[1]
# access_diff_path <- tar_read(transit_access_diff)[1]
# grid_path <- tar_read(grid_path)[1]
# measure <- "CMATT"
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
      #levels = c("depois", "contrafactual"),
      levels = c("contrafactual"),
      #labels = c("Previsto", "Alternativo")
      labels = c("depois")
    )
  ]
  
  #measures <- c("CMATT", "CMAET", "CMASB")
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
      #labels = c("Emprego", "Educação", "Saúde")
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
  
  #write.csv(access_diff_summary, "access_diff_summary.csv", row.names = F) ## acrescentado por mim
  access_diff_summary <- access_diff_summary %>%
    filter(scenario == "contrafactual")
  
  tempos_janelas <- c(
    "20min" = 20,
    "40min" = 40,
    "60min" = 60,
    "80min" = 80
  )
  oportunidades <- c("jobs","jobs","jobs","jobs")
  access <- c(0,70,6299,4794)
  access_janelas <- data.frame(oportunidades,access)
  #names(access_janelas) <- rep("", 6)
  
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
  
  
  # janelas_verticais_schools <- data.table(
  #   x = tempos_janelas,
  #   xend = tempos_janelas,
  #   y = 0,
  #   yend = subset(access_janelas$access, oportunidades=="schools")
  # )
  # janelas_horizontais_schools <- data.table(
  #   x = 0,
  #   xend = tempos_janelas,
  #   y = subset(access_janelas$access, oportunidades=="schools"),
  #   yend = subset(access_janelas$access, oportunidades=="schools")
  # )
  
  
  plot_theme <- theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray95"),
      strip.text = element_text(size = 11)
    )
  
  # plot <- ggplot(access_diff_summary) +
  #   geom_segment(
  #     aes(
  #       x = 0.0, y = 0,
  #       #xend = 60, yend = 0
  #       xend = 120, yend = 0
  #     ),
  #     color = "gray75"
  #   ) +
  # 
  #   geom_line(aes(travel_time, weighted_median, color = scenario)) +
  #   geom_ribbon(
  #     aes(travel_time, ymin = q1, ymax = q3, fill = scenario),
  #     alpha = 0.5
  #   ) +
  #   geom_segment(
  #     data = janelas_verticais,
  #     aes(x = x, xend = xend, y = y, yend = yend),
  #     linetype = "dashed",
  #     color = rep(c("black", "black", "black", "black"), each=2)
  #   ) +
  #   geom_segment(
  #     data = janelas_horizontais,
  #     aes(x = x, xend = xend, y = y, yend = yend),
  #     linetype = "dotted",
  #     color = rep(c("black", "black", "black", "black"), each=2),
  #     alpha = 0.5
  #   ) +
  # 
  #   facet_wrap(~ opportunities_factor, scales = "free") +
  #   scale_y_continuous(name = "Accessibility gain") +
  #   scale_x_continuous(name = "Travel time threshold") +
  #   scale_color_discrete(name = "Scenario") +
  #   scale_fill_discrete(name = "Scenario") +
  #   plot_theme +
  #   theme(legend.position = "bottom")
  
  
  plot_jobs <- ggplot(subset(access_diff_summary, opportunities_factor == "jobs")) +
    geom_segment(
      aes(
        x = 0.0, y = 0,
        #xend = 60, yend = 0
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

    #facet_wrap(~ opportunities_factor, scales = "free") +
    scale_y_continuous(name = "Accessibility gain") +
    scale_x_continuous(name = "Travel time threshold") +
    scale_color_discrete(name = "Scenario") +
    scale_fill_discrete(name = "Scenario") +
    plot_theme +
    theme(legend.position = "bottom")
  
  
  # plot_schools <- ggplot(subset(access_diff_summary, opportunities_factor == "schools")) +
  #   geom_segment(
  #     aes(
  #       x = 0.0, y = 0,
  #       #xend = 60, yend = 0
  #       xend = 120, yend = 0
  #     ),
  #     color = "gray75"
  #   ) +
  #   
  #   geom_line(aes(travel_time, weighted_median, color = scenario)) +
  #   geom_ribbon(
  #     aes(travel_time, ymin = q1, ymax = q3, fill = scenario),
  #     alpha = 0.5
  #   ) +
  #   geom_segment(
  #     data = janelas_verticais_jobs,
  #     aes(x = x, xend = xend, y = y, yend = yend),
  #     linetype = "dashed",
  #     color = rep(c("black", "black", "black", "black"))
  #   ) +
  #   geom_segment(
  #     data = janelas_horizontais_jobs,
  #     aes(x = x, xend = xend, y = y, yend = yend),
  #     linetype = "dotted",
  #     color = rep(c("black", "black", "black", "black")),
  #     alpha = 0.5
  #   ) +
  #   
  #   #facet_wrap(~ opportunities_factor, scales = "free") +
  #   scale_y_continuous(name = "Accessibility gain") +
  #   scale_x_continuous(name = "Travel time threshold") +
  #   scale_color_discrete(name = "Scenario") +
  #   scale_fill_discrete(name = "Scenario") +
  #   plot_theme +
  #   theme(legend.position = "bottom")
  # 
  # pf <- plot_grid(
  #   plot_jobs, plot_schools,
  #   nrow = 1,
  #   align = "h",
  #   labels = c("")
  # )
  
  
  # save the result and store the path
  
  setwd("~")
  
  dir_path <- file.path("figures")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  dir_path <- file.path(dir_path, "access_gains_comparison")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  faceted_path <- file.path(dir_path, paste0("faceted.png"))
  ggsave(
    faceted_path,
    plot,
    width = 16,
    height = 6,
    units = "cm"
  )
  
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
        #scale_y_continuous(name = "Ganho de acessibilidade") +
        scale_y_continuous(name = "Accessibility gain") +
        #scale_x_continuous(name = "Limite de tempo de viagem") +
        scale_x_continuous(name = "Travel time threshold") +
        #scale_color_discrete(name = "Cenário") +
        scale_color_discrete(name = "Scenario") +
        #scale_fill_discrete(name = "Cenário") +
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
  
  all_paths <- c(faceted_path, individual_paths)
  return(all_paths)
  
}


# city <- tar_read(both_cities)[1]
# access_diff_path <- tar_read(transit_access_diff)[1]
# grid_path <- tar_read(grid_path)[1]
calculate_avg_gains <- function(city,
                                access_diff_path,
                                grid_path) {
  
  grid <- setDT(readRDS(grid_path))
  
  relevant_vars <- paste0("only_transit_", c("CMATT", "CMAET", "CMASB"))
  
  access_diff <- readRDS(access_diff_path)
  access_diff <- access_diff[type == "rel" & travel_time == 60]
  access_diff <- melt(
    access_diff,
    measure.vars = relevant_vars,
    variable.name = "opportunity",
    value.name = "difference"
  )
  access_diff[, opportunity := gsub("only_transit_", "", opportunity)]
  access_diff[
    grid,
    on = c(fromId = "id_hex"),
    `:=`(pop = i.pop_total)
  ]
  
  # weighted mean
  
  avg_gains <- access_diff[
    ,
    .(avg_gain = weighted.mean(difference, w = pop)),
    by = .(scenario, opportunity)
  ]
  
  # polishing
  
  avg_gains[
    ,
    avg_gain := paste0(
      format(avg_gain * 100, digits = 1, nsmall = 1),
      "%"
    )
  ]
  avg_gains[
    ,
    scenario := factor(
      scenario,
      levels = c("depois", "contrafactual"),
      labels = c("Previsto", "Alternativo")
    )
  ]
  avg_gains[
    ,
    opportunity := factor(
      opportunity,
      levels = c("CMATT", "CMAET", "CMASB"),
      labels = c("Emprego", "Educação", "Saúde")
    )
  ]
  avg_gains <- dcast(
    avg_gains,
    scenario ~ opportunity,
    value.var = "avg_gain"
  )
  
  return(avg_gains)
  
}


calculate_palma <- function(access, relevant_var) {
  
  richest_10 <- access[decil == 10]
  poorest_40 <- access[decil >= 1 & decil <= 4]
  
  numerator <- weighted.mean(
    richest_10[[relevant_var]],
    w = richest_10$pop,
    na.rm = TRUE
  )
  
  denominator <- weighted.mean(
    poorest_40[[relevant_var]],
    w = poorest_40$pop,
    na.rm = TRUE
  )
  
  palma <- numerator / denominator
  
  return(palma)
  
}


round_up <- function(x, precision) {
  
  quotient_int <- ceiling(x / precision)
  return(quotient_int * precision)
  
}

library(sf)
library(data.table)
library(dplyr)
setwd("~")

#a <- c("//storage6//usuarios//Proj_acess_oport//data//avaliacao_intervencoes//for//output_access//transit_access_diff.rds")
a <- c("data//output_access//transit_access_diff.rds")
b <- c("//storage6//usuarios//Proj_acess_oport//data//acesso_oport//hex_agregados/2019/hex_agregado_for_09_2019.rds")
# 
compare_gains("for", a, b)
#test <- c("data/output_access/transit_access_Current.rds") #%>%
#  filter(travel_time == 30)

test <- readRDS(a)
test2 <- readRDS(b)

#create_diff_maps("for", a,b,80)
create_dist_maps("for",test,  "antes", b,30)





