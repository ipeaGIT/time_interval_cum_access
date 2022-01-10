# # for_goi case
#
# city <- tar_read(both_cities)[1]
# scenario <- tar_read(before_after)[1]
# ttm_path <- tar_read(transit_matrix)[1]
# grid_path <- tar_read(grid_path)[1]
# opportunities <- c("total_jobs", "total_edu", "basic_health")
#
# # for special case
# 
# city <- tar_read(only_for)
# scenario <- tar_read(before_after)[1]
# ttm_path <- tar_read(full_matrix)[1]
# grid_path <- tar_read(grid_path)[1]
# opportunities <- c("total_jobs", "total_edu", "basic_health")
create_accessibility_data <- function(city,
                                      scenario,
                                      ttm_path,
                                      grid_path,
                                      opportunities = c(
                                        "total_jobs",
                                        "total_edu",
                                        "basic_health"
                                      )) {
  
  ttm <- readRDS(ttm_path)
  grid <- setDT(readRDS(grid_path))
  
  ttm <- ttm[
    grid,
    on = c(toId = "id_hex"),
    `:=`(
      total_jobs = i.empregos_total,
      total_edu = i.edu_total,
      basic_health = i.saude_baixa
    )
  ]
  
  # if 'ttm' is the pure transit matrix change the column travel_time to
  # transit_time, so the same bits of code can be used to calculate the "full"
  # and the "transit-only" access
  
  setnames(ttm, old = "travel_time", new = "transit_time", skip_absent = TRUE)
  
  access <- lapply(1:60, calculate_accessibility, ttm, opportunities)
  access <- rbindlist(access, idcol = "travel_time")
  
  # save object and return path
  # change name of file depending if it's only transit or all modes access.
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "output_access"
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- ifelse(
    is.null(ttm$bfm_time),
    file.path(dir_path, paste0("transit_access_", scenario, ".rds")),
    file.path(dir_path, paste0("full_access_", scenario, ".rds"))
  )
  saveRDS(access, file_path)
  
  return(file_path)
  
}


calculate_accessibility <- function(threshold, ttm, opportunities) {
  
  access <- data.table(fromId = unique(ttm$fromId))
  
  transit_access <- ttm[
    transit_time <= threshold,
    lapply(.SD, function(i) sum(i, na.rm = TRUE)),
    by = .(fromId),
    .SDcols = opportunities
  ]
  access[
    transit_access,
    on = "fromId",
    `:=`(
      only_transit_CMATT = i.total_jobs,
      only_transit_CMAET = i.total_edu,
      only_transit_CMASB = i.basic_health
    )
  ]
  
  # if ttm$bfm_time is NULL then this is the transit-only matrix. therefore,
  # only the transit-only bits of the accessibility calculation will be
  # conducted.
  # otherwise we are dealing with fortaleza-specific analysis that also include
  # bike-only and bike first mile scenarios
  
  if (!is.null(ttm$bfm_time)) {
    
    only_bike_access <- ttm[
      bike_time <= threshold,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      only_bike_access,
      on = "fromId",
      `:=`(
        only_bike_CMATT = i.total_jobs,
        only_bike_CMAET = i.total_edu,
        only_bike_CMASB = i.basic_health
      )
    ]
    
    only_bfm_access <- ttm[
      bfm_time <= threshold,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      only_bfm_access,
      on = "fromId",
      `:=`(
        only_bfm_CMATT = i.total_jobs,
        only_bfm_CMAET = i.total_edu,
        only_bfm_CMASB = i.basic_health
      )
    ]
    
    transit_bike_access <- ttm[
      transit_time <= threshold | bike_time <= threshold,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      transit_bike_access,
      on = "fromId",
      `:=`(
        transit_bike_CMATT = i.total_jobs,
        transit_bike_CMAET = i.total_edu,
        transit_bike_CMASB = i.basic_health
      )
    ]
    
    all_modes_access <- ttm[
      transit_time <= threshold | bike_time <= threshold | bfm_time <= threshold,
      lapply(.SD, function(i) sum(i, na.rm = TRUE)),
      by = .(fromId),
      .SDcols = opportunities
    ]
    access[
      all_modes_access,
      on = "fromId",
      `:=`(
        all_modes_CMATT = i.total_jobs,
        all_modes_CMAET = i.total_edu,
        all_modes_CMASB = i.basic_health
      )
    ]
    
  }
  
  return(access)
  
}


# city <- "for"
# access_paths <- tar_read(access_metadata)$access_file[1:3]
# scenario <- tar_read(access_metadata)$scenario[1:3]
# after <- "contrafactual"
calculate_access_diff <- function(city,
                                  access_paths,
                                  scenario) {
  
  access <- lapply(access_paths, readRDS)
  names(access) <- scenario
  
  # retrieve the name of the columns with accessibility data to calculate diff
  # between the datasets programatically
  
  access_cols <- setdiff(names(access[["depois"]]), c("fromId", "travel_time"))
  setnames(
    access[["antes"]],
    old = access_cols,
    new = paste0(access_cols, "_antes")
  )
  
  # calculate the difference between the 'contrafactual' and 'depois' scenarios
  # to the 'antes' scenario
  
  after_scenarios <- c(contrafactual = "contrafactual", depois = "depois")
  
  access_diff <- lapply(
    after_scenarios,
    function(after) {
      
      diff_dt <- access[["antes"]][
        access[[after]],
        on = c("fromId", "travel_time")
      ]
      
      # absolute difference: after - before
      
      abs_expression <- paste0(
        "`:=`(",
        paste(
          paste0(access_cols, "_abs"),
          "=",
          access_cols,
          "-",
          paste0(access_cols, "_antes"),
          collapse = ", "
        ),
        ")"
      )
      diff_dt[, eval(parse(text = abs_expression))]
      
      # relative difference: (after - before) / before
      # substitute NaN to 0s (they shouldn't matter much because there are very
      # few places with 0 accessibility and they all have 0 accessibility in
      # both scenarios)
      # substitute Inf to the max finite value
      
      rel_expression <- paste0(
        "`:=`(",
        paste(
          paste0(access_cols, "_rel"),
          "= (",
          access_cols,
          "-",
          paste0(access_cols, "_antes"),
          ") /",
          paste0(access_cols, "_antes"),
          collapse = ", "
        ),
        ")"
      )
      diff_dt[, eval(parse(text = rel_expression))]
      
      for (col in paste0(access_cols, "_rel")) {
        data.table::set(
          diff_dt,
          i = which(is.nan(diff_dt[[col]])),
          j = col,
          value = 0
        )
        
        max_finite <- max(diff_dt[[col]][is.finite(diff_dt[[col]])])
        data.table::set(
          diff_dt,
          i = which(is.infinite(diff_dt[[col]]) & diff_dt[[col]] > 0),
          j = col,
          value = max_finite
        )
      }
      
      # melt 'diff_dt' so we have the type of difference in a column
      # remove non-difference columns before melting
      
      cols_to_keep <- setdiff(
        names(diff_dt),
        c(access_cols, paste0(access_cols, "_antes"))
      )
      diff_dt <- diff_dt[, ..cols_to_keep]
      
      diff_dt <- melt(
        diff_dt,
        id.vars = c("travel_time", "fromId"),
        measure.vars = patterns(access_cols),
        variable.name = "type",
        value.name = access_cols
      )
      diff_dt[, type := fifelse(type == 1, "abs", "rel")]
      
      return(diff_dt)
      
    }
  )
  access_diff <- rbindlist(access_diff, idcol = "scenario")
  
  # save object and return path
  
  dir_path <- file.path(
    "../../data/avaliacao_intervencoes", city, "output_access"
  )
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  file_path <- ifelse(
    any(grepl("only_bfm", names(access_diff))),
    file.path(dir_path, paste0("full_access_diff.rds")),
    file.path(dir_path, paste0("transit_access_diff.rds"))
  )
  saveRDS(access_diff, file_path)
  
  return(file_path)
  
}