library(dplyr)
library(data.table)
library(ggmap)
library(readr)

# open cnefe
cnefe <- fread("L:/Proj_acess_oport/data/geocode/streetmap_eval/cnefe_sample_streetmap.csv")


# 3.2) Listar esses enderecos
enderecos <- cnefe %>% mutate(fim = paste0(address, " - ", neighbourhood, " - ",  city, ", ", state, " - CEP ", post_code)) %>% pull(fim)

# 3.3) Registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = F)
register_google(key = my_api$V1[3])

# 3.4) Rodar o geocode do gmaps

coordenadas_google1_1 <- lapply(X=enderecos[1:5000], ggmap::geocode, output = "all")
write_rds(coordenadas_google1_1, "L:/Proj_acess_oport/data/geocode/streetmap_eval/cnefe_gmaps/output1_cnefe_gmaps_raw.rds")
coordenadas_google1_2 <- lapply(X=enderecos[5001:10000], ggmap::geocode, output = "all")
write_rds(coordenadas_google1_2, "L:/Proj_acess_oport/data/geocode/streetmap_eval/cnefe_gmaps/output2_cnefe_gmaps_raw.rds")
coordenadas_google1_3 <- lapply(X=enderecos[10001:length(enderecos)], ggmap::geocode, output = "all")
write_rds(coordenadas_google1_3, "L:/Proj_acess_oport/data/geocode/streetmap_eval/cnefe_gmaps/output3_cnefe_gmaps_raw.rds")

# join them all together
coordenadas_google1 <- c(coordenadas_google1_1, coordenadas_google1_2, coordenadas_google1_3)

# identify list names as id_estab
names(coordenadas_google1) <- cnefe$cnefe_id



# function to create data.frame from gmaps output
create_dt <- function(x) {
  
  precision_depth0 <- ifelse(length(x[["results"]][[1]][["address_components"]]) > 0, 
                             x[["results"]][[1]][["address_components"]], 
                             NA)
  
  # check length from precision depth
  precision_depth <- ifelse(is.na(precision_depth0), NA,
                            ifelse(length(precision_depth0[[1]]$types) > 0,
                                   precision_depth0[[1]]$types[[1]], 
                                   NA))
  
  a <- data.table(
    MatchedAddress = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
    # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
    PrecisionDepth = precision_depth,
    lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
    lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
  )
  
}

# 3.5) Rodar funcao que transforma todos os estabs georef em data.table
cnefe_geocoded <- lapply(coordenadas_google1, create_dt)

# 3.6) Rbind as data.table
cnefe_geocoded_dt <- rbindlist(cnefe_geocoded, idcol = "cnefe_id",
                                         use.names = TRUE)

# save
fwrite(cnefe_geocoded_dt, "../../data/geocode/streetmap_eval/cnefe_gmaps/cnefe_geocoded_gmaps.csv")

