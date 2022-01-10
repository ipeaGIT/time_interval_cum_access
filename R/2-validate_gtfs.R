


# file <- "../../../data/avaliacao_intervencoes/fortaleza/gtfs_for_metrofor_2021-01_new.zip"
# dir_output <- "../../../data/avaliacao_intervencoes/fortaleza/validator_gtfs_for_metrofor_2021-01_new.html"





validator_gtfs <- function(file, dir_output) {
  
  dir_gtfs <- file
  
  command <- sprintf("cd ../../data-raw/gtfs/feedvalidator && feedvalidator -o %s %s", dir_output, dir_gtfs)
  
  shell(command, wait = TRUE)
  
}



# 1 - Fortaleza METROFOR --------------------------------------------------


validator_gtfs("../../../data/avaliacao_intervencoes/fortaleza/gtfs_for_metrofor_2021-01_new.zip",
               "../../../data/avaliacao_intervencoes/fortaleza/validator_gtfs_for_metrofor_2021-01_new.html")

