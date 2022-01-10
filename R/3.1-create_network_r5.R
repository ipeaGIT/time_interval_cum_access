options(java.parameters = '-Xmx10G')
library(r5r)



# # copiar o novo gtfs pra pasta do r5 ------------------------------
# file.copy(from = "../../data/avaliacao_intervencoes/for/gtfs_for_metrofor_2018-11_mod_depois.zip",
#           to = "../../data/avaliacao_intervencoes/r5/graph/for_depois/", overwrite = TRUE)
file.copy(from = "../../data/avaliacao_intervencoes/goi/gtfs_goi_rmtc_2019-10_depois.zip",
          to = "../../data/avaliacao_intervencoes/r5/graph/goi_depois/", overwrite = TRUE)


create_network <- function(sigla_muni) {
  
  
  # antes
  path_antes <- sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_antes", sigla_muni)
  r5r::setup_r5(data_path = path_antes, use_elevation = TRUE, overwrite = TRUE)
  
  # depois
  path_depois <- sprintf("../../data/avaliacao_intervencoes/r5/graph/%s_depois", sigla_muni)
  r5r::setup_r5(data_path = path_depois, use_elevation = TRUE, overwrite = TRUE)
  
}



create_network("for")
create_network("goi")


