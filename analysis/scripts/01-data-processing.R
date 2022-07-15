library(sf)
library(dplyr)
library(readr)

ccpp_raw <- read_sf("analysis/data/raw/ccpp-projected.gpkg") %>%
  # st_transform(32718)%>%
  # st_buffer(dist = 5000) %>%
  # st_simplify(preserveTopology = T, dTolerance = 100) %>%
  # st_transform(4326) %>%
  st_drop_geometry()

#TODO: 1 red sin peso, 4 con peso (por rio). Filtrar ccpp con poblacion 0.

ccpp <- ccpp_raw %>%
  select(id_loc, nrohab:province, RiverName:HydroNameANA)

od_raw <- read_delim("analysis/data/raw/od-eucli-distance.csv", delim = ";")

# Hay ccpp de origen que no estan en la tabla general
# 541868000, 548270000, 532340000
# Tambien hay cccpp de destino que no estan en la tabla general
# Corregin eucleadian_distance

od <- od_raw %>%
  inner_join(ccpp, by = c("origin_id" = "id_loc")) %>%
  inner_join(
    ccpp, by = c("destination_id" = "id_loc"),
    suffix = c("_origin", "_destination")
  ) %>%
  filter(nrohab_origin > 1, nrohab_destination > 1)

ccpp_unique <- unique(c(od$origin_id, od$destination_id))

ccpp_final <- ccpp %>%
  filter(id_loc %in% ccpp_unique)

od %>%
  select(1:3) %>%
  write_csv("analysis/data/interim/od-euclidean.csv")

write_csv(ccpp_final, "analysis/data/interim/ccpp.csv")
