library(dplyr)
library(readr)
library(fs)
library(purrr)

processed_path <- path("analysis/data/processed/")
nodes_full_file <- "nodes-full.csv"
nodes_full_path <- path(processed_path, nodes_full_file)
nodes_full_raw <- read_csv(nodes_full_path)

nodes_full <- nodes_full_raw %>%
  mutate(adef = rowMeans(across(starts_with("adef"))))

nodes_weights <- nodes_full %>%
  select(ccpp_id, population, adef)

raw_path <- path("analysis/data/raw/")
od_filename <- "od-eucli-distance.csv"
od_path <- path(raw_path, od_filename)
od <- read_csv(
  od_path, col_names = c("origin_id", "destination_id", "distance"),
  col_types = "ccd", skip = 1
)

# Edges -------------------------------------------------------------------

# Hay ccpp de origen y de destino que no estan en la tabla general
# 541868000, 548270000, 532340000

nodes_population <- select(nodes_weights, ccpp_id, population)

edges_raw <- od %>%
  filter(origin_id != destination_id) %>%
  inner_join(nodes_population, by = c("origin_id" = "ccpp_id")) %>%
  inner_join(
    nodes_population, by = c("destination_id" = "ccpp_id"),
    suffix = c("_origin", "_destination")
  ) %>%
  filter(population_origin > 1, population_destination > 1) %>%
  select(origin_id, destination_id, distance) %>%
  group_by(
    origin = pmin(origin_id, destination_id),
    destination = pmax(origin_id, destination_id)
  ) %>%
  summarise(distance = mean(distance), .groups = "drop") %>%
  filter(distance > 0)

# TODO: Verificar distancia
# TODO: Probar distinct

edges_nodes <- edges %>%
  inner_join(nodes_weights, by = c("origin" = "ccpp_id")) %>%
  inner_join(
    nodes_weights, by = c("destination" = "ccpp_id"),
    suffix = c("_origin", "_destination")
  )

edges <- edges_nodes %>%
  mutate(
    inv_distance = 1 / distance,
    gravity_pop = (population_origin * population_destination) / distance,
    mod_gravity_pop_ori = distance * (population_origin / population_destination),
    mod_gravity_pop_dest = distance * (population_destination / population_origin),
    max_population = pmax(population_origin, population_destination),
    min_population = pmin(population_origin, population_destination),
    mod_gravity_pop = distance * (max_population / min_population),
    gravity_adef = (adef_origin * adef_destination) / distance,
    mod_gravity_adef_ori = distance * (adef_origin / adef_destination),
    mod_gravity_adef_dest = distance * (adef_destination / adef_origin),
    max_adef = pmax(adef_origin, adef_destination),
    min_adef = pmin(adef_origin, adef_destination),
    mod_gravity_adef = distance * (max_adef / min_adef)
  ) %>%
  select(
    -c(
      starts_with("min"), starts_with("max"), starts_with("population"),
      starts_with("adef")
    )
  )

# Nodes -------------------------------------------------------------------

ccpp_id_edges <- unique(c(edges$origin, edges$destination))

nodes <- nodes_full %>%
  select(province:hydro_name_l7, adef) %>%
  filter(ccpp_id %in% ccpp_id_edges)

edges_filename <- "edges.csv"
edges_path <- path(processed_path, edges_filename)
write_csv(edges, edges_path)

nodes_filename <- "nodes.csv"
nodes_path <- path(processed_path, nodes_filename)
write_csv(nodes, nodes_path)

