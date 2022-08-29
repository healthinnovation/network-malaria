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

nodes_raw <- nodes_full %>%
  select(ccpp_id, population, adef) %>%
  filter(population > 0, adef > 0)

raw_path <- path("analysis/data/raw/")
od_travel_time_filename <- "od-travel-time-original.csv"
od_travel_time_path <- path(raw_path, od_travel_time_filename)

od_distance_filename <- "od-distance-original.csv"
od_distance_path <- path(raw_path, od_distance_filename)

od_travel_time_raw <- read_csv(
  od_travel_time_path, col_names = c("fromid", "toid", "time"),
  col_types = "ccd", skip = 1
)

od_distance_raw <- read_csv(
  od_distance_path, col_names = c("fromid", "toid", "distance"),
  col_types = "ccd", skip = 1
)

# Edges -------------------------------------------------------------------

od_travel_time <- od_travel_time_raw %>%
  filter(fromid != toid, time > 0) %>%
  group_by(
    from = pmin(fromid, toid),
    to = pmax(fromid, toid)
  ) %>%
  summarise(time = mean(time), .groups = "drop")

od_distance <- od_distance_raw %>%
  filter(fromid != toid, distance > 0) %>%
  group_by(
    from = pmin(fromid, toid),
    to = pmax(fromid, toid)
  ) %>%
  summarise(distance = mean(distance), .groups = "drop")

edges_raw <- od_distance %>%
  inner_join(od_travel_time, by = c("from", "to"))

edges <- edges_raw %>%
  inner_join(nodes_raw, by = c("from" = "ccpp_id")) %>%
  inner_join(
    nodes_raw, by = c("to" = "ccpp_id"),
    suffix = c("_from", "_to")
  ) %>%
  mutate(
    d_inv = 1 / distance,
    d_grav_pop = (population_from * population_to) / distance,
    d_mgrav_from_pop = distance * (population_from / population_to),
    d_mgrav_to_pop = distance * (population_to / population_from),
    max_population = pmax(population_from, population_to),
    min_population = pmin(population_from, population_to),
    d_mgrav_pop = distance * (max_population / min_population),
    d_grav_adef = (adef_from * adef_to) / distance,
    d_mgravity_from_adef = distance * (adef_from / adef_to),
    d_mgravity_to_adef = distance * (adef_to / adef_from),
    max_adef = pmax(adef_from, adef_to),
    min_adef = pmin(adef_from, adef_to),
    d_mgrav_adef = distance * (max_adef / min_adef),
    t_inv = 1 / time,
    t_grav_pop = (population_from * population_to) / time,
    t_mgrav_from_pop = time * (population_from / population_to),
    t_mgrav_to_pop = time * (population_to / population_from),
    t_mgrav_pop = time * (max_population / min_population),
    t_grav_adef = (adef_from * adef_to) / time,
    t_mgravity_from_adef = time * (adef_from / adef_to),
    t_mgravity_to_adef = time * (adef_to / adef_from),
    t_mgrav_adef = time * (max_adef / min_adef)
  ) %>%
  select(
    -c(
      starts_with("min"), starts_with("max"), starts_with("population"),
      starts_with("adef")
    )
  )

# Nodes -------------------------------------------------------------------

ccpp_id_edges <- unique(c(edges$from, edges$to))

nodes <- nodes_full %>%
  select(province:hydro_name_l7, adef) %>%
  filter(ccpp_id %in% ccpp_id_edges)

edges_filename <- "edges.csv"
edges_path <- path(processed_path, edges_filename)
write_csv(edges, edges_path)

nodes_filename <- "nodes.csv"
nodes_path <- path(processed_path, nodes_filename)
write_csv(nodes, nodes_path)

