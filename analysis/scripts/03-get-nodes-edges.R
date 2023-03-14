library(dplyr)
library(readr)
library(fs)
library(purrr)
library(sf)
library(tidyr)
library(tidygraph)
library(scales)

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

edges_clean <- od_distance %>%
  inner_join(od_travel_time, by = c("from", "to"))

edges_village_ids <- unique(c(edges_clean$from, edges_clean$to))

processed_path <- path("analysis/data/processed/")
nodes_full_file <- "nodes-full.csv"
nodes_full_path <- path(processed_path, nodes_full_file)
nodes_full_raw <- read_csv(nodes_full_path)

villages_coord_raw <- st_read("analysis/data/raw/villages-original.gpkg")
villages_coord <- villages_coord_raw %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(village_id = id_loc, lon, lat)

nodes_clean <- nodes_full_raw %>%
  mutate(adef = rowMeans(across(starts_with("adef")))) %>%
  select(province:hydro_name_l7, adef) %>%
  filter(village_id %in% edges_village_ids, population > 0, adef > 0) %>%
  left_join(villages_coord, by = "village_id")

nodes_pop_adef <- select(nodes_clean, village_id, population, adef)

networks_raw <- nodes_clean %>%
  nest(nodes = -hydro_name_ana) %>%
  mutate(ids = map(nodes, ~ pull(.x, 4))) %>%
  mutate(edges = map(ids, ~ filter(edges_clean, from %in% .x, to %in% .x))) %>%
  mutate(size = map(edges, ~ dim(.x)[1])) %>%
  unnest(size)

# Actual edges and nodes --------------------------------------------------

networks <- filter(networks_raw, size > 2)

edges <- networks %>%
  select(hydro_name_ana, edges) %>%
  unnest(edges) %>%
  select(from, to, everything()) %>%
  inner_join(nodes_pop_adef, by = c("from" = "village_id")) %>%
  inner_join(
    nodes_pop_adef, by = c("to" = "village_id"),
    suffix = c("_from", "_to")
  ) %>%
  mutate(
    d_pop_grav = (population_from * population_to) / distance,
    d_adef_grav = (adef_from * adef_to) / distance,
    t_pop_grav = (population_from * population_to) / time,
    t_adef_grav = (adef_from * adef_to) / time
  ) %>%
  select(
    -c(
      starts_with("min"), starts_with("max"), starts_with("population"),
      starts_with("adef")
    )
  ) %>%
  group_by(hydro_name_ana) %>%
  mutate(across(where(is.numeric), ~ rescale(.x, to = c(0.1, 1)))) %>%
  ungroup()

nodes_id <- unique(c(edges$from, edges$to))
nodes <- filter(nodes_clean, village_id %in% nodes_id)

edges_filename <- "edges.csv"
edges_path <- path(processed_path, edges_filename)
write_csv(edges, edges_path)

nodes_filename <- "nodes.csv"
nodes_path <- path(processed_path, nodes_filename)
write_csv(nodes, nodes_path)

