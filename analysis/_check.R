library(dplyr)
library(readr)
library(fs)
library(purrr)
library(sf)
library(tidyr)
library(tidygraph)
library(scales)

processed_path <- path("analysis/data/processed/")
nodes_full_file <- "nodes-full.csv"
nodes_full_path <- path(processed_path, nodes_full_file)
nodes_full_raw <- read_csv(nodes_full_path)

nodes_full <- nodes_full_raw %>%
  mutate(adef = rowMeans(across(starts_with("adef"))))

raw_path <- path("analysis/data/raw/")
od_travel_time_filename <- "od-travel-time-original.csv"
od_travel_time_path <- path(raw_path, od_travel_time_filename)

od_distance_filename <- "od-distance-original.csv"
od_distance_path <- path(raw_path, od_distance_filename)

od_travel_time_full_raw <- read_csv(
  od_travel_time_path, col_names = c("fromid", "toid", "time"),
  col_types = "ccd", skip = 1
)

od_distance_full_raw <- read_csv(
  od_distance_path, col_names = c("fromid", "toid", "distance"),
  col_types = "ccd", skip = 1
)


# Filter population and forest loss ---------------------------------------

nodes_calculations <- nodes_full %>%
  select(hydro_name_ana, village_id, population, adef) %>%
  filter(
    population > 0, adef > 0,
    !(hydro_name_ana %in% c("Cuenca Cushabatay", "Intercuenca 49875"))
  )

od_full_raw <- od_distance_full_raw %>%
  inner_join(od_travel_time_full_raw, by = c("fromid", "toid")) %>%
  filter(fromid != toid) %>%
  group_by(
    from = pmin(fromid, toid),
    to = pmax(fromid, toid)
  ) %>%
  summarise(distance = mean(distance), time = mean(time), .groups = "drop") %>%
  inner_join(nodes_calculations, by = c("from" = "village_id")) %>%
  inner_join(
    nodes_calculations, by = c("to" = "village_id"),
    suffix = c("_from", "_to")
  ) %>%
  filter(hydro_name_ana_from == hydro_name_ana_to) %>%
  mutate(hydro_name_ana = hydro_name_ana_from) %>%
  select(-c(hydro_name_ana_from, hydro_name_ana_to))


edges_raw <- od_full_raw %>%
  filter(time > 0)

# Nodes -------------------------------------------------------------------

edges_village_ids <- unique(c(edges_clean$from, edges_clean$to))

villages_coord_raw <- st_read("analysis/data/raw/villages-original.gpkg")

villages_coord <- villages_coord_raw %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(village_id = id_loc, lon, lat)

nodes_clean <- nodes_full %>%
  select(province:hydro_name_l7, adef) %>%
  filter(village_id %in% edges_village_ids) %>%
  left_join(villages_coord, by = "village_id")

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
  inner_join(nodes_raw, by = c("from" = "village_id")) %>%
  inner_join(
    nodes_raw, by = c("to" = "village_id"),
    suffix = c("_from", "_to")
  ) %>%
  mutate(
    d_inv = 1 / distance,
    d_pop_grav = (population_from * population_to) / distance,
    d_pop_mgrav_from = distance * (population_from / population_to),
    d_pop_mgrav_to = distance * (population_to / population_from),
    max_population = pmax(population_from, population_to),
    min_population = pmin(population_from, population_to),
    d_pop_mgrav = distance * (max_population / min_population),
    d_adef_grav = (adef_from * adef_to) / distance,
    d_adef_mgrav_from = distance * (adef_from / adef_to),
    d_adef_mgrav_to = distance * (adef_to / adef_from),
    max_adef = pmax(adef_from, adef_to),
    min_adef = pmin(adef_from, adef_to),
    d_adef_mgrav = distance * (max_adef / min_adef),
    t_inv = 1 / time,
    t_pop_grav = (population_from * population_to) / time,
    t_pop_mgrav_from = time * (population_from / population_to),
    t_pop_mgrav_to = time * (population_to / population_from),
    t_pop_mgrav = time * (max_population / min_population),
    t_adef_grav = (adef_from * adef_to) / time,
    t_adef_mgrav_from = time * (adef_from / adef_to),
    t_adef_mgrav_to = time * (adef_to / adef_from),
    t_adef_mgrav = time * (max_adef / min_adef)
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

