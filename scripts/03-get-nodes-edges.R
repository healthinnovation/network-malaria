library(dplyr)

raw_path = "data/raw/"
processed_path = "data/processed/"

# Read villages dataset ---------------------------------------------------

interim_path = fs::path("data/interim/")
villages_raw_file = "01-villages-climate.csv"
villages_raw_path = fs::path(interim_path, villages_raw_file)
villages_raw = readr::read_csv(
  villages_raw_path,
  col_select = c(village_id:watershed, forest_loss_2011:forest_loss_2018),
  col_types = "ccccdcd"
)

watershed_filter_in = villages_raw |>
  group_by(watershed) |>
  summarise(n_villages = n(), .groups = "drop") |>
  filter(n_villages > 2) |>
  pull(watershed)

villages = villages_raw |>
  mutate(forest_loss = 100 * rowMeans(across(starts_with("forest_loss_")))) |>
  select(-starts_with("forest_loss_")) |>
  filter(population > 0, forest_loss > 0, watershed %in% watershed_filter_in)

# Read origin-destination datasets ----------------------------------------

time_raw_filename = "travel-time.csv"
time_raw_path = fs::path(raw_path, time_raw_filename)
time_raw = readr::read_csv(
  time_raw_path, col_names = c("from", "to", "time"), col_types = "ccd", skip = 1
)

distance_raw_filename = "distance.csv"
distance_raw_path = fs::path(raw_path, distance_raw_filename)
distance_raw = readr::read_csv(
  distance_raw_path, col_names = c("from", "to", "distance"),
  col_types = "ccd", skip = 1
)

# Wrangle origin-destination datasets -------------------------------------

time = time_raw |>
  group_by(from = pmin(from, to), to = pmax(from, to)) |>
  summarise(time = mean(time), .groups = "drop") |>
  filter(from != to) |>
  filter(from %in% villages$village_id, to %in% villages$village_id)

distance = distance_raw |>
  group_by(from = pmin(from, to), to = pmax(from, to)) |>
  summarise(distance = mean(distance), .groups = "drop") |>
  filter(from != to) |>
  filter(from %in% villages$village_id, to %in% villages$village_id)

od_dataset = inner_join(time, distance, by = c("from", "to"))
  # filter(time > 0, distance > 0)

# Group by watersheds -----------------------------------------------------

networks = villages |>
  tidyr::nest(nodes = -watershed) |>
  mutate(village_ids = purrr::map(nodes, ~ pull(.x, village_id))) |>
  mutate(
    edges = purrr::map(
      village_ids, \(x) filter(od_dataset, from %in% x, to %in% x))
  ) |>
  mutate(
    order = purrr::map(nodes, nrow),
    size = purrr::map(edges, nrow)
  ) |>
  tidyr::unnest(c(order, size))

# Actual edges and nodes --------------------------------------------------

pop_forest_loss = select(villages, village_id, population, forest_loss)

edges_raw = networks |>
  select(watershed, edges) |>
  tidyr::unnest(edges) |>
  select(from, to, everything())

edges = edges_raw |>
  filter(time > 0, distance > 0) |>
  inner_join(pop_forest_loss, by = c("from" = "village_id")) |>
  inner_join(
    pop_forest_loss, by = c("to" = "village_id"),
    suffix = c("_from", "_to")
  ) |>
  mutate(
    dist_pop_gravity = (population_from * population_to) / distance,
    dist_forest_gravity = (forest_loss_from * forest_loss_to) / distance,
    time_pop_gravity = (population_from * population_to) / time,
    time_forest_gravity = (forest_loss_from * forest_loss_to) / time
  ) |>
  select(-c(starts_with("population"), starts_with("forest_loss_"))) |>
  relocate(time, .before = time_pop_gravity) |>
  group_by(watershed) |>
  mutate(
    across(where(is.numeric), \(x) scales::rescale(x, from = c(0, max(x))))
  ) |>
  ungroup()

villages_filter_in = unique(c(edges$from, edges$to))
nodes = filter(villages, village_id %in% villages_filter_in)

nodes_filename = "00-nodes.csv"
nodes_path = fs::path(processed_path, nodes_filename)
readr::write_csv(nodes, nodes_path)

edges_filename = "01-edges.csv"
edges_path = fs::path(processed_path, edges_filename)
readr::write_csv(edges, edges_path)

