input_path = "data/raw/"
output_path = "data/interim/"

# Villages master table ---------------------------------------------------

villages_raw_filename = "villages-loreto.gpkg"
villages_raw_filepath = fs::path(input_path, villages_raw_filename)
villages_raw = sf::st_read(villages_raw_filepath)

villages = villages_raw |>
  dplyr::select(
    village_id = id_loc, village, district, province, population = nrohab
  )

watersheds_raw_filename = "watersheds-loreto-ana.gpkg"
watersheds_raw_filepath = fs::path(input_path, watersheds_raw_filename)
watersheds_raw = sf::st_read(watersheds_raw_filepath)

watersheds = watersheds_raw |>
  dplyr::select(watershed = NOMBRE) |>
  dplyr::mutate(
    watershed = ifelse(
      watershed == "Intercuenca Bajo Huallga", "Intercuenca Bajo Huallaga",
      watershed
    )
  )

villages_master = sf::st_intersection(villages, watersheds)

villages_master_filename = "00-villages.csv"
villages_master_filepath = fs::path(output_path, villages_master_filename)
readr::write_csv(
  sf::st_drop_geometry(villages_master),
  file = villages_master_filepath,
  na = ""
)
