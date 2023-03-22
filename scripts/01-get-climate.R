library(sf)
library(rgee)

input_path = "data/raw/"
output_path = "data/interim/climate/"

villages_filename = "villages-loreto.gpkg"
villages_filepath = fs::path(input_path, villages_filename)
villages = st_read(villages_filepath)

# Extract climate variables -----------------------------------------------

ee_Initialize(user = "antony.barja@upch.pe", drive = TRUE)

villages_ee = villages |>
  dplyr::select(village_id = id_loc) |>
  st_transform(32718) |>
  st_buffer(dist = 5000) |>
  st_simplify(preserveTopology = TRUE, dTolerance = 100) |>
  st_transform(4326) |>
  sf_as_ee()

## Precipitation

precipitation_raw = innovar::get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "pr",
  fun = "mean", region = villages_ee
)

precipitation = precipitation_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("pr", "precipitation_", .x, fixed = TRUE))

precipitation_filename = "00-precipitation.csv"
precipitation_filepath = fs::path(output_path, precipitation_filename)
readr::write_csv(precipitation, precipitation_filepath, na = "")

## Runoff

runoff_raw = innovar::get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "ro",
  fun = "mean", region = villages_ee
)

runoff = runoff_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("ro", "runoff_", .x, fixed = TRUE))

runoff_filename = "01-runoff.csv"
runoff_filepath = fs::path(output_path, runoff_filename)
readr::write_csv(runoff, runoff_filepath, na = "")

## Soil moisture

soil_moisture_raw = innovar::get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "soil",
  fun = "mean", region = villages_ee
)

soil_moisture = soil_moisture_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("soil", "soil_moisture_", .x, fixed = TRUE))

soil_moisture_filename = "02-soil-moisture.csv"
soil_moisture_filepath = fs::path(output_path, soil_moisture_filename)
readr::write_csv(soil_moisture, soil_moisture_filepath, na = "")

## Max temperature

max_temperature_raw = innovar::get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "tmmx",
  fun = "mean", region = villages_ee
)

max_temperature = max_temperature_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("tmmx", "max_temperature_", .x, fixed = TRUE))

max_temperature_filename = "03-max-temperature.csv"
max_temperature_filepath = fs::path(output_path, max_temperature_filename)
readr::write_csv(max_temperature, max_temperature_filepath, na = "")

## Min temperature

min_temperature_raw = innovar::get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "tmmx",
  fun = "mean", region = villages_ee
)

min_temperature = min_temperature_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("tmmx", "min_temperature_", .x, fixed = TRUE))

min_temperature_filename = "04-min-temperature.csv"
min_temperature_filepath = fs::path(output_path, min_temperature_filename)
readr::write_csv(min_temperature, min_temperature_filepath, na = "")

## Global Human Modification (GHM) Index

ghm_raw = innovar::get_ghm(region = villages_ee, fun = "mean")

ghm = rename(ghm_raw, ghm = gHM)

ghm_filename = "05-ghm.csv"
ghm_filepath = fs::path(output_path, ghm_filename)
readr::write_csv(ghm, ghm_filepath, na = "")

## Forest loss

forest_loss_raw = innovar::get_def(
  from = "2009-01-01", to = "2018-12-31", region = villages_ee
)

forest_loss = forest_loss_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("adef", "forest_loss", .x, fixed = TRUE))

forest_loss_filename = "06-forest-loss.csv"
forest_loss_filepath = fs::path(output_path, forest_loss_filename)
readr::write_csv(forest_loss, forest_loss_filepath, na = "")

## Evapotranspiration

evapotranspiration_raw = innovar::get_etp(
  from = "2009-01-01", to = "2018-12-31", band = "ET", fun = "mean",
  region = villages_ee
)

evapotranspiration = evapotranspiration_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("et", "evapotranspiration_", .x, fixed = TRUE))

evapotranspiration_filename = "07-evapotranspiration.csv"
evapotranspiration_filepath = fs::path(output_path, evapotranspiration_filename)
readr::write_csv(evapotranspiration, evapotranspiration_filepath, na = "")

## Humidity

humidity_raw = innovar::get_fldas(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "Qair_f_tavg",
  region = villages_ee, fun = "mean"
)

humidity = humidity_raw |>
  janitor::clean_names() |>
  dplyr::rename_with(~ gsub("qair_f_tavg", "humidity_", .x, fixed = TRUE))

humidity_filename = "08-humidity.csv"
humidity_filepath = fs::path(output_path, humidity_filename)
readr::write_csv(humidity, humidity_filepath, na = "")

## Population

population_raw = innovar::get_pop(
  from = "2009-01-01", to = "2018-12-31", region = villages_ee, fun = "mean"
)

population = dplyr::rename_with(
  population_raw, ~ gsub("pop", "population_", .x, fixed = TRUE)
)

population_filename = "09-population.csv"
population_filepath = fs::path(output_path, population_filename)
readr::write_csv(population, population_filepath, na = "")
