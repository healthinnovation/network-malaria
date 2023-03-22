interim_path = 'data/interim/'

# Read datasets -----------------------------------------------------------

climate_path = fs::path(interim_path, 'climate/')
climate_filepaths = fs::dir_ls(climate_path, glob = "*.csv")
climate = purrr::map(climate_filepaths, \(x) readr::read_csv(x, col_types = "cd"))

villages_path = fs::path(interim_path, '00-villages.csv')
villages = readr::read_csv(villages_path, col_types = "ccccdc")

# Join datasets -----------------------------------------------------------

climate_dataset = purrr::reduce(climate, dplyr::left_join, by = "village_id")
village_climate = dplyr::inner_join(villages, climate_dataset)

# Write dataset -----------------------------------------------------------

village_climate_filepath = fs::path(interim_path, "01-villages-climate.csv")
readr::write_csv(village_climate, village_climate_filepath, na = "")
