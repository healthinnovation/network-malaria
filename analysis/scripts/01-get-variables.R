library(innovar)
library(rgee)
library(sf)
library(dplyr)
library(janitor)
library(purrr)
library(fs)
library(readr)

output_path <- "analysis/data/interim/"

ee_Initialize(user = "diego.villa@upch.pe")

ccpp_sf <- st_read("analysis/data/raw/ccpp-projected.gpkg") %>%
  select(
    province, district, village, ccpp_id = id_loc, population = nrohab,
    river_name = RiverName, hydro_name_ana = HydroNameANA,
    hydro_name_l6 = HydroNameL6, hydro_name_l7 = HydroNameL7
  )

ccpp <- ccpp_sf %>%
  st_drop_geometry()

ccpp_filename <- "ccpp.csv"
ccpp_output_path <- path(output_path, ccpp_filename)
write_csv(ccpp, ccpp_output_path, na = "")

ccpp_ee <- ccpp_sf %>%
  select(ccpp_id) %>%
  st_transform(32718)%>%
  st_buffer(dist = 5000) %>%
  st_simplify(preserveTopology = T, dTolerance = 100) %>%
  st_transform(4326) %>%
  sf_as_ee()

# Extracting climate variables

# Precipitation -----------------------------------------------------------
pr_raw <- get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "pr", fun = "mean",
  region = ccpp_ee
)

pr <- pr_raw %>%
  clean_names() %>%
  rename_with(~ gsub("pr", "pr_", .x, fixed = TRUE))

pr_filename <- "pr.csv"
pr_output_path <- path(output_path, pr_filename)
write_csv(pr, pr_output_path, na = "")

# Runoff ------------------------------------------------------------------
ro_raw <- get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "ro", fun = "mean",
  region = ccpp_ee
)

ro <- ro_raw %>%
  clean_names() %>%
  rename_with(~ gsub("ro", "ro_", .x, fixed = TRUE))

ro_filename <- "ro.csv"
ro_output_path <- path(output_path, ro_filename)
write_csv(ro, ro_output_path, na = "")

# Soil moisture ---------------
soil_raw <- get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "soil", fun = "mean",
  region = ccpp_ee
)

soil <- soil_raw %>%
  clean_names() %>%
  rename_with(~ gsub("soil", "soil_", .x, fixed = TRUE))

soil_filename <- "soil.csv"
soil_output_path <- path(output_path, soil_filename)
write_csv(soil, soil_output_path, na = "")

# Tmmx -------------------------
tmmx_raw <- get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "tmmx", fun = "mean",
  region = ccpp_ee
)

tmmx <- tmmx_raw %>%
  clean_names() %>%
  rename_with(~ gsub("tmmx", "tmmx_", .x, fixed = TRUE))

tmmx_filename <- "tmmx.csv"
tmmx_output_path <- path(output_path, tmmx_filename)
write_csv(tmmx, tmmx_output_path, na = "")

# Tmmn -------------------------
tmmn_raw <- get_climate(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "tmmn", fun = "mean",
  region = ccpp_ee
)

tmmn <- tmmn_raw %>%
  clean_names() %>%
  rename_with(~ gsub("tmmn", "tmmn_", .x, fixed = TRUE))

tmmn_filename <- "tmmn.csv"
tmmn_output_path <- path(output_path, tmmn_filename)
write_csv(tmmn, tmmn_output_path, na = "")

# Global Human Modification ------
ghm_raw <- get_ghm(region = ccpp_ee, fun = "mean")

ghm <- rename(ghm_raw, ghm = gHM)

ghm_filename <- "ghm.csv"
ghm_output_path <- path(output_path, ghm_filename)
write_csv(ghm, ghm_output_path, na = "")

# Deforestation ------------------
adef_raw <- get_def(from = "2009-01-01", to = "2018-12-31", region = ccpp_ee)

adef <- clean_names(adef_raw)

adef_filename <- "adef.csv"
adef_output_path <- path(output_path, adef_filename)
write_csv(adef, adef_output_path, na = "")

# Evapotranspiration -------------
et_raw <- get_etp(
  from = "2009-01-01", to = "2018-12-31", band = "ET", fun = "mean",
  region = ccpp_ee
)

et <- et_raw %>%
  clean_names() %>%
  rename_with(~ gsub("et", "et_", .x, fixed = TRUE))

et_filename <- "et.csv"
et_output_path <- path(output_path, et_filename)
write_csv(et, et_output_path, na = "")

# Humidity -----------------------
sh_raw <- get_fldas(
  from = "2009-01-01", to = "2018-12-31", by = "month", band = "Qair_f_tavg",
  region = ccpp_ee, fun = "mean"
)

sh <- sh_raw %>%
  clean_names() %>%
  rename_with(~ gsub("qair_f_tavg", "sh_", .x, fixed = TRUE))

sh_filename <- "sh.csv"
sh_output_path <- path(output_path, sh_filename)
write_csv(sh, sh_output_path, na = "")

# Population ----------------------
pop_raw <- get_pop(
  from = "2009-01-01", to = "2018-12-31", region = ccpp_ee, fun = "mean"
)

pop <- rename_with(pop_raw, ~ gsub("pop", "pop_", .x, fixed = TRUE))

pop_filename <- "pop.csv"
pop_output_path <- path(output_path, pop_filename)
write_csv(pop, pop_output_path, na = "")
