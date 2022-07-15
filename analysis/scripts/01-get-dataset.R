library(innovar)
library(rgee)
library(sf)
library(tidyverse)

ee_Initialize(
  user = "antony.barja@upch.pe",
  drive = TRUE
  )
cp <- st_read(
  "analysis/data/raw/centro_poblado_movido.gpkg"
  ) %>%
  st_transform(32718)%>%
  st_buffer(dist = 5000) %>%
  st_simplify(preserveTopology = T,dTolerance = 100) %>%
  st_transform(4326) %>%
  sf_as_ee()

# Extracting climate variables:

# Precipiation -----------------
pp <- get_climate(
  from = "2009-01-01",
  to = "2018-12-31",
  by = "month",
  band = "pr",
  fun = "mean",
  region = cp)

# Runoff  ----------------------
ro <- get_climate(
  from = "2009-01-01",
  to = "2018-12-31",
  by = "month",
  band = "ro",
  fun = "mean",
  region = cp)

# Soil moisture ---------------
soil <- get_climate(
  from = "2009-01-01",
  to = "2018-12-31",
  by = "month",
  band = "soil",
  fun = "mean",
  region = cp)

# Tmmx -------------------------
tmax <- get_climate(
  from = "2009-01-01",
  to = "2018-12-31",
  by = "month",
  band = "tmmx",
  fun = "mean",
  region = cp)

# Tmmn -------------------------
tmin <- get_climate(
  from = "2009-01-01",
  to = "2018-12-31",
  by = "month",
  band = "tmmn",
  fun = "mean",
  region = cp)

# Global Human Modification ------
ghm <- get_ghm(region = cp,fun = "mean")

# Deforestation ------------------
def <- get_def(
  from = "2009-01-01",
  to = "2018-12-31",
  region = cp)

# Evapotranspiration -------------
etp <- get_etp(
  from = "2009-01-01",
  to = "2018-12-31",
  band = "ET",
  region = cp,
  fun = "mean"
  )


# Humidity -----------------------
humidity <- get_fldas(
  from = "2009-01-01",
  to = "2018-12-31",
  by = "month",
  band = "Qair_f_tavg",
  region = cp,
  fun = "mean")

# Population ----------------------
pop <- get_pop(
  from = "2009-01-01",
  to = "2018-12-31",
  region = cp,
  fun = "mean"
  )
