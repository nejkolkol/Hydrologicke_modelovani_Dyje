# R/04_orez_simulace.R

library(dplyr)
library(lubridate)

# Načtení dat
default_data <- readRDS("output/default_data.rds")
lai_data     <- readRDS("output/lai_data.rds")

# Pomocná tabulka s rozsahy podle scénářů
rozsahy <- tibble::tibble(
  scenar = c("2030_SSP126_MPI-ESM1-2-HR", "2050_SSP126_MPI-ESM1-2-HR", "2070_SSP126_MPI-ESM1-2-HR", "2085_SSP126_MPI-ESM1-2-HR",
             "2030_SSP245_MPI-ESM1-2-HR", "2050_SSP245_MPI-ESM1-2-HR", "2070_SSP245_MPI-ESM1-2-HR", "2085_SSP245_MPI-ESM1-2-HR",
             "2030_SSP370_MPI-ESM1-2-HR", "2050_SSP370_MPI-ESM1-2-HR", "2070_SSP370_MPI-ESM1-2-HR", "2085_SSP370_MPI-ESM1-2-HR",
             "2030_SSP585_MPI-ESM1-2-HR", "2050_SSP585_MPI-ESM1-2-HR", "2070_SSP585_MPI-ESM1-2-HR", "2085_SSP585_MPI-ESM1-2-HR"),
  start = as.Date(rep(c("2014-11-01", "2034-11-01", "2054-11-01", "2069-11-01"), times = 4)),
  end   = as.Date(rep(c("2034-10-31", "2054-10-31", "2074-10-31", "2089-10-31"), times = 4))
)

# Pomocná funkce pro ořez dat podle scénáře
orez_scenar <- function(data) {
  data %>%
    left_join(rozsahy, by = "scenar") %>%
    filter(date >= start & date <= end) %>%
    select(-start, -end)
}

# Ořez simulací
default_data_orez <- orez_scenar(default_data)
lai_data_orez     <- orez_scenar(lai_data)

# Uložení
saveRDS(default_data_orez, "output/default_data_orez.rds")
saveRDS(lai_data_orez,     "output/lai_data_orez.rds")

message("✅ Simulace byly oříznuty podle scénářů a uloženy jako *_orez.rds")
