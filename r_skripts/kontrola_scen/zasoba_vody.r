library(terra)
library(sf)
library(lubridate)

# Cesty k datům
base_dir <- "C:/Users/kolar/OneDrive/Desktop/data0078125/data0078125_scenarios"
scenarios <- list(
  "2030" = list(
    path = file.path(base_dir, "2030_SSP585_MPI-ESM1-2-HR"),
    start = as.POSIXct("2014-11-01"),
    end = as.POSIXct("2044-10-31")
  ),
  "2085" = list(
    path = file.path(base_dir, "2085_SSP585_MPI-ESM1-2-HR"),
    start = as.POSIXct("2069-11-01"),
    end = as.POSIXct("2099-10-31")
  )
)

# Povodí
povodi_path <- "C:/Users/kolar/OneDrive/Desktop/data0078125/vrstvy/hranice_povodi.geojson"
povodi <- st_read(povodi_path)

# Funkce pro výpočet průměrných hodnot
analyzuj_obdobi <- function(path, start_date, end_date) {
  mhm_file <- file.path(path, "mHM_Fluxes_States.nc")
  mrm_file <- file.path(path, "mRM_Fluxes_States.nc")

  preEffect <- rast(mhm_file, subds = "preEffect")
  interception <- rast(mhm_file, subds = "interception")
  aET <- rast(mhm_file, subds = "aET")

  # Zásoby
  satSTW <- rast(mhm_file, subds = "satSTW")
  unsatSTW <- rast(mhm_file, subds = "unsatSTW")
  swc_layers <- lapply(paste0("SWC_L0", 1:6), function(v) rast(mhm_file, subds = v))
  SWC_total <- Reduce(`+`, swc_layers)

  povodi_wgs <- st_transform(povodi, crs(preEffect))

  time_mhm <- as.POSIXct(terra::time(preEffect), origin = "2065-11-01", tz = "UTC")
  sel <- time_mhm >= start_date & time_mhm <= end_date

  P_crop <- mask(crop(preEffect[[sel]] + interception[[sel]], vect(povodi_wgs)), vect(povodi_wgs))
  ET_crop <- mask(crop(aET[[sel]], vect(povodi_wgs)), vect(povodi_wgs))
  sat_crop <- mask(crop(satSTW[[sel]], vect(povodi_wgs)), vect(povodi_wgs))
  unsat_crop <- mask(crop(unsatSTW[[sel]], vect(povodi_wgs)), vect(povodi_wgs))
  swc_crop <- mask(crop(SWC_total[[sel]], vect(povodi_wgs)), vect(povodi_wgs))

  list(
    srazky_mm_rok = global(app(P_crop, sum), mean, na.rm = TRUE)[[1]] / length(sel) * 12,
    evap_mm_rok = global(app(ET_crop, sum), mean, na.rm = TRUE)[[1]] / length(sel) * 12,
    sat_prumer = global(app(sat_crop, mean), mean, na.rm = TRUE)[[1]],
    unsat_prumer = global(app(unsat_crop, mean), mean, na.rm = TRUE)[[1]],
    swc_prumer = global(app(swc_crop, mean), mean, na.rm = TRUE)[[1]]
  )
}

# Výpočty
vysledky <- lapply(names(scenarios), function(klic) {
  sc <- scenarios[[klic]]
  cat("\n🔍 Analyzuji:", klic, "\n")
  vys <- analyzuj_obdobi(sc$path, sc$start, sc$end)
  data.frame(obdobi = klic, vys)
})

vysledky_df <- do.call(rbind, vysledky)

# Výstup
print(vysledky_df)

# Rozdíly mezi obdobími
delta <- vysledky_df[2, -1] - vysledky_df[1, -1]
delta$obdobi <- "Změna 2085 - 2030"
print(delta)

# Uložení do CSV
output_dir <- "C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

write.csv(rbind(vysledky_df, delta),
          file = file.path(output_dir, "bilance_zmeny_ssp585.csv"),
          row.names = FALSE)
