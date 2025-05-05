library(terra)
library(sf)
library(lubridate)

# Výstupní složka
vystup_dir <- "C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output"
if (!dir.exists(vystup_dir)) dir.create(vystup_dir, recursive = TRUE)

# Hranice povodí a profil
povodi_path <- "C:/Users/kolar/OneDrive/Desktop/data0078125/vrstvy/hranice_povodi.geojson"
povodi <- st_read(povodi_path)
profil_wgs <- st_sfc(st_point(c(16.8537248, 48.8037157)), crs = 4326)
profil_vect <- vect(profil_wgs)

# Scénáře a období (bez warm-up)
scenarios <- list(
  "2030_SSP126_MPI-ESM1-2-HR" = c("2014-11-01", "2044-10-31"),
  "2050_SSP126_MPI-ESM1-2-HR" = c("2034-11-01", "2064-10-31"),
  "2070_SSP126_MPI-ESM1-2-HR" = c("2054-11-01", "2084-10-31"),
  "2085_SSP126_MPI-ESM1-2-HR" = c("2069-11-01", "2099-10-31"),

  "2030_SSP245_MPI-ESM1-2-HR" = c("2014-11-01", "2044-10-31"),
  "2050_SSP245_MPI-ESM1-2-HR" = c("2034-11-01", "2064-10-31"),
  "2070_SSP245_MPI-ESM1-2-HR" = c("2054-11-01", "2084-10-31"),
  "2085_SSP245_MPI-ESM1-2-HR" = c("2069-11-01", "2099-10-31"),

  "2030_SSP370_MPI-ESM1-2-HR" = c("2014-11-01", "2044-10-31"),
  "2050_SSP370_MPI-ESM1-2-HR" = c("2034-11-01", "2064-10-31"),
  "2070_SSP370_MPI-ESM1-2-HR" = c("2054-11-01", "2084-10-31"),
  "2085_SSP370_MPI-ESM1-2-HR" = c("2069-11-01", "2099-10-31"),

  "2030_SSP585_MPI-ESM1-2-HR" = c("2014-11-01", "2044-10-31"),
  "2050_SSP585_MPI-ESM1-2-HR" = c("2034-11-01", "2064-10-31"),
  "2070_SSP585_MPI-ESM1-2-HR" = c("2054-11-01", "2084-10-31"),
  "2085_SSP585_MPI-ESM1-2-HR" = c("2069-11-01", "2099-10-31")
)

base_dir <- "C:/Users/kolar/OneDrive/Desktop/data0078125/data0078125_scenarios"

# Výstupní tabulka
vysledky <- data.frame()

for (sc in names(scenarios)) {
  cat("\n🔄 Zpracovávám:", sc, "\n")
  folder <- file.path(base_dir, sc)
  mhm_file <- file.path(folder, "mHM_Fluxes_States.nc")
  mrm_file <- file.path(folder, "mRM_Fluxes_States.nc")
  obd_start <- as.POSIXct(scenarios[[sc]][1])
  obd_end   <- as.POSIXct(scenarios[[sc]][2])

  preEffect <- rast(mhm_file, subds = "preEffect")
  interception <- rast(mhm_file, subds = "interception")
  aET <- rast(mhm_file, subds = "aET")

  time_mhm <- as.POSIXct(terra::time(preEffect), origin = "2065-11-01", tz = "UTC")
  sel <- time_mhm >= obd_start & time_mhm <= obd_end

  P <- preEffect[[sel]] + interception[[sel]]
  ET <- aET[[sel]]

  povodi_wgs <- st_transform(povodi, crs(P))
  P_crop <- mask(crop(P, vect(povodi_wgs)), vect(povodi_wgs))
  ET_crop <- mask(crop(ET, vect(povodi_wgs)), vect(povodi_wgs))

  P_yearly_mm <- global(app(P_crop, sum), fun = "mean", na.rm = TRUE)[[1]] / length(sel) * 12
  ET_yearly_mm <- global(app(ET_crop, sum), fun = "mean", na.rm = TRUE)[[1]] / length(sel) * 12

  Qrouted <- rast(mrm_file, subds = "Qrouted")
  time_mrm <- as.POSIXct(terra::time(Qrouted), origin = "2065-11-01", tz = "UTC")
  sel_q <- time_mrm >= obd_start & time_mrm <= obd_end
  Qrouted_sel <- Qrouted[[sel_q]]
  flow_ts <- as.numeric(extract(Qrouted_sel, profil_vect)[1, -1])
  Qrouted_avg_m3s <- mean(flow_ts, na.rm = TRUE)
  Qrouted_yearly_m3 <- Qrouted_avg_m3s * 60 * 60 * 24 * 365.25

  vysledky <- rbind(vysledky, data.frame(
    scenar = sc,
    obdobi_od = scenarios[[sc]][1],
    obdobi_do = scenarios[[sc]][2],
    srazky_mm_rok = P_yearly_mm,
    evapotranspirace_mm_rok = ET_yearly_mm,
    qrouted_m3_rok = Qrouted_yearly_m3
  ))
}

# Uložení výsledků do jednoho CSV
write.csv(vysledky, file = file.path(vystup_dir, "bilance_scenare.csv"), row.names = FALSE)
cat("\n✅ Výsledky uloženy do bilance_scenare.csv\n")
