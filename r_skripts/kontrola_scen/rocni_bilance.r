library(terra)
library(sf)
library(lubridate)

# Cesty k datům
folder <- "C:/Users/kolar/OneDrive/Desktop/data0078125/data0078125_scenarios/2030_SSP126_MPI-ESM1-2-HR"
povodi_path <- "C:/Users/kolar/OneDrive/Desktop/data0078125/vrstvy/hranice_povodi.geojson"

mhm_file <- file.path(folder, "mHM_Fluxes_States.nc")
mrm_file <- file.path(folder, "mRM_Fluxes_States.nc")

# Načti hranici povodí
povodi <- st_read(povodi_path)
# Převeď hranici povodí do CRS rastrových dat (WGS84)
povodi_wgs <- st_transform(povodi, crs(P))  # automaticky převezme CRS rastru


# Souřadnice závěrového profilu (WGS84)
profil_wgs <- st_sfc(st_point(c(16.8537248, 48.8037157)), crs = 4326)


# ==========================
# 1. mHM: načti preEffect, interception, aET
# ==========================
# Srážky = preEffect + interception
preEffect <- rast(mhm_file, subds = "preEffect")
interception <- rast(mhm_file, subds = "interception")
aET <- rast(mhm_file, subds = "aET")

# Zjisti časové informace
time_mhm <- as.POSIXct(terra::time(preEffect), origin = "2065-11-01", tz = "UTC")
sel <- time_mhm >= as.POSIXct("2014-11-01") & time_mhm <= as.POSIXct("2044-10-31")

# Vyber období bez spin-upu
P <- preEffect[[sel]] + interception[[sel]]
ET <- aET[[sel]]

# Ořízni na povodí
P_crop <- mask(crop(P, vect(povodi_wgs)), vect(povodi_wgs))
ET_crop <- mask(crop(ET, vect(povodi_wgs)), vect(povodi_wgs))

# Roční suma a průměr přes území
P_yearly_mm <- global(app(P_crop, sum), fun = "mean", na.rm = TRUE)[[1]] / 30  # cca 30 let
ET_yearly_mm <- global(app(ET_crop, sum), fun = "mean", na.rm = TRUE)[[1]] / 30


# ==========================
# 2. mRM: načti Qrouted
# ==========================
Qrouted <- rast(mrm_file, subds = "Qrouted")
time_mrm <- as.POSIXct(terra::time(Qrouted), origin = "2065-11-01", tz = "UTC")
sel_q <- time_mrm >= as.POSIXct("2014-11-01") & time_mrm <= as.POSIXct("2044-10-31")

Qrouted_sel <- Qrouted[[sel_q]]

# Převeď souřadnice závěrového profilu do terra formátu
profil_vect <- vect(profil_wgs)

# Získej časovou řadu odtoku v buňce nejblížší závěrovému profilu
flow_ts <- as.numeric(extract(Qrouted_sel, profil_vect)[1, -1])




# Spočítej roční průměrný odtok v m³/s
Qrouted_avg_m3s <- mean(flow_ts, na.rm = TRUE)

# Přepočet na m³/rok
Qrouted_yearly_m3 <- Qrouted_avg_m3s * 60 * 60 * 24 * 365.25

# ==========================
# Výstup
# ==========================
cat("📊 Průměrné roční hodnoty pro období 2014–2044:\n")
cat(sprintf("🌧️ Srážky:           %.2f mm/rok\n", P_yearly_mm))
cat(sprintf("💨 Evapotranspirace: %.2f mm/rok\n", ET_yearly_mm))
cat(sprintf("🌊 Odtok (Qrouted):  %.2f m³/rok\n", Qrouted_yearly_m3))

# ==========================
# Uložení výsledků do CSV
# ==========================
vystup <- data.frame(
  scenar = "2030_SSP126_MPI-ESM1-2-HR",
  obdobi_od = "2014-11-01",
  obdobi_do = "2044-10-31",
  srazky_mm_rok = P_yearly_mm,
  evapotranspirace_mm_rok = ET_yearly_mm,
  qrouted_m3_rok = Qrouted_yearly_m3,
)


# Vytvoř výstupní složku pokud neexistuje
vystup_dir <- "C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output"
if (!dir.exists(vystup_dir)) dir.create(vystup_dir, recursive = TRUE)

# Ulož CSV
write.csv(vystup, file = file.path(vystup_dir, "bilance_2030_SSP126.csv"), row.names = FALSE)
cat("✅ Výstup uložen do: bilance_2030_SSP126.csv\n")
