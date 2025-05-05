# Načíst knihovny
library(terra)
library(ncdf4)
library(dplyr)
library(ggplot2)

# Cesty k souborům
cesta_pet_aet <- "C:/Users/kolar/OneDrive/Desktop/data0078125/data0078125_scenarios/2085_SSP245_MPI-ESM1-2-HR/mHM_Fluxes_States.nc"
cesta_precip <- "D:/meteo_data/klimaticke_scenare/2085_SSP245_MPI-ESM1-2-HR/pre.nc"

# Načíst srážky (P)
pre <- rast(cesta_precip)

# Oříznout srážky na správné měsíce (1.11.2069 – 31.10.2099 => 30 let × 12 = 360 měsíců)
zacatek_pre <- (2069 - 1984) * 12 + 11
konec_pre <- zacatek_pre + 359
pre_hydro <- pre[[zacatek_pre:konec_pre]]

# Oprava: převod z mm/day na mm/month
# Potřebujeme počet dní v každém měsíci (včetně přestupných let)
# Vytvořit časovou osu
start_date <- as.Date("2069-11-01")
end_date <- as.Date("2099-10-31")
all_months <- seq.Date(start_date, end_date, by = "month")

# Spočítat počet dní v každém měsíci
dny_v_mesici <- sapply(all_months, function(d) {
  as.integer(as.Date(format(d, "%Y-%m-01")) + months(1) - 1 - as.Date(format(d, "%Y-%m-01")) + 1)
})

# Vynásobit každý měsíc příslušným počtem dní
pre_hydro_mesicne <- pre_hydro * dny_v_mesici

# Teď už můžeme sčítat roky
index_roky <- rep(1:30, each = 12)
P_roky <- tapp(pre_hydro_mesicne, index = index_roky, fun = sum)

# --- PET a aET ---

# Načíst PET a aET z mHM Fluxes States
nc <- nc_open(cesta_pet_aet)
PET <- ncvar_get(nc, "PET") # [lon, lat, time]
aET <- ncvar_get(nc, "aET") # [lon, lat, time]
nc_close(nc)

# Převést PET a aET na terra::rast
nc_lon <- dim(PET)[1]
nc_lat <- dim(PET)[2]
nc_time <- dim(PET)[3]
PET_rast <- rast(array(PET, dim = c(nc_lon, nc_lat, nc_time)))
aET_rast <- rast(array(aET, dim = c(nc_lon, nc_lat, nc_time)))

# Oříznout na správné měsíce
PET_hydro <- PET_rast[[49:(49+359)]]
aET_hydro <- aET_rast[[49:(49+359)]]

# Sečíst PET a aET za roky
PET_roky <- tapp(PET_hydro, index = index_roky, fun = sum)
aET_roky <- tapp(aET_hydro, index = index_roky, fun = sum)

# --- Výpočet průměrů ---

P_prumer <- mean(P_roky)
PET_prumer <- mean(PET_roky)
aET_prumer <- mean(aET_roky)

# Vytvořit datový rámec pro Budyko křivku
budyko_data <- data.frame(
  P = as.vector(values(P_prumer)),
  PET = as.vector(values(PET_prumer)),
  aET = as.vector(values(aET_prumer))
) %>%
  filter(!is.na(P), !is.na(PET), !is.na(aET)) %>%
  mutate(
    PET_div_P = PET / P,
    aET_div_P = aET / P
  )

# Teoretická Budyko křivka
x_teorie <- seq(0, 5, length.out = 500)
y_teorie <- x_teorie / (1 + x_teorie)

# Vykreslit
ggplot(budyko_data, aes(x = PET_div_P, y = aET_div_P)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_line(data = data.frame(x = x_teorie, y = y_teorie), aes(x = x, y = y), color = "red", linewidth = 1) +
  labs(
    title = "Budyko křivka – scénář 2085 SSP245",
    x = "PET / P",
    y = "aET / P"
  ) +
  theme_minimal(base_family = "Calibri")

