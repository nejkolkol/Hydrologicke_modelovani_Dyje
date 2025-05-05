# R/05_vypocty_prumeru.R

library(dplyr)
library(lubridate)
library(readr)

# Načti data
pozorovani <- readRDS("output/pozorovani.rds") %>%
  select(kodprofilu, date, prutok)

default_data <- readRDS("output/default_data_orez.rds") %>%
  select(kod_profilu, date, value, scenar) %>%
  rename(kodprofilu = kod_profilu, prutok = value)

lai_data <- readRDS("output/lai_data_orez.rds") %>%
  select(kod_profilu, date, value, scenar, lai_variant) %>%
  rename(kodprofilu = kod_profilu, prutok = value)

# Pomocná funkce pro výpočet průměrů
vypocitej_prumery <- function(data, skupiny) {
  data %>%
    mutate(
      rok = year(date),
      mesic = month(date)
    ) %>%
    group_by(across(all_of(c(skupiny)))) %>%
    summarise(prumer = mean(prutok, na.rm = TRUE), .groups = "drop")
}

# 🔁 Výpočty
# 1. Měsíční průměry po letech
mesicni_rocni_pozorovani <- vypocitej_prumery(pozorovani, c("kodprofilu", "rok", "mesic"))
mesicni_rocni_default <- vypocitej_prumery(default_data, c("kodprofilu", "scenar", "rok", "mesic"))
mesicni_rocni_lai <- vypocitej_prumery(lai_data, c("kodprofilu", "scenar", "lai_variant", "rok", "mesic"))

# 2. Roční průměry
rocni_pozorovani <- vypocitej_prumery(pozorovani, c("kodprofilu", "rok"))
rocni_default <- vypocitej_prumery(default_data, c("kodprofilu", "scenar", "rok"))
rocni_lai <- vypocitej_prumery(lai_data, c("kodprofilu", "scenar", "lai_variant", "rok"))

# 3. Dlouhodobé měsíční průměry
dlouhodobe_mesicni_pozorovani <- vypocitej_prumery(pozorovani, c("kodprofilu", "mesic"))
dlouhodobe_mesicni_default <- vypocitej_prumery(default_data, c("kodprofilu", "scenar", "mesic"))
dlouhodobe_mesicni_lai <- vypocitej_prumery(lai_data, c("kodprofilu", "scenar", "lai_variant", "mesic"))

# 💾 Uložení výsledků
saveRDS(mesicni_rocni_pozorovani, "output/mesicni_rocni_pozorovani.rds")
saveRDS(mesicni_rocni_default,    "output/mesicni_rocni_default.rds")
saveRDS(mesicni_rocni_lai,        "output/mesicni_rocni_lai.rds")

saveRDS(rocni_pozorovani,         "output/rocni_pozorovani.rds")
saveRDS(rocni_default,            "output/rocni_default.rds")
saveRDS(rocni_lai,                "output/rocni_lai.rds")

saveRDS(dlouhodobe_mesicni_pozorovani, "output/dlouhodobe_mesicni_pozorovani.rds")
saveRDS(dlouhodobe_mesicni_default,    "output/dlouhodobe_mesicni_default.rds")
saveRDS(dlouhodobe_mesicni_lai,        "output/dlouhodobe_mesicni_lai.rds")

message("✅ Výpočty průměrů byly úspěšně dokončeny a uloženy do složky output/")
