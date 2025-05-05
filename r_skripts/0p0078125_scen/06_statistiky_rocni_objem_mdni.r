# R/06_statistiky_rocni_objem_mdni.R

library(dplyr)
library(lubridate)
library(zoo)
library(purrr)

# === Načti data ===
pozorovani <- readRDS("output/pozorovani.rds") %>%
  select(kodprofilu, date, prutok)

default_data <- readRDS("output/default_data_orez.rds") %>%
  select(kod_profilu, date, value, scenar) %>%
  rename(kodprofilu = kod_profilu, prutok = value)

lai_data <- readRDS("output/lai_data_orez.rds") %>%
  select(kod_profilu, date, value, scenar, lai_variant) %>%
  rename(kodprofilu = kod_profilu, prutok = value)

# === Funkce: Celkový objem vody za celé období ===
vypocitej_objem_celkem <- function(data, extra_cols = NULL) {
  data %>%
    mutate(objem_denně = prutok * 86400) %>%
    group_by(across(all_of(c(extra_cols, "kodprofilu")))) %>%
    summarise(
      objem_m3 = sum(objem_denně, na.rm = TRUE),
      .groups = "drop"
    )
}

# === Funkce: Dlouhodobý roční průměr průtoku ===
vypocitej_dl_prumer <- function(data, extra_cols = NULL) {
  data %>%
    mutate(rok = year(date)) %>%
    group_by(across(all_of(c(extra_cols, "kodprofilu", "rok")))) %>%
    summarise(prumer = mean(prutok, na.rm = TRUE), .groups = "drop") %>%
    group_by(across(all_of(c(extra_cols, "kodprofilu")))) %>%
    summarise(dl_rocni_prumer = mean(prumer, na.rm = TRUE), .groups = "drop")
}

# === Funkce: M-denní průtoky za celé období ===
vypocitej_mdenni_celkove <- function(data, m = 30, extra_cols = NULL) {
  data %>%
    arrange(kodprofilu, date) %>%
    group_by(across(all_of(c(extra_cols, "kodprofilu")))) %>%
    summarise(
      mdenni_prutok = if (sum(!is.na(prutok)) >= m) {
        min(rollmean(prutok, m, align = "left", fill = NA), na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(m = m)
}

# === Výpočty ===

# 1. Celkový objem
objem_pozorovani <- vypocitej_objem_celkem(pozorovani)
objem_default    <- vypocitej_objem_celkem(default_data, extra_cols = "scenar")
objem_lai        <- vypocitej_objem_celkem(lai_data, extra_cols = c("scenar", "lai_variant"))

# 2. Dlouhodobý roční průměr průtoku
dl_prumer_pozorovani <- vypocitej_dl_prumer(pozorovani)
dl_prumer_default    <- vypocitej_dl_prumer(default_data, extra_cols = "scenar")
dl_prumer_lai        <- vypocitej_dl_prumer(lai_data, extra_cols = c("scenar", "lai_variant"))

# 3. M-denní průtoky (za celé období)
m_hodnoty <- c(30, 180, 330, 355)

mdenni_pozorovani <- map_dfr(m_hodnoty, ~ vypocitej_mdenni_celkove(pozorovani, m = .x))
mdenni_default    <- map_dfr(m_hodnoty, ~ vypocitej_mdenni_celkove(default_data, m = .x, extra_cols = "scenar"))
mdenni_lai        <- map_dfr(m_hodnoty, ~ vypocitej_mdenni_celkove(lai_data, m = .x, extra_cols = c("scenar", "lai_variant")))

# === Uložení výstupů ===
saveRDS(objem_pozorovani,        "output/objem_celkem_pozorovani.rds")
saveRDS(objem_default,           "output/objem_celkem_default.rds")
saveRDS(objem_lai,               "output/objem_celkem_lai.rds")

saveRDS(dl_prumer_pozorovani,    "output/dlouhodoby_prumer_pozorovani.rds")
saveRDS(dl_prumer_default,       "output/dlouhodoby_prumer_default.rds")
saveRDS(dl_prumer_lai,           "output/dlouhodoby_prumer_lai.rds")

saveRDS(mdenni_pozorovani,       "output/mdenni_prutoky_pozorovani.rds")
saveRDS(mdenni_default,          "output/mdenni_prutoky_default.rds")
saveRDS(mdenni_lai,              "output/mdenni_prutoky_lai.rds")

message("✅ Výpočty pro pozorování, výchozí a variantní simulace byly dokončeny.")
