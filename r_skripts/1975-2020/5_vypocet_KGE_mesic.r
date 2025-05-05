# === 📦 Knihovny ===
library(dplyr)
library(readr)
library(hydroGOF)
library(lubridate)
library(stringr)

# === 📁 Nastavení cest ===
base_dir <- getwd()
nazev_slozky <- basename(base_dir)
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

# Výstupní složky
output_data_dir <- file.path(base_dir, "output")
output_kge_mesic_dir <- file.path(output_data_dir, "KGE_mesic", rozliseni_format)
if (!dir.exists(output_kge_mesic_dir)) dir.create(output_kge_mesic_dir, recursive = TRUE)

# === 📥 Načtení vstupních dat ===
input_file <- file.path(output_data_dir, "mesicni_prumery", rozliseni_format, "KGE_vystup_mesic.csv")
data_porovnani_mesic <- read_csv(input_file, show_col_types = FALSE)


# Přidání měsíce z formátu YYYY-MM
data_porovnani_mesic <- data_porovnani_mesic %>%
  mutate(month = month(ymd(year_month)))

# === 🔢 Výpočet metriky KGE ===
calculate_kge <- function(obs, sim) {
  cc <- cor(obs, sim)
  alpha <- sd(sim) / sd(obs)
  beta <- mean(sim) / mean(obs)
  kge <- 1 - sqrt((cc - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  return(kge)
}

# === 🧮 Výpočty ===
vypocet_kge <- function(data) {
  data %>%
    group_by(kodprofilu) %>%
    summarise(
      KGE = calculate_kge(prutok_prumer, value_prumer),
      Pearson_Correlation = cor(prutok_prumer, value_prumer),
      Bias = mean(value_prumer) / mean(prutok_prumer),
      Variability = sd(value_prumer - prutok_prumer),
      .groups = "drop"
    )
}

KGE_mesic_ROK     <- vypocet_kge(data_porovnani_mesic) %>% mutate(typ_dat = "mesic", obdobi = NA_character_)
KGE_mesic_DJF     <- vypocet_kge(filter(data_porovnani_mesic, month %in% c(12, 1, 2))) %>% mutate(typ_dat = "mesic", obdobi = "DJF")
KGE_mesic_MAM     <- vypocet_kge(filter(data_porovnani_mesic, month %in% c(3, 4, 5))) %>% mutate(typ_dat = "mesic", obdobi = "MAM")
KGE_mesic_JJA     <- vypocet_kge(filter(data_porovnani_mesic, month %in% c(6, 7, 8))) %>% mutate(typ_dat = "mesic", obdobi = "JJA")
KGE_mesic_SON     <- vypocet_kge(filter(data_porovnani_mesic, month %in% c(9, 10, 11))) %>% mutate(typ_dat = "mesic", obdobi = "SON")
KGE_mesic_ONDJFM  <- vypocet_kge(filter(data_porovnani_mesic, month %in% c(10, 11, 12, 1, 2, 3))) %>% mutate(typ_dat = "mesic", obdobi = "ONDJFM")
KGE_mesic_AMJJAS  <- vypocet_kge(filter(data_porovnani_mesic, month %in% c(4, 5, 6, 7, 8, 9))) %>% mutate(typ_dat = "mesic", obdobi = "AMJJAS")


# === 💾 Uložení výstupů ===
write_csv(KGE_mesic_ROK,    file.path(output_kge_mesic_dir, paste0("KGE_mesic_ROK_", rozliseni_format, ".csv")))
write_csv(KGE_mesic_DJF,    file.path(output_kge_mesic_dir, paste0("KGE_mesic_DJF_", rozliseni_format, ".csv")))
write_csv(KGE_mesic_MAM,    file.path(output_kge_mesic_dir, paste0("KGE_mesic_MAM_", rozliseni_format, ".csv")))
write_csv(KGE_mesic_JJA,    file.path(output_kge_mesic_dir, paste0("KGE_mesic_JJA_", rozliseni_format, ".csv")))
write_csv(KGE_mesic_SON,    file.path(output_kge_mesic_dir, paste0("KGE_mesic_SON_", rozliseni_format, ".csv")))
write_csv(KGE_mesic_ONDJFM, file.path(output_kge_mesic_dir, paste0("KGE_mesic_ONDJFM_", rozliseni_format, ".csv")))
write_csv(KGE_mesic_AMJJAS, file.path(output_kge_mesic_dir, paste0("KGE_mesic_AMJJAS_", rozliseni_format, ".csv")))

# (Volitelně) výpis
print(KGE_mesic_ROK)
