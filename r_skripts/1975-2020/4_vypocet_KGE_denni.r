# === 📆 Skript 4: Výpočet denních KGE ===

# === 📆 Knihovny ===
library(dplyr)
library(readr)
library(hydroGOF)
library(stringr)

# === 📅 Cesty ===
base_dir <- getwd()
nazev_slozky <- basename(base_dir)
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

output_data_dir <- file.path(base_dir, "output")
output_kge_denni_dir <- file.path(output_data_dir, "KGE_denni", rozliseni_format)
if (!dir.exists(output_kge_denni_dir)) dir.create(output_kge_denni_dir, recursive = TRUE)

# === 🔢 Vstup ===
data_porovnani <- read_csv(
  file.path(base_dir, "output", "mesicni_prumery", rozliseni_format, "data_porovnani.csv"),
  show_col_types = FALSE
)


# === 📝 Funkce pro výpočet KGE ===
calculate_kge <- function(obs, sim) {
  cc <- cor(obs, sim)
  alpha <- sd(sim) / sd(obs)
  beta <- mean(sim) / mean(obs)
  kge <- 1 - sqrt((cc - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  return(kge)
}

# === 🔢 Výpočty ===
vypocet_kge <- function(data) {
  data %>%
    group_by(kodprofilu) %>%
    summarise(
      KGE = calculate_kge(prutok, value),
      Pearson_Correlation = cor(prutok, value),
      Bias = mean(value) / mean(prutok),
      Variability = sd(value - prutok),
      .groups = "drop"
    )
}

# Měsíc jako číslo (pro filtrování)
data_porovnani <- data_porovnani %>% mutate(mesic = as.numeric(mesic))

KGE_denni_ROK     <- vypocet_kge(data_porovnani)
KGE_denni_DJF     <- vypocet_kge(filter(data_porovnani, mesic %in% c(12, 1, 2)))
KGE_denni_MAM     <- vypocet_kge(filter(data_porovnani, mesic %in% c(3, 4, 5)))
KGE_denni_JJA     <- vypocet_kge(filter(data_porovnani, mesic %in% c(6, 7, 8)))
KGE_denni_SON     <- vypocet_kge(filter(data_porovnani, mesic %in% c(9, 10, 11)))
KGE_denni_ONDJFM  <- vypocet_kge(filter(data_porovnani, mesic %in% c(10, 11, 12, 1, 2, 3)))
KGE_denni_AMJJAS  <- vypocet_kge(filter(data_porovnani, mesic %in% c(4, 5, 6, 7, 8, 9)))

# === 📃 Uložení ===
write_csv(KGE_denni_ROK,    file.path(output_kge_denni_dir, paste0("KGE_denni_ROK_", rozliseni_format, ".csv")))
write_csv(KGE_denni_DJF,    file.path(output_kge_denni_dir, paste0("KGE_denni_DJF_", rozliseni_format, ".csv")))
write_csv(KGE_denni_MAM,    file.path(output_kge_denni_dir, paste0("KGE_denni_MAM_", rozliseni_format, ".csv")))
write_csv(KGE_denni_JJA,    file.path(output_kge_denni_dir, paste0("KGE_denni_JJA_", rozliseni_format, ".csv")))
write_csv(KGE_denni_SON,    file.path(output_kge_denni_dir, paste0("KGE_denni_SON_", rozliseni_format, ".csv")))
write_csv(KGE_denni_ONDJFM, file.path(output_kge_denni_dir, paste0("KGE_denni_ONDJFM_", rozliseni_format, ".csv")))
write_csv(KGE_denni_AMJJAS, file.path(output_kge_denni_dir, paste0("KGE_denni_AMJJAS_", rozliseni_format, ".csv")))

# === ✅ Volitelně: tisk ===
print(KGE_denni_ROK)
