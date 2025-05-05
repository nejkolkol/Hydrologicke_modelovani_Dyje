# Načtení knihoven
library(dplyr)
library(hydroGOF)
library(readr)
library(stringr)

# === 📁 Cesty a rozlišení ===
base_dir <- getwd()
nazev_slozky <- basename(base_dir)
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

# Výstupní složka ve formátu output/KGE_rocni/<rozliseni>
typ_vystupu <- "KGE_rocni"
output_dir <- file.path(base_dir, "output", typ_vystupu, rozliseni_format)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 📥 Načtení vstupních dat
data_path <- file.path(base_dir, "output", "rocni_prumery", rozliseni_format, "KGE_vystup_rocni.csv")
data_porovnani_rocni <- read_csv(data_path, show_col_types = FALSE)

# 🧮 Funkce pro výpočet KGE
calculate_kge <- function(obs, sim) {
  valid_indices <- complete.cases(obs, sim)
  obs <- obs[valid_indices]
  sim <- sim[valid_indices]
  
  if (length(obs) < 2 | length(sim) < 2) {
    return(NA)
  }
  
  cc <- cor(obs, sim)
  alpha <- sd(sim) / sd(obs)
  beta <- mean(sim) / mean(obs)
  kge <- 1 - sqrt((cc - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  return(kge)
}

# 📊 Výpočet KGE a dalších metrik
KGE_rocni <- data_porovnani_rocni %>%
  group_by(kodprofilu) %>%
  summarise(
    KGE = calculate_kge(prutok_prumer, value_prumer),
    Pearson_Correlation = cor(prutok_prumer, value_prumer, use = "complete.obs"),
    Bias = mean(value_prumer, na.rm = TRUE) / mean(prutok_prumer, na.rm = TRUE),
    Variability = sd(value_prumer - prutok_prumer, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    typ_dat = "rocni",
    obdobi = NA_character_
  )


# 💾 Uložení výstupu
write_csv(KGE_rocni, file.path(output_dir, paste0("KGE_rocni_", rozliseni_format, ".csv")))

# (Volitelně) výpis do konzole
print(KGE_rocni)
