# === 📦 Knihovny ===
library(dplyr)
library(sf)
library(purrr)
library(readr)
library(stringr)

# === 📁 Složky ===
base_dir <- getwd()
vrstvy_dir <- file.path(base_dir, "vrstvy")

# Zjištění rozlišení ze jména složky, např. 1975-2020_1.0 → 1p0
nazev_slozky <- basename(base_dir)
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

# Výstupní složka pro GeoJSON výstupy: output/KGE_mapy/<rozliseni>
typ_vystupu <- "KGE_mapy"
output_dir <- file.path(base_dir, "output", typ_vystupu, rozliseni_format)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# === 📄 Načtení CSV s výsledky ===
vystup_dir <- file.path(base_dir, "vystupy")

# === 🧹 Přesun souborů z podsložek do složky 'vystupy' ===

# Cesty k podsložkám, kde jsou výsledky
mesicni_slozka <- file.path(base_dir, "output", "KGE_mesic", rozliseni_format)
rocni_slozka   <- file.path(base_dir, "output", "KGE_rocni", rozliseni_format)

# Najdi všechny CSV soubory v těchto složkách
mesicni_soubory <- list.files(mesicni_slozka, pattern = "\\.csv$", full.names = TRUE)
rocni_soubory   <- list.files(rocni_slozka, pattern = "\\.csv$", full.names = TRUE)

# Přesun do složky vystupy
file.copy(from = c(mesicni_soubory, rocni_soubory), to = vystup_dir, overwrite = TRUE)

cat("✅ Měsíční a roční KGE soubory přesunuty do složky 'vystupy'\n")


kge_files <- list.files(vystup_dir, pattern = "^KGE_.*\\.csv$", full.names = TRUE)



# Načtení pouze denních, měsíčních a ročních souborů, bez dlouhodobých měsíčních
kge_files <- list.files(
  vystup_dir, 
  pattern = "^KGE_(denni|mesic|rocni).*\\.csv$", 
  full.names = TRUE
)

kge_data <- set_names(kge_files, tools::file_path_sans_ext(basename(kge_files))) %>%
  map(~ read_csv(.x, show_col_types = FALSE) %>% mutate(kodprofilu = as.character(kodprofilu)))

# Sloučení do jednoho data framu
KGE_all_results <- bind_rows(kge_data, .id = "source") %>%
  mutate(
    typ_dat = case_when(
      grepl("rocni", source, ignore.case = TRUE) ~ "rocni",
      grepl("mesicdlouhodobe", source, ignore.case = TRUE) ~ "mesicdlouhodobe",
      grepl("mesic", source, ignore.case = TRUE) ~ "mesic",
      grepl("denni", source, ignore.case = TRUE) ~ "denni",
      TRUE ~ NA_character_
    ),
    obdobi = case_when(
      grepl("ROK", source) | grepl("rocni", source) ~ NA_character_,
      TRUE ~ str_match(source, "KGE_[^_]+_([^_]+)")[, 2]
    )
  )



# === 📄 Načtení souboru s profily ===
prehled_profilu <- read.csv(file.path(base_dir, "prehled_profilu.csv"), sep = ";", header = FALSE)
colnames(prehled_profilu) <- c("lon", "lat", "kodprofilu")
prehled_profilu <- prehled_profilu %>% mutate(kodprofilu = as.character(kodprofilu))

# === 🌍 Načtení prostorových vrstev ===
hranice_povodi <- st_read(file.path(vrstvy_dir, "hranice_povodi.geojson"), quiet = TRUE)
vodni_nadrze   <- st_read(file.path(vrstvy_dir, "vodni_nadrze.geojson"), quiet = TRUE)
vodni_toky     <- st_read(file.path(vrstvy_dir, "vodni_toky.geojson"), quiet = TRUE)

# === 🧩 Spojení s profily ===
KGE_all_results <- KGE_all_results %>%
  left_join(prehled_profilu, by = "kodprofilu")

# === 🧮 Přiřazení typ_dat a období PŘED převodem na sf ===
KGE_all_results <- KGE_all_results %>%
  mutate(
    typ_dat = case_when(
      grepl("rocni", source, ignore.case = TRUE) ~ "rocni",
      grepl("mesicdlouhodobe", source, ignore.case = TRUE) ~ "mesicdlouhodobe",
      grepl("mesic", source, ignore.case = TRUE) ~ "mesic",
      grepl("denni", source, ignore.case = TRUE) ~ "denni",
      TRUE ~ NA_character_
    ),
    obdobi = case_when(
  grepl("ROK", source) | grepl("rocni", source) ~ NA_character_,
  TRUE ~ str_match(source, "KGE_[^_]+_([^_]+)")[, 2]
)
  )

# === 🧭 Převod na sf objekt a transformace ===
sf_object <- st_as_sf(KGE_all_results, coords = c("lon", "lat"), crs = 4326)
sf_object_5514 <- st_transform(sf_object, crs = 5514)

# === 📝 Uložení GeoJSON ===
geojson_path <- file.path(output_dir, paste0("vysledky_profilu_5514_", rozliseni_format, ".geojson"))
if (file.exists(geojson_path)) file.remove(geojson_path)

st_write(sf_object_5514, geojson_path, driver = "GeoJSON", append = FALSE)

# === 📄 Přidání dummy hodnot ===
dummy_path <- file.path(vrstvy_dir, "dummy.csv")
if (file.exists(dummy_path)) {
  KGE_dummy <- read.csv(dummy_path, sep = ";", header = TRUE) %>%
    mutate(
      kodprofilu = as.character(kodprofilu),
      typ_dat = as.character(typ_dat),
      obdobi = as.character(obdobi)
    )
  
  # Přidání na konec dat
  KGE_all_results <- bind_rows(KGE_all_results, KGE_dummy)
  message("✅ Dummy data úspěšně přidána")
} else {
  warning("⚠️  Dummy data nebyla nalezena v: ", dummy_path)
}


# ✅ Ukázka výsledků
print(head(KGE_all_results))
