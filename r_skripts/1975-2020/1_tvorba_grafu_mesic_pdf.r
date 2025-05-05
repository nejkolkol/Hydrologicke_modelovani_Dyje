library(tidyverse)

# === 📁 Nastavení hlavní složky ===
base_dir <- getwd()

# Zjištění rozlišení ze složky a úprava formátu (např. 1.0 → 1p0)
nazev_slozky <- basename(base_dir)
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

# Výstupní složka do struktury output/<typ>/<rozliseni>
typ_vystupu <- "mesicni_prumery"
output_dir <- file.path(base_dir, "output", typ_vystupu, rozliseni_format)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# === 📥 Cesty ke vstupním datům ===
cesta_pozorovani <- file.path(base_dir, "pozorovani")
cesta_simulace   <- file.path(base_dir, "simulace")

# === 📄 Načtení názvů profilů ===
vrstvy_dir <- file.path(base_dir, "vrstvy")
info_profily <- read_csv(file.path(vrstvy_dir, "P_ehled_profil_.csv"), show_col_types = FALSE) %>%
  mutate(kodprofilu = as.character(kodprofilu))

# === 📄 Načtení a spojení pozorování ===
files_pozorovani <- list.files(cesta_pozorovani, pattern = "\\.csv$", full.names = TRUE)

data_pozorovani <- files_pozorovani %>%
  map_dfr(~ read_csv(.x, col_names = c("kodprofilu", "typ", "rok", "mesic", "den", "prutok"), show_col_types = FALSE) %>%
            mutate(
              kodprofilu = str_extract(basename(.x), "\\d+"),
              mesic = as.integer(mesic),
              den = as.integer(den),
              date = as.Date(paste(rok, mesic, den, sep = "-"))
            ))

# === 📄 Načtení simulací ===
read_simulace <- function(file) {
  data <- read_fwf(
    file,
    skip = 2,
    fwf_empty(file, skip = 2),
    trim_ws = TRUE
  )
  colnames(data) <- c("date", "lon", "lat", "value")
  data %>%
    mutate(
      kodprofilu = str_extract(basename(file), "\\d+"),
      date = as.Date(date)
    )
}

files_simulace <- list.files(cesta_simulace, pattern = "\\.csv$", full.names = TRUE)
data_simulace <- map_dfr(files_simulace, read_simulace)

# === 🔁 Oříznutí simulací podle rozsahu pozorování ===
rozsah_pozorovani <- data_pozorovani %>%
  group_by(kodprofilu) %>%
  summarise(min_date = min(date, na.rm = TRUE),
            max_date = max(date, na.rm = TRUE), .groups = "drop")

data_simulace_upraveno <- data_simulace %>%
  left_join(rozsah_pozorovani, by = "kodprofilu") %>%
  filter(date >= min_date & date <= max_date)

# === 🔁 Spojení simulací a pozorování ===
data_porovnani <- data_simulace_upraveno %>%
  left_join(data_pozorovani, by = c("date", "kodprofilu"), suffix = c("_sim", "_poz"))

# === 📊 Výpočet měsíčních průměrů ===
data_porovnani_mesic <- data_porovnani %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  group_by(kodprofilu, year_month) %>%
  summarise(
    prutok_prumer = mean(prutok, na.rm = TRUE),
    value_prumer = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year_month = as.Date(paste0(year_month, "-01"), format = "%Y-%m-%d"))

# === 📈 Funkce pro vykreslení ===
vytvor_graf <- function(kodprofilu, data) {
  data_profil <- data %>% filter(kodprofilu == !!kodprofilu)

  # Získání názvu profilu
  info <- info_profily %>% filter(kodprofilu == !!kodprofilu)
  nazev_prof <- if (nrow(info) == 1) {
    paste0("Profil ", kodprofilu, " ", info$nazev, " – ", info$tok)
  } else {
    paste0("Profil ", kodprofilu)
  }

    graf <- ggplot(data_profil, aes(x = year_month)) +
    geom_line(aes(y = value_prumer, color = "Simulace"), linewidth = 0.3) +
    geom_line(aes(y = prutok_prumer, color = "Pozorování"), linewidth = 0.3) +
    labs(
      title = paste("Porovnání měsíčních průměrů\n", nazev_prof),
      subtitle = paste("Rozlišení simulace:", rozliseni, "°"),
      x = "Rok",
      y = "Měsíční průměr průtoku (m³/s)",
      color = "Legenda"
    ) +
    theme_minimal(base_size = 16) +
    scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "red")) +
    theme(
      plot.title = element_text(size = 40, face = "bold", hjust = 0, lineheight = 0.3),
      plot.subtitle = element_text(size = 28, face = "italic", hjust = 0),
      axis.title.x = element_text(size = 34),
      axis.title.y = element_text(size = 34),
      axis.text = element_text(size = 30),
      legend.position = "top",
      legend.title = element_text(size = 32),
      legend.text = element_text(size = 30),
      text = element_text(family = "Calibri")
    )


  # Uložení
  ggsave(file.path(output_dir, paste0(rozliseni_format, "_mesic_", kodprofilu, ".png")),
         plot = graf, width = 5.2, height = 3.8, dpi = 300, bg = "white")
}

# === 🔁 Uložení grafů ===
unique(data_porovnani_mesic$kodprofilu) %>%
  walk(~ vytvor_graf(.x, data_porovnani_mesic))

# === 💾 Uložení dat ===
write_csv(data_porovnani_mesic, file.path(output_dir, "KGE_vystup_mesic.csv"))
write_csv(data_porovnani, file.path(output_dir, "data_porovnani.csv"))
