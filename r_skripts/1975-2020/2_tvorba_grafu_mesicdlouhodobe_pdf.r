library(tidyverse)
library(lubridate)

# === 📁 Nastavení cesty ===
base_dir <- getwd()

# Zjištění rozlišení ze jména složky, např. 1975-2020_1.0 → 1p0
nazev_slozky <- basename(base_dir)
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

# Cesty ke vstupním složkám
slozka_pozorovani <- file.path(base_dir, "pozorovani")
slozka_simulace <- file.path(base_dir, "simulace")

# Výstupní složka ve struktuře output/<typ>/<rozliseni>/
typ_vystupu <- "mesicni_prumerydlouhodobe"
output_dir <- file.path(base_dir, "output", typ_vystupu, rozliseni_format)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# === 📥 Načtení souboru s názvy profilů ===
soubor_nazvy <- file.path(base_dir, "vrstvy", "p_ehled_profil_.csv")
prehled_profilu <- read_csv(soubor_nazvy, show_col_types = FALSE) %>%
  mutate(kodprofilu = as.character(kodprofilu))

# === 📥 Načtení pozorovaných dat ===
files_pozorovani <- list.files(slozka_pozorovani, pattern = "\\.csv$", full.names = TRUE)

data_pozorovani <- map_dfr(files_pozorovani, function(file) {
  kod <- str_extract(basename(file), "\\d+")
  read_csv(
    file,
    col_names = c("kodprofilu", "typ", "rok", "mesic", "den", "prutok"),
    col_types = cols(
      kodprofilu = col_character(),
      typ = col_character(),
      rok = col_integer(),
      mesic = col_integer(),
      den = col_integer(),
      prutok = col_double()
    ),
    show_col_types = FALSE
  ) %>% 
    mutate(
      kodprofilu = kod,
      date = as.Date(paste(rok, mesic, den, sep = "-"))
    )
})

# === 📥 Načtení simulací ===
files_simulace <- list.files(slozka_simulace, pattern = "\\.csv$", full.names = TRUE)

read_simulace <- function(file) {
  data <- read_fwf(file, skip = 2, fwf_empty(file, skip = 2), trim_ws = TRUE)
  colnames(data) <- c("date", "lon", "lat", "value")
  data <- data %>%
    mutate(
      kodprofilu = str_extract(basename(file), "\\d+"),
      date = as.Date(date)
    )
  return(data)
}

data_simulace <- map_dfr(files_simulace, read_simulace)

# === 📊 Výpočty dlouhodobých měsíčních průměrů ===
data_pozorovani_mesic <- data_pozorovani %>%
  mutate(month = month(date)) %>%
  group_by(kodprofilu, month) %>%
  summarise(prutok_prumer = mean(prutok, na.rm = TRUE), .groups = "drop")

data_simulace_mesic <- data_simulace %>%
  mutate(month = month(date)) %>%
  group_by(kodprofilu, month) %>%
  summarise(value_prumer = mean(value, na.rm = TRUE), .groups = "drop")

data_porovnani_mesic <- data_pozorovani_mesic %>%
  left_join(data_simulace_mesic, by = c("kodprofilu", "month"))

# Přidání názvů profilů pro popisky grafů
data_porovnani_mesic <- data_porovnani_mesic %>%
  left_join(prehled_profilu, by = "kodprofilu") %>%
  mutate(popis_profilu = paste("Profil", kodprofilu, nazev, "–", tok))

# 💾 Uložení do CSV
write_csv(data_porovnani_mesic, file.path(output_dir, "KGE_vystup_mesicdlouhodobe.csv"))

# === 📈 Tvorba a ukládání grafů ===
vytvor_graf <- function(kodprofilu, data) {
  data_profil <- data %>% filter(kodprofilu == !!kodprofilu)
  popis <- unique(data_profil$popis_profilu)

  graf <- ggplot(data_profil, aes(x = month)) +
    geom_line(aes(y = value_prumer, color = "Simulace"), linewidth = 0.3) +
    geom_line(aes(y = prutok_prumer, color = "Pozorování"), linewidth = 0.3) +
    labs(
      title = paste("Porovnání dlouhodobých měsíčních průměrů \n", popis),
      x = "Měsíc", y = "Dlouhodobý měsíční průměr průtoku (m³/s)",
      color = "Legenda"
    ) +
    scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "red")) +
    scale_x_continuous(
      breaks = 1:12,
      labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII")
    ) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 40, face = "bold", lineheight = 0.3),
      axis.title.x = element_text(size = 34),
      axis.title.y = element_text(size = 34),
      axis.text = element_text(size = 30),
      legend.position = "top",
      legend.title = element_text(size = 32),
      legend.text = element_text(size = 30),
      text = element_text(family = "Calibri")
    )

  ggsave(
    file.path(output_dir, paste0(rozliseni_format, "_mesicdlouhodobe_", kodprofilu, ".png")),
    plot = graf, width = 5.2, height = 3.8, dpi = 300, bg = "white"
  )
}

# === 🔁 Spuštění pro každý profil ===
unique(data_porovnani_mesic$kodprofilu) %>% 
  walk(~ vytvor_graf(.x, data_porovnani_mesic))
