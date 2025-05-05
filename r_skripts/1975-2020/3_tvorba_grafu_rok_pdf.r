library(tidyverse)
library(lubridate)

# === 📁 Nastavení cesty ===
base_dir <- getwd()

# Cesty ke vstupním datům
slozka_pozorovani <- file.path(base_dir, "pozorovani")
slozka_simulace <- file.path(base_dir, "simulace")

# Zjištění rozlišení ze jména složky, např. 1975-2020_1.0 → 1p0
nazev_slozky <- basename(base_dir)
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

# 📂 Výstupní adresář: output/rocni_prumery/<rozliseni>/
typ_vystupu <- "rocni_prumery"
output_dir <- file.path(base_dir, "output", typ_vystupu, rozliseni_format)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# === 📄 Názvy profilů ===
prehled_profilu <- read_csv(file.path(base_dir, "vrstvy", "p_ehled_profil_.csv"), show_col_types = FALSE) %>%
  mutate(kodprofilu = as.character(kodprofilu))

# === 📅 Načtení pozorovaných dat ===
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

# === 📅 Načtení simulací ===
files_simulace <- list.files(slozka_simulace, pattern = "\\.csv$", full.names = TRUE)

read_simulace <- function(file) {
  data <- read_fwf(file, skip = 2, fwf_empty(file, skip = 2), trim_ws = TRUE)
  colnames(data) <- c("date", "lon", "lat", "value")
  data$kodprofilu <- str_extract(basename(file), "\\d+")
  data$date <- as.Date(data$date)
  return(data)
}

data_simulace <- map_dfr(files_simulace, read_simulace)

# === 📊 Výpočty ročních průměrů ===
data_pozorovani_rocni <- data_pozorovani %>%
  mutate(year = year(date)) %>%
  group_by(kodprofilu, year) %>%
  summarise(prutok_prumer = mean(prutok, na.rm = TRUE), .groups = "drop")

data_simulace_rocni <- data_simulace %>%
  mutate(year = year(date)) %>%
  group_by(kodprofilu, year) %>%
  summarise(value_prumer = mean(value, na.rm = TRUE), .groups = "drop")

data_porovnani_rocni <- data_pozorovani_rocni %>%
  left_join(data_simulace_rocni, by = c("kodprofilu", "year")) %>%
  left_join(prehled_profilu, by = "kodprofilu") %>%
  mutate(popis_profilu = paste("Profil", kodprofilu, nazev, "–", tok))
  
data_porovnani_rocni %>%
  filter(is.na(prutok_prumer) | is.na(value_prumer))


# 📆 Uložení do CSV
write_csv(data_porovnani_rocni, file.path(output_dir, "KGE_vystup_rocni.csv"))

# === 📈 Funkce pro tvorbu a uložení grafu ===
vytvor_graf_rocni <- function(kodprofilu, data) {
  data_profil <- data %>% filter(kodprofilu == !!kodprofilu)
  popis <- unique(data_profil$popis_profilu)

  graf <- ggplot(data_profil, aes(x = year)) +
    geom_line(aes(y = value_prumer, color = "Simulace"), linewidth = 0.3) +
    geom_line(aes(y = prutok_prumer, color = "Pozorování"), linewidth = 0.3) +
    labs(
      title = paste("Porovnání ročních průměrů \n", popis),
      subtitle = paste("Rozlišení simulace:", rozliseni, "°"),
      x = "Rok", y = "Roční průměr průtoku (m³/s)",
      color = "Legenda"
    ) +
    scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "red")) +
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 40, face = "bold", lineheight = 0.3),
      plot.subtitle = element_text(size = 28, face = "italic"),
      axis.title.x = element_text(size = 34),
      axis.title.y = element_text(size = 34),
      axis.text = element_text(size = 30),
      legend.position = "top",
      legend.title = element_text(size = 32),
      legend.text = element_text(size = 30),
      text = element_text(family = "Calibri")
    )

  ggsave(file.path(output_dir, paste0(rozliseni_format, "_rocni_", kodprofilu, ".png")),
         plot = graf, width = 5.2, height = 3.8, dpi = 300, bg = "white")
}

# Iterace přes profily
unique(data_porovnani_rocni$kodprofilu) %>%
  walk(~ vytvor_graf_rocni(.x, data_porovnani_rocni))
