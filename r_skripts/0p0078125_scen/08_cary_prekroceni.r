# R/08_cary_prekroceni.R

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)

# === Cesty ===
output_dir <- "output/cary_prekroceni_grafy"
dir.create(output_dir, showWarnings = FALSE)

# === Načtení dat ===
data_pozor <- readRDS("output/pozorovani.rds")
data_default <- readRDS("output/default_data_orez.rds")
profily_info <- read_csv("vrstvy/P_ehled_profil_.csv", show_col_types = FALSE)

# === Pomocná funkce pro FDC ===
vytvor_fdc_data <- function(data, id_cols = NULL) {
  data %>%
    group_by(across(all_of(c(id_cols, "kodprofilu")))) %>%
    summarise(
      hodnoty = list(sort(prutok, decreasing = TRUE)),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      fdc_df = map2(hodnoty, n, ~ tibble(
        poradi = seq_along(.x),
        prutok = .x,
        pravdepodobnost = 100 * seq_along(.x) / .y
      ))
    ) %>%
    select(-hodnoty, -n) %>%
    unnest(fdc_df)
}

# === Příprava pozorovaných dat ===
data_pozor <- data_pozor %>%
  mutate(prutok = prutok)
fdc_pozor <- vytvor_fdc_data(data_pozor)

# === Příprava simulovaných dat ===
data_default <- data_default %>%
  rename(prutok = value, kodprofilu = kod_profilu)
fdc_sim <- vytvor_fdc_data(data_default, id_cols = "scenar") %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

# === Seznam profilů ===
profily <- unique(data_default$kodprofilu)

# === Generování grafů ===
walk(profily, function(profil) {
  nazev_profilu <- profily_info %>%
    filter(kodprofilu == profil) %>%
    mutate(popis = paste0("Profil ", kodprofilu, " ", nazev, " – ", tok)) %>%
    pull(popis)

  kombinace <- fdc_sim %>%
    filter(kodprofilu == profil) %>%
    distinct(obdobi, scenar_short)

  pwalk(kombinace, function(obdobi, scenar_short) {
    data_sim <- fdc_sim %>%
      filter(kodprofilu == profil,
             obdobi == !!obdobi,
             scenar_short == !!scenar_short)

    data_poz <- fdc_pozor %>%
      filter(kodprofilu == profil)

    p <- ggplot() +
      geom_line(data = data_sim, aes(x = pravdepodobnost, y = prutok, color = "Simulace"), linewidth = 1) +
      geom_line(data = data_poz, aes(x = pravdepodobnost, y = prutok, color = "Pozorování"), linewidth = 1, linetype = "dashed") +
      scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "black"), name = "Typ dat") +
      scale_y_log10(labels = scales::label_number()) +
      labs(title = paste("Čáry překročení -", scenar_short, "- Období", obdobi, "\n", nazev_profilu),
           x = "Pravděpodobnost překročení [%]",
           y = "Průtok [m³/s]") +
      theme_minimal() +
      theme(text = element_text(family = "Calibri"),
            panel.background = element_rect(fill = "white"),
            legend.position = "top")

    ggsave(file.path(output_dir, paste0("FDC_", profil, "_", obdobi, "_", scenar_short, ".png")),
           plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
  })
})

message("✅ FDC grafy byly úspěšně vytvořeny.")
