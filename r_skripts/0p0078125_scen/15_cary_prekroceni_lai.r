# R/15_cary_prekroceni_lai.R

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)

# === Cesty ===
output_base <- "output/cary_prekroceni_lai"
dir.create(output_base, showWarnings = FALSE, recursive = TRUE)

# === Načtení dat ===
data_default <- readRDS("output/default_data_orez.rds")
data_lai <- readRDS("output/lai_data_orez.rds")
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

# === Příprava dat ===
data_default <- data_default %>%
  rename(prutok = value, kodprofilu = kod_profilu) %>%
  mutate(varianta = "default")

data_lai <- data_lai %>%
  rename(prutok = value, kodprofilu = kod_profilu, varianta = lai_variant)

data_all <- bind_rows(data_default, data_lai) %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

# === Filtrace na 3 profily ===
vybrane_profily <- c("463000", "469000", "480500")
data_all <- data_all %>% filter(kodprofilu %in% vybrane_profily)

# === Vytvoření FDC dat ===
fdc_all <- vytvor_fdc_data(data_all, id_cols = c("scenar", "varianta", "obdobi", "scenar_short"))

# === Seznam variant ===
lai_varianty <- unique(data_lai$varianta)

# === Generování grafů ===
walk(vybrane_profily, function(profil) {
  nazev_profilu <- profily_info %>%
    filter(kodprofilu == profil) %>%
    mutate(popis = paste0("Profil ", kodprofilu, " ", nazev, " – ", tok)) %>%
    pull(popis)

  for (varianta_now in lai_varianty) {
    kombinace <- fdc_all %>%
      filter(kodprofilu == profil, varianta %in% c("default", varianta_now)) %>%
      distinct(obdobi, scenar_short)

    pwalk(kombinace, function(obdobi, scenar_short) {
      data_plot <- fdc_all %>%
        filter(
          kodprofilu == profil,
          scenar_short == !!scenar_short,
          obdobi == !!obdobi,
          varianta %in% c("default", varianta_now)
        ) %>%
        mutate(popis = ifelse(varianta == "default", "Default", varianta_now))

      out_dir <- file.path(output_base, scenar_short)
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

      p <- ggplot(data_plot, aes(x = pravdepodobnost, y = prutok, color = popis, linetype = popis)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(
    "Default" = "black",
    "lai0p5" = "#1b9e77",
    "lai6" = "#d95f02",
    "psenice" = "#7570b3",
    "samo" = "#e7298a"
  )) +
  scale_linetype_manual(values = c(
    "Default" = "dashed",
    "lai0p5" = "solid",
    "lai6" = "solid",
    "psenice" = "solid",
    "samo" = "solid"
  )) +
  scale_y_log10(labels = scales::label_number()) +
  labs(
    title = paste("Čáry překročení -", scenar_short, "- Období", obdobi, "-", nazev_profilu),
    x = "Pravděpodobnost překročení [%]",
    y = "Průtok [m³/s]",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Calibri", size = 26),
    plot.title = element_text(size = 30, face = "bold"),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "top"
  )

      ggsave(file.path(out_dir, paste0("FDC_lai_", profil, "_", scenar_short, "_", obdobi, "_", varianta_now, ".png")),
             plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
    })
  }
})

message("✅ FDC grafy pro LAI varianty byly úspěšně vytvořeny.")

