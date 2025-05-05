# cary_prekroceni_2085.R

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(tidyr)
library(patchwork)

# === Cesty ===
output_dir <- "output/cary_prekroceni_2085"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

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

# === Příprava dat ===
data_pozor <- data_pozor %>% mutate(prutok = prutok)
fdc_pozor <- vytvor_fdc_data(data_pozor)

data_default <- data_default %>%
  rename(prutok = value, kodprofilu = kod_profilu)

fdc_sim <- vytvor_fdc_data(data_default, id_cols = "scenar") %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

# === Seznam profilů ===
cilove_profily <- c(463000, 469000, 480500)
scenare <- c("SSP126", "SSP245", "SSP370", "SSP585")

# === Generování výstupů ===
for (profil in cilove_profily) {
  nazev_profilu <- profily_info %>%
    filter(kodprofilu == profil) %>%
    mutate(popis = paste0("Profil ", kodprofilu, " ", nazev, " – ", tok)) %>%
    pull(popis)

  grafy <- map(scenare, function(ssp) {
    data_sim <- fdc_sim %>%
      filter(kodprofilu == profil, obdobi == "2085", scenar_short == ssp)
    
    data_poz <- fdc_pozor %>%
      filter(kodprofilu == profil)

    ggplot() +
      geom_line(data = data_sim, aes(x = pravdepodobnost, y = prutok, color = "Simulace"), linewidth = 0.4) +
      geom_line(data = data_poz, aes(x = pravdepodobnost, y = prutok, color = "Pozorování"), linewidth = 0.4, linetype = "dashed") +
      scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "black"), name = NULL) +
      scale_y_log10(labels = scales::label_number()) +
      labs(title = ssp, x = "Pravděpodobnost překročení [%]", y = "Průtok [m³/s]") +
      theme_minimal(base_family = "Calibri") +
      theme(
        legend.position = "top",
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        panel.grid.minor = element_blank()
      )
  })

    vystup <- (grafy[[1]] + grafy[[2]]) / (grafy[[3]] + grafy[[4]]) +
    plot_annotation(
      title = paste0(nazev_profilu, " – srovnání SSP období 2085"),
      theme = theme(
        plot.title = element_text(size = 12, family = "Calibri", face = "bold", hjust = 0.5)
      )
    )


  ggsave(file.path(output_dir, paste0("FDC_2085_", profil, ".png")),
         plot = vystup, width = 5.2, height = 5.2, dpi = 300, bg = "white")
}

message("✅ Výstupy pro rok 2085 byly úspěšně vytvořeny.")
