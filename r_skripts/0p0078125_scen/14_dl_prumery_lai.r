library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(purrr)
library(lubridate)

# === Nastavení ===
profil_kody <- c("463000", "469000", "480500")
lai_varianty <- c("default", "lai0p5", "lai6", "psenice", "samo")
scenare <- c("SSP126", "SSP245", "SSP370", "SSP585")
obdobi_list <- c("2030", "2050", "2070", "2085")

# === Načtení dat ===
dl_mesic_lai <- readRDS("output/dlouhodobe_mesicni_lai.rds")
dl_rocni_lai <- readRDS("output/dlouhodoby_prumer_lai.rds")
dl_mesic_def <- readRDS("output/dlouhodobe_mesicni_default.rds")
dl_rocni_def <- readRDS("output/dlouhodoby_prumer_default.rds")

profily_popis <- read_csv("vrstvy/P_ehled_profil_.csv", show_col_types = FALSE) %>%
  mutate(kodprofilu = as.character(kodprofilu))

ziskat_nazev_profilu <- function(kod) {
  profil <- profily_popis %>% filter(kodprofilu == kod)
  if (nrow(profil) == 1) {
    paste("Profil", kod, profil$nazev, "–", profil$tok)
  } else {
    paste("Profil", kod)
  }
}

# === Výstupní složky ===
output_base <- "output/lai_porovnani"
dir.create(output_base, showWarnings = FALSE)

# === Příprava dat ===
dl_mesic_lai <- dl_mesic_lai %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

dl_mesic_def <- dl_mesic_def %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+"),
    lai_variant = "default"
  )

dl_mesic <- bind_rows(dl_mesic_lai, dl_mesic_def)

dl_rocni_lai <- dl_rocni_lai %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

dl_rocni_def <- dl_rocni_def %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+"),
    lai_variant = "default"
  )

dl_rocni <- bind_rows(dl_rocni_lai, dl_rocni_def)

# === Vykreslení grafů ===
walk(profil_kody, function(profil) {
  nazev_profilu <- ziskat_nazev_profilu(profil)

  for (scenar_now in scenare) {
    out_dir <- file.path(output_base, scenar_now)
    dir.create(out_dir, showWarnings = FALSE)

    for (obdobi_now in obdobi_list) {

      # === Měsíční graf ===
      data_mes <- dl_mesic %>%
        filter(kodprofilu == profil, scenar_short == scenar_now, obdobi == obdobi_now) %>%
        mutate(lai_variant = factor(lai_variant, levels = lai_varianty))

      p_mes <- ggplot(data_mes, aes(x = mesic, y = prumer, color = lai_variant)) +
        geom_line(linewidth = 1) +
        scale_color_manual(
          values = c(
            "default" = "black",
            "lai0p5" = "#1b9e77",
            "lai6" = "#d95f02",
            "psenice" = "#7570b3",
            "samo" = "#e7298a"
          ),
          name = "LAI varianta"
        ) +
        scale_x_continuous(breaks = 1:12, labels = as.roman(1:12)) +
        labs(title = paste("Měsíční průměry -", nazev_profilu, scenar_now, obdobi_now),
             x = "Měsíc", y = "Průtok [m³/s]") +
        theme_minimal() +
        theme(
          text = element_text(family = "Calibri", size = 24),
          legend.position = "top",
          panel.background = element_rect(fill = "white"),
          axis.text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          plot.title = element_text(size = 26, face = "bold")
        )

      ggsave(file.path(out_dir, paste0("mesic_lai_", profil, "_", scenar_now, "_", obdobi_now, ".png")),
             plot = p_mes, width = 5.2, height = 3.8, dpi = 300, bg = "white")

      # === Roční graf ===
      data_rocni <- dl_rocni %>%
        filter(kodprofilu == profil, scenar_short == scenar_now, obdobi == obdobi_now) %>%
        mutate(lai_variant = factor(lai_variant, levels = lai_varianty))

      p_rocni <- ggplot(data_rocni, aes(x = lai_variant, y = dl_rocni_prumer, fill = lai_variant)) +
        geom_col(width = 0.6) +
        scale_fill_manual(
          values = c(
            "default" = "black",
            "lai0p5" = "#1b9e77",
            "lai6" = "#d95f02",
            "psenice" = "#7570b3",
            "samo" = "#e7298a"
          ),
          name = "LAI varianta"
        ) +
        labs(title = paste("Roční průměry -", nazev_profilu, scenar_now, obdobi_now),
             x = NULL, y = "Průtok [m³/s]") +
        theme_minimal() +
        theme(
          text = element_text(family = "Calibri", size = 24),
          legend.position = "top",
          panel.background = element_rect(fill = "white"),
          axis.text = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 22),
          plot.title = element_text(size = 26, face = "bold")
        )

      ggsave(file.path(out_dir, paste0("rocni_lai_", profil, "_", scenar_now, "_", obdobi_now, ".png")),
             plot = p_rocni, width = 5.2, height = 3.8, dpi = 300, bg = "white")
    }
  }
})

message("✅ Grafy pro LAI varianty včetně default byly vygenerovány.")
