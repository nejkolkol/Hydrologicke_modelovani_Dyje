# R/07_dl_prumery_srovnani.R

library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(stringr)
library(lubridate)

# === Nastavení ===
profil_kody <- c("463000", "469000", "480500")
obdobi_list <- c("2030", "2050", "2070", "2085")
scenare <- c("SSP126", "SSP245", "SSP370", "SSP585")

# Výstupní složky
output_dir_mesic <- "output/mesicni_prumerydlouhodobe_grafy"
output_dir_rocni <- "output/rocni_prumerydlouhodobe_grafy"
dir.create(output_dir_mesic, showWarnings = FALSE)
dir.create(output_dir_rocni, showWarnings = FALSE)

# === Načtení dat ===
dl_mesic_default <- readRDS("output/dlouhodobe_mesicni_default.rds")
dl_mesic_pozor <- readRDS("output/dlouhodobe_mesicni_pozorovani.rds")
dl_rocni_default <- readRDS("output/dlouhodoby_prumer_default.rds")
dl_rocni_pozor <- readRDS("output/dlouhodoby_prumer_pozorovani.rds")
default_data <- readRDS("output/default_data_orez.rds")

# === Načtení názvů profilů ===
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

# Pomocná funkce
ziskat_obdobi <- function(scenar) {
  str_extract(scenar, "^\\d{4}")
}

# === Měsíční grafy ===
dl_mesic_default <- dl_mesic_default %>%
  mutate(
    obdobi = ziskat_obdobi(scenar),
    scenar_short = str_extract(scenar, "SSP\\d+"),
    scenar_short = factor(scenar_short, levels = scenare)
  )

dl_mesic_pozor <- dl_mesic_pozor %>%
  mutate(scenar_short = "Pozorování")

walk(profil_kody, function(profil) {
  nazev_profilu <- ziskat_nazev_profilu(profil)
  for (obdobi in obdobi_list) {
    data_sim <- dl_mesic_default %>%
      filter(kodprofilu == profil, obdobi == !!obdobi)

    data_poz <- dl_mesic_pozor %>%
      filter(kodprofilu == profil)

    data_all <- bind_rows(data_sim, data_poz) %>%
      mutate(
        scenar_short = factor(scenar_short, levels = c(scenare, "Pozorování"))
      )

    p <- ggplot(data_all, aes(x = mesic, y = prumer, color = scenar_short)) +
      geom_line(linewidth = 1) +
      scale_color_manual(
        values = c("SSP126" = "#1b9e77", "SSP245" = "#d95f02", "SSP370" = "#7570b3", "SSP585" = "#e7298a", "Pozorování" = "black"),
        name = "Legenda"
      ) +
      scale_x_continuous(breaks = 1:12, labels = as.roman(1:12)) +
      labs(title = paste("Dlouhodobý měsíční průměr", "Období", obdobi,"\n",nazev_profilu),
           x = "Měsíc", y = "Průtok [m³/s]") +
      theme_minimal() +
      theme(text = element_text(family = "Calibri"),
            legend.position = "top",
            panel.background = element_rect(fill = "white"))

    ggsave(file.path(output_dir_mesic, paste0("mesic_dl_prumer_", profil, "_", obdobi, ".png")),
           plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
  }
})

# === Sloupcový graf dlouhodobých ročních průměrů ===
dl_rocni_default <- dl_rocni_default %>%
  mutate(
    obdobi = ziskat_obdobi(scenar),
    scenar_short = factor(str_extract(scenar, "SSP\\d+"), levels = scenare)
  )

pozor_barva <- "grey30"
dl_rocni_pozor_fake <- dl_rocni_pozor %>%
  mutate(
    scenar_short = factor("Pozorování", levels = c(scenare, "Pozorování")),
    obdobi = "1980–2010"
  )

dl_rocni_all <- bind_rows(dl_rocni_default, dl_rocni_pozor_fake)

walk(profil_kody, function(profil) {
  nazev_profilu <- ziskat_nazev_profilu(profil)
  data_plot <- dl_rocni_all %>% filter(kodprofilu == profil)

  p <- ggplot() +
    geom_col(
      data = filter(data_plot, scenar_short != "Pozorování"),
      aes(x = scenar_short, y = dl_rocni_prumer, fill = obdobi),
      position = position_dodge(width = 0.8), width = 0.7
    ) +
    geom_col(
      data = filter(data_plot, scenar_short == "Pozorování"),
      aes(x = scenar_short, y = dl_rocni_prumer),
      fill = pozor_barva, width = 0.2
    ) +
    scale_fill_manual(
      values = c("1980–2010" = pozor_barva, "2030" = "#66c2a5", "2050" = "#fc8d62",
                 "2070" = "#8da0cb", "2085" = "#e78ac3"),
      name = "Období"
    ) +
    labs(title = paste("Dlouhodobé roční průměry -", nazev_profilu),
         x = "Scénář", y = "Průměrný roční průtok [m³/s]") +
    theme_minimal() +
    theme(
      text = element_text(family = "Calibri"),
      legend.position = "top",
      panel.background = element_rect(fill = "white")
    )

  ggsave(file.path(output_dir_rocni, paste0("rocni_dl_prumer_", profil, ".png")),
         plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
})

message("✅ Výstupy ze skriptu 07 úspěšně vytvořeny.")
