# R/11_mesicni_porovnani_scenare.R

library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(stringr)
library(lubridate)

# === Nastavení ===
profil_kody <- c("463000", "469000", "480500")
scenare <- c("SSP126", "SSP245", "SSP370", "SSP585")
obdobi_list <- c("2030", "2050", "2070", "2085")

# Výstupní složky
output_dir <- "output/mesicni_prumery_porovnani_scenare"
dir.create(output_dir, showWarnings = FALSE)

# === Načtení dat ===
dl_mesic_default <- readRDS("output/dlouhodobe_mesicni_default.rds")
dl_mesic_pozor <- readRDS("output/dlouhodobe_mesicni_pozorovani.rds")

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

# === Úprava dat ===
dl_mesic_default <- dl_mesic_default %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

# === Vykreslení grafů ===
walk(profil_kody, function(profil) {
  nazev_profilu <- ziskat_nazev_profilu(profil)

  walk(scenare, function(scenar_now) {
    data_sim <- dl_mesic_default %>%
      filter(kodprofilu == profil, scenar_short == scenar_now) %>%
      mutate(obdobi = factor(obdobi, levels = obdobi_list))

    data_poz <- dl_mesic_pozor %>%
  filter(kodprofilu == profil) %>%
  mutate(obdobi = factor("Pozorování", levels = c("Pozorování", obdobi_list)))


    data_all <- bind_rows(data_sim, data_poz)

    p <- ggplot(data_all, aes(x = mesic, y = prumer, color = obdobi)) +
      geom_line(linewidth = 1) +
      scale_color_manual(
        values = c("Pozorování" = "black", "2030" = "#66c2a5", "2050" = "#fc8d62",
                   "2070" = "#8da0cb", "2085" = "#e78ac3"),
        name = "Období"
      ) +
      scale_x_continuous(breaks = 1:12, labels = as.roman(1:12)) +
      labs(title = paste("Průměrné měsíční průtoky -", nazev_profilu, "- Scénář", scenar_now),
           x = "Měsíc", y = "Průtok [m³/s]") +
      theme_minimal() +
      theme(
  text = element_text(family = "Calibri", size = 26),
  legend.position = "top",
  panel.background = element_rect(fill = "white"),
  axis.title = element_text(size = 24),
  axis.text = element_text(size = 20),
  legend.text = element_text(size = 20),
  legend.title = element_text(size = 22),
  plot.title = element_text(size = 28, face = "bold")
)

    ggsave(file.path(output_dir, paste0("mesic_srovnani_", profil, "_", scenar_now, ".png")),
           plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
  })
})

message("\u2705 Měsíční srovnání napříč obdobími bylo úspěšně vytvořeno.")