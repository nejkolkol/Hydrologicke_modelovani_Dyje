# 📦 Balíčky
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(showtext)
library(sysfonts)

# === Font ===
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === Nastavení ===
profil_kody <- c("463000", "469000", "480500")
scenare <- c("SSP126", "SSP245", "SSP370", "SSP585")
obdobi_list <- c("2030", "2050", "2070", "2085")

# === Cesty ===
data_pozor <- readRDS("output/pozorovani.rds")
data_sim <- readRDS("output/default_data_orez.rds")

# === Názvy profilů ===
profily_info <- read_csv("vrstvy/P_ehled_profil_.csv", show_col_types = FALSE) %>%
  mutate(kodprofilu = as.character(kodprofilu))

# === Výstupní složka ===
dir_out <- "output/boxplot_scenare"
dir.create(dir_out, showWarnings = FALSE)

# === Příprava pozorování ===
data_pozor <- data_pozor %>%
  mutate(scenar_short = "Pozorovani")

# === Smyčka přes období a profily ===
for (obdobi_now in obdobi_list) {
  data_sim_obdobi <- data_sim %>%
    mutate(
      obdobi = str_extract(scenar, "^\\d{4}"),
      scenar_short = str_extract(scenar, "SSP\\d+")
    ) %>%
    filter(obdobi == obdobi_now)

  for (profil in profil_kody) {
    data_sim_f <- data_sim_obdobi %>% filter(kod_profilu == profil)
    data_poz_f <- data_pozor %>% filter(kodprofilu == profil)

    data_all <- bind_rows(
      data_sim_f %>% rename(prutok = value),
      data_poz_f %>% select(prutok, scenar_short, kodprofilu = kodprofilu)
    ) %>%
      mutate(scenar_short = factor(scenar_short, levels = c(scenare, "Pozorovani")))

    # 🏷 Titulek s názvem profilu
    popis <- profily_info %>%
      filter(kodprofilu == profil) %>%
      mutate(popis = paste0("Rozptyl denních průtoků Profil ", kodprofilu, " ", nazev, " – ", tok)) %>%
      pull(popis)

    # 📊 Graf
    p <- ggplot(data_all, aes(x = scenar_short, y = prutok, fill = scenar_short)) +
      geom_boxplot(width = 0.4, color = "black", outlier.shape = NA) +
      scale_fill_manual(
        values = c(
          "SSP126" = "#1b9e77",
          "SSP245" = "#d95f02",
          "SSP370" = "#7570b3",
          "SSP585" = "#e7298a",
          "Pozorovani" = "grey50"
        ),
        name = "Scénář",
        drop = FALSE
      ) +
      scale_y_log10(labels = scales::label_number()) +
      labs(
        title = paste0(popis, " Období ", obdobi_now),
        x = NULL,
        y = "Průtok [m³/s]"
      ) +
      theme_minimal() +
      theme(
        text = element_text(family = "Calibri", size = 24),
        legend.position = "top",
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22),
        plot.title = element_text(size = 26, face = "bold")
      )

    # 💾 Uložení
    ggsave(
      file.path(dir_out, paste0("boxplot_denni_scenare_", profil, "_", obdobi_now, ".png")),
      plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white"
    )
  }
}

message("✅ Boxploty pro denní průtoky byly úspěšně vytvořeny.")
