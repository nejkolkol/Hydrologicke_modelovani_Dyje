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
dir_out <- "output/boxplot_obdobi"
dir.create(dir_out, showWarnings = FALSE)

# === Příprava dat ===
data_sim <- data_sim %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

data_pozor <- data_pozor %>%
  mutate(obdobi = "1980–2010")

# === Kombinace ===
for (profil in profil_kody) {
  for (scenar_now in scenare) {
    data_sim_f <- data_sim %>%
      filter(kod_profilu == profil, scenar_short == scenar_now)

    data_poz_f <- data_pozor %>%
      filter(kodprofilu == profil) %>%
      mutate(scenar_short = scenar_now)

    data_all <- bind_rows(
      data_sim_f %>% rename(prutok = value),
      data_poz_f %>% select(prutok, obdobi, scenar_short)
    ) %>%
      mutate(
        obdobi = factor(obdobi, levels = c("1980–2010", obdobi_list))
      )

    # 🏷 Titulek s názvem profilu
    popis <- profily_info %>%
      filter(kodprofilu == profil) %>%
      mutate(popis = paste0("Profil ", kodprofilu, " ", nazev, " – ", tok)) %>%
      pull(popis)

    # 📊 Graf
    p <- ggplot(data_all, aes(x = obdobi, y = prutok, fill = obdobi)) +
      geom_boxplot(width = 0.6, outlier.shape = NA, color = "black") +
      scale_y_log10(labels = scales::label_number()) +
      scale_fill_manual(
        values = c(
          "1980–2010" = "grey40",
          "2030" = "#66c2a5",
          "2050" = "#fc8d62",
          "2070" = "#8da0cb",
          "2085" = "#e78ac3"
        ),
        name = "Období"
      ) +
      labs(
        title = paste0("Rozptyl denních průtoků ", popis, " Scénář ", scenar_now),
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

    # 💾 Uložení výstupu
    ggsave(file.path(dir_out, paste0("boxplot_denni_obdobi_", profil, "_", scenar_now, ".png")),
           plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
  }
}

message("✅ Boxploty pro různá období byly úspěšně vytvořeny.")
