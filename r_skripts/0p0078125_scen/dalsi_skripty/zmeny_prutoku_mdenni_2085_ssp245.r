# mapy_zmeny_mdenni_grid.R

library(dplyr)
library(ggplot2)
library(sf)
library(readr)
library(stringr)
library(patchwork)
library(showtext)
library(sysfonts)

# === 🖋️ Font ===
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === 📁 Složky ===
base_dir <- getwd()
vrstvy_dir <- file.path(base_dir, "vrstvy")
output_dir <- file.path(base_dir, "output/zmeny_mdenni_grid")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# === 📄 Data ===
default_data <- readRDS("output/default_data_orez.rds")
mdenni_sim <- readRDS("output/mdenni_prutoky_default.rds")
mdenni_pozor <- readRDS("output/mdenni_prutoky_pozorovani.rds")

# === 🌍 Vrstvy
hranice_povodi <- st_read(file.path(vrstvy_dir, "hranice_povodi.geojson"), quiet = TRUE)
vodni_nadrze <- st_read(file.path(vrstvy_dir, "vodni_nadrze.geojson"), quiet = TRUE)
vodni_toky <- st_read(file.path(vrstvy_dir, "vodni_toky.geojson"), quiet = TRUE)

# === 📌 Souřadnice
souradnice <- default_data %>%
  select(kod_profilu, lon, lat) %>%
  distinct() %>%
  rename(kodprofilu = kod_profilu)

# === 📊 Příprava dat
mdenni_sim <- mdenni_sim %>%
  rename(prutok_sim = mdenni_prutok) %>%
  mutate(
    m = paste0("M", m),
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

mdenni_pozor <- mdenni_pozor %>%
  rename(prutok_pozor = mdenni_prutok) %>%
  mutate(m = paste0("M", m))

data_all <- mdenni_sim %>%
  filter(scenar_short == "SSP245", obdobi %in% c("2030", "2070", "2085")) %>%
  left_join(mdenni_pozor, by = c("kodprofilu", "m")) %>%
  left_join(souradnice, by = "kodprofilu") %>%
  mutate(zmena_rel = 100 * (prutok_sim - prutok_pozor) / prutok_pozor) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(5514)

# === 🎨 Kategorie změn a barvy
zmena_breaks <- c(-Inf, -70, -50, -30, -10, 10, 30, 50, 70, Inf)
zmena_labels <- c(
  "< -70 %", "-70 až -50 %", "-50 až -30 %", "-30 až -10 %",
  "-10 až 10 %", "10 až 30 %", "30 až 50 %", "50 až 70 %", "> 70 %"
)
zmena_colors <- c(
  "#8B0000", "#E31A1C", "#FB9A29", "#FFF176", "#FFFF00",
  "#A5D6A7", "#66BB6A", "#388E3C", "#1B5E20"
)

# === 🗺️ Funkce pro vykreslení mapy
generuj_mapu <- function(m_kod, obdobi, titulek) {
  data_plot <- data_all %>%
    filter(m == m_kod, obdobi == !!obdobi) %>%
    mutate(
      kategorie = cut(zmena_rel,
                      breaks = zmena_breaks,
                      labels = zmena_labels,
                      include.lowest = TRUE),
      kategorie = factor(kategorie, levels = zmena_labels, exclude = NULL)
    )

  ggplot() +
    geom_sf(data = hranice_povodi, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = vodni_toky, color = "blue", linewidth = 0.3) +
    geom_sf(data = vodni_nadrze, fill = "cyan", color = "blue", alpha = 0.5, linewidth = 0.3) +
    geom_sf(data = data_plot, aes(color = kategorie), size = 2) +
    scale_color_manual(values = setNames(zmena_colors, zmena_labels), guide = "none", drop = FALSE) +
    labs(title = titulek) +
    theme_void(base_family = "Calibri") +
    theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
}

# === 🧭 Generuj 3x3 mapy (řádky = období, sloupce = M30, M180, M330)
obdobi_vec <- c("2030", "2070", "2085")
mdenni_vec <- c("M30", "M180", "M330")
nazvy_map <- c("Q\u2083\u2080", "Q\u2081\u2088\u2080", "Q\u2083\u2083\u2080")  # Q₃₀, Q₁₈₀, Q₃₃₀

map_list <- list()
for (obd in obdobi_vec) {
  for (i in seq_along(mdenni_vec)) {
    titulek <- paste0(nazvy_map[i], " – ", obd)
    map_list[[length(map_list) + 1]] <- generuj_mapu(mdenni_vec[i], obd, titulek)
  }
}

combined_maps <- wrap_plots(map_list, nrow = 3)

# === 📍 Manuální legenda
vytvor_legendu <- function() {
  dummy_df <- data.frame(
    x = seq_along(zmena_labels),
    y = 1,
    label = zmena_labels,
    kategorie = factor(zmena_labels, levels = zmena_labels)
  )

  ggplot(dummy_df, aes(x = x, y = y)) +
    geom_point(aes(color = kategorie), size = 2) +
    geom_text(aes(label = label), vjust = 2.5, size = 9, family = "Calibri") +
    scale_color_manual(values = zmena_colors, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    coord_cartesian(clip = "off") +
    theme_void(base_family = "Calibri") +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# === ✨ Spojení map + legenda
legenda_plot <- vytvor_legendu()

final_plot <- combined_maps / legenda_plot +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    title = "Relativní změna m-denních průtoků – SSP245",
    theme = theme(
      plot.title = element_text(size = 35, hjust = 0.5, family = "Calibri", face = "bold")
    )
  )

# === 💾 Uložení
ggsave(
  filename = file.path(output_dir, "mapy_zmeny_mdenni_grid_SSP245.png"),
  plot = final_plot, width = 5.2, height = 5.5, dpi = 300, bg = "white"
)

message("✅ Mapový výstup 3×3 změn m-denních průtoků byl vytvořen.")
