# R/17_mapy_zmeny_prutoku_2085.R

library(dplyr)
library(ggplot2)
library(sf)
library(readr)
library(stringr)
library(patchwork)
library(sysfonts)
library(showtext)

# === Font Calibri ===
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === Cesty ===
base_dir <- getwd()
vrstvy_dir <- file.path(base_dir, "vrstvy")
output_dir <- file.path(base_dir, "output/zmeny_prutoku_2085")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# === Vstupní data ===
default_data <- readRDS("output/default_data_orez.rds")
dlouhodoby_pozor <- readRDS("output/dlouhodoby_prumer_pozorovani.rds")
dlouhodoby_sim <- readRDS("output/dlouhodoby_prumer_default.rds")

# === Vrstvy ===
hranice_povodi <- st_read(file.path(vrstvy_dir, "hranice_povodi.geojson"), quiet = TRUE)
vodni_nadrze <- st_read(file.path(vrstvy_dir, "vodni_nadrze.geojson"), quiet = TRUE)
vodni_toky <- st_read(file.path(vrstvy_dir, "vodni_toky.geojson"), quiet = TRUE)

# === Souřadnice ===
souradnice <- default_data %>%
  select(kod_profilu, lon, lat) %>%
  distinct() %>%
  rename(kodprofilu = kod_profilu)

# === Výpočet relativní změny ===
dlouhodoby_sim <- dlouhodoby_sim %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

data_all <- dlouhodoby_sim %>%
  filter(obdobi == "2085") %>%
  rename(dl_rocni_sim = dl_rocni_prumer) %>%
  left_join(dlouhodoby_pozor %>% rename(dl_rocni_pozor = dl_rocni_prumer), by = "kodprofilu") %>%
  left_join(souradnice, by = "kodprofilu") %>%
  mutate(zmena_rel = 100 * (dl_rocni_sim - dl_rocni_pozor) / dl_rocni_pozor) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(5514)

# === Kategorie změn a barvy ===
zmena_breaks <- c(-Inf, -70, -50, -30, -10, 10, 30, 50, 70, Inf)
zmena_labels <- c(
  "< -70 %", "-70 až -50 %", "-50 až -30 %", "-30 až -10 %",
  "-10 až 10 %", "10 až 30 %", "30 až 50 %", "50 až 70 %", "> 70 %"
)
zmena_colors <- c(
  "#8B0000", "#E31A1C", "#FB9A29", "#FFF176", "#FFFF00",
  "#A5D6A7", "#66BB6A", "#388E3C", "#1B5E20"
)

# === Funkce pro mapu jednoho scénáře ===
generuj_mapu <- function(ssp, titulek) {
  data_plot <- data_all %>%
    filter(scenar_short == !!ssp) %>%
    mutate(kategorie = cut(zmena_rel,
                           breaks = zmena_breaks,
                           labels = zmena_labels,
                           include.lowest = TRUE))

  ggplot() +
    geom_sf(data = hranice_povodi, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = vodni_toky, color = "blue", linewidth = 0.3) +
    geom_sf(data = vodni_nadrze, fill = "cyan", color = "blue", alpha = 0.5, linewidth = 0.3) +
    geom_sf(data = data_plot, aes(color = kategorie), size = 2) +
    scale_color_manual(values = setNames(zmena_colors, zmena_labels), guide = "none", drop = FALSE) +
    labs(title = titulek) +
    theme_void(base_family = "Calibri") +
    theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"))
}

# === Generuj mapy pro 4 SSP scénáře a spoj ===
ssp_vec <- c("SSP126", "SSP245", "SSP370", "SSP585")
nazvy_map <- paste("Scénář", ssp_vec)
map_list <- map2(ssp_vec, nazvy_map, ~generuj_mapu(.x, .y))
combined_maps <- wrap_plots(map_list, nrow = 2)

# === Manuální legenda ===
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

# === Spojení map a legendy ===
legenda_plot <- vytvor_legendu()

final_plot <- combined_maps / legenda_plot +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    title = "Relativní změna průměrného ročního průtoku – období 2085",
    theme = theme(
      plot.title = element_text(size = 40, hjust = 0.5, family = "Calibri", face = "bold")
    )
  )

# === Uložení ===
ggsave(
  filename = file.path(output_dir, "mapy_zmeny_prutoku_2085.png"),
  plot = final_plot, width = 5.2, height = 5.5, dpi = 300, bg = "white"
)

message("✅ Mapový výstup změny průtoků pro období 2085 byl úspěšně vytvořen.")
