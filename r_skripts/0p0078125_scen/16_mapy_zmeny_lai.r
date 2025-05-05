# R/16_mapy_zmeny_lai.R

library(dplyr)
library(ggplot2)
library(sf)
library(readr)
library(stringr)
library(showtext)
library(sysfonts)

# === Font ===
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === Slozky ===
base_dir <- getwd()
vrstvy_dir <- file.path(base_dir, "vrstvy")
output_dir <- file.path(base_dir, "output/map_zmeny_lai")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# === Nacteni dat ===
dat_default <- readRDS("output/dlouhodoby_prumer_default.rds")
dat_lai <- readRDS("output/dlouhodoby_prumer_lai.rds")
souradnice <- readRDS("output/default_data_orez.rds") %>%
  select(kod_profilu, lon, lat) %>%
  distinct() %>%
  rename(kodprofilu = kod_profilu)

hranice_povodi <- st_read(file.path(vrstvy_dir, "hranice_povodi.geojson"), quiet = TRUE)
vodni_nadrze <- st_read(file.path(vrstvy_dir, "vodni_nadrze.geojson"), quiet = TRUE)
vodni_toky <- st_read(file.path(vrstvy_dir, "vodni_toky.geojson"), quiet = TRUE)

# === Uprava a spojeni ===
dat_default <- dat_default %>%
  rename(dl_rocni_default = dl_rocni_prumer) %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  ) %>%
  select(kodprofilu, obdobi, scenar_short, dl_rocni_default)

dat_lai <- dat_lai %>%
  rename(dl_rocni_lai = dl_rocni_prumer) %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

# === Vypocet zmeny ===
data_all <- dat_lai %>%
  left_join(dat_default, by = c("kodprofilu", "obdobi", "scenar_short")) %>%
  left_join(souradnice, by = "kodprofilu") %>%
  mutate(zmena_rel = 100 * (dl_rocni_lai - dl_rocni_default) / dl_rocni_default) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(5514)

# === Definice intervalu ===
zmena_breaks <- c(-Inf, -70, -50, -30, -10, 10, 30, 50, 70, Inf)
zmena_labels <- c("< -70 %", "-70 až -50 %", "-50 až -30 %", "-30 až -10 %",
                  "-10 až 10 %", "10 až 30 %", "30 až 50 %", "50 až 70 %", "> 70 %")
zmena_colors <- c("#8B0000", "#E31A1C", "#FB9A29", "#FFF176", "#FFFF00",
                  "#A5D6A7", "#66BB6A", "#388E3C", "#1B5E20")

# === Dummy body pro legendu ===
dummy_data <- tibble(
  kategorie = factor(zmena_labels, levels = zmena_labels),
  lon = -5000000 + seq_along(zmena_labels) * 10,
  lat = -5000000
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(5514)

# === Generovani map ===
obdobi_list <- unique(data_all$obdobi)
scenare <- unique(data_all$scenar_short)
lai_varianty <- unique(data_all$lai_variant)

for (scenar in scenare) {
  for (obdobi in obdobi_list) {
    for (varianta in lai_varianty) {
      data_plot <- data_all %>%
        filter(obdobi == !!obdobi, scenar_short == !!scenar, lai_variant == !!varianta) %>%
        mutate(
          kategorie = cut(zmena_rel, breaks = zmena_breaks, labels = zmena_labels, include.lowest = TRUE),
          kategorie = factor(kategorie, levels = zmena_labels, exclude = NULL)
        )

      p <- ggplot() +
        geom_sf(data = hranice_povodi, fill = NA, color = "black") +
        geom_sf(data = vodni_toky, color = "blue", linewidth = 0.3) +
        geom_sf(data = vodni_nadrze, fill = "cyan", color = "black", alpha = 0.5) +
        geom_sf(data = data_plot, aes(color = kategorie), size = 2) +
        geom_sf(data = dummy_data, aes(color = kategorie), size = 0, show.legend = TRUE) +
        scale_color_manual(
          values = setNames(zmena_colors, zmena_labels),
          limits = zmena_labels,
          drop = FALSE,
          name = "Změna [%]",
          guide = guide_legend(override.aes = list(size = 2))
        ) +
        labs(
          title = paste("Změna ročního průtoku –", scenar, obdobi, "LAI:", varianta),
          subtitle = "Relativní změna v % oproti výchozí variantě",
          x = NULL, y = NULL
        ) +
        theme_minimal() +
        theme(
          text = element_text(family = "Calibri", size = 26),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "right",
          plot.title = element_text(size = 30, face = "bold"),
          plot.subtitle = element_text(size = 24),
          plot.background = element_rect(fill = "white", color = NA)
        )

      out_dir <- file.path(output_dir, scenar, varianta)
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
      ggsave(file.path(out_dir, paste0("mapa_zmena_lai_", varianta, "_", obdobi, "_", scenar, ".png")),
             plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
    }
  }
}

message("\u2705 Mapy změn pro LAI varianty byly vytvořeny.")
