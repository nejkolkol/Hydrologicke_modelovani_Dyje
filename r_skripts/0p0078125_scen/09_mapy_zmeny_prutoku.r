# R/09_mapy_zmeny_prutoku.R

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

# === Složky ===
base_dir <- getwd()
vrstvy_dir <- file.path(base_dir, "vrstvy")
output_dir <- file.path(base_dir, "output/map_zmeny")
dir.create(output_dir, showWarnings = FALSE)

# === Načtení dat ===
default_data <- readRDS("output/default_data_orez.rds")
dlouhodoby_pozor <- readRDS("output/dlouhodoby_prumer_pozorovani.rds")
dlouhodoby_sim <- readRDS("output/dlouhodoby_prumer_default.rds")

# === Načtení vrstev ===
hranice_povodi <- st_read(file.path(vrstvy_dir, "hranice_povodi.geojson"), quiet = TRUE)
vodni_nadrze <- st_read(file.path(vrstvy_dir, "vodni_nadrze.geojson"), quiet = TRUE)
vodni_toky <- st_read(file.path(vrstvy_dir, "vodni_toky.geojson"), quiet = TRUE)

# === Přidání souřadnic ===
souradnice <- default_data %>%
  select(kod_profilu, lon, lat) %>%
  distinct() %>%
  rename(kodprofilu = kod_profilu)

# === Příprava a výpočet změny ===
dlouhodoby_sim <- dlouhodoby_sim %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    scenar_short = str_extract(scenar, "SSP\\d+")
  )

# Spojení dat, přejmenování a výpočet změny
data_all <- dlouhodoby_sim %>%
  rename(dl_rocni_sim = dl_rocni_prumer) %>%
  left_join(dlouhodoby_pozor %>% rename(dl_rocni_pozor = dl_rocni_prumer), by = "kodprofilu") %>%
  left_join(souradnice, by = "kodprofilu") %>%
  mutate(zmena_rel = 100 * (dl_rocni_sim - dl_rocni_pozor) / dl_rocni_pozor) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(5514)

# === Generování map ===
# === Definice kategorií změn ===
zmena_breaks <- c(-Inf, -70, -50, -30, -10, 10, 30, 50, 70, Inf)
zmena_labels <- c(
  "< -70 %", "-70 až -50 %", "-50 až -30 %", "-30 až -10 %",
  "-10 až 10 %", "10 až 30 %", "30 až 50 %", "50 až 70 %", "> 70 %"
)

# Upravená barevná paleta
zmena_colors <- c(
  "#8B0000",  # < -70 % (tmavě červená)
  "#E31A1C",  # -70 až -50 %
  "#FB9A29",  # -50 až -30 %
  "#FFF176",  # -30 až -10 %
  "#FFFF00",  # -10 až 10 % (žlutá)
  "#A5D6A7",  # 10 až 30 % (světle zelená)
  "#66BB6A",  # 30 až 50 %
  "#388E3C",  # 50 až 70 %
  "#1B5E20"   # > 70 %
)

# === Dummy body pro legendu ===
dummy_data <- tibble(
  kategorie = factor(zmena_labels, levels = zmena_labels),
  lon = -5000000 + seq_along(zmena_labels) * 10,
  lat = -5000000
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(5514)


obdobi_list <- unique(data_all$obdobi)
scenare <- unique(data_all$scenar_short)

for (obdobi in obdobi_list) {
  for (scenar in scenare) {
    data_plot <- data_all %>%
      filter(obdobi == !!obdobi, scenar_short == !!scenar) %>%
      mutate(kategorie = cut(zmena_rel,
                             breaks = zmena_breaks,
                             labels = zmena_labels,
                             include.lowest = TRUE))

    p <- ggplot() +
  geom_sf(data = hranice_povodi, fill = NA, color = "black") +
  geom_sf(data = vodni_toky, color = "blue", linewidth = 0.3) +
  geom_sf(data = vodni_nadrze, fill = "cyan", color = "black", alpha = 0.5) +
  geom_sf(data = data_plot, aes(color = kategorie), size = 2) +
  geom_sf(data = dummy_data, aes(color = kategorie), size = 0, show.legend = TRUE) +  # 👈 dummy
  scale_color_manual(
    values = setNames(zmena_colors, zmena_labels),
    limits = zmena_labels,
    drop = FALSE,
    name = "Změna [%]",
    guide = guide_legend(override.aes = list(size = 2))
  ) +
  labs(
    title = paste("Změna průměrného ročního průtoku – Období", obdobi, scenar),
    subtitle = "Relativní změna v % oproti pozorování",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Calibri", base_size = 26) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    plot.title = element_text(size = 30, face = "bold"),
    plot.subtitle = element_text(size = 24)
  )



    ggsave(file.path(output_dir, paste0("mapa_zmena_prutoku_", obdobi, "_", scenar, ".png")),
       plot = p, width = 5.2, height = 3.8, dpi = 300, bg = "white")

  }
}


message("✅ Mapy změny průměrného ročního průtoku byly vytvořeny.")
