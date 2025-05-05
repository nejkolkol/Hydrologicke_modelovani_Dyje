library(sf)
library(ggplot2)
library(patchwork)
library(grid)
library(sysfonts)
library(showtext)
library(purrr)

font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === 📁 Cesty a vstupy ===
base_dir <- "C:/Users/kolar/OneDrive/Desktop/simulace1975-2020"
rozliseni <- "0.0078125"
rozliseni_format <- gsub("\\.", "p", rozliseni)
slozka <- file.path(base_dir, paste0("1975-2020_", rozliseni))

nazvy_map <- c("Roční průměry", "Denní hodnoty")
typy_dat <- c("rocni", "denni")

# === Styl ===
intervals <- c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf)
labels <- c("0–0.1", "0.1–0.2", "0.2–0.3", "0.3–0.4", "0.4–0.5",
            "0.5–0.6", "0.6–0.7", "0.7–0.8", "0.8–0.9", "0.9–1.0")
colors <- colorRampPalette(c("darkred", "orange", "green"))(10)

# === Vrstvy ===
nacti_vrstvy <- function(cesta) {
  list(
    hranice = st_read(file.path(cesta, "vrstvy", "hranice_povodi.geojson"), quiet = TRUE),
    toky = st_read(file.path(cesta, "vrstvy", "vodni_toky.geojson"), quiet = TRUE),
    nadrze = st_read(file.path(cesta, "vrstvy", "vodni_nadrze.geojson"), quiet = TRUE)
  )
}

# === Jedna mapa bez legendy ===
generuj_mapu <- function(typ_dat, nazev_popisek, slozka, r_format) {
  geojson_path <- file.path(slozka, "output", "KGE_mapy", r_format, paste0("vysledky_profilu_5514_", r_format, ".geojson"))
  vrstvy <- nacti_vrstvy(slozka)

  data <- st_read(geojson_path, quiet = TRUE) %>%
    filter(typ_dat == !!typ_dat & (is.na(obdobi) | obdobi == "")) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 5514) %>%
    mutate(category = cut(KGE, breaks = intervals, labels = labels, include.lowest = TRUE))

  ggplot() +
    geom_sf(data = vrstvy$hranice, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = vrstvy$toky, color = "blue", linewidth = 0.3) +
    geom_sf(data = vrstvy$nadrze, fill = "cyan", color = "blue", alpha = 0.5, linewidth = 0.3) +
    geom_sf(data = data, aes(color = category), size = 2) +
    scale_color_manual(values = colors, drop = FALSE, guide = "none") +
    labs(title = nazev_popisek) +
    theme_void(base_family = "Calibri") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 28, face = "bold", family = "Calibri")
    )
}

# === Manuální legenda jako řádek barev a popisků ===
vytvor_legendu <- function() {
  dummy_df <- data.frame(
    x = seq_along(labels),
    y = 1,
    label = labels,
    category = factor(labels, levels = labels)
  )

  ggplot(dummy_df, aes(x = x, y = y)) +
    geom_point(aes(color = category), size = 2) +
    geom_text(aes(label = label), vjust = 2.5, size = 8, family = "Calibri") +
    scale_color_manual(values = colors, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    coord_cartesian(clip = "off") +
    theme_void(base_family = "Calibri") +
    theme(
      plot.margin = margin(0, 0, 0, 0)
    )
}

# === Generování map ===
map_list <- map2(typy_dat, nazvy_map, ~generuj_mapu(.x, .y, slozka, rozliseni_format))

# === Kombinace dvou map vedle sebe ===
combined_maps <- wrap_plots(map_list, nrow = 1)

# === Vytvoření legendy ===
legenda_plot <- vytvor_legendu()

# === Finální sestavení ===
final_plot <- combined_maps / legenda_plot +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    title = "KGE – roční průměry vs. denní hodnoty (rozlišení 0.0078125°)",
    theme = theme(
      plot.title = element_text(size = 36, hjust = 0.5, family = "Calibri", face = "bold")
    )
  )

# === Uložení ===
ggsave(
  filename = file.path(base_dir, "output_graphs", "KGE_rocni_vs_denni_srovnani.png"),
  plot = final_plot, width = 7.5, height = 4.8, dpi = 300, bg = "white"
)
