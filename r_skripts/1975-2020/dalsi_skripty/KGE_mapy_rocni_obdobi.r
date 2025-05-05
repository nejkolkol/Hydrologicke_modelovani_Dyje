library(sf)
library(ggplot2)
library(patchwork)
library(grid)
library(sysfonts)
library(showtext)

font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

base_dir <- "C:/Users/kolar/OneDrive/Desktop/simulace1975-2020"
rozliseni <- "0.0078125"
rozliseni_format <- "0p0078125"
obdobi_vec <- c("DJF", "MAM", "JJA", "SON")
nazvy_map <- paste("Období", obdobi_vec)

# Styl
intervals <- c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf)
labels <- c("0–0.1", "0.1–0.2", "0.2–0.3", "0.3–0.4", "0.4–0.5", "0.5–0.6", "0.6–0.7", "0.7–0.8", "0.8–0.9", "0.9–1.0")
colors <- colorRampPalette(c("darkred", "orange", "green"))(10)

# Funkce pro načtení vrstev
nacti_vrstvy <- function(cesta) {
  list(
    hranice = st_read(file.path(cesta, "vrstvy", "hranice_povodi.geojson"), quiet = TRUE),
    toky = st_read(file.path(cesta, "vrstvy", "vodni_toky.geojson"), quiet = TRUE),
    nadrze = st_read(file.path(cesta, "vrstvy", "vodni_nadrze.geojson"), quiet = TRUE)
  )
}

# Generování jedné mapy
generuj_mapu <- function(obdobi, titulek, slozka) {
  geojson_path <- file.path(slozka, "output", "KGE_mapy", rozliseni_format, paste0("vysledky_profilu_5514_", rozliseni_format, ".geojson"))
  vrstvy <- nacti_vrstvy(slozka)

  data <- st_read(geojson_path, quiet = TRUE) %>%
    filter(typ_dat == "mesic", obdobi == !!obdobi) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 5514) %>%
    mutate(category = cut(KGE, breaks = intervals, labels = labels, include.lowest = TRUE))

  ggplot() +
    geom_sf(data = vrstvy$hranice, fill = NA, color = "black", linewidth = 0.5) +
    geom_sf(data = vrstvy$toky, color = "blue", linewidth = 0.3) +
    geom_sf(data = vrstvy$nadrze, fill = "cyan", color = "blue", alpha = 0.5, linewidth = 0.3) +
    geom_sf(data = data, aes(color = category), size = 2) +
    scale_color_manual(values = colors, drop = FALSE, guide = "none") +
    labs(title = titulek) +
    theme_void(base_family = "Calibri") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, face = "bold", family = "Calibri")
    )
}

# Manuální legenda
vytvor_legendu <- function() {
  dummy_df <- data.frame(
    x = seq_along(labels),
    y = 1,
    label = labels,
    category = factor(labels, levels = labels)
  )

  ggplot(dummy_df, aes(x = x, y = y)) +
    geom_point(aes(color = category), size = 2) +
    geom_text(aes(label = label), vjust = 2.5, size = 9, family = "Calibri") +
    scale_color_manual(values = colors, guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
    coord_cartesian(clip = "off") +
    theme_void(base_family = "Calibri") +
    theme(plot.margin = margin(0, 0, 0, 0))
}

# Vygeneruj mapy a legendu
slozka <- file.path(base_dir, paste0("1975-2020_", rozliseni))
map_list <- map2(obdobi_vec, nazvy_map, ~generuj_mapu(.x, .y, slozka))
combined_maps <- wrap_plots(map_list, nrow = 2)
legenda_plot <- vytvor_legendu()

final_plot <- combined_maps / legenda_plot +
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    title = "KGE měsíční průtoky – rozlišení 0.0078125°",
    theme = theme(
      plot.title = element_text(size = 40, hjust = 0.5, family = "Calibri", face = "bold")
    )
  )

# Uložení
ggsave(
  filename = file.path(base_dir, "output_graphs", "KGE_mesicni_sezonni_srovnani_0p0078125.png"),
  plot = final_plot, width = 5.2, height = 5.5, dpi = 300, bg = "white"
)
