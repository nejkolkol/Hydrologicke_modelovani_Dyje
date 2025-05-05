# === 📦 Knihovny ===
library(sysfonts)
library(showtext)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)

# === 🔤 Font Calibri ===
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === 📁 Cesty ===
root_path <- "C:/Users/kolar/OneDrive/Desktop/simulace1975-2020"
slozka_data <- file.path(root_path, "1975-2020_0.0078125")
slozka_output <- file.path(root_path, "output_graphs")

rozliseni <- "0.0078125"
rozliseni_format <- gsub("\\.", "p", rozliseni)
rozliseni_zobraz <- rozliseni

# === 📄 Cesty k datům ===
geojson_path <- file.path(slozka_data, "output", "KGE_mapy", rozliseni_format,
                          paste0("vysledky_profilu_5514_", rozliseni_format, ".geojson"))
hranice_povodi <- st_read(file.path(slozka_data, "vrstvy", "hranice_povodi.geojson"), quiet = TRUE)
vodni_toky     <- st_read(file.path(slozka_data, "vrstvy", "vodni_toky.geojson"), quiet = TRUE)
vodni_nadrze   <- st_read(file.path(slozka_data, "vrstvy", "vodni_nadrze.geojson"), quiet = TRUE)

# === 📄 Načtení GeoJSON dat ===
KGE_all_results <- st_read(geojson_path, quiet = TRUE)

# === 📊 Definice metrik ===
intervals_colors <- list(
  KGE = list(
    breaks = c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf),
    colors = c(rgb(106,4,15,max=255), rgb(169,17,21,max=255), rgb(224,48,29,max=255),
               rgb(239,153,46,max=255), rgb(250,206,44,max=255), rgb(253,180,12,max=255),
               rgb(190,157,10,max=255), rgb(82,136,27,max=255), rgb(51,109,26,max=255),
               rgb(39,82,20,max=255)),
    labels = c("0-0.1","0.1-0.2","0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6",
               "0.6-0.7","0.7-0.8","0.8-0.9","0.9-1.0")
  ),
  Bias = list(
    breaks = c(-Inf, 0.4, 0.7, 0.9, 1.1, 1.3, 1.6, Inf),
    colors = c(rgb(190,10,255,max=255), rgb(54,67,250,max=255), rgb(10,239,255,max=255),
               rgb(85,255,81,max=255), rgb(222,255,10,max=255), rgb(255,173,0,max=255),
               rgb(255,0,0,max=255)),
    labels = c("0-0.4","0.4-0.7","0.7-0.8","0.9-1,1","1.1-1.3","1.3-1.6","1.6-2")
  )
)
intervals_colors$Pearson_Correlation <- intervals_colors$KGE
intervals_colors$Variability <- intervals_colors$Bias

get_metric_name <- function(metric) {
  switch(metric,
         "KGE" = "KGE",
         "Pearson_Correlation" = "Shoda v časové odezvě",
         "Bias" = "Systematická chyba",
         "Variability" = "Shoda ve variabilitě")
}

plot_metric_map <- function(metric, data_sf, bbox) {
  data_sf$category <- cut(
    data_sf[[metric]],
    breaks = intervals_colors[[metric]]$breaks,
    labels = intervals_colors[[metric]]$labels,
    include.lowest = TRUE
  )

  ggplot() +
    geom_sf(data = hranice_povodi, fill = NA, color = 'black', linewidth = 0.4) +
    geom_sf(data = vodni_toky, color = 'blue', linewidth = 0.3) +
    geom_sf(data = vodni_nadrze, fill = 'cyan', color = 'blue', alpha = 0.5, linewidth = 0.3) +
    geom_sf(data = data_sf, aes(color = category), size = 2) +
    scale_color_manual(
      values = intervals_colors[[metric]]$colors,
      breaks = intervals_colors[[metric]]$labels,
      labels = intervals_colors[[metric]]$labels,
      guide = "none"
    ) +
    labs(title = get_metric_name(metric)) +
    theme_void(base_family = "Calibri") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 30, vjust = 3, face = "bold")
    ) +
    coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)
}

create_legend <- function(metric) {
  labels <- intervals_colors[[metric]]$labels
  colors <- intervals_colors[[metric]]$colors
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

rocni_sf <- KGE_all_results %>%
  filter(typ_dat == "rocni" & (is.na(obdobi) | obdobi == "")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 5514)

bbox <- st_bbox(hranice_povodi)

p1 <- plot_metric_map("KGE", rocni_sf, bbox)
p2 <- plot_metric_map("Pearson_Correlation", rocni_sf, bbox)
p3 <- plot_metric_map("Bias", rocni_sf, bbox)
p4 <- plot_metric_map("Variability", rocni_sf, bbox)

legend1 <- create_legend("KGE")
legend2 <- create_legend("Bias")

row1 <- p1 | p2
row2 <- p3 | p4

final_plot <- (
  row1 / legend1 / row2 / legend2
) +
  plot_layout(heights = c(10, 1.2, 10, 1.2)) +
  plot_annotation(
    title = paste0("KGE parametry (rozlišení ", rozliseni_zobraz, "°)"),
    theme = theme(
      plot.title = element_text(family = "Calibri", size = 32, hjust = 0.5, face = "bold")
    )
  )

if (!dir.exists(slozka_output)) dir.create(slozka_output, recursive = TRUE)
ggsave(file.path(slozka_output, "srovnani_KGE_metric_rocni.png"), final_plot,
       width = 5.2, height = 6, dpi = 300, bg = "white")
