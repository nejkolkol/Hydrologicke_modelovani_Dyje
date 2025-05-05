# === 📦 Knihovny ===
library(sysfonts)
library(showtext)
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)

# === 🔤 Font Calibri ===
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === 📁 Rozlišení ze složky ===
nazev_slozky <- basename(getwd())
rozliseni <- str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)  # pro názvy souborů/složek
rozliseni_zobraz <- rozliseni  # pro subtitle

# === 📄 Cesty ===
geojson_path <- file.path("output", "KGE_mapy", rozliseni_format,
                          paste0("vysledky_profilu_5514_", rozliseni_format, ".geojson"))

hranice_povodi <- st_read("vrstvy/hranice_povodi.geojson", quiet = TRUE)
vodni_toky     <- st_read("vrstvy/vodni_toky.geojson", quiet = TRUE)
vodni_nadrze   <- st_read("vrstvy/vodni_nadrze.geojson", quiet = TRUE)

# === 📄 Načtení GeoJSON dat ===
KGE_all_results <- st_read(geojson_path, quiet = TRUE)

# === 📊 Definice metrik ===
metrics <- c('KGE', 'Pearson_Correlation', 'Bias', 'Variability')
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
  Pearson_Correlation = list( # stejné jako KGE
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
  ),
  Variability = list( # stejné jako Bias
    breaks = c(-Inf, 0.4, 0.7, 0.9, 1.1, 1.3, 1.6, Inf),
    colors = c(rgb(190,10,255,max=255), rgb(54,67,250,max=255), rgb(10,239,255,max=255),
               rgb(85,255,81,max=255), rgb(222,255,10,max=255), rgb(255,173,0,max=255),
               rgb(255,0,0,max=255)),
    labels = c("0-0.4","0.4-0.7","0.7-0.8","0.9-1,1","1.1-1.3","1.3-1.6","1.6-2")
  )
)

# === 🏷️ Funkce ===
get_metric_name <- function(metric) {
  switch(metric,
         "KGE" = "KGE",
         "Pearson_Correlation" = "Shoda v časové odezvě",
         "Bias" = "Systematická chyba",
         "Variability" = "Shoda ve variabilitě")
}

get_data_type_label <- function(typ_dat) {
  if (grepl("mesic", typ_dat)) {
    "měsíční průměry"
  } else if (typ_dat == "rocni") {
    "roční průměry"
  } else if (typ_dat == "denni") {
    "denní průměry"
  } else {
    typ_dat
  }
}

create_dummy_points <- function(metric) {
  categories <- intervals_colors[[metric]]$labels
  dummy_df <- data.frame(
    lon = rep(0, length(categories)),
    lat = rep(0, length(categories)),
    category = factor(categories, levels = categories)
  )
  st_as_sf(dummy_df, coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 5514)
}

# === 🖼️ Generování grafů ===
for (period in unique(KGE_all_results$obdobi)) {
  if (is.na(period) || period == "") {
    period_results <- KGE_all_results %>% filter(typ_dat == "rocni" & (is.na(obdobi) | obdobi == ""))
    period_label <- "cely_rok"
  } else {
    period_results <- KGE_all_results %>% filter(obdobi == period)
    period_label <- period
  }
  
  if (nrow(period_results) == 0) next
  
  for (data_type in unique(period_results$typ_dat)) {
    data_type_results <- period_results %>% filter(typ_dat == data_type)
    if (nrow(data_type_results) == 0) next
    
    data_type_sf <- st_as_sf(data_type_results, coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs = 5514)
    
    bbox <- st_bbox(hranice_povodi)
    
    for (metric in metrics) {
      data_type_sf$category <- cut(
        data_type_sf[[metric]],
        breaks = intervals_colors[[metric]]$breaks,
        labels = intervals_colors[[metric]]$labels,
        include.lowest = TRUE
      )
      
      title_text <- paste0(
        get_metric_name(metric), ", ", get_data_type_label(data_type),
        ifelse(data_type == "rocni", "", paste0(" pro období ", period_label))
      )
      
      file_name <- paste0(metric, "_", data_type, "_", period_label, ".png")
      
      p <- ggplot() +
        geom_sf(data = hranice_povodi, fill = NA, color = 'black', size = 0.5) +
        geom_sf(data = vodni_toky, color = 'blue', size = 0.3) +
        geom_sf(data = vodni_nadrze, fill = 'cyan', color = 'black', size = 0.8, alpha = 0.5) +
        geom_sf(data = data_type_sf, aes(color = category), size = 2) +
        geom_sf(data = create_dummy_points(metric), aes(color = category), size = 2, show.legend = TRUE) +
        scale_color_manual(
          values = intervals_colors[[metric]]$colors,
          breaks = intervals_colors[[metric]]$labels,
          labels = intervals_colors[[metric]]$labels
        ) +
        labs(
          title = title_text,
          subtitle = paste0("Rozlišení simulace: ", rozliseni_zobraz, "°"),
          color = NULL
        ) +
        theme_minimal(base_size = 20) +
        theme(
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(family = "Calibri", size = 30)
        ) +
        coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)
      
      # Uložení
      vystup_dir <- file.path("output", "kge_mapy")
      if (!dir.exists(vystup_dir)) dir.create(vystup_dir)
      
      ggsave(file.path(vystup_dir, file_name), p, width = 5.2, height = 3.8, dpi = 300, bg = "white")
    }
  }
}

# === 📂 Přesun souborů do podsložek podle období ===
files <- list.files("output/kge_mapy", pattern = "\\.png$", full.names = TRUE)
extract_period <- function(file_name) sub(".*_(.*)\\.png$", "\\1", basename(file_name))

for (file in files) {
  period <- extract_period(file)
  period_dir <- file.path("output/kge_mapy", period)
  if (!dir.exists(period_dir)) dir.create(period_dir)
  file.rename(file, file.path(period_dir, basename(file)))
}
