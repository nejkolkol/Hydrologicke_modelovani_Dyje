library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(showtext)

# Font Calibri
font_add("Calibri", regular = "calibri.ttf")
showtext_auto()

# Načti data
vystup_dir <- "C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output"
data <- read_csv(file.path(vystup_dir, "bilance_scenare.csv"))

# Přidej sloupce období a scénář
bilance <- data %>%
  mutate(
    obdobi = str_extract(scenar, "^\\d{4}"),
    ssp = str_extract(scenar, "SSP[0-9]+")
  )

# Připrav data pro sloupcové grafy
dlouha <- bilance %>%
  pivot_longer(cols = c(srazky_mm_rok, evapotranspirace_mm_rok, qrouted_m3_rok),
               names_to = "typ",
               values_to = "hodnota") %>%
  mutate(
    typ = recode(typ,
                 srazky_mm_rok = "Srážky",
                 evapotranspirace_mm_rok = "Evapotranspirace",
                 qrouted_m3_rok = "Průtok")
  )

# Cesta pro uložení
grafy_dir <- file.path(vystup_dir, "grafy")
dir.create(grafy_dir, showWarnings = FALSE)

# ----------
# Sada 1: Podle období
# ----------
for (rok in unique(dlouha$obdobi)) {
  podmnozina <- filter(dlouha, obdobi == rok)
  p <- ggplot(podmnozina, aes(x = ssp, y = hodnota, fill = typ)) +
    geom_col(position = "dodge") +
    facet_wrap(~typ, scales = "free_y") +
    labs(title = paste("Vodní bilance pro období", rok),
         x = "Scénář", y = "Hodnota", fill = NULL) +
    theme_minimal(base_family = "Calibri") +
    theme(legend.position = "top")

  ggsave(filename = file.path(grafy_dir, paste0("graf_obdobi_", rok, ".png")), plot = p, width = 8, height = 5, bg = "white")
}

# ----------
# Sada 2: Podle scénáře
# ----------
for (sc in unique(dlouha$ssp)) {
  podmnozina <- filter(dlouha, ssp == sc)
  p <- ggplot(podmnozina, aes(x = obdobi, y = hodnota, fill = typ)) +
    geom_col(position = "dodge") +
    facet_wrap(~typ, scales = "free_y") +
    labs(title = paste("Vodní bilance pro scénář", sc),
         x = "Období", y = "Hodnota", fill = NULL) +
    theme_minimal(base_family = "Calibri") +
    theme(legend.position = "top")

  ggsave(filename = file.path(grafy_dir, paste0("graf_scenar_", sc, ".png")), plot = p, width = 8, height = 5, bg= "white")
}

cat("✅ Grafy uložené ve složce:", grafy_dir, "\n")
