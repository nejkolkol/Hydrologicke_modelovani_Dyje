# 📦 Balíčky
packages <- c("dplyr", "ggplot2", "stringr")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# 📌 Metadata o profilu
nazev_stanice <- "Ladná"
kod_profilu <- "480500"
nazev_toku <- "Dyje"
obdobi_popis <- "srpen–prosinec 2024"
popis_profilu <- paste0("Profil ", kod_profilu, " ", nazev_stanice, " – ", nazev_toku)

# 📥 Načtení simulovaných dat
vsechna_data <- readRDS("output/lai_varianty_2024.rds") %>%
  filter(date >= as.Date("2024-08-01") & date <= as.Date("2024-12-31"))

# 📥 Načtení pozorování
pozorovani <- read.table("pozorovani/4805_ladna.txt", header = FALSE, stringsAsFactors = FALSE)
colnames(pozorovani) <- c("kod_profilu", "typ", "datum", "placeholder", "Q")

pozorovani <- pozorovani %>%
  mutate(
    kod_profilu = as.character(kod_profilu),
    date = as.Date(datum, format = "%d/%m/%Y"),
    scenar = "pozorovani",
    lai_variant = "pozorovani",
    nazev_profilu = "dyje480500",
    lon = NA_real_,
    lat = NA_real_,
    soubor = NA_character_
  ) %>%
  select(date, lon, lat, Q, soubor, nazev_profilu,
         kod_profilu, scenar, lai_variant) %>%
  filter(date >= as.Date("2024-08-01") & date <= as.Date("2024-12-31"))

# 📌 Spojení
vsechna_data <- bind_rows(vsechna_data, pozorovani)

# 📁 Výstupní složka
dir.create("output/grafy_lai_vs_pozorovani", showWarnings = FALSE)

# 📈 Varianta 1 – každý graf: 1 simulace + pozorování
lai_varianty <- vsechna_data %>%
  filter(lai_variant != "pozorovani") %>%
  pull(lai_variant) %>%
  unique()

for (variant in lai_varianty) {
  poddata <- vsechna_data %>%
    filter(lai_variant %in% c(variant, "pozorovani"))

  p <- ggplot(poddata, aes(x = date, y = Q, color = lai_variant)) +
    geom_line(size = 0.5) +
    labs(
      title = paste0(popis_profilu," (", obdobi_popis, ")"),
      x = "Datum", y = "Průtok [m³/s]", color = "Scénář"
    ) +
    theme_minimal(base_family = "Calibri") +
    theme(
      legend.position = "top",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )

  ggsave(paste0("output/grafy_lai_vs_pozorovani/porovnani_", variant, "_vs_pozorovani.png"),
         plot = p, width = 5.2, height = 3.8, dpi = 300)
}

# 📈 Varianta 2 – pouze simulace
simulace_only <- vsechna_data %>%
  filter(lai_variant != "pozorovani")

p2 <- ggplot(simulace_only, aes(x = date, y = Q, color = lai_variant)) +
  geom_line(size = 0.5) +
  labs(
    title = paste0(popis_profilu, "(", obdobi_popis, ")"),
    x = "Datum", y = "Průtok [m³/s]", color = "LAI varianta"
  ) +
  theme_minimal(base_family = "Calibri") +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/grafy_lai_vs_pozorovani/porovnani_simulaci_mezi_sebou.png",
       plot = p2, width = 5.2, height = 3.8, dpi = 300)

# 📈 Varianta A – original, lai0p5, lai6
varianta_A <- vsechna_data %>%
  filter(lai_variant %in% c("original", "lai0p5", "lai6"))

pA <- ggplot(varianta_A, aes(x = date, y = Q, color = lai_variant)) +
  geom_line(size = 0.5) +
  labs(
    title = paste0(popis_profilu, " (", obdobi_popis, ")"),
    x = "Datum", y = "Průtok [m³/s]", color = "LAI varianta"
  ) +
  theme_minimal(base_family = "Calibri") +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/grafy_lai_vs_pozorovani/porovnani_original_lai0p5_lai6.png",
       plot = pA, width = 5.2, height = 3.8, dpi = 300)

# 📈 Varianta B – original, psenice, samo
varianta_B <- vsechna_data %>%
  filter(lai_variant %in% c("original", "psenice", "samo"))

pB <- ggplot(varianta_B, aes(x = date, y = Q, color = lai_variant)) +
  geom_line(size = 0.5) +
  labs(
    title = paste0(popis_profilu, " (", obdobi_popis, ")"),
    x = "Datum", y = "Průtok [m³/s]", color = "LAI varianta"
  ) +
  theme_minimal(base_family = "Calibri") +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("output/grafy_lai_vs_pozorovani/porovnani_original_psenice_samo.png",
       plot = pB, width = 5.2, height = 3.8, dpi = 300)
