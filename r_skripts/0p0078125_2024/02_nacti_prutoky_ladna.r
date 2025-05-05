# 📦 Načtení potřebných balíčků
packages <- c("dplyr", "readr")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# 📥 Načtení simulovaných dat
lai_data <- readRDS("output/lai_varianty_2024.rds")

# 🧹 Oříznutí simulací na rok 2024
lai_data <- lai_data %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-12-31"))

# 📥 Načtení pozorovaných dat
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
         kod_profilu, scenar, lai_variant)



# 🔗 Sloučení simulací a pozorování
vsechna_data <- bind_rows(lai_data, pozorovani)

# 📌 Data jsou nyní připravena v objektu 'vsechna_data'
