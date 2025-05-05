library(tidyverse)

# 📁 Cesta ke složce s referenčními simulacemi
reference_dir <- "reference"

# 🧠 Funkce pro načtení jednoho souboru simulace
read_reference_file <- function(file) {
  data <- read_fwf(
    file,
    skip = 2,
    fwf_empty(file, skip = 2),
    trim_ws = TRUE
  )
  
  colnames(data) <- c("date", "lon", "lat", "value")
  
  data %>%
    mutate(
      kodprofilu = str_extract(basename(file), "\\d{6}"),
      date = as.Date(date)
    ) %>%
    select(kodprofilu, date, value)
}

# 📜 Seznam CSV souborů
files_reference <- list.files(reference_dir, pattern = "\\.csv$", full.names = TRUE)

# 🧩 Načtení všech souborů a spojení
data_reference <- map_dfr(files_reference, read_reference_file) %>%
  rename(prutok = value)

# ⏳ Definice společného časového rozsahu pro všechny profily
start_period <- as.Date("1980-11-01")
end_period   <- as.Date("2010-10-31")

# 🧹 Oříznutí na stejné období pro všechny profily
data_reference_filtered <- data_reference %>%
  filter(date >= start_period & date <= end_period)

# 💾 Uložení do výstupní složky
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)

saveRDS(data_reference_filtered, file = file.path(output_dir, "pozorovani.rds"))

message("✅ Načtení referenčních simulací dokončeno. Výstup uložen jako output/pozorovani.rds")
