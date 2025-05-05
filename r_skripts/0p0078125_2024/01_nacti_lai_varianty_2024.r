# 📦 Načtení nebo instalace balíčků
packages <- c("data.table", "dplyr", "purrr", "stringr")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# 📁 Cesta k datům variant LAI pro rok 2024
base_lai <- "2024"

# 🧠 Funkce pro načtení jednoho souboru simulace (verze přes readLines)
read_custom_csv <- function(file) {
  lines <- readLines(file)
  data_lines <- lines[3:length(lines)]

  data <- read.table(text = data_lines, header = FALSE, sep = "", stringsAsFactors = FALSE,
                     col.names = c("date", "lon", "lat", "Q"))
  
  data$date <- as.Date(data$date)
  return(data)
}


# 🧩 Načtení všech CSV ze všech variant LAI
lai_variant_paths <- list.dirs(base_lai, recursive = FALSE, full.names = TRUE)
lai_data_list <- list()

for (variant_path in lai_variant_paths) {
  lai_variant <- basename(variant_path)
  sim_path <- file.path(variant_path, "simulace")
  
  if (!dir.exists(sim_path)) {
    message("⚠️ Složka 'simulace' neexistuje pro variantu: ", lai_variant)
    next
  }
  
  csv_files <- list.files(sim_path, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in csv_files) {
    message("📥 Načítám soubor: ", file)
    tryCatch({
      df <- read_custom_csv(file) %>%
        mutate(
          soubor = basename(file),
          nazev_profilu = str_remove(soubor, "\\.csv$"),
          kod_profilu = str_extract(nazev_profilu, "\\d{6}"),
          scenar = "2024",
          lai_variant = lai_variant
        )
      lai_data_list[[length(lai_data_list) + 1]] <- df
    }, error = function(e) {
      message("❌ Chyba při načítání souboru: ", file)
      message("   >>> ", e$message)
    })
  }
}

# 🧱 Sloučení do jednoho datového rámce
lai_data <- bind_rows(lai_data_list)

lai_data <- lai_data %>%
  filter(date >= as.Date("2024-01-01") & date <= as.Date("2024-12-31"))


# 💾 Uložení do výstupní složky
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)
saveRDS(lai_data, file = file.path(output_dir, "lai_varianty_2024.rds"))



message("✅ Načtení dat variant LAI dokončeno. Uloženo jako output/lai_varianty_2024.rds")
