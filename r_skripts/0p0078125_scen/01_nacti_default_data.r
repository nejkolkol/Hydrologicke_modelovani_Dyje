# R/01_nacti_default_data.R

# 📦 Načtení nebo instalace balíčků
packages <- c("data.table", "dplyr", "purrr", "stringr")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# 📁 Cesta ke složce se základními simulacemi
base_default <- "data0078125_scenarios"

# 🧠 Funkce pro načtení jednoho souboru simulace (se specifickým záhlavím)
read_custom_csv <- function(file) {
  column_names <- scan(file, what = "character", skip = 0, nlines = 1, quiet = TRUE)
  column_names <- column_names[-1]
  data <- fread(file, skip = 2, header = FALSE)
  setnames(data, column_names)
  return(data)
}

# 🧩 Načtení všech CSV ze všech scénářů
default_paths <- list.dirs(base_default, recursive = FALSE)
default_data_list <- list()

for (scenar_path in default_paths) {
  scenar_name <- basename(scenar_path)
  message(">> Zpracovávám scénář: ", scenar_name)

  csv_files <- list.files(file.path(scenar_path, "simulace"), pattern = "\\.csv$", full.names = TRUE)

  for (file in csv_files) {
    message("   - Soubor: ", file)
    tryCatch({
      df <- read_custom_csv(file) %>%
        mutate(
          soubor = basename(file),
          nazev_profilu = str_remove(soubor, "\\.csv$"),
          kod_profilu = str_extract(nazev_profilu, "\\d{6}"),
          scenar = scenar_name,
          lai_variant = "default"
        )
      default_data_list[[length(default_data_list) + 1]] <- df
    }, error = function(e) {
      message("   !!! Chyba při načítání souboru: ", file)
      message("   >>> ", e$message)
    })
  }
}

# 🧱 Sloučení do jednoho datového rámce
default_data <- bind_rows(default_data_list)

# 💾 Uložení do výstupní složky
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)
saveRDS(default_data, file = file.path(output_dir, "default_data.rds"))

message("✅ Načtení výchozích dat dokončeno. Uloženo jako output/default_data.rds")
