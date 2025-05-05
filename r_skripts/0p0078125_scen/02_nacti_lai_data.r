# R/02_nacti_lai_data.R

# 📦 Načtení nebo instalace balíčků
packages <- c("data.table", "dplyr", "purrr", "stringr")
lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
})

# 📁 Cesta ke složce s variantami LAI
base_lai <- "data0078125_lai_scenarios"

# 🧠 Funkce pro načtení jednoho souboru simulace (se specifickým záhlavím)
read_custom_csv <- function(file) {
  column_names <- scan(file, what = "character", skip = 0, nlines = 1, quiet = TRUE)
  column_names <- column_names[-1]
  data <- fread(file, skip = 2, header = FALSE)
  setnames(data, column_names)
  return(data)
}

# 🧩 Načtení všech CSV ze všech scénářů a LAI variant
lai_scenar_paths <- list.dirs(base_lai, recursive = FALSE)
lai_data_list <- list()

for (scenar_path in lai_scenar_paths) {
  scenar_name <- basename(scenar_path)
  message(">> Zpracovávám scénář: ", scenar_name)

  lai_variant_paths <- list.dirs(scenar_path, recursive = FALSE)

  for (lai_path in lai_variant_paths) {
    lai_variant <- basename(lai_path)
    message("   > Varianta LAI: ", lai_variant)

    csv_files <- list.files(file.path(lai_path, "simulace"), pattern = "\\.csv$", full.names = TRUE)

    for (file in csv_files) {
      message("     - Soubor: ", file)
      tryCatch({
        df <- read_custom_csv(file) %>%
          mutate(
            soubor = basename(file),
            nazev_profilu = str_remove(soubor, "\\.csv$"),
            kod_profilu = str_extract(nazev_profilu, "\\d{6}"),
            scenar = scenar_name,
            lai_variant = lai_variant
          )
        lai_data_list[[length(lai_data_list) + 1]] <- df
      }, error = function(e) {
        message("     !!! Chyba při načítání souboru: ", file)
        message("     >>> ", e$message)
      })
    }
  }
}

# 🧱 Sloučení do jednoho datového rámce
lai_data <- bind_rows(lai_data_list)

# 💾 Uložení do výstupní složky
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir)
saveRDS(lai_data, file = file.path(output_dir, "lai_data.rds"))

message("✅ Načtení variant LAI dokončeno. Uloženo jako output/lai_data.rds")
