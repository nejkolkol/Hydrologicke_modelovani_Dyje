library(fs)
library(purrr)

# Hlavní adresář projektu
hlavni_slozka <- "C:/Users/kolar/OneDrive/Desktop/simulace1975-2020"

# Složka, kde jsou centrálně uloženy R skripty
slozka_skriptu <- file.path(hlavni_slozka, "prikazy_r")

# Získání všech podsložek se simulacemi podle formátu názvu
slozky_simulaci <- dir_ls(hlavni_slozka, type = "directory", regexp = "1975-2020_\\d+\\.\\d+")

# Seznam R skriptů ve správném pořadí
nazvy_skriptu <- c(
  "1_tvorba_grafu_mesic_pdf.r",
  "2_tvorba_grafu_mesicdlouhodobe_pdf.r",
  "3_tvorba_grafu_rok_pdf.r",
  "4_vypocet_KGE_denni.r",
  "5_vypocet_KGE_mesic.r",
  "6_vypocet_KGE_rocni.r",
  "7_priprava_KGE_map.r",
  "8_graficke_vystupy_KGE.r",
  "9_KGE_violinPlot.r"
)

# Iterace přes všechny složky simulací
walk(slozky_simulaci, function(slozka) {
  cat("\n➡️ Spouštím skripty pro složku:", slozka, "\n")
  setwd(slozka)  # 🔁 Nastaví pracovní složku na aktuální rozlišení (např. 1975-2020_0.5)

  # Spuštění všech skriptů ze složky prikazy_r
  walk(nazvy_skriptu, function(script_name) {
    cesta <- file.path(slozka_skriptu, script_name)
    if (file_exists(cesta)) {
      cat("  📄 Spouštím:", script_name, "\n")
      source(cesta, local = TRUE)
    } else {
      cat("  ⚠️  Skript nenalezen:", script_name, "\n")
    }
  })
})
