library(dplyr)
library(ggplot2)
library(purrr)
library(readr)
library(stringr)
library(here)

# === 📁 Nastavení cest ===
base_dir <- here::here()  # bude např. C:/Users/kolar/OneDrive/Desktop/simulace1975-2020
sim_dir <- base_dir
output_dir <- file.path(base_dir, "output_graphs")
if (!dir.exists(output_dir)) dir.create(output_dir)

# === 🔍 Načteme všechna rozlišení složek ===
rozliseni_slozky <- list.dirs(sim_dir, recursive = FALSE, full.names = TRUE)

# Filtrování složek s rozlišením
rozliseni_slozky <- rozliseni_slozky[str_detect(basename(rozliseni_slozky), "^1975-2020_\\d")]

# Funkce pro načtení času z `time.txt` souboru
get_simulation_time <- function(path) {
  time_file <- file.path(path, "time.txt")
  
  if (file.exists(time_file)) {
    time_data <- readLines(time_file)
    # Načteme čas v reálném čase (real čas)
    real_time <- str_extract(time_data[1], "\\d+m\\d+\\.\\d+s")
    # Převedeme čas na sekundy
    time_seconds <- str_extract(real_time, "\\d+") %>% as.numeric() * 60 +
                    str_extract(real_time, "\\d+\\.\\d+s") %>%
                    str_replace_all("s", "") %>% as.numeric()
    
    rozliseni <- str_extract(basename(path), "\\d+\\.\\d+")
    
    return(data.frame(rozliseni = rozliseni, time_seconds = time_seconds))
  } else {
    return(NULL)
  }
}

# === 📊 Načteme časy pro všechna rozlišení ===
simulation_times <- map_dfr(rozliseni_slozky, get_simulation_time) %>%
  mutate(time_hours = time_seconds / 3600)

# === 📈 Vykreslení grafu časů výpočtu v hodinách ===
graf_casy <- ggplot(simulation_times, aes(x = rozliseni, y = time_hours)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  theme_minimal(base_family = "Calibri") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25),
	axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 33),
	plot.subtitle = element_text(size = 30),
    plot.title = element_text(size = 40, face = "bold", lineheight = 0.4)
  ) +
  labs(
  title = "Porovnání času výpočtu pro jednotlivá rozlišení \nsimulace za období 1975–2020",
  x = "Rozlišení [°]",
  y = "Čas výpočtu [h]"

)

# === 💾 Uložení ===
ggsave(file.path(output_dir, "simulation_time_comparison.pdf"), plot = graf_casy, width = 10, height = 6, device = cairo_pdf)
ggsave(file.path(output_dir, "simulation_time_comparison.png"), plot = graf_casy, width = 5.2, height = 3.8, dpi = 300, bg = "white")
