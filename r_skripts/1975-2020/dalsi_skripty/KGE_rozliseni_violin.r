# === 📦 Knihovny ===
library(dplyr)
library(ggplot2)
library(readr)
library(purrr)
library(stringr)
library(showtext)
library(tidyr)
library(here)
library(scales)

library(scales)

custom_trans_half <- trans_new(
  name = "half_half",
  transform = function(x) {
    ifelse(x < 0, (x + 60) / 120, 0.5 + 0.5 * x)
  },
  inverse = function(y) {
    ifelse(y < 0.5, 120 * y - 60, 2 * y - 1)
  },
  domain = c(-60, 1)
)







# === 🎨 Aktivace fontu Calibri ===
font_add("Calibri", regular = "calibri.ttf")
showtext_auto()

# === 📁 Nastavení cest ===
base_dir <- here::here()  # bude např. C:/Users/kolar/OneDrive/Desktop/simulace1975-2020
sim_dir <- base_dir
output_dir <- file.path(base_dir, "output_graphs")
if (!dir.exists(output_dir)) dir.create(output_dir)

# Filtrování jen složek s rozlišením
rozliseni_slozky <- list.dirs(sim_dir, recursive = FALSE, full.names = TRUE)
rozliseni_slozky <- rozliseni_slozky[str_detect(basename(rozliseni_slozky), "^1975-2020_\\d")]
rozliseni_data <- map_dfr(rozliseni_slozky, function(path) {
  nazev <- basename(path)
  rozliseni <- str_extract(nazev, "\\d+\\.\\d+")
  rozliseni_format <- gsub("\\.", "p", rozliseni)
  vystup <- file.path(path, "vystupy", paste0("KGE_denni_ROK_", rozliseni_format, ".csv"))
  
  if (file.exists(vystup)) {
    df <- read_csv(vystup, show_col_types = FALSE)
    if ("kodprofilu" %in% names(df)) {
      df$rozliseni <- rozliseni
      return(df)
    } else {
      message("⚠️ Soubor neobsahuje sloupec 'kodprofilu': ", vystup)
      return(NULL)
    }
  } else {
    message("❌ Soubor nenalezen: ", vystup)
    return(NULL)
  }
})


# === 🧩 Označení závěrového profilu ===
rozliseni_data <- rozliseni_data %>%
  mutate(
    je_zaverovy_profil = kodprofilu == 480500,
    rozliseni_f = factor(rozliseni, levels = sort(unique(rozliseni)))
  )


graf_violin <- ggplot(rozliseni_data, aes(x = rozliseni_f, y = KGE)) +
  geom_violin(fill = "steelblue", alpha = 0.7, linewidth = 0.6, trim = TRUE) +
  geom_jitter(data = filter(rozliseni_data, !je_zaverovy_profil & !is.na(KGE)),
              width = 0.1, alpha = 0.4, size = 0.8, color = "grey30") +
  stat_summary(fun = median, geom = "point", color = "white", size = 1.5, shape = 16) +
  geom_point(data = filter(rozliseni_data, je_zaverovy_profil & !is.na(KGE)),
             color = "black", size = 2, shape = 16) +
  labs(
    title = "Distribuce hodnot KGE podle rozlišení simulace",
    subtitle = "\u25CF Závěrový profil  \u25CB Medián",
    x = "Rozlišení [°]",
    y = "KGE"
  ) +
  scale_y_continuous(
    trans = custom_trans_half,
    breaks = c(-60, -30, 0, 0.5, 1),
    labels = c("-60", "-30", "0", "0.5", "1")
  ) +
  theme_minimal(base_family = "Calibri") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25),
	axis.text.y = element_text(size = 25),
    axis.title = element_text(size = 33),
	plot.subtitle = element_text(size = 30),
    plot.title = element_text(size = 40, face = "bold")
  )

# Uložení do PDF
ggsave(file.path(output_dir, "KGE_violin_rozliseni.pdf"), 
       plot = graf_violin, width = 10, height = 6, device = cairo_pdf)

# Uložení do PNG
ggsave(file.path(output_dir, "KGE_violin_rozliseni.png"), 
       plot = graf_violin, width = 5.2, height = 3.8, dpi = 300, bg = "white")

