library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(readr)

# Nastavení složky
base_dir <- getwd()

# Zjištění rozlišení složky
nazev_slozky <- basename(base_dir)
rozliseni <- stringr::str_extract(nazev_slozky, "\\d+\\.\\d+")
rozliseni_format <- gsub("\\.", "p", rozliseni)

# 📁 Výstupní složka do struktury output/KGE_violin_plot/<rozliseni>
typ_vystupu <- "KGE_violin_plot"
output_dir <- file.path(base_dir, "output", typ_vystupu, rozliseni_format)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Načtení výsledků KGE
input_data_dir <- file.path(base_dir, "vystupy")
KGE_denni_ROK     <- read_csv(file.path(input_data_dir, paste0("KGE_denni_ROK_", rozliseni_format, ".csv")))
KGE_denni_AMJJAS  <- read_csv(file.path(input_data_dir, paste0("KGE_denni_AMJJAS_", rozliseni_format, ".csv")))
KGE_denni_ONDJFM  <- read_csv(file.path(input_data_dir, paste0("KGE_denni_ONDJFM_", rozliseni_format, ".csv")))
KGE_denni_DJF     <- read_csv(file.path(input_data_dir, paste0("KGE_denni_DJF_", rozliseni_format, ".csv")))
KGE_denni_MAM     <- read_csv(file.path(input_data_dir, paste0("KGE_denni_MAM_", rozliseni_format, ".csv")))
KGE_denni_JJA     <- read_csv(file.path(input_data_dir, paste0("KGE_denni_JJA_", rozliseni_format, ".csv")))
KGE_denni_SON     <- read_csv(file.path(input_data_dir, paste0("KGE_denni_SON_", rozliseni_format, ".csv")))

# Aktivace Calibri
font_add("Calibri", regular = "calibri.ttf")
showtext_auto()

# Seznam období
poradi_obdobi <- c("ROK", "AMJJAS", "ONDJFM", "DJF", "MAM", "JJA", "SON")

# Sloučení dat
df <- bind_rows(
  KGE_denni_ROK %>% mutate(obdobi = "ROK"),
  KGE_denni_AMJJAS %>% mutate(obdobi = "AMJJAS"),
  KGE_denni_ONDJFM %>% mutate(obdobi = "ONDJFM"),
  KGE_denni_DJF %>% mutate(obdobi = "DJF"),
  KGE_denni_MAM %>% mutate(obdobi = "MAM"),
  KGE_denni_JJA %>% mutate(obdobi = "JJA"),
  KGE_denni_SON %>% mutate(obdobi = "SON")
)

# Transformace + značka závěrového profilu
df_long <- df %>% 
  pivot_longer(cols = c(KGE, Pearson_Correlation, Bias, Variability),
               names_to = "Metrika", values_to = "Hodnota") %>% 
  mutate(
    obdobi = factor(obdobi, levels = poradi_obdobi),
    je_zaverovy_profil = ifelse(kodprofilu == 480500, TRUE, FALSE)
  )

# Přemapování názvů metrik
df_long$Metrika <- factor(df_long$Metrika,
                          levels = c("KGE", "Bias", "Pearson_Correlation", "Variability"),
                          labels = c("KGE", "Systematická chyba", "Shoda v časové odezvě (korelace)", "Shoda ve variabilitě"))
                          
# Ideální hodnoty pro každou metriku
ideal_values <- data.frame(
  Metrika = c("KGE", "Systematická chyba", "Shoda v časové odezvě (korelace)", "Shoda ve variabilitě"),
  ideal = c(1, 1, 1, 1)
)

# === ✏️ Úprava vzhledu grafu ===
graf_png <- ggplot(df_long, aes(x = obdobi, y = Hodnota)) +
  geom_violin(aes(fill = Metrika), trim = TRUE, alpha = 0.7, linewidth = 0.3, show.legend = FALSE) +
  geom_hline(data = ideal_values, aes(yintercept = ideal), 
             linetype = "dashed", color = "black", linewidth = 0.3, inherit.aes = FALSE) +
  geom_jitter(data = df_long %>% filter(!je_zaverovy_profil),
              width = 0.2, alpha = 0.4, size = 0.5, color = "grey30", show.legend = FALSE) +
  stat_summary(fun = median, geom = "point", color = "white", size = 1.4, shape = 16) +
  geom_point(data = df_long %>% filter(je_zaverovy_profil),
             color = "black", size = 1.8, shape = 16, show.legend = FALSE) +
  facet_wrap(~Metrika, scales = "free_y") +
  theme_minimal(base_family = "Calibri", base_size = 30) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 28),
    axis.text.y = element_text(size = 28),
    axis.title.x = element_text(size = 32),
    axis.title.y = element_text(size = 32),
    plot.title = element_text(size = 40, face = "bold"),
    plot.subtitle = element_text(size = 28, face = "italic"),
    strip.text = element_text(size = 30, face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "grey60", linewidth = 0.2),
    axis.ticks = element_line(color = "grey60", linewidth = 0.2),
    legend.position = "none"
  ) +
  labs(
    title = "Distribuce metrik pro různá období",
    subtitle = paste("Rozlišení:", rozliseni, "°", " \u2003 \u2003 ", "\u25CF Závěrový profil \u2003\u2003– – Ideální hodnota \u2003\u2003\u25CB Medián"),
    x = "Období", y = "Hodnota"
  )


# === 💾 Uložení ===
ggsave(
  file.path(output_dir, paste0("KGE_violin_plot_", rozliseni_format, ".png")),
  plot = graf_png, width = 5.2, height = 5.2, dpi = 300, bg = "white"
)
