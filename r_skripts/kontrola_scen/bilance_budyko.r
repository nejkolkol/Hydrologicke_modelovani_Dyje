# Načti balíčky
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(showtext)

# Aktivuj Calibri font
font_add("Calibri", "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# Načti data
results_df <- read_csv("C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output/Basin_water_balance.csv")

# Přepiš 'Baseline' na 'Referenční období'
results_df <- results_df %>%
  mutate(Period = ifelse(Period == "Baseline", "Referenční období", Period))

# Nastavení pořadí období (aby 'Referenční období' bylo vlevo)
results_df <- results_df %>%
  mutate(Period = factor(Period, levels = c("Referenční období", "2030", "2050", "2070", "2085")))

# Převod do long formátu + zahrnutí nových parametrů
results_long <- results_df %>%
  pivot_longer(
    cols = c(P_mm_per_year, PET_mm_per_year, aET_mm_per_year, aET_mm_per_year_Budyko_estimate, 
             Q_mm_per_year, mean_TWS_delta, Q_mm_per_year_Budyko_estimate),
    names_to = "Parametr",
    values_to = "Hodnota"
  ) %>%
  mutate(Parametr = recode(Parametr,
    "P_mm_per_year" = "Srážky",
    "PET_mm_per_year" = "Potenciální ET",
    "aET_mm_per_year" = "Reálná ET mHM",
    "aET_mm_per_year_Budyko_estimate" = "Reálná ET Budyko",
    "Q_mm_per_year" = "Odtok mHM",
    "mean_TWS_delta" = "Změna zásob",
    "Q_mm_per_year_Budyko_estimate" = "Odtok Budyko"
  ))

# Nastavení barev pro období
barvy <- c("Referenční období" = "gray30",
           "2030" = "#1f78b4",
           "2050" = "#33a02c",
           "2070" = "#ff7f00",
           "2085" = "#e31a1c")

# Nastavení pořadí parametrů pro lepší vzhled v grafu
results_long <- results_long %>%
  mutate(Parametr = factor(Parametr, levels = c("Srážky", "Potenciální ET", "Reálná ET mHM", 
                                                 "Reálná ET Budyko", "Odtok mHM", "Odtok Budyko", 
                                                 "Změna zásob")))

# Vytvoření grafu
graf <- ggplot(results_long, aes(x = Parametr, y = Hodnota, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = barvy) +
  scale_y_log10() +  # Logaritmická osa Y
  labs(title = "Odhady vodní bilance v jednotlivých obdobích",
       x = "Parametr",
       y = "Množství (mm/rok)",
       fill = "Období") +
  theme(
    text = element_text(family = "Calibri", size = 26),
    plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.text.x = element_text(size = 24, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 24),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 24),
    legend.position = "top",
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )

# Ulož graf jako PNG
ggsave("C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output/vodni_bilance_budyko.png",
       plot = graf,
       width = 5.2, height = 3.8, units = "in", dpi = 300, bg = "white")
