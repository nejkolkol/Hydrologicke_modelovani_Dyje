library(ggplot2)
library(tidyr)
library(dplyr)

# Načtení výsledků
vysledky <- read.csv("C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output/bilance_zmeny_ssp585.csv")

# Odstraníme řádek se změnami pro čisté srovnání období
data_long <- vysledky %>%
  filter(obdobi %in% c("2030", "2085")) %>%
  pivot_longer(-obdobi, names_to = "promenna", values_to = "hodnota")

# Krásnější popisky
popisky <- c(
  srazky_mm_rok = "Srážky",
  evap_mm_rok = "Evapotranspirace",
  sat_prumer = "Zásoba podzemní vody (satSTW)",
  unsat_prumer = "Zásoba nenasycené zóny (unsatSTW)",
  swc_prumer = "Zásoba vody v půdě (SWC)"
)
data_long$promenna <- factor(data_long$promenna, levels = names(popisky), labels = popisky)

# Sloupcový graf
p <- ggplot(data_long, aes(x = promenna, y = hodnota, fill = obdobi)) +
  geom_col(position = position_dodge()) +
  theme_minimal(base_family = "Calibri") +
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top",
        axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(x = NULL, y = NULL, fill = "Období", title = "Porovnání vodní bilance: SSP585 (2030 vs 2085)")

# Uložení grafu
ggsave("C:/Users/kolar/OneDrive/Desktop/Kontrola_scenare/output/grafy/graf_bilance_ssp585.png",
       plot = p, width = 8, height = 5, dpi = 300, bg = "white")

print(p)
