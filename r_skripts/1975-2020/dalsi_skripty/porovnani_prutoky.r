# === 📦 Knihovny ===
library(tidyverse)
library(patchwork)
library(sysfonts)
library(showtext)
library(lubridate)

# === 🔤 Font Calibri ===
font_add("Calibri", regular = "C:/Windows/Fonts/calibri.ttf")
showtext_auto()

# === 📁 Cesty ===
base_dir <- "C:/Users/kolar/OneDrive/Desktop/simulace1975-2020/1975-2020_0.0078125"
root_dir <- "C:/Users/kolar/OneDrive/Desktop/simulace1975-2020"
slozka_output <- file.path(root_dir, "output_graphs")
if (!dir.exists(slozka_output)) dir.create(slozka_output, recursive = TRUE)

rozliseni <- "0.0078125"
rozliseni_format <- gsub("\\.", "p", rozliseni)
prehled_profilu <- read_csv(file.path(base_dir, "vrstvy", "P_ehled_profil_.csv"), show_col_types = FALSE)

# === 📄 Cesty k výstupům ===
mesic_data <- read_csv(file.path(base_dir, "output", "mesicni_prumery", rozliseni_format, "KGE_vystup_mesic.csv"), show_col_types = FALSE)
rocni_data <- read_csv(file.path(base_dir, "output", "rocni_prumery", rozliseni_format, "KGE_vystup_rocni.csv"), show_col_types = FALSE)
mesicdl_data <- read_csv(file.path(base_dir, "output", "mesicni_prumerydlouhodobe", rozliseni_format, "KGE_vystup_mesicdlouhodobe.csv"), show_col_types = FALSE)

# === 📥 Načtení metrik KGE ===
kge_mesic <- read_csv(file.path(base_dir, "output", "KGE_mesic", rozliseni_format, paste0("KGE_mesic_ROK_", rozliseni_format, ".csv")), show_col_types = FALSE)
kge_rocni <- read_csv(file.path(base_dir, "output", "KGE_rocni", rozliseni_format, paste0("KGE_rocni_", rozliseni_format, ".csv")), show_col_types = FALSE)

# === 📈 Funkce pro jednotlivé grafy ===
vytvor_graf_mesic <- function(data, kge) {
  ggplot(data, aes(x = as.Date(year_month))) +
    geom_line(aes(y = value_prumer, color = "Simulace"), linewidth = 0.4) +
    geom_line(aes(y = prutok_prumer, color = "Pozorování"), linewidth = 0.4) +
    labs(
      title = paste0("KGE = ", sprintf("%.2f", kge)),
      x = "Rok", y = "Měsíční průtok (m³/s)", color = "Legenda"
    ) +
    scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "red")) +
    theme_minimal(base_size = 24) +
    theme(
  plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 2)),
  axis.title = element_text(size = 28),
  axis.text = element_text(size = 26),
  legend.title = element_text(size = 26),
  legend.text = element_text(size = 24),
  legend.position = "top",
  legend.box.margin = margin(b = -10),       # >>> TADY přidáme
  plot.margin = margin(5, 5, 5, 5),           # >>> A TADY
  text = element_text(family = "Calibri")
)

}

vytvor_graf_rocni <- function(data, kge) {
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = value_prumer, color = "Simulace"), linewidth = 0.4) +
    geom_line(aes(y = prutok_prumer, color = "Pozorování"), linewidth = 0.4) +
    labs(
      title = paste0("KGE = ", sprintf("%.2f", kge)),
      x = "Rok", y = "Roční průtok (m³/s)", color = "Legenda"
    ) +
    scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "red")) +
    theme_minimal(base_size = 24) +
    theme(
      plot.title = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 2)),
      axis.title = element_text(size = 28),
      axis.text = element_text(size = 26),
      legend.position = "none",
      text = element_text(family = "Calibri")
    )
}

vytvor_graf_mesicdl <- function(data) {
  ggplot(data, aes(x = month)) +
    geom_line(aes(y = value_prumer, color = "Simulace"), linewidth = 0.4) +
    geom_line(aes(y = prutok_prumer, color = "Pozorování"), linewidth = 0.4) +
    scale_x_continuous(
      breaks = 1:12,
      labels = c("I","II","III","IV","V","VI","VII","VIII","IX","X","XI","XII")
    ) +
    labs(
      title = NULL,
      x = "Měsíc", y = "Průměrný průtok (m³/s)", color = "Legenda"
    ) +
    scale_color_manual(values = c("Simulace" = "blue", "Pozorování" = "red")) +
    theme_minimal(base_size = 24) +
    theme(
      axis.title = element_text(size = 28),
      axis.text = element_text(size = 26),
      legend.position = "none",
      text = element_text(family = "Calibri")
    )
}

# === 🔁 Vygeneruj výstup pro každý profil ===
profily <- c("480500", "454000", "431000")

for (kod in profily) {
  mesic_profil <- mesic_data %>% filter(kodprofilu == kod)
  rocni_profil <- rocni_data %>% filter(kodprofilu == kod)
  mesdl_profil <- mesicdl_data %>% filter(kodprofilu == kod)

  info <- prehled_profilu %>% filter(kodprofilu == kod)
  nazev <- info$nazev
  tok <- info$tok

  kge_m <- kge_mesic %>% filter(kodprofilu == kod) %>% pull(KGE)
  kge_r <- kge_rocni %>% filter(kodprofilu == kod) %>% pull(KGE)

  g1 <- vytvor_graf_mesic(mesic_profil, kge_m)
  g2 <- vytvor_graf_rocni(rocni_profil, kge_r)
  g3 <- vytvor_graf_mesicdl(mesdl_profil)

  layout <- "
  AA
  BC
  "

    final <- wrap_plots(A = g1, B = g2, C = g3, design = layout) +
    plot_layout(heights = c(1.8, 1)) +
  # Menší mezera mezi řádky
    plot_annotation(
      title = paste0("Profil ", kod, " ", nazev, " – ", tok, " (rozlišení ", rozliseni, "°)"),
      theme = theme(
        plot.title = element_text(family = "Calibri", size = 36, hjust = 0.5, face = "bold", margin = margin(b = 4))
      )
    )

  ggsave(file.path(slozka_output, paste0("profil_", kod, "_", nazev, "_", tok, "_", rozliseni_format, ".png")),
         plot = final, width = 5.2, height = 6, dpi = 300, bg = "white")
}
