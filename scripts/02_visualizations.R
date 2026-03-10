# ============================================================
# 02_visualizations.R
# Project: Urban Crime Pattern Explorer — Hyderabad/AP Focus
# Phase 3: Visualizations
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(scales)

# ── LOAD CLEANED DATA ────────────────────────────────────────

ap_crime    <- read.csv("data/cleaned/ap_crime_clean.csv")
women_clean <- read.csv("data/cleaned/women_crime_clean.csv")
cyber_clean <- read.csv("data/cleaned/cyber_clean.csv")

dir.create("plots", showWarnings = FALSE)

cat("✅ Data loaded!\n")

# ── HELPER: Hyderabad districts ──────────────────────────────

hyd_districts <- c("HYDERABAD CITY", "CYBERABAD", "RANGA REDDY")

# ============================================================
# PLOT 1: Total IPC Crimes Trend — AP Overall (2001–2014)
# ============================================================

trend_data <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ")) %>%
  group_by(YEAR) %>%
  summarise(TOTAL = sum(TOTAL.IPC.CRIMES, na.rm = TRUE))

p1 <- ggplot(trend_data, aes(x = YEAR, y = TOTAL)) +
  geom_line(color = "#e94560", size = 1.5) +
  geom_point(color = "#e94560", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "#0f3460", linetype = "dashed", alpha = 0.2) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2001:2014) +
  labs(
    title    = "📈 Total IPC Crimes in Andhra Pradesh (2001–2014)",
    subtitle = "Overall crime trend with linear forecast line",
    x        = "Year",
    y        = "Total Crimes",
    caption  = "Source: NCRB Crime in India Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", color = "#0f3460"),
    plot.subtitle = element_text(color = "#666666"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("plots/01_crime_trend.png", p1, width = 10, height = 6, dpi = 150)
cat("✅ Plot 1 saved: Overall crime trend\n")

# ============================================================
# PLOT 2: Hyderabad vs Rest of AP — Crime Comparison
# ============================================================

hyd_vs_ap <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ")) %>%
  mutate(REGION = ifelse(DISTRICT %in% hyd_districts, "Hyderabad Region", "Rest of AP")) %>%
  group_by(YEAR, REGION) %>%
  summarise(TOTAL = sum(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(hyd_vs_ap, aes(x = YEAR, y = TOTAL, color = REGION, group = REGION)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Hyderabad Region" = "#e94560", "Rest of AP" = "#0f3460")) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2001:2014) +
  labs(
    title    = "🏙️ Hyderabad Region vs Rest of AP — Crime Trend",
    subtitle = "Hyderabad City + Cyberabad + Ranga Reddy vs all other districts",
    x        = "Year", y = "Total Crimes", color = "",
    caption  = "Source: NCRB"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", color = "#0f3460"),
    legend.position = "top",
    axis.text.x   = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("plots/02_hyd_vs_ap.png", p2, width = 10, height = 6, dpi = 150)
cat("✅ Plot 2 saved: Hyderabad vs Rest of AP\n")

# ============================================================
# PLOT 3: Crime Type Breakdown — Hyderabad Region
# ============================================================

hyd_crimes <- ap_crime %>%
  filter(DISTRICT %in% hyd_districts,
         !str_detect(DISTRICT, "TOTAL|ZZ")) %>%
  summarise(
    Murder          = sum(MURDER,          na.rm = TRUE),
    Rape            = sum(RAPE,            na.rm = TRUE),
    Kidnapping      = sum(KIDNAPPING...ABDUCTION, na.rm = TRUE),
    Robbery         = sum(ROBBERY,         na.rm = TRUE),
    Burglary        = sum(BURGLARY,        na.rm = TRUE),
    Theft           = sum(THEFT,           na.rm = TRUE),
    Riots           = sum(RIOTS,           na.rm = TRUE),
    Cheating        = sum(CHEATING,        na.rm = TRUE),
    Dowry.Deaths    = sum(DOWRY.DEATHS,    na.rm = TRUE),
    Arson           = sum(ARSON,           na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Crime_Type", values_to = "Count") %>%
  arrange(desc(Count))

p3 <- ggplot(hyd_crimes, aes(x = reorder(Crime_Type, Count), y = Count, fill = Count)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(Count)), hjust = -0.1, size = 3.5, color = "#333333") +
  coord_flip() +
  scale_fill_gradient(low = "#ffecd2", high = "#e94560") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "🔍 Crime Type Breakdown — Hyderabad Region (2001–2014)",
    subtitle = "Total crimes by category across Hyderabad City, Cyberabad & Ranga Reddy",
    x        = "", y = "Total Cases",
    caption  = "Source: NCRB"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold", color = "#0f3460"),
    axis.text.y = element_text(face = "bold")
  )

ggsave("plots/03_crime_type_breakdown.png", p3, width = 10, height = 7, dpi = 150)
cat("✅ Plot 3 saved: Crime type breakdown\n")

# ============================================================
# PLOT 4: Heatmap — Crime by District and Year
# ============================================================

heatmap_data <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")) %>%
  filter(DISTRICT %in% c(
    "HYDERABAD CITY", "CYBERABAD", "RANGA REDDY",
    "WARANGAL", "WARANGAL URBAN", "KARIMNAGAR",
    "NALGONDA", "NIZAMABAD", "ADILABAD", "KHAMMAM",
    "MEDAK", "MAHABOOBNAGAR", "MAHABOOB NAGAR"
  )) %>%
  group_by(DISTRICT, YEAR) %>%
  summarise(TOTAL = sum(TOTAL.IPC.CRIMES, na.rm = TRUE), .groups = "drop")

p4 <- ggplot(heatmap_data, aes(x = YEAR, y = DISTRICT, fill = TOTAL)) +
  geom_tile(color = "white", size = 0.3) +
  scale_fill_gradient2(
    low      = "#f8f9fa",
    mid      = "#ffa07a",
    high     = "#8b0000",
    midpoint = median(heatmap_data$TOTAL, na.rm = TRUE),
    labels   = comma
  ) +
  scale_x_continuous(breaks = 2001:2014) +
  labs(
    title    = "🔥 Crime Heatmap — Key Districts of AP/Telangana",
    subtitle = "Darker = more crimes. Hyderabad dominates throughout",
    x        = "Year", y        = "",
    fill     = "Total Crimes",
    caption  = "Source: NCRB"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", color = "#0f3460"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    axis.text.y  = element_text(face = "bold"),
    legend.position = "right"
  )

ggsave("plots/04_heatmap_district_year.png", p4, width = 12, height = 7, dpi = 150)
cat("✅ Plot 4 saved: District-year heatmap\n")

# ============================================================
# PLOT 5: Women Safety — Trend Over Years
# ============================================================

women_trend <- women_clean %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ")) %>%
  group_by(Year) %>%
  summarise(
    Rape             = sum(Rape,             na.rm = TRUE),
    Kidnapping       = sum(Kidnapping.and.Abduction, na.rm = TRUE),
    Dowry_Deaths     = sum(Dowry.Deaths,     na.rm = TRUE),
    Assault          = sum(Assault.on.women.with.intent.to.outrage.her.modesty, na.rm = TRUE),
    Cruelty          = sum(Cruelty.by.Husband.or.his.Relatives, na.rm = TRUE)
  ) %>%
  pivot_longer(-Year, names_to = "Crime_Type", values_to = "Count")

p5 <- ggplot(women_trend, aes(x = Year, y = Count, color = Crime_Type, group = Crime_Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "Rape"         = "#e94560",
    "Kidnapping"   = "#0f3460",
    "Dowry_Deaths" = "#f5a623",
    "Assault"      = "#7b2d8b",
    "Cruelty"      = "#27ae60"
  )) +
  scale_x_continuous(breaks = seq(2001, 2012, 1)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "👩 Women Safety Crimes — AP/Telangana Trend",
    subtitle = "5 categories of crimes against women (2001–2012)",
    x        = "Year", y = "Cases", color = "Crime Type",
    caption  = "Source: NCRB"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", color = "#0f3460"),
    legend.position = "right",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/05_women_safety_trend.png", p5, width = 11, height = 6, dpi = 150)
cat("✅ Plot 5 saved: Women safety trend\n")

# ============================================================
# PLOT 6: Top 10 Most Dangerous Districts (avg crimes/year)
# ============================================================

top_districts <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")) %>%
  group_by(DISTRICT) %>%
  summarise(AVG_CRIMES = mean(TOTAL.IPC.CRIMES, na.rm = TRUE)) %>%
  arrange(desc(AVG_CRIMES)) %>%
  slice_head(n = 10) %>%
  mutate(IS_HYD = DISTRICT %in% hyd_districts)

p6 <- ggplot(top_districts, aes(x = reorder(DISTRICT, AVG_CRIMES),
                                y = AVG_CRIMES, fill = IS_HYD)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(round(AVG_CRIMES))),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#e94560", "FALSE" = "#0f3460")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "🏆 Top 10 Most Crime-Prone Districts — AP/Telangana",
    subtitle = "Red = Hyderabad region. Average annual crimes per district",
    x        = "", y = "Avg Annual Crimes",
    caption  = "Source: NCRB"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold", color = "#0f3460"),
    axis.text.y = element_text(face = "bold")
  )

ggsave("plots/06_top10_districts.png", p6, width = 10, height = 7, dpi = 150)
cat("✅ Plot 6 saved: Top 10 districts\n")

# ============================================================
# PLOT 7: Violent vs Property vs Women Crimes — Stacked Bar
# ============================================================

crime_category <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ")) %>%
  group_by(YEAR) %>%
  summarise(
    Violent  = sum(VIOLENT_CRIMES,  na.rm = TRUE),
    Property = sum(PROPERTY_CRIMES, na.rm = TRUE),
    Women    = sum(WOMEN_CRIMES,    na.rm = TRUE)
  ) %>%
  pivot_longer(-YEAR, names_to = "Category", values_to = "Count")

p7 <- ggplot(crime_category, aes(x = YEAR, y = Count, fill = Category)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c(
    "Violent"  = "#e94560",
    "Property" = "#0f3460",
    "Women"    = "#f5a623"
  )) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = 2001:2014) +
  labs(
    title    = "📊 Crime Categories Over Time — AP/Telangana",
    subtitle = "Violent, Property and Women crimes stacked by year",
    x        = "Year", y = "Total Cases", fill = "Category",
    caption  = "Source: NCRB"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", color = "#0f3460"),
    legend.position = "top",
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )

ggsave("plots/07_crime_categories_stacked.png", p7, width = 10, height = 6, dpi = 150)
cat("✅ Plot 7 saved: Crime categories stacked\n")

# ============================================================
# PLOT 8: Cybercrime — AP vs Telangana vs Top States
# ============================================================

cyber_plot <- cyber_clean %>%
  filter(!str_detect(STATE, "TOTAL")) %>%
  arrange(desc(crimes_2018)) %>%
  slice_head(n = 10) %>%
  mutate(IS_AP_TG = STATE %in% c("ANDHRA PRADESH", "TELANGANA"))

p8 <- ggplot(cyber_plot, aes(x = reorder(STATE, crimes_2018),
                             y = crimes_2018, fill = IS_AP_TG)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(crimes_2018)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#e94560", "FALSE" = "#aaaaaa")) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "💻 Top 10 States by Cybercrime Cases (2018)",
    subtitle = "Red = Andhra Pradesh / Telangana",
    x        = "", y = "Cybercrime Cases",
    caption  = "Source: NCRB Cybercrime Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold", color = "#0f3460"),
    axis.text.y = element_text(face = "bold")
  )

ggsave("plots/08_cybercrime_states.png", p8, width = 10, height = 7, dpi = 150)
cat("✅ Plot 8 saved: Cybercrime by state\n")

# ── FINAL SUMMARY ────────────────────────────────────────────

cat("\n🎉 ALL 8 PLOTS SAVED to plots/ folder!\n")
cat("────────────────────────────────────────\n")
cat("01_crime_trend.png          — Overall AP trend\n")
cat("02_hyd_vs_ap.png            — Hyderabad vs Rest of AP\n")
cat("03_crime_type_breakdown.png — Crime types in Hyderabad\n")
cat("04_heatmap_district_year.png— District heatmap\n")
cat("05_women_safety_trend.png   — Women safety trends\n")
cat("06_top10_districts.png      — Most dangerous districts\n")
cat("07_crime_categories.png     — Stacked categories\n")
cat("08_cybercrime_states.png    — Cybercrime comparison\n")