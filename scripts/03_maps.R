# ============================================================
# 03_maps.R
# Project: Urban Crime Pattern Explorer — Hyderabad/AP Focus
# Phase 4: Interactive Maps
# ============================================================

library(dplyr)
library(leaflet)
library(htmlwidgets)
library(RColorBrewer)
library(stringr)

# ── LOAD DATA ────────────────────────────────────────────────

ap_crime <- read.csv("data/cleaned/ap_crime_clean.csv")

dir.create("maps", showWarnings = FALSE)

cat("✅ Data loaded!\n")

# ── DISTRICT COORDINATES ─────────────────────────────────────
# Manual lat/long for AP + Telangana districts

district_coords <- data.frame(
  DISTRICT = c(
    "HYDERABAD CITY", "CYBERABAD",      "RANGA REDDY",
    "WARANGAL",       "WARANGAL URBAN", "KARIMNAGAR",
    "NALGONDA",       "NIZAMABAD",      "ADILABAD",
    "KHAMMAM",        "MEDAK",          "MAHABOOBNAGAR",
    "MAHABOOB NAGAR", "NELLORE",        "GUNTUR",
    "KRISHNA",        "EAST GODAVARI",  "WEST GODAVARI",
    "VISAKHAPATNAM",  "SRIKAKULAM",     "VIZIANAGARAM",
    "CHITTOOR",       "ANANTAPUR",      "KURNOOL",
    "CUDDAPAH",       "PRAKASHAM",      "VIJAYAWADA",
    "VIJAYAWADA CITY","RAJAHMUNDRY"
  ),
  LAT = c(
    17.3850,  17.4399,  17.3600,
    17.9784,  17.9784,  18.4386,
    17.0575,  18.6725,  19.6667,
    17.2473,  17.9167,  16.7333,
    16.7333,  14.4426,  16.3067,
    16.5193,  17.0005,  16.9174,
    17.6868,  18.2949,  18.1066,
    13.2172,  14.6819,  15.8281,
    14.4674,  15.3647,  16.5062,
    16.5062,  16.9891
  ),
  LON = c(
    78.4867,  78.3800,  78.4000,
    79.5941,  79.5941,  79.1288,
    79.2677,  78.0941,  78.5333,
    80.1514,  78.2500,  77.9833,
    77.9833,  79.9865,  80.4365,
    80.6350,  81.8040,  81.3340,
    83.2185,  83.8938,  83.4205,
    79.1003,  77.5997,  78.0373,
    78.8242,  79.4989,  80.6480,
    80.6480,  81.7800
  ),
  stringsAsFactors = FALSE
)

cat("✅ Coordinates loaded for", nrow(district_coords), "districts!\n")

# ── AGGREGATE CRIME DATA PER DISTRICT ────────────────────────

district_summary <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")) %>%
  group_by(DISTRICT) %>%
  summarise(
    TOTAL_CRIMES    = sum(TOTAL.IPC.CRIMES,   na.rm = TRUE),
    VIOLENT_CRIMES  = sum(VIOLENT_CRIMES,     na.rm = TRUE),
    PROPERTY_CRIMES = sum(PROPERTY_CRIMES,    na.rm = TRUE),
    WOMEN_CRIMES    = sum(WOMEN_CRIMES,       na.rm = TRUE),
    MURDER          = sum(MURDER,             na.rm = TRUE),
    THEFT           = sum(THEFT,              na.rm = TRUE),
    RAPE            = sum(RAPE,               na.rm = TRUE),
    AVG_PER_YEAR    = round(mean(TOTAL.IPC.CRIMES, na.rm = TRUE)),
    YEARS_RECORDED  = n(),
    .groups         = "drop"
  ) %>%
  left_join(district_coords, by = "DISTRICT") %>%
  filter(!is.na(LAT))  # only keep districts with coordinates

cat(paste("✅ District summary ready:", nrow(district_summary), "districts with coordinates\n"))

# ── COLOR PALETTES ───────────────────────────────────────────

pal_choropleth <- colorNumeric(
  palette = c("#fff5f0", "#fee0d2", "#fc9272", "#ef3b2c", "#67000d"),
  domain  = district_summary$TOTAL_CRIMES
)

pal_bubble <- colorFactor(
  palette = c("#e94560", "#0f3460", "#f5a623", "#27ae60"),
  domain  = c("Violent", "Property", "Women", "Other")
)

# ============================================================
# MAP 1 — CHOROPLETH: Crime Intensity by District
# ============================================================

cat("Building Map 1: Choropleth...\n")

map1 <- leaflet(district_summary) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 79.5, lat = 17.5, zoom = 7) %>%
  
  # Circle markers colored by total crime
  addCircleMarkers(
    lng         = ~LON,
    lat         = ~LAT,
    radius      = ~sqrt(TOTAL_CRIMES / 500) * 8,
    color       = ~pal_choropleth(TOTAL_CRIMES),
    fillColor   = ~pal_choropleth(TOTAL_CRIMES),
    fillOpacity = 0.85,
    weight      = 2,
    opacity     = 1,
    popup = ~paste0(
      "<div style='font-family:Arial; min-width:200px'>",
      "<h3 style='color:#e94560; margin:0'>", DISTRICT, "</h3>",
      "<hr style='margin:5px 0'>",
      "<b>Total Crimes (2001–2014):</b> ", format(TOTAL_CRIMES, big.mark=","), "<br>",
      "<b>Avg Per Year:</b> ",             format(AVG_PER_YEAR, big.mark=","), "<br>",
      "<b>Violent Crimes:</b> ",           format(VIOLENT_CRIMES, big.mark=","), "<br>",
      "<b>Property Crimes:</b> ",          format(PROPERTY_CRIMES, big.mark=","), "<br>",
      "<b>Crimes Against Women:</b> ",     format(WOMEN_CRIMES, big.mark=","), "<br>",
      "</div>"
    ),
    label = ~paste0(DISTRICT, ": ", format(TOTAL_CRIMES, big.mark=",")),
    labelOptions = labelOptions(
      style     = list("font-weight" = "bold", "font-size" = "13px"),
      textsize  = "13px",
      direction = "auto"
    )
  ) %>%
  
  # Legend
  addLegend(
    position = "bottomright",
    pal      = pal_choropleth,
    values   = ~TOTAL_CRIMES,
    title    = "Total Crimes<br>(2001–2014)",
    labFormat = labelFormat(big.mark = ","),
    opacity  = 0.8
  ) %>%
  
  # Title
  addControl(
    html     = "<div style='background:white; padding:10px; border-radius:5px;
                font-family:Arial; font-size:14px; font-weight:bold; color:#0f3460'>
                🔴 Crime Intensity Map — AP/Telangana<br>
                <span style='font-size:11px; color:#666'>Click any circle for details</span>
                </div>",
    position = "topleft"
  )

saveWidget(map1, "maps/map1_choropleth.html", selfcontained = TRUE)
cat("✅ Map 1 saved: maps/map1_choropleth.html\n")

# ============================================================
# MAP 2 — HYDERABAD ZOOM: Clickable Pin Map
# ============================================================

cat("Building Map 2: Hyderabad zoom...\n")

hyd_data <- district_summary %>%
  filter(DISTRICT %in% c("HYDERABAD CITY", "CYBERABAD", "RANGA REDDY"))

# Risk level color
getRiskColor <- function(crimes) {
  if (crimes > 50000) return("#8b0000")
  if (crimes > 20000) return("#e94560")
  if (crimes > 10000) return("#f5a623")
  return("#27ae60")
}

map2 <- leaflet(hyd_data) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = 78.45, lat = 17.38, zoom = 11) %>%
  
  # Hyderabad districts as large pins
  addCircleMarkers(
    lng         = ~LON,
    lat         = ~LAT,
    radius      = 35,
    color       = "#ffffff",
    fillColor   = ~sapply(TOTAL_CRIMES, getRiskColor),
    fillOpacity = 0.75,
    weight      = 2,
    popup = ~paste0(
      "<div style='font-family:Arial; min-width:220px; background:#1a1a2e;
                    color:white; padding:10px; border-radius:8px'>",
      "<h3 style='color:#e94560; margin:0 0 8px 0'>📍 ", DISTRICT, "</h3>",
      "<table style='width:100%; font-size:13px'>",
      "<tr><td>🔢 Total Crimes:</td><td><b>", format(TOTAL_CRIMES, big.mark=","), "</b></td></tr>",
      "<tr><td>📅 Avg/Year:</td><td><b>",     format(AVG_PER_YEAR, big.mark=","), "</b></td></tr>",
      "<tr><td>⚔️ Violent:</td><td><b>",      format(VIOLENT_CRIMES, big.mark=","), "</b></td></tr>",
      "<tr><td>🏠 Property:</td><td><b>",     format(PROPERTY_CRIMES, big.mark=","), "</b></td></tr>",
      "<tr><td>👩 Women:</td><td><b>",        format(WOMEN_CRIMES, big.mark=","), "</b></td></tr>",
      "<tr><td>🔪 Murder:</td><td><b>",       format(MURDER, big.mark=","), "</b></td></tr>",
      "<tr><td>💻 Theft:</td><td><b>",        format(THEFT, big.mark=","), "</b></td></tr>",
      "</table></div>"
    ),
    label = ~DISTRICT,
    labelOptions = labelOptions(
      style     = list("font-weight" = "bold", "font-size" = "14px",
                       "color" = "white", "background" = "#0f3460",
                       "padding" = "5px 10px", "border-radius" = "5px"),
      direction = "top",
      permanent = TRUE
    )
  ) %>%
  
  # Title
  addControl(
    html = "<div style='background:#0f3460; color:white; padding:10px;
             border-radius:5px; font-family:Arial; font-size:13px'>
             📍 <b>Hyderabad Region Crime Map</b><br>
             <span style='font-size:11px; opacity:0.8'>
             Click circles for detailed stats</span></div>",
    position = "topleft"
  ) %>%
  
  addLegend(
    position = "bottomright",
    colors   = c("#8b0000", "#e94560", "#f5a623", "#27ae60"),
    labels   = c("Very High (50k+)", "High (20k–50k)",
                 "Medium (10k–20k)", "Low (<10k)"),
    title    = "Risk Level",
    opacity  = 0.9
  )

saveWidget(map2, "maps/map2_hyderabad.html", selfcontained = TRUE)
cat("✅ Map 2 saved: maps/map2_hyderabad.html\n")

# ============================================================
# MAP 3 — HEATMAP OVERLAY: Crime Density
# ============================================================

cat("Building Map 3: Heatmap overlay...\n")

# Expand rows — repeat each district proportional to crime count
# so density is visible as a heatmap using opacity layers
heat_data <- district_summary %>%
  filter(!is.na(LAT)) %>%
  mutate(INTENSITY = scales::rescale(TOTAL_CRIMES, to = c(0.1, 1.0)))

map3 <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(lng = 79.5, lat = 17.5, zoom = 7) %>%
  
  # Base layer — all districts faint
  addCircleMarkers(
    data        = heat_data,
    lng         = ~LON,
    lat         = ~LAT,
    radius      = 40,
    color       = NA,
    fillColor   = "#ff4444",
    fillOpacity = ~INTENSITY * 0.15,
    weight      = 0
  ) %>%
  
  # Mid layer — bigger glow
  addCircleMarkers(
    data        = heat_data,
    lng         = ~LON,
    lat         = ~LAT,
    radius      = 25,
    color       = NA,
    fillColor   = "#ff2222",
    fillOpacity = ~INTENSITY * 0.25,
    weight      = 0
  ) %>%
  
  # Hot core layer
  addCircleMarkers(
    data        = heat_data,
    lng         = ~LON,
    lat         = ~LAT,
    radius      = ~INTENSITY * 15,
    color       = NA,
    fillColor   = "#ff0000",
    fillOpacity = ~INTENSITY * 0.6,
    weight      = 0,
    popup = ~paste0(
      "<b>", DISTRICT, "</b><br>",
      "Total Crimes: ", format(TOTAL_CRIMES, big.mark = ",")
    )
  ) %>%
  
  # White dot center for highest crime areas
  addCircleMarkers(
    data        = heat_data %>% filter(TOTAL_CRIMES > 20000),
    lng         = ~LON,
    lat         = ~LAT,
    radius      = 5,
    color       = "white",
    fillColor   = "white",
    fillOpacity = 1,
    weight      = 1,
    label       = ~DISTRICT,
    labelOptions = labelOptions(
      style     = list("color" = "white", "background" = "#333",
                       "font-weight" = "bold", "padding" = "3px 8px"),
      direction = "top"
    )
  ) %>%
  
  addControl(
    html = "<div style='background:#0f3460; color:white; padding:10px;
             border-radius:5px; font-family:Arial; font-size:13px'>
             🌡️ <b>Crime Density Heatmap</b><br>
             <span style='font-size:11px; opacity:0.8'>
             Brighter = higher crime concentration</span></div>",
    position = "topleft"
  )

saveWidget(map3, "maps/map3_heatmap.html", selfcontained = TRUE)
cat("✅ Map 3 saved: maps/map3_heatmap.html\n")

# ============================================================
# MAP 4 — BUBBLE MAP: Crime Type Breakdown per District
# ============================================================

cat("Building Map 4: Crime type bubbles...\n")

map4 <- leaflet(district_summary) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 79.5, lat = 17.5, zoom = 7) %>%
  
  # Violent crimes — RED bubbles
  addCircleMarkers(
    data        = district_summary,
    lng         = ~LON,
    lat         = ~LAT,
    radius      = ~sqrt(VIOLENT_CRIMES / 200) * 5,
    color       = "#e94560",
    fillColor   = "#e94560",
    fillOpacity = 0.6,
    weight      = 1,
    group       = "⚔️ Violent Crimes",
    popup = ~paste0("<b>", DISTRICT, "</b><br>Violent Crimes: ",
                    format(VIOLENT_CRIMES, big.mark=","))
  ) %>%
  
  # Property crimes — BLUE bubbles
  addCircleMarkers(
    data        = district_summary,
    lng         = ~LON,
    lat         = ~LAT,
    radius      = ~sqrt(PROPERTY_CRIMES / 200) * 5,
    color       = "#0f3460",
    fillColor   = "#0f3460",
    fillOpacity = 0.6,
    weight      = 1,
    group       = "🏠 Property Crimes",
    popup = ~paste0("<b>", DISTRICT, "</b><br>Property Crimes: ",
                    format(PROPERTY_CRIMES, big.mark=","))
  ) %>%
  
  # Women crimes — ORANGE bubbles
  addCircleMarkers(
    data        = district_summary,
    lng         = ~LON,
    lat         = ~LAT,
    radius      = ~sqrt(WOMEN_CRIMES / 100) * 5,
    color       = "#f5a623",
    fillColor   = "#f5a623",
    fillOpacity = 0.6,
    weight      = 1,
    group       = "👩 Women Crimes",
    popup = ~paste0("<b>", DISTRICT, "</b><br>Women Crimes: ",
                    format(WOMEN_CRIMES, big.mark=","))
  ) %>%
  
  # Murder — BLACK bubbles
  addCircleMarkers(
    data        = district_summary,
    lng         = ~LON,
    lat         = ~LAT,
    radius      = ~sqrt(MURDER / 20) * 4,
    color       = "#1a1a2e",
    fillColor   = "#1a1a2e",
    fillOpacity = 0.7,
    weight      = 1,
    group       = "🔪 Murder",
    popup = ~paste0("<b>", DISTRICT, "</b><br>Murder Cases: ",
                    format(MURDER, big.mark=","))
  ) %>%
  
  # Layer toggle control
  addLayersControl(
    overlayGroups = c("⚔️ Violent Crimes", "🏠 Property Crimes",
                      "👩 Women Crimes",   "🔪 Murder"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addControl(
    html = "<div style='background:white; padding:10px; border-radius:5px;
             font-family:Arial; font-size:13px; font-weight:bold; color:#0f3460'>
             📊 Crime Type Bubble Map<br>
             <span style='font-size:11px; color:#666; font-weight:normal'>
             Toggle layers • Bubble size = crime volume</span></div>",
    position = "topleft"
  )

saveWidget(map4, "maps/map4_bubbles.html", selfcontained = TRUE)
cat("✅ Map 4 saved: maps/map4_bubbles.html\n")

# ── FINAL SUMMARY ────────────────────────────────────────────

cat("\n🎉 ALL 4 MAPS SAVED to maps/ folder!\n")
cat("─────────────────────────────────────────────\n")
cat("map1_choropleth.html  — Crime intensity (click for stats)\n")
cat("map2_hyderabad.html   — Hyderabad zoom (dark theme)\n")
cat("map3_heatmap.html     — Crime density heatmap\n")
cat("map4_bubbles.html     — Crime type bubbles (toggle layers)\n")
cat("\nOpen any .html file in your browser to see the maps!\n")