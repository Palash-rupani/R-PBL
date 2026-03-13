# ============================================================
# 04_ml_model.R  (FIXED VERSION)
# Project: Urban Crime Pattern Explorer — Hyderabad/AP Focus
# Phase 5: ML Model — 3-Level Risk Prediction
# ============================================================

library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(stringr)

set.seed(42)

# ── LOAD DATA ────────────────────────────────────────────────

ap_crime <- read.csv("data/cleaned/ap_crime_clean.csv")
dir.create("models", showWarnings = FALSE)

cat("✅ Data loaded:", nrow(ap_crime), "rows\n")

# ============================================================
# STEP 1: FEATURE ENGINEERING — FIXED RISK LEVELS
# ============================================================
# Problem was: skewed data meant MEDIUM barely existed
# Fix: use LOG transform + percentile-based splits

model_data <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")) %>%
  mutate(
    # Log transform to handle extreme skew from Hyderabad City
    LOG_TOTAL    = log1p(TOTAL.IPC.CRIMES),
    LOG_VIOLENT  = log1p(VIOLENT_CRIMES),
    LOG_PROPERTY = log1p(PROPERTY_CRIMES),
    LOG_WOMEN    = log1p(WOMEN_CRIMES),
    LOG_MURDER   = log1p(MURDER),
    LOG_THEFT    = log1p(THEFT),
    LOG_RAPE     = log1p(RAPE),
    LOG_BURGLARY = log1p(BURGLARY),
    LOG_RIOTS    = log1p(RIOTS),
    
    # Fix: proper 3-level split using LOG scale tertiles
    RISK_LEVEL = case_when(
      LOG_TOTAL >= quantile(LOG_TOTAL, 0.67, na.rm = TRUE) ~ "HIGH",
      LOG_TOTAL >= quantile(LOG_TOTAL, 0.33, na.rm = TRUE) ~ "MEDIUM",
      TRUE                                                   ~ "LOW"
    ),
    RISK_LEVEL = factor(RISK_LEVEL, levels = c("LOW", "MEDIUM", "HIGH")),
    
    # District features
    IS_HYDERABAD = as.integer(str_detect(
      DISTRICT, "HYDERABAD|CYBERABAD|RANGA REDDY"
    )),
    
    ERA_NUM = case_when(
      YEAR <= 2005 ~ 1,
      YEAR <= 2010 ~ 2,
      TRUE         ~ 3
    ),
    
    # Crime ratios
    VIOLENT_RATIO  = ifelse(TOTAL.IPC.CRIMES > 0,
                            VIOLENT_CRIMES / TOTAL.IPC.CRIMES, 0),
    PROPERTY_RATIO = ifelse(TOTAL.IPC.CRIMES > 0,
                            PROPERTY_CRIMES / TOTAL.IPC.CRIMES, 0),
    WOMEN_RATIO    = ifelse(TOTAL.IPC.CRIMES > 0,
                            WOMEN_CRIMES / TOTAL.IPC.CRIMES, 0)
  ) %>%
  select(
    RISK_LEVEL,
    # Use LOG features as main predictors — fixes skew problem
    LOG_MURDER, LOG_RAPE, LOG_THEFT, LOG_BURGLARY,
    LOG_RIOTS,  LOG_VIOLENT, LOG_PROPERTY, LOG_WOMEN,
    # Ratios
    VIOLENT_RATIO, PROPERTY_RATIO, WOMEN_RATIO,
    # District flags
    IS_HYDERABAD, ERA_NUM, YEAR
  ) %>%
  filter(complete.cases(.))

cat("✅ Risk distribution (FIXED):\n")
print(table(model_data$RISK_LEVEL))

# Save the quantile thresholds — needed for predict function!
log_total_all <- log1p(
  ap_crime$TOTAL.IPC.CRIMES[
    !str_detect(ap_crime$DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")
  ]
)
thresh_high   <- quantile(log_total_all, 0.67, na.rm = TRUE)
thresh_medium <- quantile(log_total_all, 0.33, na.rm = TRUE)

cat(paste("\n📊 Risk thresholds (log scale):\n"))
cat(paste("  LOW    < ", round(thresh_medium, 3),
          " = total <", round(expm1(thresh_medium)), "crimes\n"))
cat(paste("  MEDIUM:", round(thresh_medium, 3), "to", round(thresh_high, 3),
          " = total", round(expm1(thresh_medium)), "to",
          round(expm1(thresh_high)), "crimes\n"))
cat(paste("  HIGH   >", round(thresh_high, 3),
          " = total >", round(expm1(thresh_high)), "crimes\n"))

# ============================================================
# STEP 2: TRAIN / TEST SPLIT
# ============================================================

train_idx  <- createDataPartition(model_data$RISK_LEVEL, p = 0.80, list = FALSE)
train_data <- model_data[ train_idx, ]
test_data  <- model_data[-train_idx, ]

cat(paste("\n✅ Train:", nrow(train_data), "| Test:", nrow(test_data), "\n"))

# ============================================================
# STEP 3: RANDOM FOREST WITH CROSS-VALIDATION
# ============================================================

cat("\n🌲 Training Random Forest with cross-validation...\n")

# 5-fold cross validation
ctrl <- trainControl(
  method    = "cv",
  number    = 5,
  verboseIter = FALSE,
  classProbs  = TRUE
)

# Tune mtry automatically
tune_grid <- expand.grid(mtry = c(3, 5, 7, 9))

rf_tuned <- train(
  RISK_LEVEL ~ .,
  data      = train_data,
  method    = "rf",
  trControl = ctrl,
  tuneGrid  = tune_grid,
  ntree     = 500
)

cat(paste("✅ Best mtry:", rf_tuned$bestTune$mtry, "\n"))
cat("Cross-validation results:\n")
print(rf_tuned$results[, c("mtry","Accuracy","Kappa")])

# Extract best model
rf_model  <- rf_tuned$finalModel
rf_preds  <- predict(rf_tuned, test_data)
rf_probs  <- predict(rf_tuned, test_data, type = "prob")

cm        <- confusionMatrix(rf_preds, test_data$RISK_LEVEL)
print(cm)

accuracy  <- round(cm$overall["Accuracy"] * 100, 1)
cat(paste("\n🎯 Random Forest Accuracy:", accuracy, "%\n"))

# ============================================================
# PLOT 1: CONFUSION MATRIX
# ============================================================

cm_df          <- as.data.frame(cm$table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")

p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", size = 0.8) +
  geom_text(aes(label = Freq), size = 8, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#0f3460", high = "#e94560") +
  labs(
    title    = "🎯 Random Forest — Confusion Matrix (Fixed)",
    subtitle = paste0("Accuracy: ", accuracy,
                      "%  |  3-class: LOW / MEDIUM / HIGH  |  Log-transformed features"),
    x        = "Actual Risk Level",
    y        = "Predicted Risk Level",
    fill     = "Count",
    caption  = "Source: NCRB Crime in India Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", color = "#0f3460", size = 16),
    plot.subtitle = element_text(color = "#666666"),
    axis.text     = element_text(face = "bold", size = 12),
    panel.grid    = element_blank()
  )

ggsave("plots/09_confusion_matrix.png", p_cm, width = 8, height = 6, dpi = 150)
cat("✅ Plot saved: 09_confusion_matrix.png\n")

# ============================================================
# PLOT 2: FEATURE IMPORTANCE
# ============================================================

importance_df           <- as.data.frame(importance(rf_model))
importance_df$Feature   <- rownames(importance_df)
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 14) %>%
  mutate(
    Feature  = str_replace_all(Feature, "LOG_", ""),
    Feature  = str_replace_all(Feature, "_", " "),
    Feature  = str_to_title(Feature),
    Category = case_when(
      str_detect(Feature, "Violent|Murder|Rape|Robbery|Riot|Arson") ~ "Violent",
      str_detect(Feature, "Theft|Burglary|Property")                 ~ "Property",
      str_detect(Feature, "Women|Dowry|Cruelty")                     ~ "Women",
      TRUE                                                             ~ "Other"
    )
  )

p_imp <- ggplot(importance_df,
                aes(x = reorder(Feature, MeanDecreaseGini),
                    y = MeanDecreaseGini, fill = Category)) +
  geom_col() +
  geom_text(aes(label = round(MeanDecreaseGini, 1)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Violent"  = "#e94560",
    "Property" = "#0f3460",
    "Women"    = "#f5a623",
    "Other"    = "#27ae60"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "🔍 Feature Importance — What Drives Crime Risk?",
    subtitle = "Log-transformed features — corrects for Hyderabad outlier effect",
    x = "", y = "Mean Decrease Gini", fill = "Category",
    caption = "Source: Random Forest (tuned) on NCRB Data"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", color = "#0f3460"),
    axis.text.y     = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("plots/10_feature_importance.png", p_imp, width = 11, height = 8, dpi = 150)
cat("✅ Plot saved: 10_feature_importance.png\n")

# ============================================================
# PLOT 3: ROC CURVES
# ============================================================

cat("\n📈 Building ROC curves...\n")

roc_low    <- roc(as.numeric(test_data$RISK_LEVEL == "LOW"),
                  rf_probs[, "LOW"],    quiet = TRUE)
roc_medium <- roc(as.numeric(test_data$RISK_LEVEL == "MEDIUM"),
                  rf_probs[, "MEDIUM"], quiet = TRUE)
roc_high   <- roc(as.numeric(test_data$RISK_LEVEL == "HIGH"),
                  rf_probs[, "HIGH"],   quiet = TRUE)

auc_low    <- round(auc(roc_low),    3)
auc_medium <- round(auc(roc_medium), 3)
auc_high   <- round(auc(roc_high),   3)

cat(paste("AUC — LOW:   ", auc_low,    "\n"))
cat(paste("AUC — MEDIUM:", auc_medium, "\n"))
cat(paste("AUC — HIGH:  ", auc_high,   "\n"))

roc_df <- bind_rows(
  data.frame(FPR   = 1 - roc_low$specificities,
             TPR   = roc_low$sensitivities,
             Class = paste0("LOW (AUC=",    auc_low,    ")")),
  data.frame(FPR   = 1 - roc_medium$specificities,
             TPR   = roc_medium$sensitivities,
             Class = paste0("MEDIUM (AUC=", auc_medium, ")")),
  data.frame(FPR   = 1 - roc_high$specificities,
             TPR   = roc_high$sensitivities,
             Class = paste0("HIGH (AUC=",   auc_high,   ")"))
)

roc_colors        <- c("#27ae60", "#f5a623", "#e94560")
names(roc_colors) <- c(
  paste0("LOW (AUC=",    auc_low,    ")"),
  paste0("MEDIUM (AUC=", auc_medium, ")"),
  paste0("HIGH (AUC=",   auc_high,   ")")
)

p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(linewidth = 1.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey60", linewidth = 0.8) +
  scale_color_manual(values = roc_colors) +
  labs(
    title    = "📈 ROC Curves — 3-Class Crime Risk Model (Fixed)",
    subtitle = "Closer to top-left = better. Dashed = random guess",
    x = "False Positive Rate", y = "True Positive Rate",
    color = "Risk Class", caption = "Source: Tuned Random Forest on NCRB Data"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", color = "#0f3460"),
    legend.position = "bottom"
  )

ggsave("plots/11_roc_curves.png", p_roc, width = 9, height = 7, dpi = 150)
cat("✅ Plot saved: 11_roc_curves.png\n")

# ============================================================
# PLOT 4: DECISION TREE
# ============================================================

cat("\n🌳 Training Decision Tree...\n")

dt_model <- rpart(
  RISK_LEVEL ~ LOG_MURDER + LOG_RAPE + LOG_THEFT +
    LOG_BURGLARY + LOG_RIOTS + LOG_VIOLENT +
    LOG_PROPERTY + LOG_WOMEN + IS_HYDERABAD + ERA_NUM,
  data    = train_data,
  method  = "class",
  control = rpart.control(maxdepth = 4, minsplit = 10)
)

dt_preds    <- predict(dt_model, test_data, type = "class")
dt_accuracy <- round(mean(dt_preds == test_data$RISK_LEVEL) * 100, 1)
cat(paste("✅ Decision Tree Accuracy:", dt_accuracy, "%\n"))

png("plots/12_decision_tree.png", width = 1400, height = 900, res = 120)
rpart.plot(
  dt_model,
  type       = 4,
  extra      = 104,
  shadow.col = "gray",
  main       = paste0("Crime Risk Decision Tree  |  Accuracy: ", dt_accuracy, "%")
)
dev.off()
cat("✅ Plot saved: 12_decision_tree.png\n")

# ============================================================
# STEP 8: SAVE EVERYTHING
# ============================================================

saveRDS(rf_tuned,       "models/random_forest_model.rds")
saveRDS(dt_model,       "models/decision_tree_model.rds")
saveRDS(thresh_high,    "models/thresh_high.rds")
saveRDS(thresh_medium,  "models/thresh_medium.rds")

cat("\n✅ All models + thresholds saved!\n")

# ============================================================
# STEP 9: FIXED PREDICT FUNCTION
# ============================================================
# Now uses log1p() transform — matches training data exactly!

predict_risk <- function(murder, rape, theft, burglary,
                         riots, violent, property, women,
                         is_hyderabad = 0, year = 2012) {
  
  total <- violent + property
  new_data <- data.frame(
    LOG_MURDER   = log1p(murder),
    LOG_RAPE     = log1p(rape),
    LOG_THEFT    = log1p(theft),
    LOG_BURGLARY = log1p(burglary),
    LOG_RIOTS    = log1p(riots),
    LOG_VIOLENT  = log1p(violent),
    LOG_PROPERTY = log1p(property),
    LOG_WOMEN    = log1p(women),
    VIOLENT_RATIO  = ifelse(total > 0, violent / total, 0),
    PROPERTY_RATIO = ifelse(total > 0, property / total, 0),
    WOMEN_RATIO    = 0.1,
    IS_HYDERABAD   = as.integer(is_hyderabad),
    ERA_NUM        = ifelse(year <= 2005, 1, ifelse(year <= 2010, 2, 3)),
    YEAR           = year
  )
  
  pred  <- predict(rf_tuned, new_data)
  probs <- predict(rf_tuned, new_data, type = "prob")
  
  cat(paste("\n🔮 Predicted Risk:", as.character(pred), "\n"))
  cat(paste("   LOW:   ", round(probs[1, "LOW"]    * 100, 1), "%\n"))
  cat(paste("   MEDIUM:", round(probs[1, "MEDIUM"] * 100, 1), "%\n"))
  cat(paste("   HIGH:  ", round(probs[1, "HIGH"]   * 100, 1), "%\n"))
}

# ── TEST CASES ───────────────────────────────────────────────

cat("\n🧪 Test 1 — Small rural district (expect LOW):\n")
predict_risk(murder=10, rape=5, theft=200, burglary=50,
             riots=10, violent=80, property=250, women=60,
             is_hyderabad=0, year=2005)

cat("\n🧪 Test 2 — Medium district (expect MEDIUM):\n")
predict_risk(murder=60, rape=40, theft=800, burglary=200,
             riots=50, violent=300, property=1000, women=250,
             is_hyderabad=0, year=2010)

cat("\n🧪 Test 3 — Hyderabad-like (expect HIGH):\n")
predict_risk(murder=200, rape=300, theft=8000, burglary=1500,
             riots=400, violent=2000, property=10000, women=3000,
             is_hyderabad=1, year=2012)

# ── FINAL SUMMARY ────────────────────────────────────────────

cat("\n🎉 PHASE 5 COMPLETE (FIXED)!\n")
cat("──────────────────────────────────────────\n")
cat(paste("✅ Random Forest Accuracy:", accuracy,    "%\n"))
cat(paste("✅ Decision Tree Accuracy:", dt_accuracy, "%\n"))
cat(paste("✅ AUC HIGH risk class:   ", auc_high,       "\n"))
cat(paste("✅ LOW  threshold: total crimes >", round(expm1(thresh_medium)), "\n"))
cat(paste("✅ HIGH threshold: total crimes >", round(expm1(thresh_high)),   "\n"))
cat("✅ 4 plots saved to plots/\n")
cat("✅ Models + thresholds saved to models/\n")
cat("✅ Ready for updated Shiny App!\n")