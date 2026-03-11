# ============================================================
# 04_ml_model.R
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
# STEP 1: FEATURE ENGINEERING
# ============================================================

model_data <- ap_crime %>%
  filter(!str_detect(DISTRICT, "TOTAL|ZZ|RLY|RAILWAY")) %>%
  mutate(
    RISK_LEVEL = case_when(
      TOTAL.IPC.CRIMES >= quantile(TOTAL.IPC.CRIMES, 0.66, na.rm = TRUE) ~ "HIGH",
      TOTAL.IPC.CRIMES >= quantile(TOTAL.IPC.CRIMES, 0.33, na.rm = TRUE) ~ "MEDIUM",
      TRUE ~ "LOW"
    ),
    RISK_LEVEL = factor(RISK_LEVEL, levels = c("LOW", "MEDIUM", "HIGH")),
    
    IS_HYDERABAD = as.integer(str_detect(
      DISTRICT, "HYDERABAD|CYBERABAD|RANGA REDDY"
    )),
    
    ERA_NUM = case_when(
      YEAR <= 2005 ~ 1,
      YEAR <= 2010 ~ 2,
      YEAR <= 2014 ~ 3,
      TRUE         ~ 4
    ),
    
    VIOLENT_RATIO  = ifelse(TOTAL.IPC.CRIMES > 0,
                            VIOLENT_CRIMES / TOTAL.IPC.CRIMES, 0),
    PROPERTY_RATIO = ifelse(TOTAL.IPC.CRIMES > 0,
                            PROPERTY_CRIMES / TOTAL.IPC.CRIMES, 0),
    WOMEN_RATIO    = ifelse(TOTAL.IPC.CRIMES > 0,
                            WOMEN_CRIMES / TOTAL.IPC.CRIMES, 0)
  ) %>%
  select(
    RISK_LEVEL,
    MURDER, RAPE, KIDNAPPING...ABDUCTION,
    ROBBERY, BURGLARY, THEFT, AUTO.THEFT,
    RIOTS, CHEATING, ARSON,
    DOWRY.DEATHS, CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES,
    CAUSING.DEATH.BY.NEGLIGENCE,
    VIOLENT_CRIMES, PROPERTY_CRIMES, WOMEN_CRIMES,
    VIOLENT_RATIO, PROPERTY_RATIO, WOMEN_RATIO,
    IS_HYDERABAD, ERA_NUM, YEAR
  ) %>%
  filter(complete.cases(.))

cat(paste("✅ Model data ready:", nrow(model_data), "rows\n"))
cat("Risk distribution:\n")
print(table(model_data$RISK_LEVEL))

# ============================================================
# STEP 2: TRAIN / TEST SPLIT
# ============================================================

train_idx  <- createDataPartition(model_data$RISK_LEVEL, p = 0.80, list = FALSE)
train_data <- model_data[ train_idx, ]
test_data  <- model_data[-train_idx, ]

cat(paste("\n✅ Train:", nrow(train_data), "| Test:", nrow(test_data), "\n"))

# ============================================================
# STEP 3: RANDOM FOREST
# ============================================================

cat("\n🌲 Training Random Forest...\n")

rf_model <- randomForest(
  RISK_LEVEL  ~ .,
  data        = train_data,
  ntree       = 500,
  mtry        = 5,
  importance  = TRUE,
  keep.forest = TRUE
)

cat("✅ Random Forest trained!\n")
print(rf_model)

rf_preds <- predict(rf_model, test_data)
rf_probs <- predict(rf_model, test_data, type = "prob")

cm       <- confusionMatrix(rf_preds, test_data$RISK_LEVEL)
print(cm)

accuracy <- round(cm$overall["Accuracy"] * 100, 1)
cat(paste("\n🎯 Random Forest Accuracy:", accuracy, "%\n"))

# ============================================================
# PLOT 1: CONFUSION MATRIX
# ============================================================

cm_df <- as.data.frame(cm$table)
colnames(cm_df) <- c("Predicted", "Actual", "Freq")

p_cm <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white", size = 0.8) +
  geom_text(aes(label = Freq), size = 8, fontface = "bold",
            color = "white") +
  scale_fill_gradient(low = "#0f3460", high = "#e94560") +
  labs(
    title    = "🎯 Random Forest — Confusion Matrix",
    subtitle = paste0("Accuracy: ", accuracy, "%  |  3-class: LOW / MEDIUM / HIGH"),
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

importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 15) %>%
  mutate(
    Feature  = str_replace_all(Feature, "\\.", " "),
    Feature  = str_to_title(Feature),
    Category = case_when(
      str_detect(Feature, "Violent|Murder|Rape|Robbery|Riot|Arson|Kidnap") ~ "Violent",
      str_detect(Feature, "Theft|Burglary|Property|Auto")                  ~ "Property",
      str_detect(Feature, "Women|Dowry|Cruelty|Modesty")                   ~ "Women",
      TRUE                                                                   ~ "Other"
    )
  )

p_imp <- ggplot(importance_df,
                aes(x = reorder(Feature, MeanDecreaseGini),
                    y = MeanDecreaseGini, fill = Category)) +
  geom_col() +
  geom_text(aes(label = round(MeanDecreaseGini, 1)),
            hjust = -0.1, size = 3.5) +
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
    subtitle = "Higher = more important for predicting risk level",
    x        = "", y = "Mean Decrease Gini", fill = "Category",
    caption  = "Source: Random Forest on NCRB Data"
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
             Class = paste0("LOW (AUC=", auc_low, ")")),
  data.frame(FPR   = 1 - roc_medium$specificities,
             TPR   = roc_medium$sensitivities,
             Class = paste0("MEDIUM (AUC=", auc_medium, ")")),
  data.frame(FPR   = 1 - roc_high$specificities,
             TPR   = roc_high$sensitivities,
             Class = paste0("HIGH (AUC=", auc_high, ")"))
)

# Build color vector safely
roc_colors        <- c("#27ae60", "#f5a623", "#e94560")
names(roc_colors) <- c(
  paste0("LOW (AUC=",    auc_low,    ")"),
  paste0("MEDIUM (AUC=", auc_medium, ")"),
  paste0("HIGH (AUC=",   auc_high,   ")")
)

p_roc <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Class)) +
  geom_line(size = 1.3) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey60", size = 0.8) +
  scale_color_manual(values = roc_colors) +
  labs(
    title    = "📈 ROC Curves — 3-Class Crime Risk Model",
    subtitle = "Closer to top-left = better model. Dashed line = random guess",
    x        = "False Positive Rate",
    y        = "True Positive Rate",
    color    = "Risk Class",
    caption  = "Source: Random Forest on NCRB Data"
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
  RISK_LEVEL ~ MURDER + RAPE + THEFT + BURGLARY +
    RIOTS + VIOLENT_CRIMES + PROPERTY_CRIMES +
    WOMEN_CRIMES + IS_HYDERABAD + ERA_NUM,
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
# STEP 8: SAVE MODELS
# ============================================================

saveRDS(rf_model, "models/random_forest_model.rds")
saveRDS(dt_model, "models/decision_tree_model.rds")
saveRDS(names(train_data), "models/feature_names.rds")

cat("\n✅ Models saved to models/\n")

# ============================================================
# STEP 9: TEST PREDICTOR FUNCTION
# ============================================================

predict_risk <- function(murder, rape, theft, burglary,
                         riots, violent, property, women,
                         is_hyderabad = 0, year = 2012) {
  
  new_data <- data.frame(
    MURDER                              = murder,
    RAPE                                = rape,
    KIDNAPPING...ABDUCTION              = 0,
    ROBBERY                             = 0,
    BURGLARY                            = burglary,
    THEFT                               = theft,
    AUTO.THEFT                          = 0,
    RIOTS                               = riots,
    CHEATING                            = 0,
    ARSON                               = 0,
    DOWRY.DEATHS                        = 0,
    CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES = 0,
    CAUSING.DEATH.BY.NEGLIGENCE         = 0,
    VIOLENT_CRIMES                      = violent,
    PROPERTY_CRIMES                     = property,
    WOMEN_CRIMES                        = women,
    VIOLENT_RATIO  = ifelse(violent + property > 0, violent/(violent+property), 0),
    PROPERTY_RATIO = ifelse(violent + property > 0, property/(violent+property), 0),
    WOMEN_RATIO    = 0.1,
    IS_HYDERABAD   = is_hyderabad,
    ERA_NUM        = ifelse(year <= 2005, 1, ifelse(year <= 2010, 2, 3)),
    YEAR           = year
  )
  
  pred  <- predict(rf_model, new_data)
  probs <- predict(rf_model, new_data, type = "prob")
  
  cat(paste("\n🔮 Predicted Risk:", as.character(pred), "\n"))
  cat(paste("   LOW:   ", round(probs[,"LOW"]    * 100, 1), "%\n"))
  cat(paste("   MEDIUM:", round(probs[,"MEDIUM"] * 100, 1), "%\n"))
  cat(paste("   HIGH:  ", round(probs[,"HIGH"]   * 100, 1), "%\n"))
}

cat("\n🧪 Test — Hyderabad-like district:\n")
predict_risk(
  murder = 80, rape = 120, theft = 2000, burglary = 500,
  riots = 100, violent = 400, property = 2500, women = 300,
  is_hyderabad = 1, year = 2012
)

# ── FINAL SUMMARY ────────────────────────────────────────────

cat("\n🎉 PHASE 5 COMPLETE!\n")
cat("──────────────────────────────────────────\n")
cat(paste("✅ Random Forest Accuracy:", accuracy,    "%\n"))
cat(paste("✅ Decision Tree Accuracy:", dt_accuracy, "%\n"))
cat(paste("✅ AUC HIGH risk class:   ", auc_high,       "\n"))
cat("✅ 4 plots saved to plots/\n")
cat("✅ Models saved to models/\n")
cat("✅ Ready for Phase 6 — Shiny App!\n")