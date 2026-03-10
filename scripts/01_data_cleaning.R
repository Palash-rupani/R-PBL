# ============================================================
# 01_data_cleaning.R
# Project: Urban Crime Pattern Explorer — Hyderabad/AP Focus
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)

# ── STEP 1: LOAD ALL FILES ───────────────────────────────────

ipc_old  <- read.csv("data/raw/crime_in_india/crime/01_District_wise_crimes_committed_IPC_2001_2012.csv")
ipc_2013 <- read.csv("data/raw/crime_in_india/crime/01_District_wise_crimes_committed_IPC_2013.csv")
ipc_2014 <- read.csv("data/raw/crime_in_india/crime/01_District_wise_crimes_committed_IPC_2014.csv")
women    <- read.csv("data/raw/crime_in_india/crime/42_District_wise_crimes_committed_against_women_2001_2012.csv")
cyber    <- read.csv("data/raw/cyber_crimes.csv")

cat("✅ All files loaded!\n")

# ── STEP 2: FIX ipc_2014 COLUMN NAMES ───────────────────────
# 2014 has different column names — rename to match old format

ipc_2014_clean <- ipc_2014 %>%
  rename(
    STATE.UT                              = States.UTs,
    DISTRICT                              = District,
    YEAR                                  = Year,
    MURDER                                = Murder,
    ATTEMPT.TO.MURDER                     = Attempt.to.commit.Murder,
    CULPABLE.HOMICIDE.NOT.AMOUNTING.TO.MURDER = Culpable.Homicide.not.amounting.to.Murder,
    RAPE                                  = Rape,
    KIDNAPPING...ABDUCTION                = Kidnapping...Abduction_Total,
    DACOITY                               = Dacoity,
    ROBBERY                               = Robbery,
    BURGLARY                              = Criminal.Trespass.Burglary,
    THEFT                                 = Theft,
    AUTO.THEFT                            = Auto.Theft,
    RIOTS                                 = Riots,
    CRIMINAL.BREACH.OF.TRUST              = Criminal.Breach.of.Trust,
    CHEATING                              = Cheating,
    ARSON                                 = Arson,
    DOWRY.DEATHS                          = Dowry.Deaths,
    CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES   = Cruelty.by.Husband.or.his.Relatives,
    CAUSING.DEATH.BY.NEGLIGENCE           = Causing.Death.by.Negligence,
    OTHER.IPC.CRIMES                      = Other.IPC.crimes,
    TOTAL.IPC.CRIMES                      = Total.Cognizable.IPC.crimes
  ) %>%
  # Keep only columns that exist in ipc_old
  select(any_of(colnames(ipc_old)))

cat("✅ ipc_2014 columns fixed!\n")

# ── STEP 3: STANDARDISE STATE NAMES ─────────────────────────
# ipc_old uses UPPER CASE, ipc_2013 uses Title Case — fix to UPPER

ipc_2013 <- ipc_2013 %>%
  mutate(STATE.UT = str_to_upper(STATE.UT),
         DISTRICT = str_to_upper(DISTRICT))

ipc_2014_clean <- ipc_2014_clean %>%
  mutate(STATE.UT = str_to_upper(STATE.UT),
         DISTRICT = str_to_upper(DISTRICT))

cat("✅ State names standardised!\n")

# ── STEP 4: COMBINE ALL IPC FILES ───────────────────────────

ipc_all <- bind_rows(ipc_old, ipc_2013, ipc_2014_clean)

cat(paste("✅ Combined IPC data:", nrow(ipc_all), "rows,", 
          min(ipc_all$YEAR), "to", max(ipc_all$YEAR), "\n"))

# ── STEP 5: FILTER FOR ANDHRA PRADESH + TELANGANA ───────────
# Note: Telangana = Andhra Pradesh before 2014

ap_crime <- ipc_all %>%
  filter(STATE.UT %in% c("ANDHRA PRADESH", "TELANGANA")) %>%
  arrange(YEAR, DISTRICT)

cat(paste("✅ AP/Telangana rows:", nrow(ap_crime), "\n"))
cat("Districts found:\n")
print(unique(ap_crime$DISTRICT))

# ── STEP 6: ADD USEFUL COLUMNS ───────────────────────────────

ap_crime <- ap_crime %>%
  mutate(
    # Flag Hyderabad specifically
    IS_HYDERABAD = ifelse(str_detect(DISTRICT, "HYDERABAD|RANGAREDDY|MEDCHAL"), TRUE, FALSE),
    
    # Group era
    ERA = case_when(
      YEAR <= 2005 ~ "2001-2005",
      YEAR <= 2010 ~ "2006-2010",
      YEAR <= 2014 ~ "2011-2014",
      TRUE         ~ "2015+"
    ),
    
    # Violent crime total
    VIOLENT_CRIMES = MURDER + ATTEMPT.TO.MURDER + RAPE + KIDNAPPING...ABDUCTION + ROBBERY + DACOITY,
    
    # Property crime total
    PROPERTY_CRIMES = THEFT + AUTO.THEFT + BURGLARY,
    
    # Women safety index
    WOMEN_CRIMES = DOWRY.DEATHS + ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY +
      CRUELTY.BY.HUSBAND.OR.HIS.RELATIVES + INSULT.TO.MODESTY.OF.WOMEN,
    
    # Crime risk label for ML (High if above median total IPC crimes)
    RISK_LEVEL = ifelse(TOTAL.IPC.CRIMES > median(TOTAL.IPC.CRIMES, na.rm = TRUE),
                        "HIGH", "LOW")
  )

cat("✅ New columns added!\n")

# ── STEP 7: FIX CYBER DATA ───────────────────────────────────
# It's tab-separated inside one messy column — fix it!

cyber_clean <- cyber %>%
  # The whole thing is in one column — split by tab
  separate(col = 1,
           into = c("SNo", "Category", "STATE", "crimes_2016", 
                    "crimes_2017", "crimes_2018", "pct_share",
                    "population_lakhs", "rate_per_lakh"),
           sep = "\t",
           convert = TRUE) %>%
  mutate(STATE = str_to_upper(str_trim(STATE))) %>%
  filter(!is.na(crimes_2018))

cat("✅ Cyber data fixed!\n")
cat("Cyber states:\n")
print(unique(cyber_clean$STATE))

# Filter cyber for AP/Telangana
ap_cyber <- cyber_clean %>%
  filter(str_detect(STATE, "ANDHRA|TELANGANA"))

cat(paste("✅ AP/Telangana cyber rows:", nrow(ap_cyber), "\n"))

# ── STEP 8: CLEAN WOMEN DATA ─────────────────────────────────

women_clean <- women %>%
  mutate(STATE.UT = str_to_upper(STATE.UT),
         DISTRICT = str_to_upper(DISTRICT)) %>%
  filter(STATE.UT %in% c("ANDHRA PRADESH", "TELANGANA")) %>%
  mutate(TOTAL_WOMEN_CRIMES = Rape + Kidnapping.and.Abduction +
           Dowry.Deaths + Assault.on.women.with.intent.to.outrage.her.modesty +
           Cruelty.by.Husband.or.his.Relatives)

cat(paste("✅ Women crime data cleaned:", nrow(women_clean), "rows\n"))

# ── STEP 9: REMOVE NAs AND DUPLICATES ────────────────────────

ap_crime    <- ap_crime %>% distinct() %>% filter(!is.na(TOTAL.IPC.CRIMES))
women_clean <- women_clean %>% distinct() %>% filter(!is.na(TOTAL_WOMEN_CRIMES))

cat(paste("✅ Final IPC rows after cleaning:", nrow(ap_crime), "\n"))
cat(paste("✅ Final Women crime rows:", nrow(women_clean), "\n"))

# ── STEP 10: SAVE CLEANED FILES ──────────────────────────────

dir.create("data/cleaned", showWarnings = FALSE)

write.csv(ap_crime,    "data/cleaned/ap_crime_clean.csv",    row.names = FALSE)
write.csv(women_clean, "data/cleaned/women_crime_clean.csv", row.names = FALSE)
write.csv(cyber_clean, "data/cleaned/cyber_clean.csv",       row.names = FALSE)

cat("\n🎉 ALL DONE! Cleaned files saved to data/cleaned/\n")
cat("Files created:\n")
cat("  → data/cleaned/ap_crime_clean.csv\n")
cat("  → data/cleaned/women_crime_clean.csv\n")
cat("  → data/cleaned/cyber_clean.csv\n")

# ── STEP 11: QUICK SANITY CHECK ──────────────────────────────

cat("\n📊 Quick Preview:\n")
cat(paste("Years covered:", min(ap_crime$YEAR), "to", max(ap_crime$YEAR), "\n"))
cat(paste("Districts:", length(unique(ap_crime$DISTRICT)), "\n"))
cat(paste("HIGH risk rows:", sum(ap_crime$RISK_LEVEL == "HIGH"), "\n"))
cat(paste("LOW risk rows:",  sum(ap_crime$RISK_LEVEL == "LOW"),  "\n"))