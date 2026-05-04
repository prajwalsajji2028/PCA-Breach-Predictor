# =============================================================================
# PREDICTING RBI PROMPT CORRECTIVE ACTION (PCA) BREACH PROBABILITY
# Panel Probit Analysis — Indian Scheduled Commercial Banks
# Dataset: PCA_Panel_Dataset_v3.xlsx  |  FY2005–FY2024  |  139 Banks
# =============================================================================
# Author  : [Your Name]
# Data    : RBI Statistical Tables, DBIE, WSS Table 5, MoSPI
# Method  : Random Effects Panel Probit (primary) + Robustness Checks
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 0: INSTALL & LOAD PACKAGES
# ─────────────────────────────────────────────────────────────────────────────

# Run this block once to install all required packages
packages <- c(
  "readxl",        # Read Excel file
  "dplyr",         # Data manipulation
  "tidyr",         # Reshaping
  "ggplot2",       # Visualisation
  "plm",           # Panel data models (RE probit via pglm)
  "pglm",          # Panel GLM — random effects probit
  "lmtest",        # Coefficient tests
  "sandwich",      # Clustered standard errors
  "margins",       # Average marginal effects
  "pROC",          # ROC curve & AUC
  "car",           # VIF test
  "corrplot",      # Correlation matrix plot
  "stargazer",     # Regression output tables
  "knitr",         # Table formatting
  "ggcorrplot",    # Correlation heatmap
  "scales",        # Plot formatting
  "MASS",          # Ordered probit (polr)
  "survival",      # Kaplan-Meier survival analysis
  "survminer",     # KM plot
  "haven",         # Export to Stata .dta
  "writexl"        # Export results to Excel
)

# Uncomment the line below to install:
# install.packages(packages)

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

cat("✅ All packages loaded.\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1: LOAD DATA
# ─────────────────────────────────────────────────────────────────────────────

# ── Set your working directory to the folder containing the Excel file ────────
# setwd("C:/Your/Folder/Path")   # Windows
# setwd("~/Documents/Research")  # Mac/Linux

df_raw <- read_excel("PCA_Panel_Dataset_Final.xlsx", sheet = "Panel_Dataset", skip = 1)

# Remove the title row (row 1 in Excel is the merged title — skip=1 handles it)
# Column names are now in row 1 of the imported df
cat("Raw data loaded:", nrow(df_raw), "rows ×", ncol(df_raw), "columns\n")
cat("Columns:", paste(names(df_raw), collapse = ", "), "\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: DATA CLEANING & PREPARATION
# ─────────────────────────────────────────────────────────────────────────────

df <- df_raw %>%
  # Rename for easier typing in R
  rename(
    fy                 = FY,
    bank               = BANK,
    bank_group         = BANK_GROUP,
    psb                = PSB_DUMMY,
    pca_breach         = PCA_BREACH,
    merger             = MERGER_DUMMY,
    total_assets       = TOTAL_ASSETS_CR,
    advances           = TOTAL_ADVANCES_CR,
    deposits           = TOTAL_DEPOSITS_CR,
    investments        = INVESTMENTS_CR,
    capital            = CAPITAL_CR,
    reserves           = RESERVES_CR,
    borrowings         = BORROWINGS_CR,
    int_earned         = INTEREST_EARNED_CR,
    other_income       = OTHER_INCOME_CR,
    int_expended       = INTEREST_EXPENDED_CR,
    staff_exp          = STAFF_EXP_CR,
    op_exp             = OP_EXP_CR,
    net_profit         = NET_PROFIT_CR,
    op_profit          = OP_PROFIT_CR,
    nii                = NET_INTEREST_INCOME_CR,
    employees          = TOTAL_EMPLOYEES,
    gnpa_amt           = GNPA_AMT_CR,
    gnpa_ratio         = GNPA_RATIO_PCT,
    roa                = ROA_PCT,
    roe                = ROE_PCT,
    nim                = NIM_PCT,
    cost_income        = COST_INCOME_PCT,
    cd_ratio           = CD_RATIO_PCT,
    inv_dep            = INV_DEP_RATIO_PCT,
    log_assets         = LOG_ASSETS,
    staff_ratio        = STAFF_EXP_RATIO_PCT,
    biz_per_emp        = BUSINESS_PER_EMP_CR,
    gdp_growth         = GDP_GROWTH_PCT,
    m3_growth          = M3_GROWTH_PCT,
    repo_rate          = REPO_RATE_PCT,
    cpi                = CPI_INFLATION_PCT,
    pca_rev2017        = PCA_REVISION_2017,
    pca_rev2022        = PCA_REVISION_2022,
    ibc                = IBC_DUMMY,
    covid              = COVID_DUMMY,
    moratorium         = MORATORIUM,
    credit_growth      = CREDIT_GROWTH_PCT,
    deposit_growth     = DEPOSIT_GROWTH_PCT,
    tier1_crar         = TIER1_CRAR_PCT,
    tier2_crar         = TIER2_CRAR_PCT,
    total_crar         = TOTAL_CRAR_PCT,
    crar_framework     = CRAR_FRAMEWORK,
    repo_avg           = REPO_RATE_AVG_PCT,
    crr                = CRR_AVG_PCT,
    slr                = SLR_AVG_PCT,
    call_money         = CALL_MONEY_AVG_PCT,
    tbill_91d          = TBILL_91D_AVG_PCT,
    tbill_364d         = TBILL_364D_AVG_PCT,
    gsec_10yr          = GSEC_10YR_AVG_PCT,
    inr_usd            = INR_USD_AVG,
    cd_ratio_sys       = CD_RATIO_SYSTEM_PCT,
    inv_dep_sys        = INV_DEP_SYSTEM_PCT
  ) %>%
  # Ensure correct types
  mutate(
    fy          = as.integer(fy),
    pca_breach  = as.integer(pca_breach),
    psb         = as.integer(psb),
    merger      = as.integer(merger),
    bank_group  = as.factor(bank_group),
    bank        = as.character(bank),
    # Create numeric bank ID for panel indexing
    bank_id     = as.integer(as.factor(bank))
  )

cat("Cleaned dataset:", nrow(df), "rows |", n_distinct(df$bank), "banks |",
    n_distinct(df$fy), "years\n")
cat("PCA breach events:", sum(df$pca_breach, na.rm = TRUE), "\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3: CREATE LAGGED VARIABLES (1-year lag within each bank)
# ─────────────────────────────────────────────────────────────────────────────

df <- df %>%
  arrange(bank, fy) %>%
  group_by(bank) %>%
  mutate(
    # Core predictors — lagged 1 year (key for predictive validity)
    L1_total_crar    = lag(total_crar,    1),
    L1_tier1_crar    = lag(tier1_crar,    1),
    L1_gnpa_ratio    = lag(gnpa_ratio,    1),
    L1_roa           = lag(roa,           1),
    L1_roe           = lag(roe,           1),
    L1_nim           = lag(nim,           1),
    L1_cost_income   = lag(cost_income,   1),
    L1_cd_ratio      = lag(cd_ratio,      1),
    L1_inv_dep       = lag(inv_dep,       1),
    L1_log_assets    = lag(log_assets,    1),
    L1_credit_growth = lag(credit_growth, 1),
    L1_deposit_growth= lag(deposit_growth,1),
    L1_staff_ratio   = lag(staff_ratio,   1),
    # 2-year lag for credit growth (boom-bust cycle)
    L2_credit_growth = lag(credit_growth, 2),
    L2_gnpa_ratio    = lag(gnpa_ratio,    2)
  ) %>%
  ungroup()

cat("Lagged variables created.\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4: WINSORISATION (1st–99th percentile for continuous vars)
# ─────────────────────────────────────────────────────────────────────────────

winsorise <- function(x, p = 0.01) {
  lo <- quantile(x, p,     na.rm = TRUE)
  hi <- quantile(x, 1 - p, na.rm = TRUE)
  pmax(pmin(x, hi), lo)
}

win_vars <- c("L1_total_crar", "L1_gnpa_ratio", "L1_roa", "L1_nim",
              "L1_cost_income", "L1_cd_ratio", "L1_log_assets",
              "L1_credit_growth", "L2_credit_growth")

df <- df %>%
  mutate(across(all_of(win_vars), winsorise))

cat("Winsorisation applied to:", paste(win_vars, collapse = ", "), "\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5: DEFINE ESTIMATION SAMPLES
# ─────────────────────────────────────────────────────────────────────────────

# Full sample: FY2008–FY2024 (CRAR available from FY2008)
df_full <- df %>%
  filter(fy >= 2008, fy <= 2024) %>%
  filter(merger == 0)    # Exclude year of merger

# Core regression sample: complete cases on main regressors
core_vars <- c("pca_breach", "L1_total_crar", "L1_gnpa_ratio", "L1_roa",
               "L1_cost_income", "L1_log_assets", "L1_credit_growth",
               "gdp_growth", "repo_rate", "psb",
               "pca_rev2017", "ibc", "covid")

df_reg <- df_full %>%
  drop_na(all_of(core_vars))

# PSB-only subsample (for robustness)
df_psb <- df_reg %>% filter(psb == 1)

# Pre-2017 revision subsample
df_pre2017  <- df_reg %>% filter(fy < 2018)
df_post2017 <- df_reg %>% filter(fy >= 2018)

cat("\nSample sizes:\n")
cat("  Full (FY2008-2024, no mergers) :", nrow(df_full), "obs |",
    n_distinct(df_full$bank), "banks\n")
cat("  Regression (complete cases)    :", nrow(df_reg), "obs |",
    n_distinct(df_reg$bank), "banks\n")
cat("  PCA events in regression sample:", sum(df_reg$pca_breach), "\n")
cat("  PSB subsample                  :", nrow(df_psb), "obs\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6: DESCRIPTIVE STATISTICS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== DESCRIPTIVE STATISTICS ======\n")

desc_vars <- c("pca_breach", "total_crar", "gnpa_ratio", "roa", "nim",
               "cost_income", "log_assets", "cd_ratio", "credit_growth",
               "gdp_growth", "repo_rate", "cpi", "psb")

# Full sample summary
summary_full <- df_reg %>%
  select(all_of(desc_vars)) %>%
  summary()
print(summary_full)

# Mean comparison: PCA vs Non-PCA
cat("\n--- MEAN COMPARISON: PCA=1 vs PCA=0 ---\n")
mean_compare <- df_reg %>%
  group_by(pca_breach) %>%
  summarise(across(all_of(setdiff(desc_vars, "pca_breach")),
                   ~ round(mean(.x, na.rm = TRUE), 3))) %>%
  t()
print(mean_compare)

# T-tests for key variables
cat("\n--- T-TESTS (PCA vs Non-PCA) ---\n")
ttest_vars <- c("total_crar", "gnpa_ratio", "roa", "nim", "cost_income", "log_assets")
for (v in ttest_vars) {
  tt <- t.test(df_reg[[v]] ~ df_reg$pca_breach)
  cat(sprintf("  %-20s  t = %6.3f  p = %.4f  [PCA mean=%.3f | Non-PCA mean=%.3f]\n",
              v, tt$statistic, tt$p.value,
              tt$estimate[2], tt$estimate[1]))
}


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 7: CORRELATION MATRIX & VIF CHECK
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== CORRELATION MATRIX ======\n")

cor_vars <- c("L1_total_crar", "L1_gnpa_ratio", "L1_roa", "L1_nim",
              "L1_cost_income", "L1_cd_ratio", "L1_log_assets",
              "L1_credit_growth", "gdp_growth", "repo_rate", "cpi", "psb")

cor_mat <- df_reg %>%
  select(all_of(cor_vars)) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)
print(cor_mat)

# Correlation heatmap
corrplot(cor_mat, method = "color", type = "upper",
         tl.cex = 0.7, tl.col = "black",
         title = "Correlation Matrix — PCA Predictor Variables",
         mar = c(0, 0, 2, 0))

# VIF check using OLS proxy (LPM)
cat("\n--- VIF CHECK (via LPM) ---\n")
lpm_vif <- lm(pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
                L1_cost_income + L1_log_assets + L1_credit_growth +
                gdp_growth + repo_rate + cpi + psb +
                pca_rev2017 + ibc + covid,
              data = df_reg)
vif_vals <- vif(lpm_vif)
print(round(vif_vals, 3))
cat("Max VIF:", round(max(vif_vals), 2),
    if (max(vif_vals) < 5) "✅ No multicollinearity concern\n"
    else "⚠️ Review high-VIF variables\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 8: PANEL DATA SETUP
# ─────────────────────────────────────────────────────────────────────────────

# Set panel structure using plm
pdata <- pdata.frame(df_reg, index = c("bank_id", "fy"))
cat("\nPanel data frame created.\n")
cat("  Balanced?", is.pbalanced(pdata), "\n")
cat("  Dimensions:", pdim(pdata)$nT$n, "banks ×",
    pdim(pdata)$nT$T, "avg periods\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 9: MODEL 1 — POOLED PROBIT (BASELINE)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== MODEL 1: POOLED PROBIT ======\n")

m1_pooled <- glm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + cpi +
    psb + pca_rev2017 + ibc + covid + merger,
  data   = df_reg,
  family = binomial(link = "probit")
)

# Clustered standard errors at bank level
m1_cse <- coeftest(m1_pooled,
                   vcov = vcovCL(m1_pooled,
                                 cluster = ~ bank_id,
                                 data    = df_reg))
print(m1_cse)

# Average marginal effects
m1_ame <- margins(m1_pooled)
cat("\n--- AVERAGE MARGINAL EFFECTS (Pooled Probit) ---\n")
print(summary(m1_ame))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 10: MODEL 2 — RANDOM EFFECTS PANEL PROBIT (PRIMARY MODEL)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== MODEL 2: RANDOM EFFECTS PANEL PROBIT (PRIMARY) ======\n")

# pglm implements RE probit with Gaussian quadrature
m2_re <- pglm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + cpi +
    psb + pca_rev2017 + ibc + covid,
  data   = df_reg,
  family = binomial(link = "probit"),
  model  = "random",
  index  = c("bank_id", "fy"),
  method = "bfgs"
)

cat("\n--- PRIMARY MODEL RESULTS ---\n")
print(summary(m2_re))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 11: MODEL 3 — EXTENDED MODEL (+ NIM, CD RATIO, TIER1 CRAR)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== MODEL 3: EXTENDED SPECIFICATION ======\n")

m3_extended <- pglm(
  pca_breach ~ L1_tier1_crar + L1_gnpa_ratio + L1_roa +
    L1_nim + L1_cost_income + L1_log_assets +
    L1_credit_growth + L2_credit_growth +
    L1_cd_ratio + L1_inv_dep +
    gdp_growth + repo_rate + cpi + m3_growth +
    psb + pca_rev2017 + ibc + covid + moratorium,
  data   = df_reg %>% drop_na(L1_tier1_crar, L1_nim, L1_cd_ratio, L2_credit_growth),
  family = binomial(link = "probit"),
  model  = "random",
  index  = c("bank_id", "fy"),
  method = "bfgs"
)

print(summary(m3_extended))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 12: MODEL 4 — ORDERED PROBIT (BREACH SEVERITY)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== MODEL 4: ORDERED PROBIT — BREACH SEVERITY ======\n")

# Create ordered severity variable
# Grade 0 = No breach
# Grade 1 = Early breach (first 1-2 years)
# Grade 2 = Sustained breach (3-4 years)
# Grade 3 = Long breach (5+ years, mergers/delayed exit)

pca_duration <- df %>%
  filter(pca_breach == 1) %>%
  group_by(bank) %>%
  summarise(pca_duration = n()) %>%
  mutate(pca_grade = case_when(
    pca_duration <= 2 ~ 1L,
    pca_duration <= 4 ~ 2L,
    TRUE              ~ 3L
  ))

df_reg <- df_reg %>%
  left_join(pca_duration %>% select(bank, pca_grade), by = "bank") %>%
  mutate(
    pca_grade = ifelse(pca_breach == 0, 0L, pca_grade),
    pca_grade = factor(pca_grade, levels = 0:3, ordered = TRUE)
  )

m4_oprobit <- polr(
  pca_grade ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + psb + pca_rev2017 + ibc + covid,
  data   = df_reg %>% drop_na(pca_grade),
  method = "probit",
  Hess   = TRUE
)

cat("\n--- ORDERED PROBIT RESULTS ---\n")
print(summary(m4_oprobit))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 13: AVERAGE MARGINAL EFFECTS — PRIMARY MODEL
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== AVERAGE MARGINAL EFFECTS — POOLED PROBIT (Interpretable) ======\n")

# Use pooled probit for AME since pglm margins are not directly supported
# RE probit coefficients must be scaled by (1 / sqrt(1 + sigma^2)) for AME

m_ame_base <- glm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + cpi +
    psb + pca_rev2017 + ibc + covid,
  data   = df_reg,
  family = binomial(link = "probit")
)

ame_results <- summary(margins(m_ame_base))
cat("\n--- AVERAGE MARGINAL EFFECTS (dy/dx) ---\n")
print(ame_results[, c("factor", "AME", "SE", "z", "p", "lower", "upper")])


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 14: PREDICTIVE PERFORMANCE — ROC CURVE & AUC
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== PREDICTIVE PERFORMANCE ======\n")

# Predicted probabilities from pooled probit
df_reg$pred_prob <- predict(m_ame_base, type = "response")

# ROC curve
roc_obj <- roc(df_reg$pca_breach, df_reg$pred_prob, quiet = TRUE)
cat("AUC:", round(auc(roc_obj), 4), "\n")

# Plot ROC curve
plot(roc_obj, col = "#C00000", lwd = 2,
     main = paste("ROC Curve — PCA Breach Prediction\nAUC =",
                  round(auc(roc_obj), 3)),
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")
abline(a = 0, b = 1, lty = 2, col = "grey")

# Optimal threshold (Youden's J)
best_thresh <- coords(roc_obj, "best", ret = "threshold")$threshold
cat("Optimal threshold (Youden's J):", round(best_thresh, 4), "\n")

df_reg$pred_class <- as.integer(df_reg$pred_prob >= best_thresh)

# Confusion matrix
conf_mat <- table(Predicted = df_reg$pred_class, Actual = df_reg$pca_breach)
cat("\nConfusion Matrix:\n"); print(conf_mat)

tp <- conf_mat[2, 2]; fp <- conf_mat[2, 1]
tn <- conf_mat[1, 1]; fn <- conf_mat[1, 2]
cat(sprintf("  Sensitivity (Recall) : %.3f\n", tp / (tp + fn)))
cat(sprintf("  Specificity          : %.3f\n", tn / (tn + fp)))
cat(sprintf("  Precision            : %.3f\n", tp / (tp + fp)))
cat(sprintf("  Accuracy             : %.3f\n", (tp + tn) / (tp + tn + fp + fn)))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 15: ROBUSTNESS CHECKS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== ROBUSTNESS CHECKS ======\n")

# ── R1: PSB-only sample ───────────────────────────────────────────
cat("\n--- R1: PSB Banks Only ---\n")
m_r1 <- glm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + pca_rev2017 + ibc + covid,
  data   = df_psb,
  family = binomial(link = "probit")
)
print(coeftest(m_r1, vcov = vcovCL(m_r1, cluster = ~ bank_id, data = df_psb)))

# ── R2: Exclude COVID years ───────────────────────────────────────
cat("\n--- R2: Exclude COVID Years (FY2020-2022) ---\n")
m_r2 <- glm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + cpi +
    psb + pca_rev2017 + ibc,
  data   = df_reg %>% filter(fy < 2020 | fy > 2022),
  family = binomial(link = "probit")
)
print(summary(m_r2)$coefficients)

# ── R3: Linear Probability Model (LPM) — for AME comparison ──────
cat("\n--- R3: Linear Probability Model (Two-Way FE) ---\n")
pdata_lpm <- pdata.frame(df_reg, index = c("bank_id", "fy"))
m_r3 <- plm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + cpi + pca_rev2017 + ibc + covid,
  data   = pdata_lpm,
  model  = "within",
  effect = "twoways"
)
print(coeftest(m_r3, vcov = vcovHC(m_r3, type = "HC1", cluster = "group")))

# ── R4: Pre vs Post 2017 PCA Revision ────────────────────────────
cat("\n--- R4a: Pre-2017 PCA Revision (FY2008-2017) ---\n")
m_r4a <- glm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + psb + ibc,
  data   = df_pre2017,
  family = binomial(link = "probit")
)
print(summary(m_r4a)$coefficients)

cat("\n--- R4b: Post-2017 PCA Revision (FY2018-2024) ---\n")
m_r4b <- glm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets + L1_credit_growth +
    gdp_growth + repo_rate + cpi + psb + ibc + covid,
  data   = df_post2017,
  family = binomial(link = "probit")
)
print(summary(m_r4b)$coefficients)

# ── R5: Interaction — PSB × Credit Growth ────────────────────────
cat("\n--- R5: Interaction PSB × Credit Growth ---\n")
m_r5 <- glm(
  pca_breach ~ L1_total_crar + L1_gnpa_ratio + L1_roa +
    L1_cost_income + L1_log_assets +
    L1_credit_growth * psb +          # Interaction term
    gdp_growth + repo_rate + cpi +
    pca_rev2017 + ibc + covid,
  data   = df_reg,
  family = binomial(link = "probit")
)
print(coeftest(m_r5, vcov = vcovCL(m_r5, cluster = ~ bank_id, data = df_reg)))


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 16: KAPLAN-MEIER SURVIVAL ANALYSIS
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== KAPLAN-MEIER: TIME TO PCA BREACH ======\n")

# Build survival data: time = years from FY2008, event = first PCA breach
surv_df <- df %>%
  filter(fy >= 2008) %>%
  group_by(bank) %>%
  arrange(fy) %>%
  mutate(
    time_period = row_number(),
    ever_pca    = any(pca_breach == 1)
  ) %>%
  summarise(
    time  = ifelse(any(pca_breach == 1),
                   min(which(pca_breach == 1)),
                   n()),
    event = as.integer(any(pca_breach == 1)),
    psb   = first(psb)
  )

surv_obj <- Surv(time = surv_df$time, event = surv_df$event)
km_fit   <- survfit(surv_obj ~ psb, data = surv_df)

ggsurvplot(km_fit,
           data       = surv_df,
           pval       = TRUE,
           conf.int   = TRUE,
           legend.labs= c("Private Banks", "Public Sector Banks"),
           palette    = c("#2E75B6", "#C00000"),
           title      = "Kaplan-Meier: Survival Probability — Time to PCA Breach",
           xlab       = "Years from FY2008",
           ylab       = "Probability of NOT being under PCA")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 17: PUBLICATION-READY TABLES (stargazer)
# ─────────────────────────────────────────────────────────────────────────────

cat("\n====== PUBLICATION TABLE: REGRESSION RESULTS ======\n")

stargazer(
  m1_pooled, m_ame_base, m_r1, m_r2,
  type        = "text",
  title       = "Determinants of PCA Breach Probability — Probit Results",
  dep.var.labels = "PCA_BREACH (=1 if under PCA)",
  column.labels  = c("Pooled Probit", "Core Spec", "PSB Only", "No COVID"),
  covariate.labels = c(
    "CRAR (t-1)", "GNPA Ratio (t-1)", "ROA (t-1)",
    "Cost-Income (t-1)", "Log Assets (t-1)", "Credit Growth (t-1)",
    "GDP Growth", "Repo Rate", "CPI Inflation",
    "PSB Dummy", "PCA Revision 2017", "IBC Dummy", "COVID Dummy"
  ),
  omit.stat   = c("f", "ser"),
  no.space    = TRUE,
  digits      = 4,
  star.cutoffs = c(0.10, 0.05, 0.01),
  notes       = "*, **, *** denote significance at 10%, 5%, 1% levels. Clustered SE at bank level."
)


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 18: VISUALISATIONS
# ─────────────────────────────────────────────────────────────────────────────

# ── Plot 1: GNPA ratio trend — PCA vs Non-PCA banks ──────────────
plot_gnpa <- df %>%
  filter(!is.na(gnpa_ratio)) %>%
  mutate(group = ifelse(bank %in% c(
    "UNITED BANK OF INDIA", "INDIAN OVERSEAS BANK", "BANK OF MAHARASHTRA",
    "DHANLAXMI BANK LIMITED", "IDBI BANK LIMITED", "UCO BANK", "DENA BANK",
    "CENTRAL BANK OF INDIA", "BANK OF INDIA", "ORIENTAL BANK OF COMMERCE",
    "CORPORATION BANK", "ALLAHABAD BANK"), "PCA Banks", "Non-PCA Banks")) %>%
  group_by(fy, group) %>%
  summarise(avg_gnpa = mean(gnpa_ratio, na.rm = TRUE), .groups = "drop")

ggplot(plot_gnpa, aes(x = fy, y = avg_gnpa, colour = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("PCA Banks" = "#C00000", "Non-PCA Banks" = "#2E75B6")) +
  labs(title = "Average GNPA Ratio: PCA vs Non-PCA Banks  |  FY2005–FY2024",
       x = "Financial Year", y = "GNPA Ratio (%)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ── Plot 2: ROA trend — PCA vs Non-PCA ───────────────────────────
plot_roa <- df %>%
  filter(!is.na(roa)) %>%
  mutate(group = ifelse(bank %in% c(
    "UNITED BANK OF INDIA", "INDIAN OVERSEAS BANK", "BANK OF MAHARASHTRA",
    "DHANLAXMI BANK LIMITED", "IDBI BANK LIMITED", "UCO BANK", "DENA BANK",
    "CENTRAL BANK OF INDIA", "BANK OF INDIA", "ORIENTAL BANK OF COMMERCE",
    "CORPORATION BANK", "ALLAHABAD BANK"), "PCA Banks", "Non-PCA Banks")) %>%
  group_by(fy, group) %>%
  summarise(avg_roa = mean(roa, na.rm = TRUE), .groups = "drop")

ggplot(plot_roa, aes(x = fy, y = avg_roa, colour = group, group = group)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  scale_colour_manual(values = c("PCA Banks" = "#C00000", "Non-PCA Banks" = "#2E75B6")) +
  labs(title = "Average ROA: PCA vs Non-PCA Banks  |  FY2005–FY2024",
       x = "Financial Year", y = "ROA (%)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# ── Plot 3: Predicted probability distribution ────────────────────
ggplot(df_reg, aes(x = pred_prob, fill = factor(pca_breach))) +
  geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("0" = "#2E75B6", "1" = "#C00000"),
                    labels = c("Non-PCA", "PCA")) +
  labs(title = "Distribution of Predicted PCA Breach Probabilities",
       x = "Predicted Probability", y = "Count", fill = NULL) +
  theme_minimal(base_size = 12)

# ── Plot 4: CRAR by bank group over time ─────────────────────────
plot_crar <- df %>%
  filter(!is.na(total_crar), fy >= 2008,
         bank_group %in% c("SBI Group", "Nationalised", "Old Private", "New Private")) %>%
  group_by(fy, bank_group) %>%
  summarise(avg_crar = mean(total_crar, na.rm = TRUE), .groups = "drop")

ggplot(plot_crar, aes(x = fy, y = avg_crar, colour = bank_group)) +
  geom_line(linewidth = 1) + geom_point(size = 1.5) +
  geom_hline(yintercept = 9, linetype = "dashed", colour = "red", alpha = 0.6) +
  annotate("text", x = 2009, y = 8.6, label = "Min CRAR Trigger", size = 3, colour = "red") +
  labs(title = "Average Total CRAR by Bank Group  |  FY2008–FY2024",
       x = "Financial Year", y = "CRAR (%)", colour = "Bank Group") +
  theme_minimal(base_size = 12)


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 19: EXPORT RESULTS TO EXCEL
# ─────────────────────────────────────────────────────────────────────────────

# Regression coefficients summary
results_m2 <- as.data.frame(summary(m2_re)$estimate) %>%
  tibble::rownames_to_column("Variable") %>%
  rename(Coefficient = Estimate, Std_Error = `Std. Error`,
         z_stat = `z-value`, p_value = `Pr(>|z|)`) %>%
  mutate(Significance = case_when(
    p_value < 0.01 ~ "***",
    p_value < 0.05 ~ "**",
    p_value < 0.10 ~ "*",
    TRUE           ~ ""
  ))

# AME table
ame_export <- ame_results %>%
  select(factor, AME, SE, z, p, lower, upper) %>%
  rename(Variable = factor, Marginal_Effect = AME,
         Std_Error = SE, z_stat = z, p_value = p,
         CI_Lower = lower, CI_Upper = upper)

# Descriptive stats
desc_export <- df_reg %>%
  select(all_of(desc_vars)) %>%
  summarise(across(everything(),
                   list(N      = ~ sum(!is.na(.)),
                        Mean   = ~ round(mean(., na.rm = TRUE), 4),
                        SD     = ~ round(sd(., na.rm = TRUE), 4),
                        Min    = ~ round(min(., na.rm = TRUE), 4),
                        Max    = ~ round(max(., na.rm = TRUE), 4)))) %>%
  pivot_longer(everything(),
               names_to  = c("Variable", ".value"),
               names_sep = "_(?=[^_]+$)")

write_xlsx(
  list(
    "RE_Probit_Coefficients" = results_m2,
    "Avg_Marginal_Effects"   = ame_export,
    "Descriptive_Stats"      = as.data.frame(desc_export),
    "Predicted_Probs"        = df_reg %>%
      select(bank, fy, pca_breach, pred_prob, pred_class) %>%
      arrange(desc(pred_prob))
  ),
  path = "PCA_Regression_Results.xlsx"
)

cat("\n✅ Results exported to PCA_Regression_Results.xlsx\n")


# ─────────────────────────────────────────────────────────────────────────────
# SECTION 20: EXPORT DATASET TO STATA .dta (optional)
# ─────────────────────────────────────────────────────────────────────────────

# Export the regression-ready dataset to Stata format
df_stata <- df_reg %>%
  select(bank_id, fy, pca_breach, starts_with("L1_"), starts_with("L2_"),
         gdp_growth, repo_rate, cpi, m3_growth,
         psb, pca_rev2017, pca_rev2022, ibc, covid, moratorium,
         bank_group, merger, pred_prob)

haven::write_dta(df_stata, "PCA_Panel_Stata.dta")
cat("✅ Stata .dta file exported: PCA_Panel_Stata.dta\n")

cat("\n====== ANALYSIS COMPLETE ======\n")
cat("Models estimated: M1 Pooled Probit | M2 RE Panel Probit | M3 Extended | M4 Ordered Probit\n")
cat("Robustness checks: PSB-only | No COVID | LPM-FE | Pre/Post 2017 | Interaction\n")
cat("Outputs: Regression tables | ROC curve | KM survival plot | Trend plots | Excel results\n")