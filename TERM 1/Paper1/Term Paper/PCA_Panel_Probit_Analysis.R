# ============================================================
# PCA PROBIT ANALYSIS + WORD EXPORT (ALL IN ONE SCRIPT)
# ============================================================

# ---------------------------
# 1 Install & Load Packages
# ---------------------------

packages <- c("readxl","dplyr","pROC","officer","flextable","broom")

installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed])

library(readxl)
library(dplyr)
library(pROC)
library(officer)
library(flextable)
library(broom)

cat("Packages loaded\n")


# ---------------------------
# 2 Load Dataset
# ---------------------------

df_reg <- read_excel("PCA_Panel_Dataset_Final.xlsx", skip = 1)
df_reg <- as.data.frame(df_reg)

cat("Dataset loaded\n")


# ---------------------------
# 3 Rename Variables
# ---------------------------

df_reg <- df_reg %>%
  rename(
    pca_breach = PCA_BREACH,
    bank_id = BANK,
    year = FY,
    psb = PSB_DUMMY,
    gnpa_ratio = GNPA_RATIO_PCT,
    roa = ROA_PCT,
    cost_income = COST_INCOME_PCT,
    log_assets = LOG_ASSETS,
    credit_growth = CREDIT_GROWTH_PCT,
    gdp_growth = GDP_GROWTH_PCT,
    repo_rate = REPO_RATE_PCT,
    cpi = CPI_INFLATION_PCT,
    total_crar = TOTAL_CRAR_PCT,
    pca_rev2017 = PCA_REVISION_2017,
    ibc = IBC_DUMMY,
    covid = COVID_DUMMY
  )

cat("Variables renamed\n")


# ---------------------------
# 4 Create Lag Variables
# ---------------------------

df_reg <- df_reg %>%
  arrange(bank_id, year) %>%
  group_by(bank_id) %>%
  mutate(
    L1_total_crar = lag(total_crar,1),
    L1_gnpa_ratio = lag(gnpa_ratio,1),
    L1_roa = lag(roa,1),
    L1_cost_income = lag(cost_income,1),
    L1_log_assets = lag(log_assets,1),
    L1_credit_growth = lag(credit_growth,1)
  ) %>%
  ungroup()

cat("Lag variables created\n")


# ---------------------------
# 5 Estimate Probit Model
# ---------------------------

m1_pooled <- glm(
  pca_breach ~
    L1_total_crar +
    L1_gnpa_ratio +
    L1_roa +
    L1_cost_income +
    L1_log_assets +
    L1_credit_growth +
    gdp_growth +
    repo_rate +
    cpi +
    psb +
    pca_rev2017 +
    ibc +
    covid,
  
  data = df_reg,
  family = binomial(link = "probit")
)

cat("Probit model estimated\n")


# ---------------------------
# 6 ROC Curve
# ---------------------------

roc_obj <- roc(m1_pooled$y, fitted(m1_pooled))
auc_value <- auc(roc_obj)

cat("AUC:", auc_value,"\n")


# ---------------------------
# 7 Prepare Model Table
# ---------------------------

model_results <- tidy(m1_pooled)

model_results <- model_results[,c("term","estimate","std.error","statistic","p.value")]

names(model_results) <- c(
  "Variable",
  "Coefficient",
  "Std_Error",
  "z_value",
  "p_value"
)

# ---------------------------
# Prepare Model Table
# ---------------------------

model_results <- tidy(m1_pooled)

model_results <- model_results[,c("term","estimate","std.error","statistic","p.value")]

names(model_results) <- c(
  "Variable",
  "Coefficient",
  "Std_Error",
  "z_value",
  "p_value"
)

# Round numeric columns only
model_results$Coefficient <- round(model_results$Coefficient,4)
model_results$Std_Error <- round(model_results$Std_Error,4)
model_results$z_value <- round(model_results$z_value,4)
model_results$p_value <- round(model_results$p_value,4)

# ---------------------------
# 8 Convert to Flextable
# ---------------------------

table_model <- flextable(model_results)
table_model <- autofit(table_model)
table_model <- theme_booktabs(table_model)

roc_table <- data.frame(
  Metric = "AUC (ROC Curve)",
  Value = round(as.numeric(auc_value),4)
)

roc_table <- flextable(roc_table)
roc_table <- autofit(roc_table)


# ---------------------------
# 9 Export to Word
# ---------------------------

doc <- read_docx()

doc <- body_add_par(doc,
                    "PCA Probit Regression Results",
                    style = "heading 1")

doc <- body_add_par(doc,
                    "Model Estimates",
                    style = "heading 2")

doc <- body_add_flextable(doc, table_model)

doc <- body_add_par(doc,
                    "Model Performance",
                    style = "heading 2")

doc <- body_add_flextable(doc, roc_table)

print(doc, target = "PCA_Probit_Results.docx")

cat("\nWord file created successfully\n")
cat("Saved in:", getwd(), "\n")