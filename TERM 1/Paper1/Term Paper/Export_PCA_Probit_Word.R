# =====================================================
# EXPORT PROBIT RESULTS TO WORD
# =====================================================

# Install packages if needed
packages <- c("officer","flextable","broom","pROC")

installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed])

library(officer)
library(flextable)
library(broom)
library(pROC)

cat("Packages loaded\n")


# =====================================================
# CHECK REQUIRED OBJECTS
# =====================================================

if(!exists("m1_pooled")){
  stop("m1_pooled model not found. Run the model first.")
}

if(!exists("roc_obj")){
  stop("roc_obj not found. Run ROC code first.")
}


# =====================================================
# EXTRACT MODEL RESULTS
# =====================================================

model_results <- tidy(m1_pooled)

model_results <- model_results[,c("term","estimate","std.error","statistic","p.value")]

names(model_results) <- c(
  "Variable",
  "Coefficient",
  "Std_Error",
  "z_value",
  "p_value"
)

model_results$Coefficient <- round(model_results$Coefficient,4)
model_results$Std_Error <- round(model_results$Std_Error,4)
model_results$z_value <- round(model_results$z_value,3)
model_results$p_value <- round(model_results$p_value,4)

cat("Model results extracted\n")


# =====================================================
# CREATE TABLE
# =====================================================

table_model <- flextable(model_results)

table_model <- autofit(table_model)

table_model <- theme_booktabs(table_model)


# =====================================================
# ROC / AUC
# =====================================================

auc_value <- auc(roc_obj)

roc_table <- data.frame(
  Metric = "AUC (ROC Curve)",
  Value = round(as.numeric(auc_value),4)
)

roc_table <- flextable(roc_table)
roc_table <- autofit(roc_table)


# =====================================================
# CREATE WORD DOCUMENT
# =====================================================

doc <- read_docx()

doc <- body_add_par(doc,
                    "PCA Probit Regression Results",
                    style = "heading 1")

doc <- body_add_par(doc,
                    "Probit Model Estimates",
                    style = "heading 2")

doc <- body_add_flextable(doc, table_model)

doc <- body_add_par(doc,
                    "Model Performance",
                    style = "heading 2")

doc <- body_add_flextable(doc, roc_table)


# =====================================================
# SAVE DOCUMENT
# =====================================================

print(doc, target = "PCA_Probit_Results.docx")

cat("\nWord file created successfully\n")
cat("File name: PCA_Probit_Results.docx\n")
cat("Location:", getwd(), "\n")