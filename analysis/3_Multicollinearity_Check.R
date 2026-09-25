######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Multicollinearity Test                         ###
### Output        : R-Tables, R-Plot                               ###
### Date          : 31.01.2025                                     ###
######################################################################

###################################################################
##  Test for Multicollinearity                                  ###
###################################################################

# Variables for utility and membership
vars_util1 <- c(
  "OrgCit","OrgMun","PartiInv","PartiMem","GoalSoc","GoalEco","GoalBoth","Con","Price",
  "inter_env_awareness_score",
  "inter_sex",
  "inter_income",
  "inter_age",
  "inter_educ_years",
  "inter_tenant",
  "inter_mfh",
  "inter_lowincome"
)

apollo_ready_dataset <- df_long
apollo_ready_dataset_clean <- apollo_ready_dataset %>%
  filter(if_all(all_of(vars_util1), is.finite))
database <- as.data.frame(apollo_ready_dataset_clean)
database <- database[order(database$i_NUMBER),]

hetcor_result <- hetcor(database[, c(
  "inter_env_awareness_score",
  "inter_income",
  "inter_age",
  "inter_educ_years",
  "inter_sex",
  "inter_tenant",
  "inter_mfh",
  "inter_lowincome"
)])
print(hetcor_result$correlations)

# correlation matrix from hetcor
corr_mat <- hetcor_result$correlations

# Keep only lower triangle + diagonal, set other values to NA
corr_mat_lower <- corr_mat
corr_mat_lower[upper.tri(corr_mat_lower)] <- NA

# Round values to 2 decimal places
corr_mat_lower <- round(corr_mat_lower, 2)

# rename labels
colnames(corr_mat_lower) <- c("Env. Awareness","Income","Age","Education","Sex","Tenant","MFH","Low Income")
rownames(corr_mat_lower) <- colnames(corr_mat_lower)

# Convert to data frame for flextable
corr_df <- as.data.frame(corr_mat_lower)
corr_df <- tibble::rownames_to_column(corr_df, var = "Variable")

# Create flextable
ft <- flextable(corr_df)
print(ft,preview="docx")



