######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Conditional Logit Model using Apollo Package   ###
###                 Preference Space                               ###
### Output        : txt, rds, csv, Word, R-Table                   ###
### Date          : 21.05.2025                                     ###
######################################################################


###################################################################
##  INITIALISE APOLLO AND LOAD DATA                              ##
###################################################################
library(apollo)
### Initialise Apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "CL_PS",
  modelDescr ="Conditional Logit in preference space",
  indivID = "i_NUMBER",
  mixing = FALSE, # True if it were Mixed Logit
  outputDirectory = "Hauptstudie/Estimation_results/CL/Preference_Space/"
)

# Load dataset
database<- df_long %>%
  arrange(i_NUMBER) %>%
  as.data.frame()

###################################################################
##  DEFINE apollo_beta()                                        ###
###################################################################

### Set starting values
# set values to 0 for conditional logit model
apollo_beta=c(asc = 0,
              borgcit = 0, #citizens
              borgmun = 0, #municipality
              bpartiinv = 0, #investor
              bpartimem = 0, #member
              bgoalsoc = 0, #social goal
              bgoaleco = 0, #ecological goal
              bgoalboth = 0, #social and ecological goal
              bconsplit = 0, #contract split
              bprice = 0) #price

### Don't keep any parameters fixed
apollo_fixed = c()

### Validate
apollo_inputs = apollo_validateInputs()

###################################################################
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                       ###
###################################################################

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = borgcit*OrgCit + borgmun*OrgMun + bpartiinv * PartiInv + bpartimem * PartiMem + bgoalsoc * GoalSoc + bgoaleco * GoalEco + bgoalboth * GoalBoth +  bconsplit*Con +  bprice*Price
  V[['alt2']] = asc

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
  )

  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


####################################################################
##  MODEL ESTIMATION                                              ##
####################################################################

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(estimationRoutine = "bfgs", scaleAfterConvergence = FALSE,  hessianRoutine="maxLik", scaleHessian = FALSE ))


####################################################################
##  MODEL OUTPUTS                                                 ##
####################################################################

apollo_modelOutput(model)
apollo_saveOutput(model, saveOutput_settings = list(printPVal = 1))

# Estimate, SE und p-values
est <- model$estimate
se  <- model$robse
p   <- 2 * (1 - pnorm(abs(est / se)))

# Significance notation
p_wert <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Legible Labels
pretty_labels <- c(
  "asc"        = "ASC: Keeping electricity provider",
  "bpartiinv"  = "Investor",
  "bpartimem"  = "Member",
  "borgcit"    = "Organizer: Citizens",
  "borgmun"    = "Organizer: Municipality",
  "bgoalsoc"   = "Social goal",
  "bgoaleco"   = "Ecological goal",
  "bgoalboth"  = "Soc. & Eco. goal",
  "bconsplit"  = "Split supply",
  "bprice"     = "Price"
)

# Only use main parameter
main_params <- names(est)

# Prepare rows
rows <- list()

for (param in main_params) {
  label <- pretty_labels[[param]]
  if (is.null(label)) label <- param
  
  est_mean <- est[param]
  se_mean  <- se[param]
  p_mean   <- p[param]
  mean_str <- paste0(sprintf("%.2f", est_mean), p_wert(p_mean))
  se_str   <- paste0("(", sprintf("%.2f", se_mean), ")")
  mean_combined <- paste0(mean_str, " ", se_str)
  
  rows[[param]] <- c(label, mean_combined)
}

# Data frame without SD column
result_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(result_df) <- c("Parameter", "Estimate")

# Model statistics
stats_labels <- c("No. Observations", "No. Respondents", "LL(0)", "LL(final)",
                  "Adj. Rho-squared", "AIC", "BIC")
stats_values <- c(
  model$nObs,
  model$nObs / 10,  # ggf. anpassen
  model$LL0[1],
  model$LLout[1],
  model$adjRho2_0,
  model$AIC,
  model$BIC
)
stats_df <- data.frame(
  Parameter = stats_labels,
  Estimate = format(round(stats_values, 2), scientific = FALSE)
)

# Optional: sort parameter names
param_order <- c(
  "asc","bpartiinv", "bpartimem", 
  "borgcit", "borgmun",
  "bgoalsoc", "bgoaleco", "bgoalboth",
  "bconsplit",
  "bprice"
)
param_order <- param_order[param_order %in% rownames(result_df)]
result_df <- result_df[param_order, ]

# Final result
result_df_final <- rbind(result_df, stats_df)

# Estimate, SE und p-values
est <- model$estimate
se  <- model$robse
p   <- 2 * (1 - pnorm(abs(est / se)))

# Significance notation
p_wert <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}

# Legible labels
pretty_labels <- c(
  "asc"        = "ASC: Keeping electricity provider",
  "bpartiinv"  = "Investor",
  "bpartimem"  = "Member",
  "borgcit"    = "Organizer: Citizens",
  "borgmun"    = "Organizer: Municipality",
  "bgoalsoc"   = "Social goal",
  "bgoaleco"   = "Ecological goal",
  "bgoalboth"  = "Soc. & Eco. goal",
  "bconsplit"  = "Split supply",
  "bprice"     = "Price"
)

# Only use main parameters
main_params <- names(est)

# Prepare rows
rows <- list()

for (param in main_params) {
  label <- pretty_labels[[param]]
  if (is.null(label)) label <- param
  
  est_mean <- est[param]
  se_mean  <- se[param]
  p_mean   <- p[param]
  mean_str <- paste0(sprintf("%.2f", est_mean), p_wert(p_mean))
  se_str   <- paste0("(", sprintf("%.2f", se_mean), ")")
  mean_combined <- paste0(mean_str, " ", se_str)
  
  rows[[param]] <- c(label, mean_combined)
}

# Data frame without SD column
result_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(result_df) <- c("Parameter", "Estimate")

# Model statistics
stats_labels <- c("No. Observations", "No. Respondents", "LL(0)", "LL(final)",
                  "Adj. Rho-squared", "AIC", "BIC")
stats_values <- c(
  model$nObs,
  model$nObs / 10,  # ggf. anpassen
  model$LL0[1],
  model$LLout[1],
  model$adjRho2_0,
  model$AIC,
  model$BIC
)


####################################################################
##  Flextable                                                     ##
####################################################################

# Header
my_header <- data.frame(
  col_keys = colnames(result_df_final),
  line1 = c("Conditional Logit Model - Preference Space"),
  line2 = c("Parameter", "Estimate (SE)"),
  stringsAsFactors = FALSE
)

# Footer 
my_footer <- data.frame(
  col_keys = colnames(result_df_final),
  line1 = c("** p<0.01, * p<0.05"),
  line2 = c("Reference: Customer at municipal utility with full supply and no statutory goal"),
  stringsAsFactors = FALSE
)

# Flextable 
flex <- flextable(result_df_final) %>%
  theme_booktabs() %>%
  set_header_df(mapping = my_header, key = "col_keys") %>%
  set_footer_df(mapping = my_footer, key = "col_keys") %>%
  border(i = 2, border.bottom = fp_border(color = "black", width = 1), part = "header") %>%
  fontsize(size = 12, i = 1, part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = "Estimate", align = "right", part = "body") %>%
  align(align = "right", part = "footer") %>%
  border(border.top = fp_border(color = "black", width = 1), part = "footer") %>%
  bold(j = 1, part = "body") %>%
  bold(i = 1:2, part = "header") %>%
  font(fontname = "Calibri", part = "all") %>%
  merge_h(part = "footer") %>%
  merge_at(i = 1, j = 1:2, part = "header") %>%
  autofit()


### Preview in R:

print(flex)
output_dir <- model$apollo_control$outputDirectory
fname <- paste0(output_dir, "/", apollo_control$modelName, ".docx")
save_as_docx(flex, path = fname)#, pr_section = set_prop)

