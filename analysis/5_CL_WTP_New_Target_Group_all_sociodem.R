######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Conditional Logit Model using Apollo Package   ###
###                 WTP Space – Subsample New Target Groups        ###
### Output        : txt, rds, csv, Word, R-Table                   ###
### Date          : 21.05.2025                                     ###
######################################################################


###################################################################
##  INITIALISE APOLLO AND LOAD DATA                              ##
###################################################################

### Initialise Apollo and core settings
apollo_initialise()
apollo_control= list (
  modelName = "CL_WTP_New_Target_Group_allsociodem",
  modelDescr ="Conditional Logit in WTP space - Subsample New Target Groups",
  outputDirectory = "Hauptstudie/Estimation_results/CL/WTP_Space/",
  indivID = "i_NUMBER",
  mixing = FALSE # True if it were Mixed Logit
)


### Check whether there are n.a. in relevant variables ####

# List of variables to be checked
vars_util1 <- c(
  "OrgCit","OrgMun","PartiInv","PartiMem","GoalSoc","GoalEco","GoalBoth","Con","Price",
  "inter_env_awareness_score",
  "inter_sex",
  "inter_age",
  "inter_educ_years"
)

apollo_ready_dataset <- df_long 
apollo_ready_dataset_clean <- apollo_ready_dataset %>%
  filter(
    if_all(all_of(vars_util1), is.finite),
    lowincome == 1 | tenant == 1 | mfh == 1
  )
database <- as.data.frame(apollo_ready_dataset_clean)
database <- database[order(database$i_NUMBER),]


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
              bprice = 0, #price
              
              asc_env_awareness_score = 0,
              bpartimem_env_awareness_score = 0, #member
              
              asc_sex = 0,
              bpartimem_sex = 0, #member
              
              asc_age = 0,
              bpartimem_age = 0, #member
              
              asc_educ_years = 0,
              bpartimem_educ_years = 0)

### Don't keep any parameters fixed
apollo_fixed = c()

### Validate
apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = 
    # Main effects without interaction
    -bprice*(
      borgcit        * OrgCit + 
        borgmun        * OrgMun +
        bpartiinv      * PartiInv + 
        bpartimem      * PartiMem +
        bgoalsoc       * GoalSoc + 
        bgoaleco       * GoalEco + 
        bgoalboth      * GoalBoth +
        bconsplit      * Con - 
        Price +
        
        bpartimem_env_awareness_score   * PartiMem   * inter_env_awareness_score +
        bpartimem_sex           * PartiMem   * inter_sex +
        bpartimem_age           * PartiMem   * inter_age +
        bpartimem_educ_years    * PartiMem   * inter_educ_years)
  
  V[['alt2']] = 
    -bprice* 
    (asc +
       asc_env_awareness_score * inter_env_awareness_score +
       asc_sex                 * inter_sex +
       asc_age                 * inter_age +
       asc_educ_years          * inter_educ_years)
  
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
  
  ### Average across inter-individual draws - only for Mixed Logit!
  #P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


####################################################################
##  MODEL ESTIMATION                                              ##
####################################################################

model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, estimate_settings=list(estimationRoutine = "bfgs", scaleAfterConvergence = FALSE,  hessianRoutine="numDeriv", scaleHessian = FALSE) )


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
  "bprice"     = "Price",
  "asc_env_awareness_score" = "ASC × Env. Awareness",
  "bpartimem_env_awareness_score" = "Member × Env. Awareness",
  "asc_sex" = "ASC × Female",
  "bpartimem_sex" = "Member × Female",
  "asc_age" = "ASC × Age",
  "bpartimem_age" = "Member × Age",
  "asc_educ_years" = "ASC × Education",
  "bpartimem_educ_years" = "Member × Education"
)


# Main parameters (all)
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

# Data frame without SD
result_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(result_df) <- c("Parameter", "Estimate")

param_order <- c(
  "asc","bpartiinv", "bpartimem", 
  "borgcit", "borgmun",
  "bgoalsoc", "bgoaleco", "bgoalboth",
  "bconsplit",
  "bprice", 
  "asc_env_awareness_score", "bpartimem_env_awareness_score",
  "asc_sex", "bpartimem_sex",
  "asc_age", "bpartimem_age",
  "asc_educ_years", "bpartimem_educ_years"
)

param_order <- param_order[param_order %in% rownames(result_df)]

result_df <- result_df[param_order, ]


# Model statistics
stats_labels <- c("No. Observations", "No. Respondents", "LL(0)", "LL(final)",
                  "Adj. Rho-squared", "AIC", "BIC")
stats_values <- c(
  model$nObs,
  model$nObs / 10,
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

# Final results data frame
result_df_final <- rbind(result_df, stats_df)

####################################################################
##  Flextable                                                     ##
####################################################################

# Header
my_header <- data.frame(
  col_keys = colnames(result_df_final),
  line1 = c("Conditional Logit Model - WTP Space"),
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
print(flex,preview="docx")
output_dir <- model$apollo_control$outputDirectory
fname <- paste0(output_dir, "/", apollo_control$modelName, ".docx")
save_as_docx(flex, path = fname)#, pr_section = set_prop)
