######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Script for Mixed Logit Model                   ###
###                 WTP Space                                      ###
###                 Including Socio-demographics                   ###
### Output        : Flextable (Word file)                          ###
### Date          : 18.07.2025                                     ###
######################################################################

# Load apollo package
library(apollo)

# Search-range helper for apollo_searchStart() (defines make_searchStart_bounds()).
if (!exists("make_searchStart_bounds")) {
  source("6_MXL_searchStart_ranges.R", encoding = "UTF-8")
}

n_draws <- 1000
# Number of cores: all but one. Under a job scheduler only the allocated
# cores count, because detectCores() sees every core of the machine.
n_cores <- max(1, as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", parallel::detectCores())) - 1)

# Initialize model
apollo_initialise()

### Output directory
out_base <- "Hauptstudie/Estimation_results"
output_dir <- file.path(out_base, "MXL", "WTP_Space")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

### Set core controls
apollo_control = list(
  modelName  = "mxl_WTP_allsociodem_combined_mfh_tenant",
  modelDescr = "MXL in WTP Space with interactions not random",
  indivID    ="i_NUMBER",
  mixing     = TRUE,
  HB= FALSE,
  nCores     = n_cores,
  outputDirectory = output_dir
)

### Check whether there are n.a. in relevant variables ####

# List of variables to be checked
vars_util1 <- c(
  "OrgCit","OrgMun","PartiInv","PartiMem","GoalSoc","GoalEco","GoalBoth","Con","Price",
  "inter_env_awareness_score",
  "inter_sex",
  "inter_age",
  "inter_educ_years",
  "inter_lowincome",
  "inter_highincome",
  "inter_mfh_or_tenant"
)

apollo_ready_dataset <- df_long 
apollo_ready_dataset_clean <- apollo_ready_dataset %>%
  filter(if_all(all_of(vars_util1), is.finite))
database <- as.data.frame(apollo_ready_dataset_clean)
database <- database[order(database$i_NUMBER),]


##### Starting values for the WTP-space model
# Self-contained WTP-space starting values (no dependency on the
# preference-space model output, so the models can run independently /
# in parallel on the cluster). apollo_searchStart() below refines these.

apollo_beta <- c(
  asc        = -0.35,
  borgcit    =  0.80,
  borgmun    =  0.50,
  bpartiinv  = -0.10,
  bpartimem  = -0.05,
  bgoalsoc   =  0.70,
  bgoaleco   =  1.40,
  bgoalboth  =  1.50,
  bconsplit  = -0.60,
  bprice     = -0.35,

  sig_borgcit   = 0.40,
  sig_borgmun   = 0.30,
  sig_bpartiinv = 0.40,
  sig_bpartimem = 0.30,
  sig_bgoalsoc  = 0.40,
  sig_bgoaleco  = 0.40,
  sig_bgoalboth = 0.40,
  sig_bconsplit = 0.40,
  sig_bprice    = 0.50,
  sig_asc       = 3.00,

  asc_env_awareness_score       = 0,
  bpartimem_env_awareness_score = 0, #member

  asc_sex                       = 0,
  bpartimem_sex                 = 0, #member

  asc_age                       = 0,
  bpartimem_age                 = 0, #member

  asc_educ_years                = 0,
  bpartimem_educ_years          = 0, #member

  asc_lowincome                 = 0,
  bpartimem_lowincome           = 0, #member

  asc_highincome                = 0,
  bpartimem_highincome          = 0, #low income

  asc_mfh_or_tenant             = 0,
  bpartimem_mfh_or_tenant       = 0
)


### Specify parameters that should be kept fixed, here = none
apollo_fixed = c()

### Set parameters for generating draws, use 2000 sobol draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = n_draws,
  interUnifDraws = c(),
  interNormDraws = c("draws_borgcit", "draws_borgmun", "draws_bpartiinv","draws_bpartimem", "draws_bgoalsoc","draws_bgoaleco",
                     "draws_bgoalboth", "draws_bconsplit", "draws_asc","draws_bprice"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters, define distribution of the parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["ranborgcit"]] = borgcit + sig_borgcit * draws_borgcit 
  randcoeff[["ranborgmun"]] = borgmun + sig_borgmun * draws_borgmun
  randcoeff[["ranbpartiinv"]] = bpartiinv + sig_bpartiinv * draws_bpartiinv
  randcoeff[["ranbgoalsoc"]] = bgoalsoc + sig_bgoalsoc * draws_bgoalsoc
  randcoeff[["ranbgoaleco"]] = bgoaleco + sig_bgoaleco * draws_bgoaleco
  randcoeff[["ranbgoalboth"]] = bgoalboth + sig_bgoalboth* draws_bgoalboth
  randcoeff[["ranbconsplit"]] = bconsplit + sig_bconsplit* draws_bconsplit
  randcoeff[["ranbpartimem"]] = bpartimem + 
    bpartimem_env_awareness_score * inter_env_awareness_score+
    bpartimem_sex           * inter_sex +
    bpartimem_age           * inter_age+
    bpartimem_educ_years    * inter_educ_years+
    bpartimem_lowincome     * inter_lowincome+
    bpartimem_highincome    * inter_highincome +
    bpartimem_mfh_or_tenant * inter_mfh_or_tenant+
    sig_bpartimem           * draws_bpartimem 
  
  
  randcoeff[["ranbprice"]] = -exp(bprice + sig_bprice*draws_bprice)
  
  
  randcoeff[["ranasc"]] = asc +
    asc_env_awareness_score * inter_env_awareness_score+
    asc_sex                 * inter_sex +
    asc_age                 * inter_age+
    asc_educ_years          * inter_educ_years+
    asc_lowincome           * inter_lowincome+
    asc_highincome          * inter_highincome +
    asc_mfh_or_tenant       * inter_mfh_or_tenant+
    sig_asc                 * draws_asc
  return(randcoeff)
}


### Validate 
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  #### List of utilities (later integrated in mnl_settings below)  ####
  # Define utility functions here:
  
  V = list()
  
  V[['alt1']] = 
    -ranbprice*(ranborgcit        * OrgCit + 
    ranborgmun        * OrgMun +
    ranbpartiinv      * PartiInv + 
    ranbpartimem      * PartiMem +
    ranbgoalsoc       * GoalSoc + 
    ranbgoaleco       * GoalEco + 
    ranbgoalboth      * GoalBoth +
    ranbconsplit      * Con - 
    Price)
  
  V[['alt2']] = 
    -ranbprice*ranasc
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2),
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V#,  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


##################################################################
##  MODEL ESTIMATION                                            ##
##################################################################

### Search for good starting values (WTP-space ranges, 100 candidates)
beta_bounds <- make_searchStart_bounds(apollo_beta, space = "WTP")
apollo_beta = apollo_searchStart(apollo_beta, apollo_fixed,
                                 apollo_probabilities, apollo_inputs,
                                 searchStart_settings = list(
                                   nCandidates   = 100,
                                   apolloBetaMin = beta_bounds$min,
                                   apolloBetaMax = beta_bounds$max
                                 ))


model = apollo_estimate(apollo_beta, apollo_fixed,
                                                   apollo_probabilities, apollo_inputs, 
                                                   estimate_settings=list(maxIterations=400,
                                                                          estimationRoutine="bfgs",
                                                                          hessianRoutine="numDeriv"))


##################################################################
##  MODEL OUTPUTS                                               ##
##################################################################

apollo_saveOutput(model, saveOutput_settings = list(printPVal = 1))

# Estimate, SE and p-values
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
  "bprice"     = "Price",
  "asc_env_awareness_score" = "ASC × Env. Awareness",
  "bpartimem_env_awareness_score" = "Member × Env. Awareness",
  "asc_sex" = "ASC × Female",
  "bpartimem_sex" = "Member × Female",
  "asc_age" = "ASC × Age",
  "bpartimem_age" = "Member × Age",
  "asc_educ_years" = "ASC × Education",
  "bpartimem_educ_years" = "Member × Education",
  "asc_lowincome" = "ASC × Low Income",
  "bpartimem_lowincome" = "Member × Low Income",
  "asc_highincome" = "ASC × High Income",
  "bpartimem_highincome" = "Member × High Income",
  "asc_mfh_or_tenant" = "ASC × Tenant/MFH",
  "bpartimem_mfh_or_tenant" = "Member × Tenant/MFH"
)

# Separate parameters
main_params <- names(est)[!grepl("^sig_", names(est))]
sd_params   <- names(est)[grepl("^sig_", names(est))]
sd_base_names <- sub("^sig_", "", sd_params)

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
  
  # SD value (only if available)
  if (param %in% sd_base_names) {
    sd_name <- paste0("sig_", param)
    est_sd <- est[sd_name]
    se_sd  <- se[sd_name]
    p_sd   <- p[sd_name]
    sd_str <- paste0(sprintf("%.2f", est_sd), p_wert(p_sd))
    se_sd_str <- paste0("(", sprintf("%.2f", se_sd), ")")
    sd_combined <- paste0(sd_str, " ", se_sd_str)
  } else {
    sd_combined <- ""
  }
  
  rows[[param]] <- c(label, mean_combined, sd_combined)
}

# Data frame
result_df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
colnames(result_df) <- c("Parameter", "Mean", "SD")

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
  Mean = format(round(stats_values, 2), scientific = FALSE),
  SD = ""
)

# Number of statistics rows
n_stats <- nrow(stats_df)

# Combine mean and SD in stats_df (if both columns are filled)
stats_df$Mean <- ifelse(
  stats_df$SD != "",
  paste(stats_df$Mean, stats_df$SD),
  stats_df$Mean
)
stats_df$SD <- ""


param_order <- c(
  "asc","bpartiinv", "bpartimem", 
  "borgcit", "borgmun",
  "bgoalsoc", "bgoaleco", "bgoalboth",
  "bconsplit",
  "bprice", 
  "asc_env_awareness_score", "bpartimem_env_awareness_score",
  "asc_sex", "bpartimem_sex",
  "asc_age", "bpartimem_age",
  "asc_educ_years", "bpartimem_educ_years",
  "asc_lowincome", "bpartimem_lowincome",
  "asc_highincome", "bpartimem_highincome",
  "asc_mfh_or_tenant", "bpartimem_mfh_or_tenant"
)

param_order <- param_order[param_order %in% rownames(result_df)]

result_df <- result_df[param_order, ]

# New final data frame
result_df_final <- rbind(result_df, stats_df)

# Header information
my_header <- data.frame(
  col_keys = colnames(result_df_final),
  line1 = c("Mixed Logit Model - WTP Space"),
  line2 = c("Parameter", "Mean (SE)", "SD (SE)"),
  stringsAsFactors = FALSE
)

# Footer
my_footer <- data.frame(
  col_keys = colnames(result_df_final),
  line1 = c("** p<0.01, * p<0.05"),
  line2 = c("Reference: Customer at municipal utility with full supply and no statutory goal"),
  stringsAsFactors = FALSE
)

flex <- flextable(result_df_final) %>%
  theme_booktabs() %>%
  set_header_df(mapping = my_header, key = "col_keys") %>%
  set_footer_df(mapping = my_footer, key = "col_keys") %>%
  border(i = 2, border.bottom = fp_border(color = "black", width = 1), part = "header") %>%
  fontsize(size = 12, i = 1, part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = c("Mean", "SD"), align = "right", part = "body") %>%
  align(align = "right", part = "footer") %>%
  border(border.top = fp_border(color = "black", width = 1), part = "footer") %>%
  bold(j = 1, part = "body") %>%
  bold(i = 1:2, part = "header") %>%
  font(fontname = "Calibri", part = "all") %>%
  merge_h(part = "footer") %>%
  merge_at(i = 1, j = 1:3, part = "header") %>%
  merge_h(i = (nrow(result_df_final) - n_stats + 1):nrow(result_df_final), part = "body") %>%
  autofit()


#----------------------------------------------
# Export Table
#----------------------------------------------

### Preview in R (interactive sessions only):

if (interactive()) {
  print(flex)
  print(flex, preview = "docx")
}
output_dir <- model$apollo_control$outputDirectory
fname <- paste0(output_dir, "/", apollo_control$modelName, ".docx")
save_as_docx(flex, path = fname)#, pr_section = set_prop)

