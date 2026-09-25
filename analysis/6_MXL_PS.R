######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Script for Mixed Logit Model in Preference space##
### Output        : Flextable (Word file)                          ###
### Date          : 11.06.2025                                     ###
######################################################################


#### Apollo standard script #####

# Load apollo package
library(apollo)

# Search-range helper for apollo_searchStart() (defines make_searchStart_bounds()).
if (!exists("make_searchStart_bounds")) {
  source("Scripts/models/mxl/searchStart_ranges.R", encoding = "UTF-8")
}

n_draws <- 1000
# Number of cores: respect a value set by the calling driver (e.g. SLURM),
# otherwise use all but one of the locally available cores.
if (!exists("n_cores")) n_cores <- max(1, parallel::detectCores() - 1)

# Initialize model
apollo_initialise()

### Output directory: use the server-provided results path when available,
### otherwise fall back to the local project folder.
out_base <- if (exists("results_path") && nzchar(results_path)) results_path else "Hauptstudie/Estimation_results"
output_dir <- file.path(out_base, "MXL", "Preference_Space")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

### Set core controls
apollo_control = list(
  modelName  = "MXL_base",
  modelDescr = "MXL preference space baseline",
  indivID    ="i_NUMBER",
  mixing     = TRUE,
  HB= FALSE,
  nCores     = n_cores,
  outputDirectory = output_dir
)

apollo_ready_dataset <- df_long
database <- as.data.frame(apollo_ready_dataset)
database <- database[order(database$i_NUMBER),]

### Check whether there are n.a. in relevant variables ####

# List of variables to be checked
vars_to_check <- c("OrgCit", "OrgMun", "PartiInv", "PartiMem", 
                   "GoalSoc", "GoalEco", "GoalBoth","Con", "Price")

# Verify that each selected variable contains only the expected unique values
lapply(database[vars_to_check], function(x) unique(x))


##### Define model parameters depending on attributes and model specification ####
# Set values to 0 for conditional logit model

apollo_beta=c(asc = 0,
              borgcit = 0.06, #citizens
              borgmun = 0.16, #municipality
              bpartiinv = 0.08, #investor
              bpartimem = 0.05, #member
              bgoalsoc = 0.16, #social goal
              bgoaleco = 0.26, #ecological goal
              bgoalboth = 0.21, #social and ecological goal
              bconsplit = -0.13, #contract split 
              bprice = -0.15, #price
              sig_borgcit = 0,
              sig_borgmun = 0,
              sig_bpartiinv = 0,
              sig_bpartimem = 0,
              sig_bgoalsoc = 0,
              sig_bgoaleco = 0,
              sig_bgoalboth = 0,
              sig_bconsplit = 0,
              sig_bprice = 0,
              sig_asc = 0)

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
  randcoeff[["ranbpartimem"]] = bpartimem + sig_bpartimem * draws_bpartimem
  randcoeff[["ranbgoalsoc"]] = bgoalsoc + sig_bgoalsoc * draws_bgoalsoc
  randcoeff[["ranbgoaleco"]] = bgoaleco + sig_bgoaleco * draws_bgoaleco
  randcoeff[["ranbgoalboth"]] = bgoalboth + sig_bgoalboth* draws_bgoalboth
  randcoeff[["ranbconsplit"]] = bconsplit + sig_bconsplit* draws_bconsplit
  randcoeff[["ranasc"]] = asc + sig_asc* draws_asc
  randcoeff[["ranbprice"]] = -exp(bprice + sig_bprice*draws_bprice)
  
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
  
  V[['alt1']] = ranborgcit*OrgCit + ranborgmun*OrgMun + ranbpartiinv * PartiInv + ranbpartimem * PartiMem + ranbgoalsoc * GoalSoc + ranbgoaleco * GoalEco + ranbgoalboth * GoalBoth +  ranbconsplit*Con + ranbprice*  Price
  V[['alt2']] = ranasc
  
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

### Search for good starting values (preference-space ranges, 100 candidates)
beta_bounds <- make_searchStart_bounds(apollo_beta, space = "PS")
apollo_beta = apollo_searchStart(apollo_beta, apollo_fixed,
                                 apollo_probabilities, apollo_inputs,
                                 searchStart_settings = list(
                                   nCandidates   = 100,
                                   apolloBetaMin = beta_bounds$min,
                                   apolloBetaMax = beta_bounds$max
                                 ))

# Estimate model with bfgs algorithm

MXL_base = apollo_estimate(apollo_beta, apollo_fixed,
                               apollo_probabilities, apollo_inputs,
                               estimate_settings=list(maxIterations=400,
                                                      estimationRoutine="bfgs",
                                                      hessianRoutine="numDeriv"))



# ##################################################################
# ##  MODEL OUTPUTS                                               ##
# ##################################################################

apollo_saveOutput(MXL_base, saveOutput_settings = list(printPVal = 1))


