######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Script for Mixed Logit Model in Preference     ###
###                 Space for Subsample New Target Group           ###
### Output        : Flextable (Word file)                          ###
### Date          : 11.06.2025                                     ###
######################################################################


#### Apollo standard script #####

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
output_dir <- file.path(out_base, "MXL", "Preference_Space")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

### Set core controls
apollo_control = list(
  modelName  = "MXL_PS_Subsample_New_Target_Group",
  modelDescr = "MXL preference space baseline",
  indivID    ="i_NUMBER",
  mixing     = TRUE,
  HB= FALSE,
  nCores     = n_cores,
  outputDirectory = output_dir
)


# Filter to exclude all individuals who are neither tenants, nor low-income, nor living in a multi-family building (MFH)
apollo_ready_dataset <- df_long
apollo_ready_dataset_clean <- apollo_ready_dataset %>%
  filter(
    mfh == 1 | lowincome == 1 | tenant == 1)
database <- as.data.frame(apollo_ready_dataset_clean)
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
model = apollo_estimate(apollo_beta, apollo_fixed,
                           apollo_probabilities, apollo_inputs,
                           estimate_settings=list(maxIterations=400,
                                                  estimationRoutine="bfgs",
                                                  hessianRoutine="numDeriv"))



# ##################################################################
# ##  MODEL OUTPUTS                                               ##
# ##################################################################
apollo_saveOutput(model, saveOutput_settings = list(printPVal = 1))

# Create data frame for flextable
dat <- data.frame(matrix(NA,nrow=48,ncol=2))
dat[,1] <- c("asc: Keeping electricity provider", "asc",
             "Investor","1",
             "Member","2",
             "Organisator: Citizens","3",
             "Organisator: Municipality","4",
             "Social goal","5",
             "Ecological goal","6",
             "Social and ecological goal","7",
             "Split supply","8",
             "Price","9",
             "SD asc", "10",
             "SD Investor","11",
             "SD Member","12",
             "SD Organisator: Citizens","13",
             "SD Organisator: Municipality","14",
             "SD Social goal","15",
             "SD Ecological goal","16",
             "SD Social and ecological goal","17",
             "SD Split supply","18",
             "SD Price","19",
             "Number of Observations","LL(start)","LL(0)","LL(final)","Rho-squared","Adj. Rho-squared","AIC","BIC")


z <- length(model$estimate)

### Extract relevant variables from model output
est <- c(model$estimate[1:z-1],model$estimate[z]) 
se  <- c(model$robse[1:(z-1)], model$robse[z])
p <- 2 * (1 - pnorm(abs(est / se)))
n <- nrow(database)/10
param_names <- names(model$apollo_beta)

target_order <- c(
  # Main parameters
  "asc", "bpartiinv", "bpartimem", "borgcit", "borgmun",
  "bgoalsoc", "bgoaleco", "bgoalboth", "bconsplit", "bprice",
  
  # Standard deviations (sig_)
  "sig_asc", "sig_bpartiinv", "sig_bpartimem", "sig_borgcit", "sig_borgmun",
  "sig_bgoalsoc", "sig_bgoaleco", "sig_bgoalboth", "sig_bconsplit", "sig_bprice"
)

idx <- match(target_order, param_names)

est_reordered <- est[idx]
se_reordered <- se[idx]
p_reordered  <- p[idx]




nParams     <- length(model$apollo_beta)
nFreeParams <- nParams
if(!is.null(model$apollo_fixed)) nFreeParams <- nFreeParams - length(model$apollo_fixed)

p_wert <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else return("")
}


dat[1:(z*2), 2] <- unlist(lapply(1:z, function(j) {
  if (j == z) {
    x <- as.numeric(format(est_reordered[j], digits = 2, nsmall = 2))
    y <- match(TRUE, round(x, 1:20) == x)
    return(c(paste0(x, p_wert(p_reordered[j])),
             paste0("(", format(round(se_reordered[j], digits = y), scientific = FALSE), ")")))
  } else {
    return(c(paste0(round(est_reordered[j], 2), p_wert(p_reordered[j])),
             paste0("(", round(se_reordered[j], 2), ")")))
  }
}))


# dat[1:(z*2), 2] <- unlist(lapply(1:z, function(j) {
#   if (j == z) {
#     x <- as.numeric(format(est[j], digits = 2, nsmall = 2))
#     y <- match(TRUE, round(x, 1:20) == x)
#     return(c(paste0(x, p_wert(p[j])), paste0("(", format(round(se[j], digits = y), scientific = FALSE), ")")))
#   } else {
#     return(c(paste0(round(est[j], 2), p_wert(p[j])), paste0("(", round(se[j], 2), ")")))
#   }
# }))

dat[z*2+1,2] <- n
dat[z*2+2,2] <- round(model$LLStart,4)
dat[z*2+3,2] <- round(model$LL0,4)
dat[z*2+4,2] <- round(model$LLout,4)
dat[z*2+5,2] <- round(1-(model$maximum/model$LL0),4)
dat[z*2+6,2] <- round(1-((model$maximum-nFreeParams)/model$LL0),4)
dat[z*2+7,2] <- round(-2*model$maximum + 2*nFreeParams,2)
dat[z*2+8,2] <- round(-2*model$maximum + nFreeParams*log(model$nObs),2)


####################################################################
##  Flextable                                                     ##
####################################################################

### Header and footer settings

my_header <- data.frame(col_keys=colnames(dat),
                        line1 = c("Model Results - Mixed logit - Preference Space"),
                        line2 = c("Attribute","Preference"),
                        stringsAsFactors=FALSE)

my_footer <- data.frame(col_keys=colnames(dat),
                        line1 = c("** 0.01 , * 0.05"),
                        line2 = c("Reference contract: Only customer at a municipal utility with full supply and no additional goals"),
                        stringsAsFactors=FALSE)

### Graphical formatting of Flextable
flex <- flextable(dat) %>%
  theme_booktabs() %>%
  set_header_df(mapping = my_header, key="col_keys") %>%
  set_footer_df(mapping = my_footer, key="col_keys") %>%
  border(i=2,border.bottom=fp_border(color="black",width=1),part="header") %>%
  fontsize(size=12,i=1,part="header") %>%
  align(align="center",part="header") %>%
  align(align="right",part="footer") %>%
  border(border.top=fp_border(color="black",width=1),part="footer") %>%
  border(i=21,border.top=fp_border(width=1)) %>%
  border(i=41,border.top=fp_border(width=1)) %>%
  bold(j=1,part="body") %>%
  bold(i=1:2,part="header") %>%
  font(fontname="Calibri",part="body") %>%
  font(fontname="Calibri",part="header") %>%
  merge_h(part="footer") %>%
  merge_at(i =1:2,j=1,part="body") %>%
  merge_at(i =3:4,j=1,part="body") %>%
  merge_at(i =5:6,j=1,part="body") %>%
  merge_at(i =7:8,j=1,part="body") %>%
  merge_at(i =9:10,j=1,part="body") %>%
  merge_at(i =11:12,j=1,part="body") %>%
  merge_at(i =13:14,j=1,part="body") %>%
  merge_at(i =15:16,j=1,part="body") %>%
  merge_at(i =17:18,j=1,part="body") %>%
  merge_at(i =19:20,j=1,part="body") %>%
  merge_at(i =21:22,j=1,part="body") %>%
  merge_at(i =23:24,j=1,part="body") %>%
  merge_at(i =25:26,j=1,part="body") %>%
  merge_at(i =27:28,j=1,part="body") %>%
  merge_at(i =29:30,j=1,part="body") %>%
  merge_at(i =31:32,j=1,part="body") %>%
  merge_at(i =33:34,j=1,part="body") %>%
  merge_at(i =35:36,j=1,part="body") %>%
  merge_at(i =37:38,j=1,part="body") %>%
  merge_at(i =39:40,j=1,part="body") %>%
  merge_at(i=1,part="header") %>%
  width(j=2,width=1.2) %>%
  width(j=1,width=4)

#----------------------------------------------
# Export Table
#----------------------------------------------

if (interactive()) {
  print(flex)
  print(flex, preview = "docx")
}
output_dir <- model$apollo_control$outputDirectory
fname <- paste0(output_dir, "/", apollo_control$modelName, ".docx")
save_as_docx(flex, path = fname)#, pr_section = set_prop)


