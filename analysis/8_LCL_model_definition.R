######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Shared definition of the latent class models      ###
###               (conditional logit, WTP space) for any number of  ###
###               classes K, with or without socio-demographic      ###
###               variables in the class allocation function.       ###
###               Defines objects and helper functions only;        ###
###               nothing is estimated here.                        ###
### Used by     : 9_ to 13_                                         ###
### Requires    : df_long in the workspace (1_Data_Procession.R)    ###
### Date        : 24.09.2026                                        ###
######################################################################
###
### Model
###   Utility of the energy sharing offer (alt1) and of keeping the
###   current contract (alt2) in class s, WTP space:
###     V1 = -bprice_s * (sum_k wtp_k,s * x_k - Price)
###     V2 = -bprice_s * asc_s
###   The attribute parameters are willingness to pay in percentage
###   points of the monthly electricity bill; bprice_s is the scale.
###
###   Class allocation (softmax):
###     V_class_s = delta_s + sum_v delta_<v>_s * <v>
###   with v = the seven mean-centred socio-demographic variables.
###
### Normalisation
###   The softmax is invariant to adding a constant to all class
###   utilities. Class a is therefore the reference: delta_a and the
###   seven delta_inter_<v>_a are fixed at zero (8 parameters).
###   Log-likelihood, class shares and WTP do not depend on this.
###
### Model without socio-demographics
###   Same code with ALL interaction terms fixed at zero, so both
###   specifications are estimated on the same sample and their
###   log-likelihoods are comparable.
######################################################################

library(apollo)
library(dplyr)

## -------------------------------------------------------------- ##
## Constants                                                       ##
## -------------------------------------------------------------- ##

# WTP parameters of the attribute levels
ATTR <- c("bgoalsoc", "bgoaleco", "bgoalboth", "bconsplit",
          "borgcit", "borgmun", "bpartimem", "bpartiinv")

# Socio-demographic variables in the class allocation (mean-centred)
inter_vars <- c("inter_env_awareness_score", "inter_sex", "inter_age",
                "inter_educ_years", "inter_lowincome", "inter_highincome",
                "inter_mfh_or_tenant")

vars_util1 <- c("OrgCit", "OrgMun", "PartiInv", "PartiMem",
                "GoalSoc", "GoalEco", "GoalBoth", "Con", "Price",
                inter_vars)

# Parameter stems of the class allocation function
alloc_bases <- c("delta", paste0("delta_", inter_vars))

# Fixed parameters of the model WITH socio-demographics (reference class a)
FIXED <- c("delta_a", paste0("delta_", inter_vars, "_a"))

# choice == 2 is the "keep current provider" (status quo) alternative
SQ_ALT <- 2

## -------------------------------------------------------------- ##
## Estimation sample                                               ##
## -------------------------------------------------------------- ##
# Respondents with missing socio-demographic information are excluded,
# so that all latent class models use the same 2,199 respondents.

if (!exists("df_long")) {
  stop("df_long not found. Run 1_Data_Procession.R (or 0_Main_Script.R) first.")
}

database <- as.data.frame(df_long %>% filter(if_all(all_of(vars_util1), is.finite)))
database <- database[order(database$i_NUMBER), ]
database <<- database

cat("Estimation sample:", nrow(database), "rows,",
    length(unique(database$i_NUMBER)), "respondents\n")

## -------------------------------------------------------------- ##
## Model code for K classes                                        ##
## -------------------------------------------------------------- ##
# apollo_lcPars and apollo_probabilities are written out as text for
# the given K and evaluated in the global environment. Apollo rejects
# functions that reuse a loop index (checkIndices) and does not always
# pass global objects to parallel workers; fully expanded code avoids
# both. The same function is used with and without socio-demographics.

build_lcl <- function(K) {

  stopifnot(K >= 2, K <= 26)
  cls <- letters[1:K]

  ## ---- apollo_lcPars ------------------------------------------- ##
  L <- c("apollo_lcPars <- function(apollo_beta, apollo_inputs){",
         "  lcpars = list()")
  for (p in c("asc", ATTR, "bprice"))
    L <- c(L, sprintf("  lcpars[[\"%s\"]] = list(%s)",
                      p, paste0(p, "_", cls, collapse = ", ")))
  L <- c(L, "  V = list()")
  for (s in cls) {
    terms <- paste0("delta_", s)
    for (v in inter_vars)
      terms <- c(terms, sprintf("delta_%s_%s * %s", v, s, v))
    L <- c(L, sprintf("  V[[\"class_%s\"]] = %s", s,
                      paste(terms, collapse = " + ")))
  }
  L <- c(L,
    sprintf("  lcpars[[\"pi_values\"]] = apollo_classAlloc(list(classes=c(%s), utilities=V))",
            paste0("class_", cls, "=", seq_len(K), collapse = ", ")),
    "  return(lcpars)", "}")
  eval(parse(text = paste(L, collapse = "\n")), envir = globalenv())

  ## ---- apollo_probabilities ------------------------------------ ##
  ptxt <- sprintf('
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  mnl_settings = list(alternatives = c(alt1=1, alt2=2), avail = 1, choiceVar = choice)
  for (s in 1:%d) {
    V = list()
    V[["alt1"]] = -bprice[[s]] * (borgcit[[s]]*OrgCit + borgmun[[s]]*OrgMun +
                                  bpartiinv[[s]]*PartiInv + bpartimem[[s]]*PartiMem +
                                  bgoalsoc[[s]]*GoalSoc + bgoaleco[[s]]*GoalEco +
                                  bgoalboth[[s]]*GoalBoth + bconsplit[[s]]*Con - Price)
    V[["alt2"]] = -bprice[[s]] * asc[[s]]
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_", s)
    P[[paste0("Class_", s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_", s)]] = apollo_panelProd(P[[paste0("Class_", s)]], apollo_inputs, functionality)
  }
  P[["model"]] = apollo_lc(list(inClassProb = P, classProb = pi_values), apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}', K)
  eval(parse(text = ptxt), envir = globalenv())

  assign("CLS", cls, envir = globalenv())
  invisible(cls)
}

## -------------------------------------------------------------- ##
## Helpers                                                         ##
## -------------------------------------------------------------- ##

# Fixed parameters for K classes, with or without socio-demographics
fixed_params <- function(K, covariates = TRUE) {
  if (covariates) FIXED else
    c("delta_a", as.vector(outer(paste0("delta_", inter_vars), letters[1:K],
                                 paste, sep = "_")))
}

# Number of free parameters: 11 per class (price, asc, 8 WTP, constant)
# plus 7 interactions per class with covariates, minus the reference class
n_free_params <- function(K, covariates = TRUE)
  if (covariates) 18 * K - 8 else 11 * K - 1

# Full parameter vector for K classes, all values zero (names only)
beta_template <- function(K) {
  cls <- letters[1:K]
  nm <- unlist(lapply(cls, function(s)
    c(paste0("bprice_", s), paste0("asc_", s), paste0(ATTR, "_", s),
      paste0("delta_", s), paste0("delta_", inter_vars, "_", s))))
  setNames(rep(0, length(nm)), nm)
}

# Imposes the normalisation on a parameter vector: class-a values are
# subtracted from all allocation parameters, fixed parameters set to 0.
# The likelihood is unchanged.
normalise_beta <- function(beta, K, covariates = TRUE) {
  cls <- letters[1:K]
  for (base in alloc_bases) {
    nm  <- paste0(base, "_", cls)
    ref <- paste0(base, "_a")
    nm  <- nm[nm %in% names(beta)]
    if (ref %in% names(beta) && length(nm)) beta[nm] <- beta[nm] - beta[[ref]]
  }
  fx <- intersect(fixed_params(K, covariates), names(beta))
  beta[fx] <- 0
  beta
}

# Apollo pastes modelName directly onto outputDirectory, so the path
# must end with a slash
apollo_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  paste0(sub("[\\\\/]+$", "", path), "/")
}

# Builds the model code for K classes, sets apollo_beta, apollo_fixed
# and apollo_control in the global environment and validates the inputs
lcl_inputs <- function(K, covariates = TRUE, modelName, modelDescr,
                       outputDirectory, beta, nCores = 4, noValidation = FALSE) {
  build_lcl(K)
  # A mixed logit model estimated earlier in the same session leaves its
  # draws and random coefficients behind, and apollo would use them
  rm(list = intersect(c("apollo_draws", "apollo_randCoeff"), ls(globalenv())),
     envir = globalenv())
  assign("apollo_beta",  beta, envir = globalenv())
  assign("apollo_fixed", fixed_params(K, covariates), envir = globalenv())
  assign("apollo_control", list(
    modelName       = modelName,
    modelDescr      = modelDescr,
    indivID         = "i_NUMBER",
    nCores          = nCores,
    outputDirectory = apollo_dir(outputDirectory),
    noValidation    = noValidation), envir = globalenv())
  inputs <- apollo_validateInputs()
  assign("apollo_inputs", inputs, envir = globalenv())
  invisible(inputs)
}

# Estimation settings of all reported latent class models
LCL_ESTIMATE_SETTINGS <- list(estimationRoutine     = "bfgs",
                              scaleAfterConvergence = TRUE,
                              maxIterations         = 400,
                              hessianRoutine        = "analytic")

# Warns if a Word document in the target folder is open (lock file)
check_no_lockfiles <- function(path) {
  locks <- list.files(dirname(path), pattern = "^~\\$", all.files = TRUE)
  if (length(locks)) {
    cat("\nWARNING: lock files in", dirname(path), ":",
        paste(locks, collapse = ", "),
        "\n         Close the open Word documents, otherwise the file cannot be written.\n")
  }
  invisible(length(locks) == 0)
}

# Closes sinks left open by an aborted log file
close_sinks <- function() {
  while (sink.number() > 0) sink()
  if (sink.number(type = "message") != 2) sink(type = "message")
  invisible(TRUE)
}

cat("8_LCL_model_definition.R loaded. build_lcl(K) creates the model code for K classes.\n")
