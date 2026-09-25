######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Estimation of all latent class conditional logit  ###
###               models, WTP space, with 2 to 7 classes            ###
###               A  with all socio-demographic variables in the    ###
###                  class allocation function                      ###
###                  (K = 5 is the reported model, Table 5)         ###
###               B  without socio-demographic variables            ###
###               All models use the same estimation sample, so     ###
###               their log-likelihoods are comparable (Table A.2). ###
###               Tables and tests are built by scripts 11 to 13    ###
###               from the saved models.                            ###
### Requires    : df_long (1_Data_Procession.R),                    ###
###               8_LCL_model_definition.R, 8_LCL_start_values.R    ###
### Output      : .../WTP_Space/reported/        (K = 5, with       ###
###                                               socio-demographics)###
###               .../WTP_Space/enumeration_sociodem/K<k>/          ###
###               .../WTP_Space/enumeration_nocov/K<k>/             ###
###               model (.rds), Apollo output, estimates and        ###
###               covariance matrices (.csv),                       ###
###               enumeration_estimation_summary.csv                ###
### Runtime     : several hours (12 models, analytic Hessian)       ###
### Date        : 24.09.2026                                        ###
######################################################################

library(apollo)
library(dplyr)

apollo_initialise()
source("8_LCL_model_definition.R")
if (!exists("LCL_START_VALUES")) source("8_LCL_start_values.R")
close_sinks()

## -------------------------------------------------------------- ##
## Settings                                                        ##
## -------------------------------------------------------------- ##

K_SET   <- 2:7
N_CORES <- 4

BASE    <- file.path("Hauptstudie", "Estimation_results", "LCLogit", "WTP_Space")
OUT_DIR <- c(sociodem = file.path(BASE, "enumeration_sociodem"),
             nocov    = file.path(BASE, "enumeration_nocov"))

# Models to estimate: every K with and without socio-demographics.
# The reported model (five classes with socio-demographics) comes first.
jobs <- expand.grid(K = K_SET, spec = c("sociodem", "nocov"), stringsAsFactors = FALSE)
jobs <- jobs[order(!(jobs$K == 5 & jobs$spec == "sociodem")), ]

# The reported model is saved in its own folder
REPORTED_DIR <- file.path(BASE, "reported")
is_reported  <- function(K, spec) K == 5 && spec == "sociodem"

## -------------------------------------------------------------- ##
## Estimation                                                      ##
## -------------------------------------------------------------- ##

summary_rows <- list()

for (i in seq_len(nrow(jobs))) {

  K          <- jobs$K[i]
  spec       <- jobs$spec[i]
  covariates <- spec == "sociodem"
  key        <- paste0(spec, "_K", K)
  model_name <- sprintf("lc%dcl_WTP_%s", K, spec)
  out_k      <- if (is_reported(K, spec)) REPORTED_DIR else
                  file.path(OUT_DIR[[spec]], paste0("K", K))

  cat("\n=====================================================\n")
  cat("K =", K, "classes,", if (covariates) "with" else "without",
      "socio-demographics |", format(Sys.time()), "\n")
  cat("=====================================================\n")

  # Starting values: best solution of the multi-start search
  beta_start <- normalise_beta(lcl_start_values(K, covariates), K, covariates)

  lcl_inputs(K, covariates,
             modelName  = model_name,
             modelDescr = paste("Latent class conditional logit, WTP space,", K, "classes,",
                                if (covariates) "all socio-demographics in the class allocation,"
                                else "no socio-demographics in the class allocation,",
                                "normalised (class a is the reference).",
                                "Estimation sample of the reported model.",
                                "Starting values from the best solution of the multi-start search."),
             outputDirectory = out_k,
             beta   = beta_start,
             nCores = N_CORES)

  t0 <- Sys.time()
  model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities,
                           apollo_inputs, estimate_settings = LCL_ESTIMATE_SETTINGS)
  model[["unconditionals"]] <- tryCatch(
    apollo_lcUnconditionals(model, apollo_probabilities, apollo_inputs),
    error = function(e) NULL)
  saveRDS(model, file.path(out_k, paste0(model_name, "_model.rds")))

  # Apollo output; for the reported model including covariance and
  # correlation matrices
  full <- is_reported(K, spec)
  tryCatch(apollo_saveOutput(model, saveOutput_settings = list(
    printPVal = 2, printClassical = TRUE, printDiagnostics = TRUE,
    printCovar = full, printCorr = full, printOutliers = full, printChange = TRUE,
    saveEst = TRUE, saveCov = TRUE, saveCorr = full, saveModelObject = FALSE)),
    error = function(e) cat("\napollo_saveOutput failed:", conditionMessage(e), "\n"))

  # Diagnostics and comparison with the reported log-likelihood
  ll     <- unname(model$LLout[1])
  ll_rep <- LCL_REPORTED_LL[[key]]
  ev     <- if (!is.null(model$hessian) && all(is.finite(model$hessian)))
              eigen(model$hessian, symmetric = TRUE, only.values = TRUE)$values else NULL
  shares <- if (!is.null(model$unconditionals$pi_values))
              sapply(model$unconditionals$pi_values, function(x) mean(as.matrix(x))) else NA

  cat("\nLL(final)                 :", round(ll, 4), "\n")
  cat("LL reported in the paper  :", round(ll_rep, 4),
      if (abs(ll - ll_rep) <= 0.01) "- reached" else "- NOT REACHED", "\n")
  cat("AIC / BIC                 :", round(model$AIC, 2), "/", round(model$BIC, 2), "\n")
  if (!is.null(ev)) cat("Hessian negative definite :", all(ev < 0), "\n")
  cat("Class shares (%)          :",
      paste(formatC(100 * shares, format = "f", digits = 1), collapse = " | "), "\n")
  cat("Minutes                   :",
      round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "\n")

  summary_rows[[key]] <- data.frame(
    model = key, K = K, covariates = covariates,
    nPar = n_free_params(K, covariates), LL = round(ll, 4),
    LL_reported = ll_rep, reached = abs(ll - ll_rep) <= 0.01,
    AIC = round(model$AIC, 2), BIC = round(model$BIC, 2),
    hessian_neg_def = if (is.null(ev)) NA else all(ev < 0),
    stringsAsFactors = FALSE)
}

## -------------------------------------------------------------- ##
## Summary                                                         ##
## -------------------------------------------------------------- ##

summary_tab <- do.call(rbind, summary_rows)
cat("\n=== Class enumeration: estimated models ===\n")
print(summary_tab, row.names = FALSE)
if (!all(summary_tab$reached))
  cat("\nWARNING: not every model reached the reported log-likelihood.\n")
write.csv(summary_tab, file.path(BASE, "enumeration_estimation_summary.csv"),
          row.names = FALSE)
