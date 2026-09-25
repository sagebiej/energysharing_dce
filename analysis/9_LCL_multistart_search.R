######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Multi-start search for the latent class models    ###
###               (WTP space, 2 to 7 classes, without and with      ###
###               socio-demographics in the class allocation)       ###
###               Only run if RUN_SEARCH = TRUE in 0_Main_Script.R. ###
###               Otherwise the stored starting values in           ###
###               8_LCL_start_values.R are used.                    ###
### Requires    : df_long (1_Data_Procession.R),                    ###
###               8_LCL_model_definition.R, 8_LCL_start_values.R    ###
### Output      : .../WTP_Space/search/run_<date_time>/<model>/     ###
###                 multistart_results.csv (one row per start), log ###
###               .../WTP_Space/search/                             ###
###                 search_start_values.rds (best solution per      ###
###                 model, read by lcl_start_values())              ###
###                 search_record.csv (starts per model, Table A.2) ###
### Runtime     : one to two days with the default settings         ###
### Date        : 24.09.2026                                        ###
######################################################################
###
### Why a multi-start search
###   The log-likelihood of a latent class model has many local optima.
###   Each model is therefore estimated from many starting values and
###   the best solution is kept. The number of starts that reach the
###   best solution indicates how reliably it is found.
###
### Types of starting values
###   random  wide random draws for all parameters
###   split   the solution with K-1 classes of this run, one class
###           duplicated with some noise (the new class starts as a
###           copy of an existing one)
###   nested  (only with socio-demographics) the solution without
###           socio-demographics for the same K, interactions at zero
###   pert    random perturbations of the nested start
###
### Order: all models without socio-demographics first (K = 2 to 7),
### then all models with socio-demographics, so that the nested starts
### are available.
###
### Note: the search is reproducible for a given seed, number of cores
### and Apollo version. On another machine it may find the same optima
### by different starts, or miss an optimum that is reached rarely.
### The number of starts reaching the best solution in Table A.2 then
### differs from the paper.
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

SEED  <- 20260925
K_SET <- 2:7

# Random starts per model (the other start types come on top)
N_RANDOM <- list(
  nocov    = c("2" = 5, "3" = 8,  "4" = 12, "5" = 20, "6" = 20, "7" = 15),
  sociodem = c("2" = 8, "3" = 12, "4" = 20, "5" = 30, "6" = 30, "7" = 30))
N_PERTURB   <- 4       # perturbations of the nested start
SPLIT_NOISE <- 0.25    # relative noise on the duplicated class

MAX_HOURS_MODEL <- 4   # no new starts for a model after this time
MAXITER         <- 500
N_CORES         <- 4

RUN_DIR <- file.path(SEARCH_DIR, paste0("run_", format(Sys.time(), "%Y%m%d_%H%M")))
dir.create(RUN_DIR, recursive = TRUE, showWarnings = FALSE)

START_VALUES_FILE <- file.path(SEARCH_DIR, "search_start_values.rds")
RECORD_FILE       <- file.path(SEARCH_DIR, "search_record.csv")

## -------------------------------------------------------------- ##
## Starting values                                                 ##
## -------------------------------------------------------------- ##

CLASS_BASES <- c("bprice", "asc", ATTR, alloc_bases)
TASTE_BASES <- c("bprice", "asc", ATTR)

# SD of the socio-demographic variables, scales the random draws of
# the interaction terms
cov_sd <- sapply(inter_vars, function(v) sd(database[[v]], na.rm = TRUE))

# All parameters of class s as a vector over CLASS_BASES
extract_class <- function(est, s)
  setNames(vapply(CLASS_BASES, function(b) {
    nm <- paste0(b, "_", s)
    if (nm %in% names(est)) unname(est[[nm]]) else 0
  }, numeric(1)), CLASS_BASES)

# List of class vectors -> full, normalised parameter vector. The price
# parameter must stay negative, otherwise the WTP-space model flips sign.
assemble_beta <- function(class_list, covariates) {
  K <- length(class_list)
  b <- beta_template(K)
  for (i in seq_len(K)) {
    s <- letters[i]
    for (bb in CLASS_BASES) b[[paste0(bb, "_", s)]] <- class_list[[i]][[bb]]
    if (b[[paste0("bprice_", s)]] >= -0.01) b[[paste0("bprice_", s)]] <- -0.01
  }
  normalise_beta(b, K, covariates)
}

draw_random <- function(K, covariates) {
  b <- beta_template(K)
  for (s in letters[1:K]) {
    b[[paste0("bprice_", s)]] <- -exp(runif(1, log(0.02), log(2.0)))
    b[[paste0("asc_", s)]]    <- runif(1, -30, 20)
    for (p in ATTR) b[[paste0(p, "_", s)]] <- runif(1, -8, 12)
    b[[paste0("delta_", s)]]  <- runif(1, -2, 2)
    if (covariates) for (v in inter_vars)
      b[[paste0("delta_", v, "_", s)]] <- runif(1, -2, 2) / cov_sd[[v]]
  }
  b[fixed_params(K, covariates)] <- 0
  b
}

draw_perturbed <- function(b0, noise, K, covariates) {
  b    <- b0
  free <- setdiff(names(b), fixed_params(K, covariates))
  b[free] <- b[free] + rnorm(length(free), 0, noise * pmax(abs(b[free]), 0.15))
  for (s in letters[1:K]) {
    nm <- paste0("bprice_", s); if (b[[nm]] >= -0.01) b[[nm]] <- -0.01
  }
  b[fixed_params(K, covariates)] <- 0
  b
}

# K-1 starts from the (K-1)-class solution: one class split into two
# halves (class constant - log 2), the copy with noise on the tastes
starts_split <- function(est_prev, K, covariates, prefix) {
  if (is.null(est_prev)) return(list())
  cl <- lapply(letters[1:(K - 1)], function(s) extract_class(est_prev, s))
  lapply(seq_len(K - 1), function(r) list(
    tag  = sprintf("%s_split%02d", prefix, r),
    note = paste0("split class ", letters[r], " of the ", K - 1, "-class solution"),
    gen  = local({ r <- r; function() {
      orig <- cl[[r]]; orig[["delta"]] <- orig[["delta"]] - log(2)
      copy <- orig
      copy[TASTE_BASES] <- copy[TASTE_BASES] * (1 + rnorm(length(TASTE_BASES), 0, SPLIT_NOISE)) +
                           rnorm(length(TASTE_BASES), 0, 0.1)
      cl2 <- cl; cl2[[r]] <- orig
      assemble_beta(c(cl2, list(copy)), covariates)
    } })))
}

## -------------------------------------------------------------- ##
## Search for one model                                            ##
## -------------------------------------------------------------- ##
# Each start is estimated without Hessian. Returns the best solution
# and the log-likelihood of every start.

run_search <- function(K, covariates, jobs, out_dir) {
  results_csv <- file.path(out_dir, "multistart_results.csv")
  best  <- list(LL = -Inf, estimate = NULL)
  t_mod <- Sys.time()
  for (j in jobs) {
    if (as.numeric(difftime(Sys.time(), t_mod, units = "hours")) >= MAX_HOURS_MODEL) {
      cat("\nTime budget for this model used up.\n"); break
    }
    cat("\n##### START", j$tag, "|", j$note, "\n")
    t1 <- Sys.time()
    m <- tryCatch({
      lcl_inputs(K, covariates, modelName = j$tag, modelDescr = paste("search", j$tag),
                 outputDirectory = out_dir, beta = j$gen(), nCores = N_CORES)
      apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,
                      estimate_settings = list(estimationRoutine     = "bfgs",
                                               scaleAfterConvergence = FALSE,
                                               maxIterations         = MAXITER,
                                               hessianRoutine        = "none",
                                               writeIter             = FALSE))
    }, error = function(e) { cat("ESTIMATION FAILED:", conditionMessage(e), "\n"); NULL })

    ll  <- if (is.null(m)) NA_real_ else unname(m$LLout[1])
    row <- data.frame(K = K, covariates = covariates, run = j$tag, note = j$note,
                      status = if (is.null(m)) "failed" else "ok", LL = round(ll, 4),
                      code = if (is.null(m) || is.null(m$code)) NA else unname(m$code),
                      minutes = round(as.numeric(difftime(Sys.time(), t1, units = "mins")), 1),
                      stringsAsFactors = FALSE)
    write.table(row, results_csv, sep = ",", row.names = FALSE,
                col.names = !file.exists(results_csv), append = file.exists(results_csv))
    if (is.finite(ll) && ll > best$LL) {
      best <- list(LL = ll, estimate = normalise_beta(m$estimate, K, covariates))
      cat("--> new best LL =", round(ll, 4), "\n")
    }
  }
  res <- read.csv(results_csv, stringsAsFactors = FALSE)
  res <- res[res$status == "ok" & is.finite(res$LL), ]
  best$starts      <- nrow(res)
  best$starts_best <- sum(abs(res$LL - best$LL) < 0.01)
  best
}

## -------------------------------------------------------------- ##
## Main loop                                                       ##
## -------------------------------------------------------------- ##

found  <- list()
record <- list()

for (spec in c("nocov", "sociodem")) {
  covariates <- spec == "sociodem"

  for (K in K_SET) {
    key     <- paste0(spec, "_K", K)
    out_dir <- file.path(RUN_DIR, key)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    set.seed(SEED + 100 * covariates + K)

    con <- file(file.path(out_dir, "log.txt"), open = "wt")
    sink(con, split = TRUE); sink(con, type = "message")

    tryCatch({
      cat("\n=====================================================\n")
      cat("Search:", key, "| seed", SEED + 100 * covariates + K, "|", format(Sys.time()), "\n")
      cat("=====================================================\n")

      # Starting values for this model
      jobs <- starts_split(found[[paste0(spec, "_K", K - 1)]], K, covariates, key)
      if (covariates && !is.null(found[[paste0("nocov_K", K)]])) {
        b_nested <- normalise_beta(found[[paste0("nocov_K", K)]], K, covariates = TRUE)
        jobs <- c(jobs,
          list(list(tag = paste0(key, "_nested"), gen = function() b_nested,
                    note = "solution without socio-demographics, interactions at zero")),
          lapply(seq_len(N_PERTURB), function(i) {
            nz <- rep(c(0.3, 0.6, 1.0), length.out = N_PERTURB)[i]
            list(tag  = sprintf("%s_pert%02d", key, i),
                 note = paste0("nested start + noise sd = ", nz, " x |estimate|"),
                 gen  = local({ nz <- nz; function() draw_perturbed(b_nested, nz, K, TRUE) }))
          }))
      }
      jobs <- c(jobs, lapply(seq_len(N_RANDOM[[spec]][[as.character(K)]]), function(i)
        list(tag = sprintf("%s_rand%02d", key, i), note = "wide random draw",
             gen = function() draw_random(K, covariates))))
      cat("Starts planned:", length(jobs), "\n")

      best <- run_search(K, covariates, jobs, out_dir)
      if (is.null(best$estimate)) stop("no start converged")

      cat("\nBest LL:", round(best$LL, 4), "| reached by", best$starts_best, "of",
          best$starts, "starts | reported:", LCL_REPORTED_LL[[key]], "\n")

      found[[key]]  <- best$estimate
      record[[key]] <- data.frame(model = key, covariates = covariates, K = K,
                                  starts = best$starts, starts_best = best$starts_best,
                                  best_LL = round(best$LL, 4),
                                  reported_LL = LCL_REPORTED_LL[[key]],
                                  stringsAsFactors = FALSE)
      # Saved after every model, so an interrupted run keeps its results
      saveRDS(found, START_VALUES_FILE)
      write.csv(do.call(rbind, record), RECORD_FILE, row.names = FALSE)

    }, error = function(e) cat("\n", key, "ABORTED:", conditionMessage(e), "\n"),
       finally = { close_sinks(); try(close(con), silent = TRUE) })
  }
}

file.copy(c(START_VALUES_FILE, RECORD_FILE), RUN_DIR, overwrite = TRUE)

cat("\n=== Multi-start search: summary ===\n")
print(do.call(rbind, record), row.names = FALSE)
cat("\nThe estimation script 10_LCL_WTP_K_Classes.R now starts from these solutions.\n")
