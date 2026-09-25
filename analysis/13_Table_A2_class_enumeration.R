######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Table A.2 - selection of the number of classes.   ###
###               Latent class models with 2 to 7 classes, with     ###
###               socio-demographics (upper panel) and without      ###
###               (lower panel), on the estimation sample of the    ###
###               reported model (2,199 respondents).               ###
###               Collects the estimated models, recomputes the     ###
###               class assignment statistics and writes the table. ###
###               Nothing is estimated.                             ###
### Requires    : df_long (1_Data_Procession.R),                    ###
###               8_LCL_model_definition.R, 8_LCL_start_values.R,   ###
###               models from 10_LCL_WTP_K_Classes.R                ###
### Output      : .../WTP_Space/enumeration_sociodem/               ###
###               TableA2_enumeration_sociodem.csv / .docx          ###
###               TableA2_enumeration_sociodem_long.csv             ###
###               TableA2_enumeration_sociodem_panelB_nocov.csv     ###
### Date        : 24.09.2026                                        ###
######################################################################
###
### Selection criteria (defined before the enumeration was run):
###   1  BIC, primary criterion
###   2  smallest class at least 5 % of the sample
###   3  price parameter significant in every class. In WTP space it is
###      the scale parameter; if it is not identified, the WTP values of
###      that class cannot be interpreted.
###   4  best solution reached by more than one start
### AIC is reported but not used for the selection: with 21,990
### observations its penalty is small and it keeps adding classes.
######################################################################

library(apollo)
library(dplyr)
library(flextable)
library(officer)

apollo_initialise()
source("8_LCL_model_definition.R")
if (!exists("LCL_START_VALUES")) source("8_LCL_start_values.R")
close_sinks()

## -------------------------------------------------------------- ##
## Settings                                                        ##
## -------------------------------------------------------------- ##

K_ALL        <- 2:7
MIN_SHARE    <- 0.05
ALPHA        <- 0.05

BASE     <- file.path("Hauptstudie", "Estimation_results", "LCLogit", "WTP_Space")
ENUM_DIR <- file.path(BASE, "enumeration_sociodem")
NC_DIR   <- file.path(BASE, "enumeration_nocov")
OUT_STEM <- file.path(ENUM_DIR, "TableA2_enumeration_sociodem")
TMP_DIR  <- file.path(ENUM_DIR, "_tmp_posteriors")

first_existing <- function(x) { x <- x[file.exists(x)]; if (length(x)) x[1] else NA_character_ }

# Model files per K (10_LCL_WTP_K_Classes.R): the five-class model is
# the reported model in reported/, all others are in enumeration_sociodem/
paths_for <- function(K) {
  d    <- if (K == 5) file.path(BASE, "reported") else file.path(ENUM_DIR, paste0("K", K))
  stem <- sprintf("lc%dcl_WTP_sociodem", K)
  list(model = first_existing(file.path(d, paste0(stem, "_model.rds"))),
       est   = first_existing(file.path(d, paste0(stem, "_estimates.csv"))),
       out   = first_existing(file.path(d, paste0(stem, "_output.txt"))))
}

# Number of starts and starts reaching the best solution
# (8_LCL_start_values.R, or the new search if RUN_SEARCH = TRUE)
search_rec <- lcl_search_record()
rec_for <- function(K, covariates) {
  r <- search_rec[search_rec$K == K & search_rec$covariates == covariates, ]
  if (nrow(r)) r[1, c("starts", "starts_best")] else data.frame(starts = NA, starts_best = NA)
}

## -------------------------------------------------------------- ##
## Helpers                                                         ##
## -------------------------------------------------------------- ##

# Robust p-values from the Apollo estimates file
read_estimates <- function(f) {
  if (is.na(f)) return(NULL)
  d <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
  names(d)[1] <- "parameter"
  pcol <- grep("^Rob.p", names(d), value = TRUE)[1]
  data.frame(parameter = d$parameter, estimate = d$Estimate,
             p_rob = if (is.na(pcol)) NA_real_ else suppressWarnings(as.numeric(d[[pcol]])),
             stringsAsFactors = FALSE)
}

ll_from_output <- function(f) {
  if (is.na(f)) return(NA_real_)
  l <- grep("LL\\(final, whole model\\)|^LL\\(final\\)", readLines(f, warn = FALSE), value = TRUE)[1]
  if (is.na(l)) NA_real_ else suppressWarnings(as.numeric(trimws(sub(".*:", "", l))))
}

hessian_from_output <- function(f) {
  if (is.na(f)) return(NA_character_)
  l <- grep("hessian properties", readLines(f, warn = FALSE), value = TRUE)[1]
  if (is.na(l)) NA_character_ else trimws(sub(".*:", "", l))
}

## -------------------------------------------------------------- ##
## Collect                                                         ##
## -------------------------------------------------------------- ##

dir.create(TMP_DIR, recursive = TRUE, showWarnings = FALSE)
rows <- list()

for (K in K_ALL) {
  cat("\n---------------- K =", K, "----------------\n")
  pth <- paths_for(K)
  if (is.na(pth$model)) { cat("No model found - K =", K, "is left out.\n"); next }
  cat("Model:", pth$model, "\n")

  model <- readRDS(pth$model)
  est   <- model$estimate
  cls   <- letters[1:K]
  nFree <- length(est) - length(intersect(FIXED, names(est)))
  ll    <- unname(model$LLout[1])

  ## ---- model on the data: posteriors, shares, LL check -------- ##
  nObs <- nrow(database); nResp <- length(unique(database$i_NUMBER))
  lcl_inputs(K, covariates = TRUE,
             modelName = paste0("post_K", K), modelDescr = "posterior probabilities",
             outputDirectory = TMP_DIR, beta = est, nCores = 1, noValidation = TRUE)

  # Recompute the LL with the model code as a consistency check
  ll_check <- tryCatch({
    P <- apollo_probabilities(est, apollo_inputs, functionality = "estimate")
    sum(log(if (is.list(P)) P[["model"]] else P))
  }, error = function(e) { cat("LL check failed:", conditionMessage(e), "\n"); NA_real_ })
  cat("LL stored:", round(ll, 4), "| recomputed:", round(ll_check, 4), "\n")
  if (is.finite(ll_check) && abs(ll_check - ll) > 0.01)
    warning("K = ", K, ": recomputed LL differs from the stored one - check the model file.")

  cond <- as.data.frame(apollo_lcConditionals(model, apollo_probabilities, apollo_inputs))
  pm   <- as.matrix(cond[, setdiff(names(cond), c("ID", "id", "i_NUMBER"))])
  ent      <- -sum(pm * log(pmax(pm, .Machine$double.eps)))
  R2_ent   <- 1 - ent / (nrow(pm) * log(ncol(pm)))
  mean_max <- mean(apply(pm, 1, max))

  unc <- if (!is.null(model$unconditionals)) model$unconditionals else
           apollo_lcUnconditionals(model, apollo_probabilities, apollo_inputs)
  shares <- sapply(unc$pi_values, function(x) mean(as.matrix(x)))

  ## ---- significance of the price (scale) parameter ------------ ##
  # The estimates file must belong to the same solution as the model
  e <- read_estimates(pth$est)
  if (!is.null(e)) {
    common <- intersect(e$parameter, names(est))
    dev    <- max(abs(e$estimate[match(common, e$parameter)] - est[common]))
    ll_out <- ll_from_output(pth$out)
    same_ll <- is.finite(ll_out) && abs(ll_out - ll) < 0.01
    if (dev > 1e-3 && !same_ll) {
      cat("WARNING: estimates file does not match the model (max diff", signif(dev, 3),
          ", LL in output", ll_out, ") - significance not used.\n"); e <- NULL
    } else if (dev > 1e-3) {
      cat("Estimates file from the re-estimation of the same solution (LL", round(ll_out, 4),
          ", max parameter diff", signif(dev, 3), ") - used for significance.\n")
    }
  }
  price_sig <- if (is.null(e)) NA_integer_ else
    sum(e$p_rob[match(paste0("bprice_", cls), e$parameter)] < ALPHA, na.rm = TRUE)
  max_wtp <- max(abs(est[as.vector(outer(ATTR, cls, paste, sep = "_"))]))

  ## ---- multi-start search ------------------------------------- ##
  rec      <- rec_for(K, covariates = TRUE)
  n_starts <- rec$starts
  n_best   <- rec$starts_best

  rows[[as.character(K)]] <- data.frame(
    K = K, nPar = nFree, nObs = nObs, nResp = nResp,
    LL = ll, LL_check = ll_check,
    AIC = -2 * ll + 2 * nFree, BIC = -2 * ll + nFree * log(nObs),
    smallest_share = min(shares), shares = paste(sprintf("%.1f", 100 * shares), collapse = " / "),
    entropy_R2 = R2_ent, mean_max_post = mean_max,
    price_sig = price_sig, max_attr_wtp = max_wtp,
    starts = n_starts, starts_best = n_best,
    hessian = hessian_from_output(pth$out),
    model_file = pth$model, stringsAsFactors = FALSE)
}

tab <- do.call(rbind, rows)
if (is.null(tab) || !nrow(tab)) stop("No models found.")
tab$dBIC <- tab$BIC - min(tab$BIC)
tab$dAIC <- tab$AIC - min(tab$AIC)

# Criteria check per K
tab$crit_share <- tab$smallest_share >= MIN_SHARE
tab$crit_price <- tab$price_sig == tab$K
tab$crit_repl  <- tab$starts_best > 1

cat("\n=====================================================\n")
cat("CLASS ENUMERATION WITH SOCIO-DEMOGRAPHICS\n")
cat("=====================================================\n")
print(transform(tab[, c("K", "nPar", "LL", "AIC", "BIC", "dBIC", "smallest_share",
                        "entropy_R2", "price_sig", "starts", "starts_best",
                        "crit_share", "crit_price", "crit_repl")],
                LL = round(LL, 2), AIC = round(AIC, 2), BIC = round(BIC, 2),
                dBIC = round(dBIC, 2), smallest_share = round(100 * smallest_share, 1),
                entropy_R2 = round(entropy_R2, 3)), row.names = FALSE)
cat("\nBIC minimum at K =", tab$K[which.min(tab$BIC)],
    "| AIC minimum at K =", tab$K[which.min(tab$AIC)], "\n")
if (!all(K_ALL %in% tab$K))
  cat("MISSING K:", paste(setdiff(K_ALL, tab$K), collapse = ", "),
      "- the table is incomplete.\n")

write.csv(tab, paste0(OUT_STEM, "_long.csv"), row.names = FALSE)

## -------------------------------------------------------------- ##
## Panel B: the same models WITHOUT socio-demographics, same sample ##
## -------------------------------------------------------------- ##

nocov <- do.call(rbind, lapply(tab$K, function(K) {
  mf   <- first_existing(file.path(NC_DIR, paste0("K", K), sprintf("lc%dcl_WTP_nocov_model.rds", K)))
  nPar <- n_free_params(K, covariates = FALSE); nObs <- tab$nObs[1]
  rec  <- rec_for(K, covariates = FALSE)
  if (is.na(mf))
    return(data.frame(K = K, LL = NA_real_, BIC = NA_real_, smallest = NA_real_,
                      starts = NA_integer_, best = NA_integer_))
  m  <- readRDS(mf); ll <- unname(m$LLout[1])
  sh <- if (!is.null(m$unconditionals$pi_values))
          min(sapply(m$unconditionals$pi_values, function(x) mean(as.matrix(x)))) else NA_real_
  data.frame(K = K, LL = ll, BIC = -2 * ll + nPar * log(nObs), smallest = sh,
             starts = rec$starts, best = rec$starts_best)
}))
cat("\n=== Panel B: without covariates, same sample ===\n"); print(nocov, row.names = FALSE)
if (all(is.na(nocov$LL))) nocov <- NULL
write.csv(nocov, paste0(OUT_STEM, "_panelB_nocov.csv"), row.names = FALSE)

## -------------------------------------------------------------- ##
## Table                                                           ##
## -------------------------------------------------------------- ##

f2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
f1 <- function(x) formatC(x, format = "f", digits = 1)
f3 <- function(x) formatC(x, format = "f", digits = 3)
na_dash <- function(x, f) ifelse(is.na(x), "–", f(x))

cols <- paste(tab$K, "classes")
body <- rbind(
  "No. of free parameters"                   = as.character(tab$nPar),
  "LL(final)"                                = f2(tab$LL),
  "AIC"                                      = f2(tab$AIC),
  "BIC"                                      = f2(tab$BIC),
  "ΔBIC to minimum"                     = f2(tab$dBIC),
  "Smallest class (%)"                       = f1(100 * tab$smallest_share),
  "Entropy R²"                          = f3(tab$entropy_R2),
  "Mean posterior probability"               = f3(tab$mean_max_post),
  "Classes with significant price parameter" = paste0(na_dash(tab$price_sig, as.character), " of ", tab$K),
  "Largest |attribute WTP| (pp)"             = f1(tab$max_attr_wtp),
  "Starts"                                   = as.character(tab$starts),
  "Starts reaching the best LL"              = as.character(tab$starts_best))
if (!is.null(nocov)) {
  m <- match(tab$K, nocov$K)
  body <- rbind(body,
    "Without covariates: LL(final)"          = na_dash(nocov$LL[m], f2),
    "Without covariates: BIC"                = na_dash(nocov$BIC[m], f2),
    "Without covariates: smallest class (%)" = na_dash(100 * nocov$smallest[m], f1),
    "Without covariates: starts (best)"      = ifelse(is.na(nocov$starts[m]), "\u2013",
      paste0(nocov$starts[m], ifelse(is.na(nocov$best[m]), "", paste0(" (", nocov$best[m], ")")))))
}
tbl <- data.frame(Statistic = rownames(body), body, check.names = FALSE,
                  stringsAsFactors = FALSE, row.names = NULL)
names(tbl) <- c("Statistic", cols)

note <- paste0(
  "Notes: Latent class conditional logit models in WTP space with all socio-demographic ",
  "variables in the class-allocation function, estimated on the sample of the model reported ",
  "in Table 5 (", format(tab$nObs[1], big.mark = ","), " choices of ",
  format(tab$nResp[1], big.mark = ","), " respondents). Each model is the best of a ",
  "multi-start search (random, perturbed and nested starting values; see Supplementary ",
  "material A.3). The price parameter is the scale parameter in WTP space; a class ",
  "whose price parameter is not significant at the 5% level has no interpretable WTP values. ",
  "Entropy R² and mean posterior probability of the assigned class describe how well ",
  "respondents are separated into classes. ",
  if (!is.null(nocov)) paste0("The last four rows report the same models without ",
    "socio-demographic covariates on the same sample; without covariates the BIC is lowest at ",
    nocov$K[which.min(nocov$BIC)], " classes. ") else "",
  "Minimum AIC and BIC in bold.")

ft <- flextable(tbl) |>
  theme_booktabs() |>
  add_header_lines("Table A.2 Latent class models with two to seven classes – model selection") |>
  add_footer_lines(note) |>
  align(j = 2:ncol(tbl), align = "center", part = "all") |>
  bold(i = which(tbl$Statistic == "AIC"), j = 1 + which.min(tab$AIC)) |>
  bold(i = which(tbl$Statistic == "BIC"), j = 1 + which.min(tab$BIC)) |>
  font(fontname = "Calibri", part = "all") |>
  fontsize(size = 9, part = "body") |> fontsize(size = 8, part = "footer") |>
  width(j = 1, width = 2.3) |> width(j = 2:ncol(tbl), width = 0.8)
if (!is.null(nocov))
  ft <- border(ft, i = which(tbl$Statistic == "Without covariates: LL(final)"),
               border.top = fp_border(width = 1))

print(tbl, row.names = FALSE, right = FALSE)
write.csv(tbl, paste0(OUT_STEM, ".csv"), row.names = FALSE, fileEncoding = "UTF-8")
check_no_lockfiles(paste0(OUT_STEM, ".docx"))
tryCatch(save_as_docx(ft, path = paste0(OUT_STEM, ".docx")),
         error = function(e) cat("\nCould not write the .docx:", conditionMessage(e), "\n"))

unlink(TMP_DIR, recursive = TRUE)
cat("\nWritten:", paste0(OUT_STEM, c(".csv", ".docx", "_long.csv", "_panelB_nocov.csv")), sep = "\n  ")
