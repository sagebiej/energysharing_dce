######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Results of the reported latent class model        ###
###               (WTP space, five classes, all socio-demographic   ###
###               variables in the class allocation function)       ###
###               1 diagnostics, class shares, class order          ###
###               2 plain-text model output, tidy estimates         ###
###               3 joint Wald tests on class membership,           ###
###                 hypothesis tests (Table 6), WTP sums per class  ###
###               4 Table 5 with 95% confidence intervals           ###
###               Nothing is estimated; the model comes from        ###
###               10_LCL_WTP_K_Classes.R.                           ###
### Requires    : df_long (1_Data_Procession.R),                    ###
###               8_LCL_model_definition.R, 8_LCL_start_values.R,   ###
###               reported/lc5cl_WTP_sociodem_model.rds             ###
### Output      : Table5_LCL_WTP_sociodem.csv / .docx,              ###
###               joint_tests_membership.csv, hypothesis_tests.csv, ###
###               package_wtp.csv, class_order.csv,                 ###
###               estimates_tidy.csv, lc5cl_WTP_sociodem_output_    ###
###               plain.txt                                         ###
### Date        : 24.09.2026                                        ###
######################################################################

library(apollo)
library(dplyr)
library(flextable)
library(officer)

apollo_initialise()
source("8_LCL_model_definition.R")
if (!exists("LCL_START_VALUES")) source("8_LCL_start_values.R")

## -------------------------------------------------------------- ##
## Settings                                                        ##
## -------------------------------------------------------------- ##

K          <- 5
MODEL_NAME <- "lc5cl_WTP_sociodem"
OUT_DIR    <- file.path("Hauptstudie", "Estimation_results", "LCLogit",
                        "WTP_Space", "reported")

# Reference class of the class allocation parameters in Table 5.
# "auto" = the class with the highest observed share of status quo
# choices (the status quo group).
REF_CLASS   <- "auto"   # "auto", or "a".."e"

# Order of the class columns in Table 5 (class numbers of a latent
# class model are arbitrary):
#   "sq_share"   observed share of status quo choices, descending
#   "share"      reference class first, then by class share
#   "estimation" as estimated, a .. e
CLASS_ORDER <- "sq_share"

CONF_LEVEL  <- 0.95
DIGITS_WTP  <- 2
DIGITS_MEMB <- 3        # membership parameters need three digits (age)
STAR_LEVELS <- c("**" = 0.01, "*" = 0.05)

OUT_STEM    <- file.path(OUT_DIR, "Table5_LCL_WTP_sociodem")
MODEL_RDS   <- file.path(OUT_DIR, paste0(MODEL_NAME, "_model.rds"))
LL_REPORTED <- LCL_REPORTED_LL[["sociodem_K5"]]

## -------------------------------------------------------------- ##
## 1  Model                                                        ##
## -------------------------------------------------------------- ##

if (!file.exists(MODEL_RDS))
  stop(MODEL_RDS, " not found. Run 10_LCL_WTP_K_Classes.R first.")
model <- readRDS(MODEL_RDS)
cat("\nModel:", MODEL_RDS, "\n")

lcl_inputs(K, covariates = TRUE, modelName = MODEL_NAME,
           modelDescr = "results of the reported model", outputDirectory = OUT_DIR,
           beta = model$estimate, nCores = 1, noValidation = TRUE)
if (is.null(model$unconditionals))
  model[["unconditionals"]] <- apollo_lcUnconditionals(model, apollo_probabilities,
                                                       apollo_inputs)

## ---- diagnostics ------------------------------------------------ ##

ev   <- if (!is.null(model$hessian) && all(is.finite(model$hessian)))
          eigen(model$hessian, symmetric = TRUE)$values else NULL
nbad <- if (!is.null(model$varcov))
          sum(!is.finite(diag(model$varcov)) | diag(model$varcov) < 0) else NA_integer_

cat("\n=====================================================\n")
cat("DIAGNOSTICS\n")
cat("=====================================================\n")
cat("LL(final)                 :", format(round(model$LLout[1], 4), nsmall = 4), "\n")
cat("LL reported in the paper  :", format(round(LL_REPORTED, 4), nsmall = 4), "\n")
cat("Optimiser message         :",
    if (is.null(model$message)) "n/a" else model$message, "\n")
cat("Optimiser code            :",
    if (is.null(model$code)) NA else model$code,
    "(maxLik: 0-2 means a proper convergence)\n")
cat("Iterations                :", ifelse(is.null(model$nIter), NA, model$nIter), "\n")
if (!is.null(ev)) {
  cat("Hessian negative definite :", all(ev < 0), "\n")
  cat("Largest eigenvalue        :", format(max(ev), digits = 4), "\n")
} else cat("Hessian                   : not evaluable\n")
cat("Negative variances        :", nbad, "\n")
if (abs(model$LLout[1] - LL_REPORTED) > 0.01) {
  cat("\nWARNING: LL(final) differs from the reported LL. The estimation did\n",
      "        not reach the reported solution; check the starting values.\n")
} else cat("Replication check         : reported LL reached\n")

## ---- class shares ----------------------------------------------- ##
# Model-based class shares = mean class allocation probability

class_shares <- sapply(model$unconditionals$pi_values,
                       function(x) mean(as.matrix(x)))
names(class_shares) <- paste("Class", seq_along(class_shares))
cat("\nClass shares (mean allocation probability):\n")
print(round(100 * class_shares, 1))

## ---- observed behaviour per class ------------------------------- ##
# Each respondent is assigned to the class with the highest posterior
# probability. The observed share of status quo choices per class
# defines the reference class and the column order of Table 5.
# 12_LCL_class_profiles.R reads this order from class_order.csv.

lcl_inputs(K, covariates = TRUE, modelName = "class_assignment",
           modelDescr = "posterior membership", outputDirectory = OUT_DIR,
           beta = model$estimate, nCores = 1, noValidation = TRUE)

cond <- as.data.frame(apollo_lcConditionals(model, apollo_probabilities, apollo_inputs))
id_col    <- intersect(names(cond), c("ID", "id", "i_NUMBER"))[1]
prob_cols <- setdiff(names(cond), c("ID", "id", "i_NUMBER"))
pm        <- as.matrix(cond[, prob_cols])
post <- data.frame(i_NUMBER = cond[[id_col]], class = max.col(pm))

# Class separation: mean posterior probability of the assigned class and
# entropy R-squared (reported in the note of Table 5)
ent      <- -sum(pm * log(pmax(pm, .Machine$double.eps)))
R2_ent   <- 1 - ent / (nrow(pm) * log(ncol(pm)))
mean_max <- mean(apply(pm, 1, max))
cat("\nClass separation: mean posterior", round(mean_max, 3),
    "| entropy R-squared", round(R2_ent, 3), "\n")

per_ind <- merge(database[, c("i_NUMBER", "choice")], post, by = "i_NUMBER")
per_ind <- aggregate(cbind(n_sq = per_ind$choice == SQ_ALT, n_tasks = 1) ~
                       i_NUMBER + class, data = per_ind, FUN = sum)
sq_share_obs <- tapply(per_ind$n_sq / per_ind$n_tasks, per_ind$class, mean)
sq_share_obs <- setNames(as.numeric(sq_share_obs)[match(seq_along(CLS),
                          as.integer(names(sq_share_obs)))], CLS)

cat("\nObserved status-quo share by class (%):\n")
print(round(100 * sq_share_obs, 1))

## ---- reference class and column order --------------------------- ##

sq_lean <- sapply(CLS, function(s)
  abs(model$estimate[[paste0("bprice_", s)]]) * model$estimate[[paste0("asc_", s)]])
names(sq_lean) <- CLS
cat("\nStatus-quo leaning from the parameters (|bprice| * asc):\n")
print(round(sq_lean, 3))

if (identical(REF_CLASS, "auto")) {
  REF_CLASS <- names(which.max(sq_share_obs))
  cat("REF_CLASS from observed choices: class", match(REF_CLASS, CLS),
      paste0("(\"", REF_CLASS, "\")"), "\n")
  if (REF_CLASS != names(which.max(sq_lean)))
    cat("NOTE: the parameter-based criterion points at class",
        match(names(which.max(sq_lean)), CLS), "\n")
}
stopifnot(REF_CLASS %in% CLS)

ord <- switch(CLASS_ORDER,
  "sq_share"   = CLS[order(-sq_share_obs)],
  "share"      = c(REF_CLASS, setdiff(CLS[order(-class_shares)], REF_CLASS)),
  "estimation" = CLS,
  stop("Unknown CLASS_ORDER: ", CLASS_ORDER))

order_note <- switch(CLASS_ORDER,
  "sq_share"   = paste("Classes are ordered by the observed share of status-quo",
                       "choices, in descending order."),
  "share"      = paste("The reference class is shown first, the remaining classes",
                       "in descending order of class share."),
  "estimation" = "Classes are shown in the order in which they were estimated.")

write.csv(data.frame(
  display_position = seq_along(ord),
  estimation_letter = ord,
  estimation_index  = match(ord, CLS),
  class_share_pct   = round(100 * class_shares[match(ord, CLS)], 2),
  sq_share_obs_pct  = round(100 * sq_share_obs[ord], 2),
  is_reference      = ord == REF_CLASS,
  stringsAsFactors = FALSE),
  file.path(OUT_DIR, "class_order.csv"), row.names = FALSE)

cat("\nColumn order of Table 5 (display position -> estimated class):\n")
print(data.frame(position = seq_along(ord), estimated = match(ord, CLS),
                 share_pct = round(100 * class_shares[match(ord, CLS)], 1),
                 sq_share_pct = round(100 * sq_share_obs[ord], 1)), row.names = FALSE)

## -------------------------------------------------------------- ##
## 2  Plain-text model output and tidy estimates                  ##
## -------------------------------------------------------------- ##

txt <- capture.output(apollo_modelOutput(model, modelOutput_settings = list(
  printPVal = 2, printClassical = TRUE, printDiagnostics = TRUE,
  printCovar = FALSE, printCorr = FALSE, printOutliers = FALSE,
  printChange = TRUE)))

writeLines(c(
  "######################################################################",
  "# Latent class conditional logit - WTP space - five classes",
  "# All socio-demographics in the class allocation",
  "#",
  "# NORMALISED SPECIFICATION: the class allocation uses a softmax and is",
  "# therefore invariant to adding a constant to all class utilities.",
  "# delta_a and the seven delta_inter_<var>_a are fixed at zero, so",
  "# class a is the reference and all other allocation parameters are",
  "# contrasts against it. This is a reparameterisation, not a",
  "# restriction: log-likelihood, class shares and WTP are unaffected,",
  "# the parameter count falls from 89 to 82.",
  "#",
  "# Starting values    : best solution of the multi-start search (8_LCL_start_values.R)",
  "# Estimation         : bfgs, hessianRoutine = analytic",
  paste0("# Class shares       : ",
         paste0(formatC(100 * class_shares, format = "f", digits = 1), "%",
                collapse = " | ")),
  paste0("# Written            : ", format(Sys.time())),
  "######################################################################",
  "",
  txt),
  file.path(OUT_DIR, paste0(MODEL_NAME, "_output_plain.txt")))

## ---- tidy estimates --------------------------------------------- ##

se_raw <- setNames(rep(NA_real_, length(model$estimate)), names(model$estimate))
if (!is.null(model$robse)) {
  cm <- intersect(names(se_raw), names(model$robse))
  se_raw[cm] <- model$robse[cm]
}
z_raw <- model$estimate / se_raw
write.csv(data.frame(
  parameter = names(model$estimate),
  fixed     = names(model$estimate) %in% FIXED,
  estimate  = round(unname(model$estimate), 6),
  robse     = round(unname(se_raw), 6),
  t         = round(unname(z_raw), 4),
  p         = round(2 * (1 - pnorm(abs(unname(z_raw)))), 6),
  ci_lo     = round(unname(model$estimate - qnorm(0.975) * se_raw), 6),
  ci_hi     = round(unname(model$estimate + qnorm(0.975) * se_raw), 6),
  stringsAsFactors = FALSE),
  file.path(OUT_DIR, "estimates_tidy.csv"), row.names = FALSE, na = "")

## -------------------------------------------------------------- ##
## 3  Class allocation: reference class and joint Wald tests       ##
## -------------------------------------------------------------- ##
# Changing the reference class is a linear reparameterisation. Point
# estimates and standard errors follow exactly from the robust
# covariance matrix.

V <- NULL
for (nm in c("robvarcov", "robVarcov", "robcovar", "varcov")) {
  if (!is.null(model[[nm]])) { V <- model[[nm]]
    cat("\nCovariance matrix used:", nm, "\n"); break }
}
if (is.null(V)) stop("No covariance matrix in the model object.")

est <- model$estimate
se  <- se_raw

re_reference <- function(base, ref) {
  d <- setNames(rep(0, length(CLS)), CLS)
  for (s in CLS) {
    nm <- paste0(base, "_", s)
    if (nm %in% names(est)) d[[s]] <- est[[nm]]
  }
  ref_nm <- paste0(base, "_", ref)
  in_V   <- rownames(V)
  e <- s2 <- setNames(rep(NA_real_, length(CLS)), CLS)
  for (s in CLS) {
    e[[s]] <- d[[s]] - d[[ref]]
    if (s == ref) { s2[[s]] <- NA_real_; next }
    s_nm <- paste0(base, "_", s)
    vs <- if (s_nm   %in% in_V) V[s_nm,   s_nm]   else 0
    vr <- if (ref_nm %in% in_V) V[ref_nm, ref_nm] else 0
    cv <- if (s_nm %in% in_V && ref_nm %in% in_V) V[s_nm, ref_nm] else 0
    s2[[s]] <- vs + vr - 2 * cv
  }
  list(est = e, se = sqrt(pmax(s2, 0)))
}

for (b in alloc_bases) {
  rr <- re_reference(b, REF_CLASS)
  for (s in CLS) {
    nm <- paste0(b, "_", s)
    est[nm] <- rr$est[[s]]
    se[nm]  <- rr$se[[s]]
  }
}

p     <- 2 * (1 - pnorm(abs(est / se)))
zc    <- qnorm(1 - (1 - CONF_LEVEL) / 2)
ci_lo <- est - zc * se
ci_hi <- est + zc * se

# Joint Wald test per covariate. H0: the covariate has the same effect
# on membership in every class, i.e. all four free contrasts are zero.
# Independent of the choice of the reference class.
joint <- do.call(rbind, lapply(inter_vars, function(v) {
  pars <- paste0("delta_", v, "_", setdiff(CLS, "a"))
  pars <- pars[pars %in% rownames(V)]
  if (!length(pars)) return(NULL)
  b0 <- model$estimate[pars]
  W  <- tryCatch(as.numeric(t(b0) %*% solve(V[pars, pars, drop = FALSE]) %*% b0),
                 error = function(e) NA_real_)
  data.frame(variable = v, df = length(pars), wald_chi2 = round(W, 2),
             p_value = round(1 - pchisq(W, length(pars)), 6),
             stringsAsFactors = FALSE)
}))
joint$signif <- ifelse(joint$p_value < 0.01, "**",
                ifelse(joint$p_value < 0.05, "*", ""))

cat("\n=== Joint Wald tests on class membership (chi2, df = 4) ===\n")
print(joint, row.names = FALSE)
write.csv(joint, file.path(OUT_DIR, "joint_tests_membership.csv"), row.names = FALSE)

## -------------------------------------------------------------- ##
## 3b Confirmatory hypothesis tests (Table 6) and multiplicity     ##
## -------------------------------------------------------------- ##
# One joint Wald test per preregistered hypothesis.
# H1-H3 : class-specific WTP parameters, H0 = zero in every class.
# H4-H11: membership parameters of the covariate, H0 = all free
#         contrasts zero (as in joint_tests_membership.csv).
# The tests are non-directional; the direction is read from Table 5.
# H9 and H11 use the same indicator (tenant/MFH) and are the same test.
# Holm and Benjamini-Hochberg adjustment over the family of tests:
#   "hypotheses" = 11 rows, the shared test counted twice
#   "tests"      = 10 distinct tests, H9/H11 counted once (used in the paper)
HYP_FAMILY <- "tests"

wald_joint <- function(pars, b = model$estimate, S = V) {
  miss <- setdiff(pars, rownames(S))
  if (length(miss)) stop("Parameters missing from covariance matrix: ",
                         paste(miss, collapse = ", "))
  bb <- b[pars]
  W  <- as.numeric(t(bb) %*% solve(S[pars, pars, drop = FALSE]) %*% bb)
  c(wald_chi2 = W, df = length(pars), p_value = 1 - pchisq(W, length(pars)))
}
memb <- function(v) paste0("delta_inter_", v, "_", setdiff(CLS, "a"))
taste <- function(base) paste0(base, "_", CLS)

hyp_def <- list(
  H1  = list(test = "member",        pars = taste("bpartimem")),
  H2  = list(test = "goals",         pars = c(taste("bgoalsoc"), taste("bgoaleco"),
                                              taste("bgoalboth"))),
  H3  = list(test = "split supply",  pars = taste("bconsplit")),
  H4  = list(test = "env_awareness", pars = memb("env_awareness_score")),
  H5  = list(test = "female",        pars = memb("sex")),
  H6  = list(test = "income (low+high)", pars = c(memb("lowincome"), memb("highincome"))),
  H7  = list(test = "age",           pars = memb("age")),
  H8  = list(test = "education",     pars = memb("educ_years")),
  H9  = list(test = "tenant/MFH",    pars = memb("mfh_or_tenant")),
  H10 = list(test = "low income",    pars = memb("lowincome")),
  H11 = list(test = "tenant/MFH",    pars = memb("mfh_or_tenant"))
)

hyp <- do.call(rbind, lapply(names(hyp_def), function(h) {
  r <- wald_joint(hyp_def[[h]]$pars)
  data.frame(hypothesis = h, test = hyp_def[[h]]$test, df = r[["df"]],
             wald_chi2 = r[["wald_chi2"]], p_value = r[["p_value"]],
             stringsAsFactors = FALSE)
}))

fam <- if (HYP_FAMILY == "tests") hyp$hypothesis != "H11" else rep(TRUE, nrow(hyp))
hyp$p_holm <- hyp$p_bh <- NA_real_
hyp$p_holm[fam] <- p.adjust(hyp$p_value[fam], method = "holm")
hyp$p_bh[fam]   <- p.adjust(hyp$p_value[fam], method = "BH")
if (HYP_FAMILY == "tests") {                       # H11 = H9, same test
  hyp[hyp$hypothesis == "H11", c("p_holm", "p_bh")] <-
    hyp[hyp$hypothesis == "H9",  c("p_holm", "p_bh")]
}
hyp$family_size <- sum(fam)
hyp$reject_5pct_unadj <- hyp$p_value < 0.05
hyp$reject_5pct_holm  <- hyp$p_holm  < 0.05
hyp$reject_5pct_bh    <- hyp$p_bh    < 0.05

cat("\n=== Confirmatory hypothesis tests (joint Wald, robust covariance) ===\n")
cat("Family:", HYP_FAMILY, "- size", sum(fam), "\n")
print(transform(hyp, wald_chi2 = round(wald_chi2, 2), p_value = signif(p_value, 3),
                p_holm = signif(p_holm, 3), p_bh = signif(p_bh, 3)), row.names = FALSE)
write.csv(hyp, file.path(OUT_DIR, "hypothesis_tests.csv"), row.names = FALSE)

## -------------------------------------------------------------- ##
## 3c Sum of WTP for energy sharing attributes per class (Discussion)
## -------------------------------------------------------------- ##
# Per class and attribute (role, organiser, goal): the level with the
# highest WTP among the levels that are positive and significant at 5%
# (robust SE). Levels of one attribute exclude each other, so at most
# one level per attribute enters the sum. Split supply and the ASC are
# not included. Confidence interval of the sum by the delta method; it
# is conditional on the selection of significant levels.
# EUR_PER_PP: one percentage point of the monthly premium as an annual
# amount (four-person household in a multi-family building, co2online
# 2025; 39 ct/kWh, Eurostat 2026).
PKG_ATTR <- list(role      = c("bpartiinv", "bpartimem"),
                 organiser = c("borgcit", "borgmun"),
                 goal      = c("bgoalsoc", "bgoaleco", "bgoalboth"))
EUR_PER_PP <- 11.33
se_rob <- sqrt(diag(V))

pkg <- do.call(rbind, lapply(CLS, function(s) {
  chosen <- unlist(lapply(names(PKG_ATTR), function(a) {
    nm  <- paste0(PKG_ATTR[[a]], "_", s)
    b   <- model$estimate[nm]
    p   <- 2 * (1 - pnorm(abs(b / se_rob[nm])))
    ok  <- nm[b > 0 & p < 0.05]
    if (length(ok)) ok[which.max(model$estimate[ok])] else character(0)
  }))
  if (!length(chosen))
    return(data.frame(class_est = s, levels = "", wtp_pp = 0, ci_lo = NA, ci_hi = NA,
                      eur_per_year = 0, stringsAsFactors = FALSE))
  m  <- sum(model$estimate[chosen])
  se <- sqrt(sum(V[chosen, chosen, drop = FALSE]))
  data.frame(class_est = s, levels = paste(chosen, collapse = " + "), wtp_pp = m,
             ci_lo = m - 1.96 * se, ci_hi = m + 1.96 * se,
             eur_per_year = m * EUR_PER_PP, stringsAsFactors = FALSE)
}))
cat("\n=== Sum of significant positive attribute WTP per class (pp of monthly cost) ===\n")
print(transform(pkg, wtp_pp = round(wtp_pp, 2), ci_lo = round(ci_lo, 2),
                ci_hi = round(ci_hi, 2), eur_per_year = round(eur_per_year)), row.names = FALSE)
write.csv(pkg, file.path(OUT_DIR, "package_wtp.csv"), row.names = FALSE)

## -------------------------------------------------------------- ##
## 4  Table 5                                                      ##
## -------------------------------------------------------------- ##

pretty_labels <- c(
  "asc"        = "asc: Keeping electricity provider",
  "bpartiinv"  = "Investor",
  "bpartimem"  = "Member",
  "borgcit"    = "Organiser: Citizens",
  "borgmun"    = "Organiser: Municipality",
  "bgoalsoc"   = "Social goal",
  "bgoaleco"   = "Ecological goal",
  "bgoalboth"  = "Social and ecological goal",
  "bconsplit"  = "Split supply",
  "bprice"     = "Price",
  "delta"      = "Constant",
  "delta_inter_env_awareness_score" = "Environmental awareness",
  "delta_inter_sex"           = "Female",
  "delta_inter_age"           = "Age",
  "delta_inter_educ_years"    = "Education",
  "delta_inter_lowincome"     = "Low income",
  "delta_inter_highincome"    = "High income",
  "delta_inter_mfh_or_tenant" = "Tenant/MFH")

var_order <- c("asc", "bpartiinv", "bpartimem", "borgcit", "borgmun",
               "bgoalsoc", "bgoaleco", "bgoalboth", "bconsplit", "bprice",
               alloc_bases)

p_star <- function(pv) {
  if (is.na(pv)) return("")
  lv <- sort(STAR_LEVELS)
  for (i in seq_along(lv)) if (pv < lv[i]) return(names(lv)[i])
  ""
}
fmt <- function(x, d) formatC(x, format = "f", digits = d)
digits_for <- function(base) if (base %in% alloc_bases) DIGITS_MEMB else DIGITS_WTP

label_column <- as.vector(rbind(unname(pretty_labels[var_order]),
                                rep("", length(var_order))))

class_columns <- lapply(ord, function(s) {
  cells <- character(0)
  for (base in var_order) {
    nm <- paste0(base, "_", s)
    d  <- digits_for(base)
    if (!(nm %in% names(est))) { cells <- c(cells, "", ""); next }
    if (base %in% alloc_bases && s == REF_CLASS) {
      cells <- c(cells, "0", "(ref.)"); next
    }
    cells <- c(cells,
               paste0(fmt(est[[nm]], d), p_star(p[[nm]])),
               if (is.na(se[[nm]])) "(fixed)" else
                 paste0("[", fmt(ci_lo[[nm]], d), "; ", fmt(ci_hi[[nm]], d), "]"))
  }
  cells
})
names(class_columns) <- ord

result_df <- data.frame(Parameter = label_column, class_columns,
                        stringsAsFactors = FALSE)
colnames(result_df) <- c("Parameter", paste("Class", seq_along(CLS)))

stats_labels <- c("No. Observations", "No. Respondents", "LL(0)", "LL(final)",
                  "Adj. Rho-squared", "AIC", "BIC")
stats_values <- c(model$nObs, length(unique(database$i_NUMBER)),
                  model$LL0[1], model$LLout[1], model$adjRho2_0,
                  model$AIC, model$BIC)
stats_df <- data.frame(Parameter = stats_labels,
                       matrix("", nrow = length(stats_labels), ncol = length(CLS)),
                       stringsAsFactors = FALSE)
colnames(stats_df) <- colnames(result_df)
stats_df[, 2] <- format(round(stats_values, 2), scientific = FALSE, trim = TRUE)

result_df_final <- rbind(result_df, stats_df)

# Shares in display order
class_shares_pct <- paste0("(", formatC(100 * class_shares[match(ord, CLS)],
                                        format = "f", digits = 1), "%)")

# Observed share of status quo choices per class (defines the column order)
sq_row <- paste0("SQ ", formatC(100 * sq_share_obs[ord], format = "f",
                                digits = 1), "%")

row_membership <- (which(var_order == "delta") - 1) * 2 + 1
row_stats      <- length(var_order) * 2 + 1

star_note <- paste(rev(paste0(names(sort(STAR_LEVELS)), " p < ", sort(STAR_LEVELS))),
                   collapse = ", ")
ci_note   <- paste0(formatC(100 * CONF_LEVEL, format = "f", digits = 0),
                    "% confidence intervals in brackets; robust standard errors.")
ref_note  <- paste0("Class-allocation parameters are contrasts against Class ",
                    match(REF_CLASS, ord),
                    "; positive values indicate a higher probability of belonging",
                    " to that class than to the reference class.")
joint_note <- paste0("Joint Wald tests on class membership, chi2(4): ",
  paste(paste0(unname(pretty_labels[paste0("delta_", joint$variable)]), " ",
               formatC(joint$wald_chi2, format = "f", digits = 1), joint$signif),
        collapse = "; "), ".")

my_header <- data.frame(
  col_keys = colnames(result_df),
  line1 = "Latent class conditional logit - WTP space - including socio-demographic parameters",
  line2 = c("Attribute", paste("Class", seq_along(CLS))),
  line3 = c("", class_shares_pct),
  line4 = c("", sq_row),
  stringsAsFactors = FALSE)

my_footer <- data.frame(
  col_keys = colnames(result_df),
  line1 = star_note,
  line2 = ci_note,
  line3 = ref_note,
  line4 = joint_note,
  line5 = paste(order_note,
    "Class shares in brackets are model-based (mean allocation probability).",
    "SQ is the mean share of status-quo choices among the respondents assigned",
    "to a class by their highest posterior probability; the mean posterior",
    "probability of the assigned class is",
    formatC(mean_max, format = "f", digits = 3),
    "and the entropy R-squared is",
    paste0(formatC(R2_ent, format = "f", digits = 3), ".")),
  line6 = "Reference contract: Only customer at a municipal utility with full supply",
  stringsAsFactors = FALSE)

flex_ci <- flextable(result_df_final) |>
  theme_booktabs() |>
  set_header_df(mapping = my_header, key = "col_keys") |>
  set_footer_df(mapping = my_footer, key = "col_keys") |>
  border(i = 4, border.bottom = fp_border(color = "black", width = 1), part = "header") |>
  fontsize(size = 12, i = 1, part = "header") |>
  fontsize(size = 9, part = "footer") |>
  align(align = "center", part = "header") |>
  align(align = "center", j = 2:6, part = "body") |>
  align(align = "left", part = "footer") |>
  border(border.top = fp_border(color = "black", width = 1), part = "footer") |>
  border(i = row_membership, border.top = fp_border(width = 1)) |>
  border(i = row_stats,      border.top = fp_border(width = 1)) |>
  bold(j = 1, part = "body") |>
  bold(i = 1:2, part = "header") |>
  font(fontname = "Calibri", part = "all") |>
  fontsize(size = 9, part = "body") |>
  merge_h(part = "footer") |>
  merge_at(i = 1, part = "header") |>
  width(j = 1, width = 2.0) |>
  width(j = 2:6, width = 1.0)

print(result_df_final, right = FALSE)

write.csv(result_df_final, paste0(OUT_STEM, ".csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

check_no_lockfiles(paste0(OUT_STEM, ".docx"))
tryCatch(save_as_docx(flex_ci, path = paste0(OUT_STEM, ".docx")),
         error = function(e)
           cat("\nCould not write the .docx:", conditionMessage(e),
               "\nClose the file in Word and re-run the last block.\n"))

cat("\n-----------------------------------------------------\n")
cat("Reference class :", match(REF_CLASS, ord), "in the table (estimated as class",
    paste0(match(REF_CLASS, CLS), ")"), "\n")
cat("Column order    :", paste(match(ord, CLS), collapse = " "),
    " (display position -> estimated class)\n")
cat("Written to      :", OUT_DIR, "\n")
cat("   Table5_LCL_WTP_sociodem.csv / .docx\n")
cat("   joint_tests_membership.csv, hypothesis_tests.csv, package_wtp.csv, estimates_tidy.csv\n")
