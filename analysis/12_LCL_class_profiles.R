######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Profiles of the five latent classes of the        ###
###               reported model                                    ###
###               - posterior class assignment and class separation ###
###               - observed choice behaviour per class             ###
###               - stated reasons for keeping the status quo       ###
###               - socio-demographic composition per class         ###
###               Nothing is re-estimated. The classes are          ###
###               described by observed choices, not by estimated   ###
###               parameters.                                       ###
### Requires    : df_long (1_Data_Procession.R),                    ###
###               8_LCL_model_definition.R, model from              ###
###               10_LCL_WTP_K_Classes.R, class_order.csv from      ###
###               11_LCL_Table5_tests.R                             ###
### Output      : Tables A.4 to A.6 (TableA_class_assignment.docx,  ###
###               TableA_class_assignment.csv, TableA_avepp_matrix  ###
###               .csv, TableA_class_composition.csv),              ###
###               class_separation.csv, class_profiles.csv,         ###
###               class_sq_reasons.csv,                             ###
###               posterior_class_assignment.csv                    ###
### Date        : 24.09.2026                                        ###
######################################################################

library(apollo)
library(dplyr)
library(flextable)
library(officer)

apollo_initialise()
source("8_LCL_model_definition.R")

## -------------------------------------------------------------- ##
## Settings                                                        ##
## -------------------------------------------------------------- ##

K       <- 5
OUT_DIR <- file.path("Hauptstudie", "Estimation_results", "LCLogit",
                     "WTP_Space", "reported")
MODEL_RDS <- file.path(OUT_DIR, "lc5cl_WTP_sociodem_model.rds")

SQ_MIN_TASKS <- 7   # the question on reasons was only shown to respondents
                    # with at least this many status quo choices

if (!file.exists(MODEL_RDS)) stop(MODEL_RDS, " not found. Run 10_LCL_WTP_K_Classes.R first.")
cat("\nModel:", MODEL_RDS, "\n")

## -------------------------------------------------------------- ##
## Posterior class probabilities                                   ##
## -------------------------------------------------------------- ##

model <- readRDS(MODEL_RDS)
cat("LL(final):", round(model$LLout[1], 4), "\n")

lcl_inputs(K, covariates = TRUE,
           modelName  = "class_profiles",
           modelDescr = "posterior class membership",
           outputDirectory = OUT_DIR,
           beta   = model$estimate,
           nCores = 1,
           noValidation = TRUE)
assign("apollo_fixed", model$apollo_fixed, envir = globalenv())

cond      <- as.data.frame(apollo_lcConditionals(model, apollo_probabilities, apollo_inputs))
id_col    <- intersect(names(cond), c("ID", "id", "i_NUMBER"))[1]
prob_cols <- setdiff(names(cond), c("ID", "id", "i_NUMBER"))
pm        <- as.matrix(cond[, prob_cols])

post <- data.frame(i_NUMBER = cond[[id_col]],
                   class    = max.col(pm),
                   max_prob = apply(pm, 1, max))

# Entropy R-squared: close to 1 means well separated classes
ent      <- -sum(pm * log(pmax(pm, .Machine$double.eps)))
R2_ent   <- 1 - ent / (nrow(pm) * log(ncol(pm)))
mean_max <- mean(post$max_prob)
avg_by_class <- tapply(post$max_prob, post$class, mean)

cat("\n=== Class separation ===\n")
cat("Average posterior probability of the assigned class:", round(mean_max, 3), "\n")
cat("Entropy R-squared:", round(R2_ent, 3), "\n")
cat("Average posterior probability by assigned class:\n")
print(round(avg_by_class, 3))

write.csv(data.frame(statistic = c("mean_max_posterior", "entropy_R2"),
                     value = round(c(mean_max, R2_ent), 4)),
          file.path(OUT_DIR, "class_separation.csv"), row.names = FALSE)
# Model-based shares (mean allocation probability) next to the shares
# of the modal assignment; they differ when classes overlap
if (!is.null(model$unconditionals$pi_values)) {
  model_shares <- sapply(model$unconditionals$pi_values, function(x) mean(as.matrix(x)))
} else {
  model_shares <- sapply(apollo_lcUnconditionals(model, apollo_probabilities,
                                                 apollo_inputs)$pi_values,
                         function(x) mean(as.matrix(x)))
}
modal_shares <- as.numeric(table(factor(post$class, levels = seq_along(CLS)))) / nrow(post)
cat("\nClass shares, model-based vs. modal assignment (%):\n")
print(data.frame(class = seq_along(CLS),
                 model_based = round(100 * model_shares, 1),
                 modal       = round(100 * modal_shares, 1)))

## -------------------------------------------------------------- ##
## Display order                                                   ##
## -------------------------------------------------------------- ##
# Class order of Table 5, written by 11_LCL_Table5_tests.R.
# Without that file the estimation order is used.

ORDER_FILE <- file.path(OUT_DIR, "class_order.csv")
disp <- NULL
if (file.exists(ORDER_FILE)) {
  co   <- read.csv(ORDER_FILE, stringsAsFactors = FALSE)
  disp <- setNames(co$display_position, as.character(co$estimation_index))
  cat("\nDisplay order from", basename(ORDER_FILE), ":",
      paste(co$estimation_index, "->", co$display_position, collapse = ", "), "\n")
} else {
  cat("\nNOTE: class_order.csv not found. Estimation order is used here;",
      "run 11_LCL_Table5_tests.R first to match Table 5.\n")
}
to_display <- function(x) if (is.null(disp)) x else unname(disp[as.character(x)])

post$class_display <- to_display(post$class)
write.csv(post, file.path(OUT_DIR, "posterior_class_assignment.csv"), row.names = FALSE)

## -------------------------------------------------------------- ##
## Observed behaviour per class                                    ##
## -------------------------------------------------------------- ##

per_obs <- database %>% select(i_NUMBER, choice) %>% inner_join(post, by = "i_NUMBER")

per_ind <- per_obs %>%
  group_by(i_NUMBER, class) %>%
  summarise(n_sq = sum(choice == SQ_ALT), n_tasks = n(), .groups = "drop") %>%
  mutate(share_sq = n_sq / n_tasks)

profiles <- per_ind %>%
  group_by(class) %>%
  summarise(n_respondents       = n(),
            share_of_sample_pct = round(100 * n() / nrow(per_ind), 1),
            mean_sq_share_pct   = round(100 * mean(share_sq), 1),
            median_sq_choices   = median(n_sq),
            pct_always_sq       = round(100 * mean(n_sq == n_tasks), 1),
            pct_sq_at_least_7   = round(100 * mean(n_sq >= SQ_MIN_TASKS), 1),
            pct_never_sq        = round(100 * mean(n_sq == 0), 1),
            .groups = "drop") %>%
  left_join(data.frame(class = seq_along(CLS),
                       mean_posterior = round(as.numeric(avg_by_class), 3)),
            by = "class")

profiles$class_display <- to_display(profiles$class)
profiles <- profiles[order(profiles$class_display), ]
profiles <- profiles[, c("class_display", setdiff(names(profiles), "class_display"))]

cat("\n=== Observed status-quo behaviour, in the order of Table 5 ===\n")
cat("    class_display = column in Table 5, class = class as estimated\n")
print(as.data.frame(profiles), row.names = FALSE)
write.csv(profiles, file.path(OUT_DIR, "class_profiles.csv"), row.names = FALSE)

## -------------------------------------------------------------- ##
## Which class is the status-quo group?                            ##
## -------------------------------------------------------------- ##
# Class with the highest observed share of status quo choices, compared
# with the class with the largest |bprice| * asc (parameter-based)

sq_class_obs <- profiles$class[which.max(profiles$mean_sq_share_pct)]
sq_lean <- sapply(CLS, function(s)
  abs(model$estimate[[paste0("bprice_", s)]]) * model$estimate[[paste0("asc_", s)]])
sq_class_par <- which.max(sq_lean)

cat("\n=== Status-quo class ===\n")
cat("From observed choices      : Class", sq_class_obs,
    paste0("(", profiles$mean_sq_share_pct[profiles$class == sq_class_obs],
           "% status-quo choices, ",
           profiles$share_of_sample_pct[profiles$class == sq_class_obs],
           "% of the sample)"), "\n")
cat("From the parameters        : Class", sq_class_par, "\n")
if (sq_class_obs != sq_class_par) cat("NOTE: the two criteria disagree.\n")

## -------------------------------------------------------------- ##
## Stated reasons for choosing the status quo                      ##
## -------------------------------------------------------------- ##
# The question was only shown to respondents with at least seven status
# quo choices. Percentages are conditional on having been asked.

grund_vars <- paste0("F7.2A", 1:12)
grund_labels <- c(
  "Did not understand the options",
  "Switching effort too high",
  "Satisfied with current contract",
  "Afraid of additional effort",
  "Wait until energy communities are established",
  "Want to be able to switch at any time",
  "Prefer an established supplier",
  "Do not want to deal with electricity matters",
  "Do not want to pay more than currently",
  "EUR 100 for a share or bond too much",
  "Did not want to commit for one year",
  "Too uncertain whether supply would be reliable")

if (all(grund_vars %in% names(df_long))) {
  asked <- per_ind %>% filter(n_sq >= SQ_MIN_TASKS) %>% select(i_NUMBER, class)

  cat("\nRespondents shown the reason battery (>=", SQ_MIN_TASKS,
      "status-quo choices):\n")
  print(table(asked$class))

  reasons <- df_long %>%
    select(i_NUMBER, all_of(grund_vars)) %>%
    distinct(i_NUMBER, .keep_all = TRUE) %>%
    inner_join(asked, by = "i_NUMBER") %>%
    group_by(class) %>%
    summarise(n_asked = n(),
              across(all_of(grund_vars),
                     ~ round(100 * mean(.x == 1, na.rm = TRUE), 1)),
              .groups = "drop")

  # Columns in the class order of Table 5
  reasons <- reasons[order(to_display(reasons$class)), ]
  tab <- as.data.frame(t(reasons[, grund_vars]))
  colnames(tab) <- paste0("Class_", to_display(reasons$class),
                          "_n", reasons$n_asked)
  tab <- cbind(Reason = grund_labels, tab)

  cat("\n=== Stated reasons, % of those ASKED (>=", SQ_MIN_TASKS,
      "status-quo choices) ===\n")
  print(tab, row.names = FALSE)
  write.csv(tab, file.path(OUT_DIR, "class_sq_reasons.csv"), row.names = FALSE)
} else {
  cat("\nNOTE: F7.2A1-12 not found in df_long, reasons table skipped.\n")
}

## -------------------------------------------------------------- ##
## Appendix: class assignment and separation                       ##
## -------------------------------------------------------------- ##
# Tables A.4 to A.6 of the supplementary material, in the class order
# of Table 5

ord_idx <- if (is.null(disp)) seq_along(prob_cols) else
  co$estimation_index[order(co$display_position)]
K       <- length(ord_idx)
cls_lab <- paste("Class", seq_len(K))
pm_ord  <- pm[, ord_idx, drop = FALSE]

fmt1 <- function(x) formatC(x, format = "f", digits = 1)
fmt3 <- function(x) formatC(x, format = "f", digits = 3)

## ---- A) sizes, separation and observed behaviour ---------------- ##

model_shares_ord <- 100 * model_shares[ord_idx]
n_ord   <- profiles$n_respondents[match(seq_len(K), profiles$class_display)]
modal_p <- 100 * n_ord / sum(n_ord)
# (not called "get", which would mask base::get)
pcol <- function(col) profiles[[col]][match(seq_len(K), profiles$class_display)]

tabA <- data.frame(
  Statistic = c("Class share, model-based (%)",
                "Class share, modal assignment (%)",
                "Respondents assigned (n)",
                "Mean posterior probability of the assigned class",
                "Status-quo choices, mean share (%)",
                "Median number of status-quo choices (out of 10)",
                "Respondents choosing the status quo in all 10 tasks (%)",
                "Respondents never choosing the status quo (%)"),
  matrix(c(fmt1(model_shares_ord), fmt1(modal_p), as.character(n_ord),
           fmt3(pcol("mean_posterior")), fmt1(pcol("mean_sq_share_pct")),
           as.character(pcol("median_sq_choices")),
           fmt1(pcol("pct_always_sq")), fmt1(pcol("pct_never_sq"))),
         nrow = 8, byrow = TRUE),
  stringsAsFactors = FALSE)
colnames(tabA) <- c("Statistic", cls_lab)
write.csv(tabA, file.path(OUT_DIR, "TableA_class_assignment.csv"), row.names = FALSE)

mx    <- apply(pm, 1, max)
bands <- c(mean(mx > 0.9), mean(mx > 0.7 & mx <= 0.9),
           mean(mx > 0.5 & mx <= 0.7), mean(mx <= 0.5))
noteA <- paste0(
  "Class shares are model-based (mean allocation probability); the modal ",
  "assignment allocates each respondent to the class with the highest ",
  "posterior probability. Entropy R-squared ", fmt3(R2_ent),
  "; mean posterior probability of the assigned class ", fmt3(mean_max),
  ". Of all ", nrow(pm), " respondents, ", fmt1(100 * bands[1]),
  "% are assigned with a posterior probability above 0.9, ",
  fmt1(100 * bands[2]), "% between 0.7 and 0.9, ",
  fmt1(100 * bands[3]), "% between 0.5 and 0.7 and ",
  fmt1(100 * bands[4]), "% at or below 0.5.")

## ---- B) average posterior probability matrix -------------------- ##
# Rows: respondents assigned to that class. Columns: their mean
# probability of each class. High diagonal and low off-diagonal values
# indicate well separated classes.

avepp <- t(sapply(ord_idx, function(k)
  colMeans(pm_ord[post$class == k, , drop = FALSE])))
tabB <- data.frame(`Assigned to` = paste("Class", seq_len(K)),
                   matrix(fmt3(avepp), nrow = K), stringsAsFactors = FALSE,
                   check.names = FALSE)
colnames(tabB) <- c("Assigned to", cls_lab)
write.csv(tabB, file.path(OUT_DIR, "TableA_avepp_matrix.csv"), row.names = FALSE)

## ---- C) socio-demographic composition --------------------------- ##
# Means of the variables of the class allocation function by assigned
# class (raw variables; the inter_* versions in the model are
# mean-centred). Descriptive only: the effects on class membership are
# the allocation parameters in Table 5 and the joint Wald tests.

soc_defs <- list(
  "Age (years)"                        = quote(2025 - S2A1),
  "Years of education"                 = quote(educ_years),
  "Female (%)"                         = quote(100 * sex),
  "Environmental awareness score"      = quote(env_awareness_score),
  "Low income, below EUR 3,000 (%)"    = quote(100 * lowincome),
  "High income, above EUR 5,000 (%)"   = quote(100 * highincome),
  "Tenant or multi-family house (%)"   = quote(100 * mfh_or_tenant))

ind <- df_long[!duplicated(df_long$i_NUMBER), ]
ind <- merge(ind, post[, c("i_NUMBER", "class")], by = "i_NUMBER")

rows <- list(); kept <- character(0)
for (nm in names(soc_defs)) {
  v <- tryCatch(eval(soc_defs[[nm]], ind), error = function(e) NULL)
  if (is.null(v) || all(is.na(v))) {
    cat("NOTE: variable for '", nm, "' not found in df_long - row skipped.\n", sep = "")
    next
  }
  per_class <- sapply(ord_idx, function(k) mean(v[ind$class == k], na.rm = TRUE))
  rows[[length(rows) + 1]] <- c(fmt1(per_class), fmt1(mean(v, na.rm = TRUE)))
  kept <- c(kept, nm)
}
tabC <- data.frame(Characteristic = kept,
                   matrix(unlist(rows), nrow = length(kept), byrow = TRUE),
                   stringsAsFactors = FALSE)
colnames(tabC) <- c("Characteristic", cls_lab, "All respondents")
write.csv(tabC, file.path(OUT_DIR, "TableA_class_composition.csv"), row.names = FALSE)

## ---- Word output ------------------------------------------------ ##

mk <- function(df, title, note, first_width = 2.6) {
  nc <- ncol(df)
  flextable(df) |>
    theme_booktabs() |>
    add_header_lines(values = title) |>
    add_footer_lines(values = note) |>
    align(align = "center", j = 2:nc, part = "body") |>
    align(align = "center", part = "header") |>
    align(align = "left", j = 1, part = "body") |>
    align(align = "left", part = "footer") |>
    bold(i = 1, part = "header") |>
    font(fontname = "Calibri", part = "all") |>
    fontsize(size = 9, part = "body") |>
    fontsize(size = 8, part = "footer") |>
    width(j = 1, width = first_width) |>
    width(j = 2:nc, width = 0.85)
}

fA <- mk(tabA, "Table A: Class assignment and separation", noteA, 3.0)
fB <- mk(tabB, "Table B: Average posterior probability by assigned class",
         paste("Each row gives, for the respondents assigned to that class, their",
               "mean posterior probability of belonging to each class. Values on",
               "the diagonal close to one and small off-diagonal values indicate",
               "well separated classes."), 1.6)
fC <- mk(tabC, "Table C: Socio-demographic composition by assigned class",
         paste("Means of the raw variables that enter the class-allocation model,",
               "by modal class assignment. Descriptive only: the effect of a",
               "characteristic on class membership is given by the allocation",
               "parameters in Table 5 and the joint Wald tests, not by these means."),
         3.0)

docx_path <- file.path(OUT_DIR, "TableA_class_assignment.docx")
check_no_lockfiles(docx_path)
tryCatch(save_as_docx(fA, fB, fC, path = docx_path),
         error = function(e)
           cat("\nCould not write", docx_path, ":", conditionMessage(e),
               "\nClose the file in Word and re-run.\n"))

cat("\n=== Appendix tables ===\n")
print(tabA, row.names = FALSE); cat("\n")
print(tabB, row.names = FALSE); cat("\n")
print(tabC, row.names = FALSE); cat("\n")
cat(strwrap(noteA, 78), sep = "\n")

cat("\nWritten to:", OUT_DIR, "\n")
