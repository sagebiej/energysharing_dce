######################################################################
### Study       : DCE Energy Sharing                               ###
### Description : Table A.8 - conditional logit models in WTP space ###
###               with all socio-demographics, for the new target   ###
###               group, and without socio-demographics.            ###
###               Collects the estimated models and writes the      ###
###               table. Nothing is estimated.                      ###
### Requires    : models from 5_CL_WTP_all_sociodem.R,              ###
###               5_CL_WTP_New_Target_Group_all_sociodem.R and      ###
###               5_CL_WTP.R                                        ###
### Output      : .../CL/WTP_Space/TableA8_conditional_logit.csv    ###
###               .../CL/WTP_Space/TableA8_conditional_logit.docx   ###
######################################################################

library(flextable)
library(officer)
library(magrittr)

OUT_DIR <- "Hauptstudie/Estimation_results/CL/WTP_Space"

# Columns of the table and the apollo modelName of each model
MODELS <- c("Including all socio-demographics" = "CL_WTP_allcociodem",
            "New target group"                 = "CL_WTP_New_Target_Group_allsociodem",
            "No socio-demographics"            = "CL_WTP")

# Rows of the table, in the order of the paper
LABELS <- c(asc                           = "ASC: keeping electricity provider",
            bpartiinv                     = "Investor",
            bpartimem                     = "Member",
            borgcit                       = "Organizer: citizens",
            borgmun                       = "Organizer: municipality",
            bgoalsoc                      = "Social goal",
            bgoaleco                      = "Ecological goal",
            bgoalboth                     = "Soc. & eco. goal",
            bconsplit                     = "Split supply",
            bprice                        = "Price",
            asc_env_awareness_score       = "ASC × env. awareness",
            bpartimem_env_awareness_score = "Member × env. awareness",
            asc_sex                       = "ASC × female",
            bpartimem_sex                 = "Member × female",
            asc_age                       = "ASC × age",
            bpartimem_age                 = "Member × age",
            asc_educ_years                = "ASC × education",
            bpartimem_educ_years          = "Member × education",
            asc_lowincome                 = "ASC × low income",
            bpartimem_lowincome           = "Member × low income",
            asc_highincome                = "ASC × high income",
            bpartimem_highincome          = "Member × high income",
            asc_mfh_or_tenant             = "ASC × tenant/MFH resident",
            bpartimem_mfh_or_tenant       = "Member × tenant/MFH resident")

# Load a saved model; apollo names the file <modelName>_model.rds
load_model <- function(name) {
  f <- file.path(OUT_DIR, paste0(name, "_model.rds"))
  if (!file.exists(f)) stop("Model not found: ", f, ". Run the 5_CL_WTP scripts first.")
  readRDS(f)
}
models <- lapply(MODELS, load_model)

# Estimate with significance stars and robust standard error
stars <- function(z) ifelse(abs(z) > qnorm(0.995), "**", ifelse(abs(z) > qnorm(0.975), "*", ""))
cell <- function(m, p) {
  if (!p %in% names(m$estimate)) return("")
  e <- m$estimate[[p]]; s <- m$robse[[p]]
  sprintf("%.2f%s (%.2f)", e, stars(e / s), s)
}

coef_rows <- data.frame(Parameter = unname(LABELS),
                        sapply(models, function(m) sapply(names(LABELS), cell, m = m)),
                        check.names = FALSE, row.names = NULL)

# Model statistics; BIC uses the number of observations
stat <- function(m) {
  k  <- m$nFreeParams
  ll <- m$maximum
  c("No. observations" = sprintf("%d", m$nObs),
    "No. respondents"  = sprintf("%d", m$nIndivs),
    "LL(0)"            = sprintf("%.0f", m$LL0),
    "LL(final)"        = sprintf("%.0f", ll),
    "Adj. rho-squared" = sprintf("%.2f", 1 - (ll - k) / m$LL0),
    "AIC"              = sprintf("%.0f", -2 * ll + 2 * k),
    "BIC"              = sprintf("%.0f", -2 * ll + k * log(m$nObs)))
}
stat_rows <- data.frame(Parameter = names(stat(models[[1]])),
                        sapply(models, stat), check.names = FALSE, row.names = NULL)

table_a8 <- rbind(coef_rows, stat_rows)

write.csv(table_a8, file.path(OUT_DIR, "TableA8_conditional_logit.csv"), row.names = FALSE)

ft <- flextable(table_a8) %>%
  hline(i = nrow(coef_rows), border = fp_border(width = 1)) %>%
  add_footer_lines(c("Robust standard errors in parentheses. ** p<0.01, * p<0.05",
                     "Reference: Customer at municipal utility with full supply and no statutory goal")) %>%
  font(fontname = "Calibri", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  autofit()

save_as_docx(ft, path = file.path(OUT_DIR, "TableA8_conditional_logit.docx"))

cat("Table A.8 written to", OUT_DIR, "\n")
