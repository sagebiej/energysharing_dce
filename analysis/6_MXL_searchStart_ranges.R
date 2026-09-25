######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Helper for apollo_searchStart() - builds        ###
###                 sensible lower/upper bounds for the random      ###
###                 starting values.                                ###
###                                                                 ###
### The parameters of the preference-space and the WTP-space        ###
### models live on very different scales (utility units vs. money   ###
### / willingness to pay).  The function therefore returns          ###
### DIFFERENT ranges depending on the `space` argument:             ###
###   - "PS"  : preference-space models   (6_MXL_PS*)               ###
###   - "WTP" : willingness-to-pay models (7_MXL_WTP*)              ###
######################################################################

# make_searchStart_bounds()
#
# Arguments:
#   apollo_beta : named numeric vector of starting values. The returned
#                 bounds use exactly the same names / ordering.
#   space       : "PS" or "WTP".
#
# Value:
#   A list with two named numeric vectors, `min` and `max`, ready to be
#   passed to apollo_searchStart() as apolloBetaMin / apolloBetaMax.
make_searchStart_bounds <- function(apollo_beta, space = c("PS", "WTP")) {
  space <- match.arg(space)
  nm <- names(apollo_beta)
  if (is.null(nm)) stop("apollo_beta must be a *named* numeric vector.")

  lo <- stats::setNames(numeric(length(nm)), nm)
  hi <- stats::setNames(numeric(length(nm)), nm)

  for (p in nm) {
    if (p == "sig_asc") {
      # SD of the ASC: typically large, and much larger in WTP space
      rng <- if (space == "WTP") c(0.5, 10.0) else c(0.10, 3.0)
    } else if (p == "sig_bprice") {
      # SD of the (log) price coefficient: kept moderate to avoid overflow
      rng <- if (space == "WTP") c(0.10, 1.5) else c(0.05, 1.0)
    } else if (grepl("^sig_", p)) {
      # SD of the remaining random taste parameters
      rng <- if (space == "WTP") c(0.05, 1.5) else c(0.05, 1.0)
    } else if (p == "bprice") {
      # price enters on the log scale via -exp(bprice + sig_bprice * draw)
      rng <- if (space == "WTP") c(-1.5, 0.0) else c(-1.5, 0.5)
    } else if (grepl("^(asc_|bpartimem_)", p)) {
      # socio-demographic interaction terms: small and centred on zero
      # (kept tight because covariates such as age / education are not scaled)
      rng <- c(-0.5, 0.5)
    } else if (p == "asc") {
      # alternative-specific constant (mean)
      rng <- if (space == "WTP") c(-2.0, 2.0) else c(-1.0, 1.0)
    } else {
      # mean taste parameters; WTP values span a wider (money) range
      rng <- if (space == "WTP") c(-1.0, 2.0) else c(-0.6, 0.8)
    }
    lo[p] <- rng[1]
    hi[p] <- rng[2]
  }

  list(min = lo, max = hi)
}
