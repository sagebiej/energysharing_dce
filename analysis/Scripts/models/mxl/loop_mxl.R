######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : SLURM array driver for the Mixed Logit models   ###
###                                                                 ###
### One array task = one model. The array index (1-6) selects       ###
### which Mixed Logit script is run:                                ###
###   1  6_MXL_PS.R                                  (pref. space)  ###
###   2  6_MXL_PS_all_sociodem.R                     (pref. space)  ###
###   3  6_MXL_PS_New_Target_Group_allsociodem.R     (pref. space)  ###
###   4  7_MXL_WTP.R                                 (WTP space)     ###
###   5  7_MXL_WTP_all_sociodem.R                    (WTP space)     ###
###   6  7_MXL_WTP_New_Target_Group_all_sociodem.R   (WTP space)     ###
###                                                                 ###
### Usage: Rscript Scripts/models/mxl/loop_mxl.R <index> <results>  ###
###   <index>   : 1-6 (e.g. $SLURM_ARRAY_TASK_ID)                   ###
###   <results> : output directory for the estimation results       ###
###                                                                 ###
### The working directory must be the project root.                ###
######################################################################

## --- 1. Data preparation -------------------------------------------
## Creates `df_long` in the global environment.
## IMPORTANT: 1_Data_Procession.R (sourced from here) begins with
## rm(list = ls()); therefore the command-line arguments and all other
## settings are defined AFTER this step.
source("run_data_preparation.R", encoding = "UTF-8")

## --- 2. Packages ----------------------------------------------------
if (!require(pacman)) { install.packages("pacman"); library(pacman) }
p_load(apollo, dplyr, tidyr, flextable, magrittr, officer, tibble)

## --- 3. Command-line arguments -------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Provide the model index (1-6) as the first argument.")
}
task_id <- suppressWarnings(as.integer(args[1]))
results_path <- if (length(args) >= 2 && nzchar(args[2])) {
  args[2]
} else {
  "Hauptstudie/Estimation_results"
}

## --- 4. Cores -------------------------------------------------------
## Respect the SLURM allocation when present, otherwise use all but one
## of the locally available cores.
slurm_cpus <- Sys.getenv("SLURM_CPUS_PER_TASK")
n_cores <- if (nzchar(slurm_cpus)) {
  as.integer(slurm_cpus)
} else {
  max(1, parallel::detectCores() - 1)
}

## --- 5. Search-range helper ----------------------------------------
source("Scripts/models/mxl/searchStart_ranges.R", encoding = "UTF-8")

## --- 6. Map the array index to a model script ----------------------
mxl_scripts <- c(
  "6_MXL_PS.R",
  "6_MXL_PS_all_sociodem.R",
  "6_MXL_PS_New_Target_Group_allsociodem.R",
  "7_MXL_WTP.R",
  "7_MXL_WTP_all_sociodem.R",
  "7_MXL_WTP_New_Target_Group_all_sociodem.R"
)

if (is.na(task_id) || task_id < 1 || task_id > length(mxl_scripts)) {
  stop("Model index must be an integer between 1 and ", length(mxl_scripts), ".")
}

script <- mxl_scripts[task_id]

message("=====================================================")
message(" Running model ", task_id, "/", length(mxl_scripts), ": ", script)
message(" results_path : ", results_path)
message(" n_cores      : ", n_cores)
message("=====================================================")

## --- 7. Run the selected model -------------------------------------
## `results_path` and `n_cores` are picked up inside the model script.
source(script, encoding = "UTF-8")

message("Finished model ", task_id, ": ", script)
