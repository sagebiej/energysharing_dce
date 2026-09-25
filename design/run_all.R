rm(list = ls())

## Work in the folder of this script, wherever R was started.
## source() stores the path in ofile; Rscript passes it as --file=, with
## spaces written as ~+~; RStudio knows it when the script is run line by line.
script <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  script <- gsub("~+~", " ", script, fixed = TRUE)
}
if (length(script) == 0 && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script <- rstudioapi::getSourceEditorContext()$path
}
## file.exists is FALSE after source(chdir = TRUE), which is already in the folder
if (length(script) == 1 && nzchar(script) && file.exists(script)) setwd(dirname(normalizePath(script)))


## Run the design script to create an efficient design
## This creates a new design and overwrites design_final/design_final.RDS.
## To work with the design used in the study, skip this line and let the
## simulation script download the design from Zenodo.

source("Energy_Sharing_Design.R")

### Once the design is saved in the folder design_final, we use simulations to test it and calculate power.
 ## Run the simulations

source("Energy_Sharing_Simulation.R")

## To create html outputs with the files, we can run the script run_html

source("run_html.R")
