########################################################################
### Project       : DCE Energy Sharing                               ###
### Description   : Central script that runs all evaluation scripts  ###
###                 and reproduces the results of the paper          ###
###                 (Wiesenthal et al., Preferences for Energy       ###
###                 Sharing, Utilities Policy, revised version)      ###
### Datum         : 24.09.2026                                       ###
########################################################################
###
### The script sets the working directory to its own folder, so it can be
### run from anywhere, with Rscript or from RStudio.
### The raw data are downloaded from Zenodo by 1_Data_Procession.R (doi 10.5281/zenodo.22829058).
### No estimated model is needed: all models are estimated from scratch.
###
### Runtime (rough): descriptive scripts and conditional logit minutes;
### mixed logit 20 to 55 minutes per model on 25 cores from the stored
### starting values (the search took 8 to 21 hours per model); latent
### class models several hours on 4 cores.
########################################################################

# Clear environment
rm(list=ls())

# Work in the folder of this script, wherever R was started.
# source() stores the path in ofile; Rscript passes it as --file=, with
# spaces written as ~+~; RStudio knows it when the script is run line by line.
script <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
  script <- gsub("~+~", " ", script, fixed = TRUE)
}
if (length(script) == 0 && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script <- rstudioapi::getSourceEditorContext()$path
}
# file.exists is FALSE after source(chdir = TRUE), which is already in the folder
if (length(script) == 1 && nzchar(script) && file.exists(script)) setwd(dirname(normalizePath(script)))

plot.new()

########################################################################
### Settings                                                         ###
########################################################################

# Mixed logit (scripts 6 and 7) and latent class models (8 to 13):
# FALSE  estimate from the stored starting values in 6_MXL_start_values.R
#        and 8_LCL_start_values.R (the best solutions of the searches;
#        for the latent class models reported in A.3)
# TRUE   repeat the searches from random starting values first
#        (apollo_searchStart() in scripts 6 and 7, 8 to 21 hours per
#        model; 9_LCL_multistart_search.R, one to two days) and start
#        from their solutions
RUN_SEARCH <- FALSE

# Stored as an option, because 1_Data_Procession.R clears the workspace
options(dce.run_search = RUN_SEARCH)

# Load packages
if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}

p_load(car, dplyr, readr, ggplot2, haven, tidyr, ggpubr, reshape2, apollo, flextable, magrittr, officer, texreg, janitor, polycor, tibble)

# Output folders. A fresh clone has none of them, and apollo creates its
# outputDirectory without recursive = TRUE, so the nested ones must exist first.
for (d in c("Hauptstudie/Estimation_results/CL/Preference_Space",
            "Hauptstudie/Estimation_results/CL/WTP_Space",
            "Hauptstudie/Paper",
            "Barcharts")) {

  if (!dir.exists(d)) {

    dir.create(d, recursive = TRUE)
  }
}

########################################################################
### Data preparation and descriptive analysis                        ###
########################################################################

### Script for data preprocessing
source("1_Data_Procession.R",encoding="UTF-8")
#creates Apollo ready dataset named "df_long"

### Script for plausibility checks
source("2_Plausichecks_main.R",encoding="UTF-8")
#checks whether questions about environmental awareness are answered plausible
#checks whether the cent/kWh price (monthly payment/yearly electricity consumption) seems plausible
#checks whether demand per person seems plausible

### Script for choice behavior (Figure 2, Figure A.1)
source("3_Choice_behaviour.R",encoding="UTF-8")
#analysis how often for each choice situation/block "I stay was chosen"
#analysis approval rate dependent on Attribute Levels and Choices
#analysis approval rate dependent on Attribute Levels, Price level and Choices
#analysis approval rate dependent on Price level and target group

### Script for multicollinearity check (Table A.3)
source("3_Multicollinearity_Check.R",encoding="UTF-8")
#analysis whether there is multicollinearity between variables

### Script for sociodemographic characteristics (Table 4)
source("3_Soziodem.R",encoding="UTF-8")
# summary statistics of sample for...
# Gender (male), Education (University degree),
# Housing (Renter), Household income (over EUR 4,000 net), Age,
# Number of Persons in Household, Income class, Years of education,
# Employment status, Full-time, Living environment, Suburban, Type of dwelling,
# Apartment in Multi-family Building, Federal state

### Script for Status Quo choosers (Table A.9)
source("3_Status_Quo_choosers.R",encoding="UTF-8")
#analysis why people choose the status Quo

########################################################################
### Conditional Logit Models (Table A.8)                             ###
########################################################################
#in preference space
source("4_CL_PS.R",encoding="UTF-8")
source("4_CL_PS_all_sociodem.R",encoding="UTF-8")
source("4_CL_PS_New_Target_Group_all_sociodem.R",encoding="UTF-8")
#in WTP space
source("5_CL_WTP.R",encoding="UTF-8")
source("5_CL_WTP_all_sociodem.R",encoding="UTF-8")
source("5_CL_WTP_New_Target_Group_all_sociodem.R",encoding="UTF-8")
#Table A.8 from the three models in WTP space
source("5_CL_Table_A8.R",encoding="UTF-8")

########################################################################
### Mixed Logit Models (Table A.7)                                   ###
########################################################################
#in preference space
source("6_MXL_PS.R",encoding="UTF-8")
source("6_MXL_PS_all_sociodem.R",encoding="UTF-8")
source("6_MXL_PS_New_Target_Group_allsociodem.R",encoding="UTF-8")
#in WTP space
source("7_MXL_WTP.R",encoding="UTF-8")
source("7_MXL_WTP_all_sociodem.R",encoding="UTF-8")
source("7_MXL_WTP_New_Target_Group_all_sociodem.R",encoding="UTF-8")

########################################################################
### Latent Class Models, WTP space (Table 5, Table 6, A.2, A.4-A.6)  ###
########################################################################
# Results are written to Hauptstudie/Estimation_results/LCLogit/WTP_Space/

### Shared definition of all latent class models (also sourced by 9 to 13)
source("8_LCL_model_definition.R",encoding="UTF-8")

### Starting values of the latent class models and record of the search
source("8_LCL_start_values.R",encoding="UTF-8")

### Optional: repeat the multi-start search (RUN_SEARCH = TRUE)
if (RUN_SEARCH) source("9_LCL_multistart_search.R",encoding="UTF-8")

### Estimation of all latent class models: 2 to 7 classes, with and
### without socio-demographics (five classes with socio-demographics
### = reported model)
source("10_LCL_WTP_K_Classes.R",encoding="UTF-8")

### Reported model: Table 5, joint Wald tests on class membership,
### hypothesis tests (hypothesis_tests.csv = Table 6), WTP sums per class
source("11_LCL_Table5_tests.R",encoding="UTF-8")

### Profiles of the five classes: Tables A.4 to A.6
source("12_LCL_class_profiles.R",encoding="UTF-8")

### Selection of the number of classes: Table A.2
source("13_Table_A2_class_enumeration.R",encoding="UTF-8")
