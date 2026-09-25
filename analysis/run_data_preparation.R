######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Phase 1 - Data preparation for the models       ###
###                                                                 ###
### Runs the data-preprocessing required by the Conditional /       ###
### Mixed Logit / Latent Class models.  After sourcing this file    ###
### the Apollo-ready data set `df_long` is available in the global  ###
### environment.                                                    ###
###                                                                 ###
### Only 1_Data_Procession.R is needed for model estimation. The    ###
### scripts 2_* and 3_* are descriptive / plausibility checks and   ###
### add columns that the models do not use, so they are not run     ###
### here.                                                           ###
###                                                                 ###
### Usage (stand-alone): Rscript run_data_preparation.R             ###
######################################################################

# NOTE: 1_Data_Procession.R starts with rm(list = ls()), so do not keep
# any objects defined before this point - they will be removed.
source("1_Data_Procession.R", encoding = "UTF-8")

# Basic sanity check so a broken data step fails loudly and early.
if (!exists("df_long")) {
  stop("Data preparation failed: object 'df_long' was not created.")
}

message("Data preparation complete: df_long has ",
        nrow(df_long), " rows and ", ncol(df_long), " columns (",
        length(unique(df_long$i_NUMBER)), " respondents).")
