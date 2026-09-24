rm(list = ls())


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
