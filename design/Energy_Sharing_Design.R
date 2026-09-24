# Load or install the 'spdesign' package ----
if(!require(spdesign)){
  install.packages("spdesign")  # Install 'spdesign' if it is not already installed
  library(spdesign)             # Load the 'spdesign' package
}

# Define utility functions for each alternative ----
# The utilities are specified using attribute levels and corresponding coefficients
# Priors for alternatives are based on pretest and assumptions
# all levels of attributes are dummy coded except simplicity which is binary
# There are three types of organizers (Organisator):  default is municipal utility company, ESC_Organisator1 is citizens, and ESC_organisator2 is municipality.
# There are three types of investment and co-determination (Participation): the default is customer only, Participation1 is investor, and Participation2 is member.
# There are four types of additional statutory goals (Motive): the default is none, Motive1 is social, Motive2 is ecological, and Motive3 is both
# There are two types of contractual mode (Simplicity): the default is full supply and the alternative share supply
# There are six price levels (0%, 3%, 6%, 9%, 12% and 15% on top of the current electricity bill). Adding (4:8) makes sure that each level of price is at least four times and a maximum of eight times present in the calculated design
# The price priors were meant to be -0.3, -0.6, -0.9, -1.2 and -1.5. The last two were written with one decimal too many. The design used in the study was generated with the values as they stand here, so we do not change them

utility <- list(
  alt1 = "b_ESCorganisator_dummy[c(0.05, -0.1)] * ESCorganisator[c(0,1,2)] +
          b_Participation_dummy[c(-0.1, 0.5)] * Participation[c(0, 1, 2)] +
          b_Motive_dummy[c(0.2, 0.3, 0.5)] * Motive[c(0, 1, 2, 3)]  +
          b_Simplicity[-0.3] * Simplicity[c(0, 1)]  +
          b_Price_dummy[c(-0.3, -0.6, -0.9, -0.12, -0.15)] * Price[c(0, 1, 2, 3, 4, 5)](4:8)",
  alt2 = "b_sq[0] * sq[1]"  # Status quo alternative, the current electricity contract
)

# Generate an efficient experimental design based on the specified utility functions ----
# The algorithm draws candidate designs at random, so a new run gives a different design. The design used in the study is design_final.RDS on Zenodo (see README)
design <- generate_design(
  utility,
  rows = 40,                      # Number of choice tasks (rows in the design)
  model = "mnl",                  # Model type: Multinomial Logit
  efficiency_criteria = "d-error",# Optimization criterion: D-error (for statistical efficiency)
  algorithm = "federov"          # Algorithm used to generate the design
)

# Split the design into blocks (e.g., for survey versions) ----
design <- block(design, 4)        # Divide the design into 4 blocks

# Output diagnostics ----
probabilities(design)            # Calculate and display choice probabilities
summary(design)                  # Display summary statistics of the design

# Save the final design object to an RDS file for later use ----
if (!dir.exists("design_final")) {
  dir.create("design_final")
}

saveRDS(design, "design_final/design_final.RDS")
