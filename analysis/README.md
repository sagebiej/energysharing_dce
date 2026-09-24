# Analysis

TODO: the analysis code goes here once it is final.

It comes from `energycommunities_Auswertung`. Copy the working tree, not the git history:
the history of that repository contains the raw pilot data (`Pretest/Data/`), which must not
become public.

Before the first commit, check that:

- `1_Data_Procession.R` downloads the survey data from Zenodo and holds no view-only token
- no data file is tracked, the `.gitignore` in the repository root covers the usual formats
- every script creates the folders it writes into, because a fresh clone has none of them
