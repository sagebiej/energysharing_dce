# Willingness to pay for key attributes of energy sharing in Germany

Code for the discrete choice experiment on energy sharing in Germany: the experimental
design, the simulations we used to test it, and the analysis of the survey data.

The data and the other materials are archived on Zenodo,
https://doi.org/10.5281/zenodo.22829058. The scripts download what they need from there, so
you do not have to collect any file by hand.

We preregistered the study on OSF, https://doi.org/10.17605/OSF.IO/95BE2.

## Structure

| Folder | What it holds |
|---|---|
| `design/` | Generates the experimental design and runs the Monte Carlo simulations |
| `analysis/` | Prepares the survey data and estimates the models |

## design/

| File | What it does |
|---|---|
| `Energy_Sharing_Design.R` | Generates the efficient design with `spdesign` and saves it to `design_final/design_final.RDS` |
| `Energy_Sharing_Simulation.R` | Runs the simulations with `simulateDCE`, 500 runs with 1,500 respondents each, for 4 sets of true parameters |
| `simulation_output.qmd` | Report of one set of simulation results |
| `run_html.R` | Downloads the simulation results from Zenodo if needed and renders the report for each of the 4 simulations |
| `run_all.R` | Runs the 3 steps above in order |

Install `spdesign`, `simulateDCE`, `quarto`, `dplyr`, `kableExtra` and `gridExtra`, then run
`design/run_all.R`, which finds its own folder. To run the scripts one after another
instead, start R in `design/`. The scripts create the folders `design_final`
and `sim_results` themselves.

The design script takes about 15 minutes. The simulations take several hours and produce
about 3 GB of output, so the results are archived on Zenodo rather than here.

### The design used in the study

The search algorithm draws candidate designs at random and no seed was set, so a new run of
`Energy_Sharing_Design.R` gives a different design. The design we fielded is
`design_final.RDS` on Zenodo. `Energy_Sharing_Simulation.R` downloads it if it is missing, so
the simulations can be reproduced without regenerating the design.

We used the same design in the second pretest in March 2025 and in the main study.

### Priors

The priors are in the utility function at the top of `Energy_Sharing_Design.R`. They come
from the first pretest in January 2025 and from our own assumptions where the pretest gave no
clear signal.

The price priors read `-0.3, -0.6, -0.9, -0.12, -0.15` for surcharges of 3, 6, 9, 12 and 15%
on the current electricity bill. The last 2 were meant to be `-1.2` and `-1.5`, which would
make the prior linear in price. We generated the design with the values as they stand, so we
keep them in the code. The simulations with the pretest estimates as true parameters show
that the design recovers all parameters.

## analysis/

Prepares the survey data and estimates the conditional logit, mixed logit and latent class
models. Run `analysis/0_Main_Script.R`, which finds its own folder. See
`analysis/README.md` for the script by script description, the output folders, and how the
latent class models handle starting values and local optima.

## Versions

We generated the design on 1 March 2025 and ran the simulations in April 2025, with R 4.4.2.
The design code runs with the current CRAN versions, `spdesign` 0.0.6 and `simulateDCE` 0.3.2.

We ran the analysis with R 4.6.0 and `apollo` 0.3.8 on Windows. A fresh clone reproduces the
reported estimates and log-likelihoods with R 4.4.2 and `apollo` 0.3.9 on Linux.
