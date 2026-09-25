# Analysis

Analysis code for Wiesenthal et al., *Preferences for Energy Sharing* (discrete choice
experiment, Utilities Policy, revised version).

## How to reproduce the results

Run `0_Main_Script.R`, with `Rscript analysis/0_Main_Script.R` or from RStudio. It sets
the working directory to its own folder, so it does not matter where R was started.

The raw data (`Results_Survey_Final_Sample.xlsx`) are downloaded from Zenodo by
`1_Data_Procession.R`, https://doi.org/10.5281/zenodo.22829058. No estimated model is needed:
all models are estimated from scratch.

Software used for the reported results: R 4.6.0, Apollo 0.3.8 (Windows).

### Latent class models and starting values

Latent class models have many local optima. The reported latent class models are the best
solutions of a multi-start search (Supplementary material A.3). That search took several
nights. There are two ways to run the latent class part:

| `RUN_SEARCH` in `0_Main_Script.R` | What happens | Runtime |
|---|---|---|
| `FALSE` (default) | The models are estimated from the stored starting values in `8_LCL_start_values.R` (the best solutions of the search). The scripts check that the reported log-likelihoods are reached. | several hours |
| `TRUE` | `9_LCL_multistart_search.R` first repeats the search from random starting values. The models are then estimated from the solutions it finds, and Table A.2 reports its start counts. | one to two days |

The search is reproducible for a given seed, number of cores and Apollo version. On another
machine it may reach the same optima through different starts, or miss an optimum that is
reached rarely.

### Mixed logit models and starting values

Each mixed logit model searches for its starting values with `apollo_searchStart()` over 100
candidate sets. `6_MXL_searchStart_ranges.R` sets the search ranges by parameter type and by
model space, because the parameters live on different scales (utility units against money).
For example the mean taste parameters use [-0.6, 0.8] in preference space but [-1.0, 2.0] in
WTP space. The models are estimated with 1,000 draws.

## Scripts

| Script | Content | Paper |
|---|---|---|
| `0_Main_Script.R` | runs all scripts in order | |
| `1_Data_Procession.R` | data preparation, creates `df_long` | |
| `2_Plausichecks_main.R` | plausibility checks | |
| `3_Choice_behaviour.R` | choice behaviour, approval rates | Figure 2, Figure A.1 |
| `3_Multicollinearity_Check.R` | correlation of socio-demographic variables, saved to `Hauptstudie/Paper/Table_A3_correlations.docx` | Table A.3 |
| `3_Soziodem.R` | sample description | Table 4 |
| `3_Status_Quo_choosers.R` | reasons for keeping the status quo | Table A.9 |
| `4_CL_*.R`, `5_CL_*.R` | conditional logit, preference and WTP space | Table A.8 |
| `6_MXL_*.R`, `7_MXL_*.R` | mixed logit, preference and WTP space | Table A.7 |
| `6_MXL_searchStart_ranges.R` | search ranges for the starting values of the mixed logit models | |
| `8_LCL_model_definition.R` | shared definition of all latent class models | |
| `8_LCL_start_values.R` | starting values and search record of the latent class models | |
| `9_LCL_multistart_search.R` | multi-start search (only with `RUN_SEARCH = TRUE`) | A.3 |
| `10_LCL_WTP_K_Classes.R` | estimation of all latent class models: 2 to 7 classes, with and without socio-demographics (five classes with socio-demographics = reported model) | |
| `11_LCL_Table5_tests.R` | reported model: Table 5, Wald tests, hypothesis tests, WTP sums | Table 5, Table 6 (`hypothesis_tests.csv`) |
| `12_LCL_class_profiles.R` | class assignment, separation and composition | Tables A.4 to A.6 |
| `13_Table_A2_class_enumeration.R` | selection of the number of classes | Table A.2 |

## Output

- Estimation results: `Hauptstudie/Estimation_results/` (CL, MXL, `LCLogit/WTP_Space/`)
  - `reported/`: five-class model, Table 5, tests, Tables A.4 to A.6
  - `enumeration_sociodem/`, `enumeration_nocov/`: models with 2 to 7 classes, Table A.2
  - `search/`: results of the multi-start search (only with `RUN_SEARCH = TRUE`)
- Figures: `Hauptstudie/Paper/`, `Barcharts/`

Only `10_LCL_WTP_K_Classes.R` estimates latent class models. Scripts 11 to 13 build the
tables from the saved models and can be re-run without estimating again.
