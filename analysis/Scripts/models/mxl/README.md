# Mixed Logit models on the cluster (SLURM)

This folder contains everything needed to re-estimate the six Mixed Logit
(MXL) models on a SLURM cluster, each model with an `apollo_searchStart()`
search over 100 candidate sets of starting values.

## Files

| File | Purpose |
|------|---------|
| `run_mxl.slurm`         | SLURM batch script. Submits a 1–6 job array, one model per array task. |
| `loop_mxl.R`            | Driver. Runs the data preparation, then the model selected by the array index. |
| `searchStart_ranges.R`  | Helper `make_searchStart_bounds()` returning sensible `apolloBetaMin`/`apolloBetaMax` ranges, with **different ranges for preference-space and WTP-space models**. |
| `../../../run_data_preparation.R` | Phase 1: data preparation (creates `df_long`). Lives in the project root. |

## Array index → model

| Index | Script | Space |
|-------|--------|-------|
| 1 | `6_MXL_PS.R`                                 | Preference |
| 2 | `6_MXL_PS_all_sociodem.R`                    | Preference |
| 3 | `6_MXL_PS_New_Target_Group_allsociodem.R`    | Preference |
| 4 | `7_MXL_WTP.R`                                | WTP |
| 5 | `7_MXL_WTP_all_sociodem.R`                   | WTP |
| 6 | `7_MXL_WTP_New_Target_Group_all_sociodem.R`  | WTP |

## How to run

Submit from the **project root** (so the relative paths resolve):

```bash
sbatch Scripts/models/mxl/run_mxl.slurm
```

Each array task:

1. runs `run_data_preparation.R` (builds `df_long`);
2. estimates one MXL model, using `apollo_searchStart()` with
   `nCandidates = 100` and space-appropriate ranges;
3. writes the Apollo output and a `.docx` results table to
   `$RESULTS_PATH/MXL/{Preference_Space,WTP_Space}`.

Edit `RESULTS_PATH` (and, if needed, the `#SBATCH` directives) in
`run_mxl.slurm` for your cluster. The number of cores is taken from
`SLURM_CPUS_PER_TASK`.

### Running a single model locally

```bash
Rscript Scripts/models/mxl/loop_mxl.R 4 Hauptstudie/Estimation_results
```

When `results_path` / SLURM variables are absent, the model scripts fall
back to the local `Hauptstudie/Estimation_results` folder and use
`detectCores() - 1` cores, so they remain usable from `0_Main_Script.R`.

## Starting-value ranges

`searchStart_ranges.R` sets the search ranges by parameter type and by
model space (`"PS"` vs `"WTP"`), because the parameters live on different
scales (utility units vs. money / willingness to pay). For example the
mean taste parameters use `[-0.6, 0.8]` in preference space but
`[-1.0, 2.0]` in WTP space, and `sig_asc` uses `[0.10, 3.0]` (PS) vs.
`[0.5, 10.0]` (WTP). See the file for the full set of rules.

All six models are self-contained (no model reads another model's output),
so the array tasks can run in parallel.
