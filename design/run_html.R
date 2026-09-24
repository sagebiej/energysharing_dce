
options(timeout = 3000)


# the simulation results, archived on Zenodo (doi 10.5281/zenodo.22829058)
# the four files together are about 3 GB, so the download takes a while
simfiles <- c("simulationresults_Pretest_2_Predicted_Priors500respondents_1500.RDS",
              "simulationresults_Pretest_2_Estimated_Priors500respondents_1500.RDS",
              "simulationresults_Pretest_2_powa88sim500respondents1500.RDS",
              "simulationresults_Pretest_2_powa95sim500respondents1500.RDS")

#check if folder exists
if (!dir.exists("sim_results")) {

  dir.create("sim_results")
}

# download the simulation results if they do not exist
for (f in simfiles) {

  if (!file.exists(file.path("sim_results", f))) {

    download.file(url = paste0("https://zenodo.org/records/22829058/files/", f, "?download=1"),
                  destfile = file.path("sim_results", f), mode = "wb")
  }
}


quarto::quarto_render("simulation_output.qmd" , output_file = "simulation_output_p95.html" ,
                      execute_params = list(path = "sim_results/simulationresults_Pretest_2_powa95sim500respondents1500.RDS"))

quarto::quarto_render("simulation_output.qmd" , output_file = "simulation_output_p88.html" ,
                      execute_params = list(path = "sim_results/simulationresults_Pretest_2_powa88sim500respondents1500.RDS"))


quarto::quarto_render("simulation_output.qmd" , output_file = "simulation_output_estim.html" ,
                      execute_params = list(path = "sim_results/simulationresults_Pretest_2_Estimated_Priors500respondents_1500.RDS"))

quarto::quarto_render("simulation_output.qmd" , output_file = "simulation_output_pred.html" ,
                      execute_params = list(path = "sim_results/simulationresults_Pretest_2_Predicted_Priors500respondents_1500.RDS"))

