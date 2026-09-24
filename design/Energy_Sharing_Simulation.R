rm(list=ls())

# Installing necessary packages

library(simulateDCE)
if(!require(rlang)){
  install.packages("rlang")
  library(rlang)
}
if(!require(formula.tools)){
  install.packages("formula.tools")
  library(formula.tools)
}

library(dplyr)

# the design used in the study, archived on Zenodo (doi 10.5281/zenodo.22829058)
url_finaldesign <- "https://zenodo.org/records/22829058/files/design_final.RDS?download=1"

# download final design if it does not exist
if (!file.exists("design_final/design_final.RDS")) {

  #check if folder exists
  if (!dir.exists("design_final")) {

        dir.create("design_final")
  }
        download.file(url = url_finaldesign, destfile = "design_final/design_final.RDS", mode = "wb")
}

#path for designs that should be simulated
designpath<- "design_final/"
resps = 1500  # number of respondents
nosim= 500 # number of simulations to run

# folder for the simulation results
if (!dir.exists("sim_results")) {

  dir.create("sim_results")
}

#################################################################################
###### Simulation with assumed priors from finding efficient design##############
#################################################################################

#true priors are taken from design
bcoeff <- readRDS("design_final/design_final.RDS")$prior_values[[1]] %>% as.list
names(bcoeff) <- names(bcoeff) %>% tolower() %>%  stringr::str_remove_all("_")

# #utility function
ul<-list( u1 =

            list(
              v1 =V.1~  bescorganisator2*alt1.ESCorganisator1 + bescorganisator3*alt1.ESCorganisator2 + bparticipation2 * alt1.Participation1 + bparticipation3 * alt1.Participation2 + bmotive2 * alt1.Motive1 + bmotive3*alt1.Motive2 + bmotive4 * alt1.Motive3 +  bsimplicity*alt1.Simplicity + bprice2 * alt1.Price1 + bprice3*alt1.Price2 + bprice4 * alt1.Price3 + bprice5 * alt1.Price4 + bprice6 * alt1.Price5,
              v2 =V.2~  0
            )
)

simulation <- sim_all(nosim = nosim, resps=resps,
                   designpath = designpath, u=ul, bcoeff = bcoeff, utility_transform_type = "exact" ) #,mode = "parallel"

saveRDS(simulation, paste0("sim_results/simulationresults_Pretest_2_Predicted_Priors",nosim,"respondents_",resps,".RDS"))

#################################################################################
###### Simulation with priors estimated in the second pretest ###################
#################################################################################

# priors based on estimates of conditional logit model of second pretest
bcoeff  = list(
  bescorganisator2 = 0.094,
  bescorganisator3 = -0.201,
  bparticipation2 = -0.135,
  bparticipation3 = 0.275,
  bmotive2 = 0.179,
  bmotive3 = 0.436,
  bmotive4 = 0.373,
  bsimplicity = -0.037,
  bprice2= -0.918,
  bprice3= -1.068,
  bprice4= -2.071,
  bprice5= -1.684,
  bprice6= -2.617,
  bsq = -0.169)

#utility function
ul<-list( u1 =

            list(
              v1 =V.1~  bescorganisator2*alt1.ESCorganisator1 + bescorganisator3*alt1.ESCorganisator2 + bparticipation2 * alt1.Participation1 + bparticipation3 * alt1.Participation2 + bmotive2 * alt1.Motive1 + bmotive3*alt1.Motive2 + bmotive4 * alt1.Motive3 +  bsimplicity*alt1.Simplicity + bprice2 * alt1.Price1 + bprice3*alt1.Price2 + bprice4 * alt1.Price3 + bprice5 * alt1.Price4 + bprice6 * alt1.Price5,
              v2 =V.2~  bsq
            )
)

simulation <- sim_all(nosim = nosim, resps=resps,
                      designpath = designpath, u=ul, bcoeff = bcoeff, utility_transform_type = "exact" ) #,mode = "parallel"

saveRDS(simulation, paste0("sim_results/simulationresults_Pretest_2_Estimated_Priors",nosim,"respondents_",resps,".RDS"))

#################################################################################
###### Powa Simulation###########################################################
#################################################################################

## simulation for reaching a powa of 88.4

# # priors based on estimates of conditional logit model of second pretest
bcoeff  = list(
  bescorganisator2 = 0.25, #0.094
  bescorganisator3 = -0.201,
  bparticipation2 = -0.2, #-0.135
  bparticipation3 = 0.275,
  bmotive2 = 0.25, #0.179
  bmotive3 = 0.436,
  bmotive4 = 0.373,
  bsimplicity = -0.2, #0.037
  bprice2= -0.918,
  bprice3= -1.068,
  bprice4= -2.071,
  bprice5= -1.684,
  bprice6= -2.617,
  bsq = -0.25) #-0.169

#utility function
ul<-list( u1 =

            list(
              v1 =V.1~  bescorganisator2*alt1.ESCorganisator1 + bescorganisator3*alt1.ESCorganisator2 + bparticipation2 * alt1.Participation1 + bparticipation3 * alt1.Participation2 + bmotive2 * alt1.Motive1 + bmotive3*alt1.Motive2 + bmotive4 * alt1.Motive3 +  bsimplicity*alt1.Simplicity + bprice2 * alt1.Price1 + bprice3*alt1.Price2 + bprice4 * alt1.Price3 + bprice5 * alt1.Price4 + bprice6 * alt1.Price5,
              v2 =V.2~  bsq
            )
)

simulation <- sim_all(nosim = nosim, resps=resps,
                      designpath = designpath, u=ul, bcoeff = bcoeff, utility_transform_type = "exact" ) #,mode = "parallel"

saveRDS(simulation, paste0("sim_results/simulationresults_Pretest_2_powa88sim",nosim,"respondents",resps,".RDS"))

# simulation for reaching a powa of 95
bcoeff  = list(
  bescorganisator2 = 0.25, # 0.094 is prior of pretest 2
  bescorganisator3 = -0.25,# -0.201 is prior of pretest 2
  bparticipation2 = -0.25, # -0.135 is prior of pretest 2
  bparticipation3 = 0.275,
  bmotive2 = 0.25, # 0.179 is prior of pretest 2
  bmotive3 = 0.436,
  bmotive4 = 0.373,
  bsimplicity = -0.2, # 0.037 is prior of pretest 2
  bprice2= -0.918,
  bprice3= -1.068,
  bprice4= -2.071,
  bprice5= -1.684,
  bprice6= -2.617,
  bsq = -0.25) # -0.169 is prior of pretest 2

#utility function
ul<-list( u1 =

            list(
              v1 =V.1~  bescorganisator2*alt1.ESCorganisator1 + bescorganisator3*alt1.ESCorganisator2 + bparticipation2 * alt1.Participation1 + bparticipation3 * alt1.Participation2 + bmotive2 * alt1.Motive1 + bmotive3*alt1.Motive2 + bmotive4 * alt1.Motive3 +  bsimplicity*alt1.Simplicity + bprice2 * alt1.Price1 + bprice3*alt1.Price2 + bprice4 * alt1.Price3 + bprice5 * alt1.Price4 + bprice6 * alt1.Price5,
              v2 =V.2~  bsq
            )
)

simulation <- sim_all(nosim = nosim, resps=resps,
                      designpath = designpath, u=ul, bcoeff = bcoeff, utility_transform_type = "exact" ) #,mode = "parallel"

saveRDS(simulation, paste0("sim_results/simulationresults_Pretest_2_powa95sim",nosim,"respondents",resps,".RDS"))


