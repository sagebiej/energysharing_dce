######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Data Pre-processing for Main Survey            ###
### Date          : 21.03.2025                                     ###
######################################################################

#===========================
# Preparation
#===========================
rm(list=ls())

if(!require(pacman)){
  install.packages("pacman")
  library(pacman)
}
p_load(dplyr, tidyr, readxl, openxlsx, psych, stringr)

if (!file.exists("Hauptstudie/Rohdaten/Results_Survey_Final_Sample.xlsx")) {
  dir.create("Hauptstudie/Rohdaten", recursive = TRUE)
  download.file(
    # the survey data, archived on Zenodo (doi 10.5281/zenodo.22829058)
    url = "https://zenodo.org/records/22829058/files/Results_Survey_Final_Sample.xlsx?download=1",
    destfile = "Hauptstudie/Rohdaten/Results_Survey_Final_Sample.xlsx",
    mode = "wb"  # wichtig: Binary mode für Windows
  )
}

#===========================
# Import Data
#===========================
# df <- read_excel(file.choose())
df <- read_excel("Hauptstudie/Rohdaten/Results_Survey_Final_Sample.xlsx")

#===========================
# Transform from Wide to Long Format
#===========================
df_long <- df %>%
  pivot_longer(
    cols = starts_with("ChoiceExp_C"),
    names_to = "choice_task",
    values_to = "choice"
  ) %>%
  mutate(
    task_id = as.numeric(gsub("ChoiceExp_C|A1", "", choice_task)),
    choice = as.numeric(choice)
  ) %>%
  select(-choice_task)

#===========================
# Extract & Categorize Attributes
#===========================

df_attributes <- df %>%
  select(i_NUMBER, starts_with("choiceA")) %>%
  pivot_longer(
    cols = starts_with("choiceA"),
    names_to = "attribute",
    values_to = "value"
  ) %>%
  mutate(
    attr_id = as.numeric(gsub("choiceA", "", attribute)),
    task_id = ceiling(attr_id / 5),
    attr_type = case_when(
      attr_id %% 5 == 1 ~ "Organisator",
      attr_id %% 5 == 2 ~ "Participation",
      attr_id %% 5 == 3 ~ "Goal",
      attr_id %% 5 == 4 ~ "Contract",
      attr_id %% 5 == 0 ~ "Price"
    )
  ) %>%
  select(-attribute, -attr_id)


#===========================
# Combine Choice & Attribute Data
#===========================
df_long <- df_long %>%
  left_join(df_attributes, by = c("i_NUMBER", "task_id"))

#===========================
#  Create and Mean-center Interaction Variables
#===========================
df_long <- df_long %>%
  mutate(
    #H4: Environmental awareness has a positive effect on individuals' WTP to join an energy sharing community.
    #In questions F8.4_1_1,F8.4_1_2,F8.4_1_4,F8.4_1_5: one is the highest environmental awareness;
    #in question F8.4_1_3 five reflects the highest environmental awareness;
    #answer six is no answer and hence is excluded here.

    F8.4_1_1 = if_else(F8.4_1_1 == 6, NA_real_, 6 - as.numeric(F8.4_1_1)),
    F8.4_2_1 = if_else(F8.4_2_1 == 6, NA_real_, 6 - as.numeric(F8.4_2_1)),
    F8.4_3_1 = if_else(F8.4_3_1 == 6, NA_real_,     as.numeric(F8.4_3_1)),
    F8.4_4_1 = if_else(F8.4_4_1 == 6, NA_real_, 6 - as.numeric(F8.4_4_1)),
    F8.4_5_1 = if_else(F8.4_5_1 == 6, NA_real_, 6 - as.numeric(F8.4_5_1)),

    #The total environmental score is the sum of all the individual items: minimum 5 and maximum 15
    env_awareness_score = if_else(
      if_all(c(F8.4_1_1, F8.4_2_1, F8.4_3_1, F8.4_4_1, F8.4_5_1), is.finite),
      rowSums(across(c(F8.4_1_1, F8.4_2_1, F8.4_3_1, F8.4_4_1, F8.4_5_1))),
      NA_real_
      ),
    #Generate mean centered variables
    inter_env_awareness_score = env_awareness_score - mean(env_awareness_score, na.rm = TRUE),

    #H5: Women exhibit a higher WTP join an energy sharing community than men.
    sex = case_when(
      S1 == 1 ~ 0, #Male
      S1 == 2 ~ 1, #Female
      S1 == 3 ~ NA_real_, #Diverse due to small sample kicked out
      TRUE ~ NA_real_
    ),
    inter_sex = sex - mean(sex, na.rm = TRUE),

    #H6: Household income has a positive effect on individuals' WTP to join an energy sharing community.
    S8 = as.numeric(S8),
    S8 = na_if(S8, 99), #excluding 99 (do not want to say) from sample
    inter_income = S8 - mean(S8, na.rm = TRUE),

    #H7: Age has a negative effect on individuals' WTP to join an energy sharing community.
    S2A1 = as.numeric(S2A1),
    inter_age = (2025 - S2A1 - mean(2025 - S2A1, na.rm = TRUE)),

    #H8: Education has a positive effect on individuals' WTP to join an energy sharing community.
    # Years in school from S5
    educ_base = case_when(
      S5 == 1 ~ 8, #No school degree yet
      S5 == 2 ~ 9, #Basic secondary school diploma (Volks-/Hauptschule)
      S5 == 3 ~ 10, #Intermediate secondary school diploma (Mittlere Reife, Realschule)
      S5 == 4 ~ 12.5, #University entrance qualification (Abitur or Fachabitur)
      S5 %in% c(5, 99) ~ NA_real_,
      TRUE ~ NA_real_
    ),

    # Additional years of schooling
    educ_add = case_when(
      S6 == 1 ~ 0, #None
      S6 == 2 ~ 3, #Vocational qualification
      S6 == 3 ~ 5, #Master craftsman qualification
      S6 == 4 ~ 5.5, #University or college degree
      TRUE ~ NA_real_ #Excludes everyone who selected something else, "I do not want to say", or "something else" in S5
    ),

    # Sum school years
    educ_years = if_else(is.na(educ_base), NA_real_, educ_base + educ_add),
    inter_educ_years = educ_years - mean(educ_years, na.rm = TRUE),

    #H9: Individuals living in multi-family buildings exhibit a positive WTP to become part of an energy sharing community.
    mfh = case_when(
      S10 == 4 ~ NA_real_, #When selected "something else" => excluded
      S10 == 99 ~ NA_real_,#When selected "Do not want to say" => excluded
      S10 == 3 ~ 1,#mfh =1
      TRUE ~ 0
    ),
    inter_mfh = mfh - mean(mfh, na.rm = TRUE),

    #H10: Individuals with low income exhibit a positive WTP to become part of an energy sharing community.
    lowincome = case_when(
      S8 <= 3 ~ 1, #When selected "something else" => excluded
      S8 == 99 ~ NA_real_,#When selected "Do not want to say" => excluded
      TRUE ~ 0
    ),
    inter_lowincome = lowincome - mean(lowincome, na.rm = TRUE),
    
    highincome = case_when(
      S8 == 6 ~ 1, #>5000€ => high income
      S8 == 99 ~ NA_real_,
      TRUE ~ 0
    ),
    inter_highincome = highincome - mean(highincome, na.rm = TRUE), 
    

    #H11: Tenants exhibit a positive WTP to become part of an energy sharing community.
    tenant = case_when(
      S11 == 1 ~ 1, #tenants = 1
      S11 == 99 ~ NA_real_,#when selected "Do not want to say" => excluded
      TRUE ~ 0
    ),
    inter_tenant = tenant - mean(tenant, na.rm = TRUE),
    
    mfh_or_tenant = case_when(
      is.na(mfh) & is.na(tenant) ~ NA_real_,  # beide NA -> NA (numeric NA)
      mfh == 1 ~ 1, #tenants = 1
      tenant == 1 ~ 1,
      TRUE ~ 0
    ),
    inter_mfh_or_tenant = mfh_or_tenant - mean(mfh_or_tenant, na.rm = TRUE)
    
  )

#===========================
# Pivot back to Wide Format
#===========================
df_long <- df_long %>%
  pivot_wider(names_from = attr_type, values_from = value)

#===========================
# Factor Variable Encoding
#===========================
df_long <- df_long %>%
  mutate(
    Organisator = factor(Organisator, levels = c("Bürger*innen", "Kommune (Dorf-, Stadt, Gemeinde-, Kreisverwaltung)", "Stadtwerke")),
    Participation = factor(Participation, levels = c("Ausschließlich Kund*in der BEG", "Investition in die Anlagen der BEG", "Mitgliedschaft in der BEG")),
    Goal = factor(Goal, levels = c("Kein zusätzliches Satzungsziel", "Zusätzliches soziales Satzungsziel", "Zusätzliches ökologisches Satzungsziel", "Zusätzliches soziales und ökologisches Satzungsziel")),
    Contract = factor(Contract, levels = c("Vollversorgung", "Geteilte Versorgung")),
    Price = factor(Price, levels = c("nicht anders", "3 % höher", "6 % höher", "9 % höher", "12 % höher", "15 % höher"))
  )

#===========================
# Create Dummy Variables
#===========================
df_long <- df_long %>%
  mutate(
    OrgCit = if_else(Organisator == "Bürger*innen", 1, 0),
    OrgMun = if_else(Organisator == "Kommune (Dorf-, Stadt, Gemeinde-, Kreisverwaltung)", 1, 0),
    PartiInv = if_else(Participation == "Investition in die Anlagen der BEG", 1, 0),
    PartiMem = if_else(Participation == "Mitgliedschaft in der BEG", 1, 0),
    GoalSoc = if_else(Goal == "Zusätzliches soziales Satzungsziel", 1, 0),
    GoalEco = if_else(Goal == "Zusätzliches ökologisches Satzungsziel", 1, 0),
    GoalBoth = if_else(Goal == "Zusätzliches soziales und ökologisches Satzungsziel", 1, 0),
    Con= if_else(Contract == "Geteilte Versorgung", 1, 0),
    Price3 = if_else(Price == "3 % höher", 1, 0),
    Price6 = if_else(Price == "6 % höher", 1, 0),
    Price9 = if_else(Price == "9 % höher", 1, 0),
    Price12 = if_else(Price == "12 % höher", 1, 0),
    Price15 = if_else(Price == "15 % höher", 1, 0),
    Price = case_when(
      Price == "nicht anders" ~ 0,
      grepl("% höher", Price) ~ as.numeric(str_extract(Price, "\\d+")),
      TRUE ~ NA_real_
    )
)

#===========================
# Translate Data
#===========================

df_long <- df_long %>%
  mutate(Organisator = case_when(
    Organisator == "Bürger*innen" ~ "Citizens",
    Organisator == "Kommune (Dorf-, Stadt, Gemeinde-, Kreisverwaltung)" ~ "Municipality",
    Organisator == "Stadtwerke" ~ "Municipal utility",
    TRUE ~ Organisator
  ))

df_long <- df_long %>%
  mutate(Participation = case_when(
    Participation == "Investition in die Anlagen der BEG" ~ "Investor",
    Participation == "Mitgliedschaft in der BEG" ~  "Member",
    Participation == "Ausschließlich Kund*in der BEG" ~ "Customer",
    TRUE ~ Participation
  ))

df_long <- df_long %>%
  mutate(Goal = case_when(
    Goal == "Kein zusätzliches Satzungsziel" ~ "None",
    Goal == "Zusätzliches soziales Satzungsziel" ~  "Social",
    Goal == "Zusätzliches ökologisches Satzungsziel" ~ "Ecological",
    Goal == "Zusätzliches soziales und ökologisches Satzungsziel" ~ "Both",
    TRUE ~ Goal
  ))

df_long <- df_long %>%
  mutate(Contract = case_when(
    Contract == "Geteilte Versorgung" ~ "Split",
    Contract == "Vollversorgung" ~  "Full",
    TRUE ~ Contract
  ))


#===========================
# Financial Literacy Score
#===========================
df_long <- df_long %>%
  mutate(across(c(F8.5a, F8.5b, F8.5c), as.numeric)) %>%
  mutate(fin_lit_score = rowSums(cbind(F8.5a == 1, F8.5b == 3, F8.5c == 2)))

#===========================
# Drop Raw Attribute Columns
#===========================

df_long <- df_long %>% select(
  -starts_with("choiceA"))


