######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Sociodemographic Characteristics               ###
### Output        : Excel, PDF, R-Table, R-Plots  n                ###
### Date          : 01.04.2025                                     ###
######################################################################

#=====================================================================
# Preparation
#=====================================================================

# Load data
df <- df_long 

#=====================================================================
# Create Summary Table
#=====================================================================

#Table for paper
#Number of participants
respondents_count <- nrow(df) / 10

Labels <- c(
  "Female",
  "Male",
  "No response and gender = diverse",
  "Age 18–29",
  "Age 30–39",
  "Age 40–49",
  "Age 50–64",
  "Age 65+",
  "No response and age out of range",
  "(No) school-leaving qualification yet",
  "Lower secondary school certificate (Hauptschule)",
  "Intermediate school certificate (Realschule)",
  "Higher education entrance qualification (Abitur/Fachabitur)",
  "Other school-leaving qualification",
  "No response education",
  "Household net income < 1,000 €",
  "Household net income 1,000–2,000 €",
  "Household net income 2,000–3,000 €",
  "Household net income 3,000–4,000 €",
  "Household net income 4,000–5,000 €",
  "Household net income > 5,000 €",
  "No response income",
  "Tenants",
  "No response (tenant)",
  "MFH residents",
  "No response (MFH resident)"
)

Anzahl <- c(
  sum(df$S1 == 2, na.rm = TRUE)/10,             # Female
  sum(df$S1 == 1, na.rm = TRUE)/10,             # Male
  sum(is.na(df$S1))/10 +sum(df$S1 == 3, na.rm = TRUE)/10, # No response and gender = diverse
  sum(df$alter_kat == 2, na.rm = TRUE)/10,      # Age 18–29
  sum(df$alter_kat == 3, na.rm = TRUE)/10,      # Age 30–39
  sum(df$alter_kat == 4, na.rm = TRUE)/10,      # Age 40–49
  sum(df$alter_kat == 5, na.rm = TRUE)/10,      # Age 50–64
  sum(df$alter_kat == 6, na.rm = TRUE)/10,      # Age 65+
  sum(is.na(df$alter_kat))/10,                  # No response (age)
  sum(df$S5 == 1, na.rm = TRUE)/10,             # (No) school-leaving qualification yet
  sum(df$S5 == 2, na.rm = TRUE)/10,             # Lower secondary school certificate (Hauptschule)
  sum(df$S5 == 3, na.rm = TRUE)/10,             # Intermediate school certificate (Realschule)
  sum(df$S5 == 4, na.rm = TRUE)/10,             # Higher education entrance qualification (Abitur/Fachabitur)
  sum(df$S5 == 5, na.rm = TRUE)/10,             # Other school-leaving qualification
  sum(is.na(df$S5))/10,                         # No response (education)
  sum(df$S8 == 1, na.rm = TRUE)/10,             # Household net income < 1,000 €
  sum(df$S8 == 2, na.rm = TRUE)/10,             # Household net income 1,000–2,000 €
  sum(df$S8 == 3, na.rm = TRUE)/10,             # Household net income 2,000–3,000 €
  sum(df$S8 == 4, na.rm = TRUE)/10,             # Household net income 3,000–4,000 €
  sum(df$S8 == 5, na.rm = TRUE)/10,             # Household net income 4,000–5,000 €
  sum(df$S8 == 6, na.rm = TRUE)/10,             # Household net income > 5,000 €
  sum(is.na(df$S8))/10,                         # No response (income)
  sum(df$tenant == 1, na.rm = TRUE)/10,         # Tenants
  sum(is.na(df$tenant))/10,                     # No response (tenant)
  sum(df$mfh == 1, na.rm = TRUE)/10,            # MFH residents
  sum(is.na(df$mfh))/10                         # No response (MFH resident)
)


#study population based on "Ergebnisdaten_XYZ_Netto-Grundgesamtheit_sozio Daten_(n=5.796).xlsx" 
study_population <- c(
  2966, 2801, 29, 
  888, 896, 894, 1596, 1459, 63, 
  131, 1201, 1915, 2466,  27, 56, 
  342, 1269, 1275, 952, 715, 964,279
)

study_population_rel <- c(
  "51.4%","48.6%", "n.a.", #gender
  "15.5%", "15.6%", "15.6%", "27.8%", "25.4%", "n.a.",#age groups
  "2.3%", "21.0%", "33.5%", "43.2%", "n.a.","n.a.", #education groups
  "6.2%", "23.0%", "23.1%", "17.3%", "13.0%", "17.5%", "n.a." #income groups
)


German_average <- c(
  "51.1%","48.9%", "n.a.", #gender
  "15.7%", "15.8%", "14.8%", "26.8%", "26.9%", "n.a.",#age groups
  "8.7%", "23.5%", "30.2%", "37.6%", "n.a.","n.a.", #education groups
  "6.6%", "21.9%", "23.1%", "16.1%", "11.8%", "20.4%", "n.a." #income groups
)

# Extend study_population and study_population_rel with "n.a." for tenants and MFH
study_population <- c(study_population, rep("n.a.", 4))
study_population_rel <- c(study_population_rel, rep("n.a.", 4))

German_average <- c(
  German_average,
  "58%",  # Tenants
  "n.a.", # No response (tenant)
  "53%",  # MFH residents
  "n.a."  # No response (MFH resident)
)


# Percentage values based on respondents_count
Prozent <- paste0(
  round(100 * Anzahl / respondents_count, 1),
  "%"
)

demotab <- data.frame(
  Variable = Labels,
  Respondents  = Anzahl,
  "Respondents rel." = Prozent,
    "Study population" = study_population, 
  "Study population rel." =study_population_rel,
  "German average" = German_average
)

print(demotab)


# Calculate derived variables
df <- df %>% mutate(
  People_household = ifelse(S3 == 99, NA, S3),
  Age = 2025 - S2A1,
  Education = ifelse(S6 == 4, "Yes", "No"),
  Housing = ifelse(S11 == 1, "Renter", "Owner"),
  Income = ifelse(S8 >= 5, "Yes", "No"),
  F9.1 = factor(F9.1, levels = 1:7, labels = c("Full-time", "Part-time", "Student/Trainee/Voluntary or Military Service", "Retired", "Unemployed", "Housekeeping or Parental/Caregiver Leave", "Permanently Incapacitated")),
  S12 = factor(S12, levels = 1:3, labels = c("Urban", "Suburban", "Rural")),
  S10 = factor(S10, levels = c(1, 2, 3), labels = c("Detached House", "Terraced/Semi-detached House", "Apartment in Multi-family Building")),
  S9 = factor(S9, levels = 1:16, labels = c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"))
) %>% filter(!is.na(Age))

respondents_count <- nrow(df) / 10

# Function to calculate mode
calculate_mode <- function(x) {
  x <- na.omit(x)
  if(length(x) == 0) return("-")
  tab <- table(x)
  mode_val <- names(tab[tab == max(tab)])[1]
  return(mode_val)
}

# Summary table for binary variables
binary_summary <- data.frame(
  Variable = c("Gender (male)", "Education (University degree)", "Housing (Renter)", "Household income (over EUR 4,000 net)"),
  Percent = c(
    round(10 * sum(df$S1 == 1, na.rm = TRUE) / respondents_count, 1),
    round(10 * sum(df$Education == "Yes", na.rm = TRUE) / respondents_count, 1),
    round(10 * sum(df$Housing == "Renter", na.rm = TRUE) / respondents_count, 1),
    round(10 * sum(df$Income == "Yes", na.rm = TRUE) / respondents_count, 1)
  ),
  Mean = "",
  SD = "",
  Mode = ""
)

# Summary table for numeric variables
numeric_vars <- c("Age", "People_household", "S8","educ_years")
numerical_summary <- data.frame(
  Variable = c("Age", "Number of Persons in Household", "Income class","Years of education"),  
  Percent = "",
  Mean = sapply(numeric_vars, function(var) round(mean(df[[var]], na.rm = TRUE), 1)),
  SD = sapply(numeric_vars, function(var) round(sd(df[[var]], na.rm = TRUE), 1)),
  Mode = sapply(numeric_vars, function(var) calculate_mode(df[[var]]))
)

# Summary table for categorical variables
categorical_summary <- data.frame(
  Variable = c("Employment status", "Living environment", "Type of dwelling", "Federal state"),
  Percent = "",
  Mean = "",
  SD = "",
  Mode = c(
    calculate_mode(df$F9.1),
    calculate_mode(df$S12),
    calculate_mode(df$S10),
    calculate_mode(df$S9)
  )
)

# Combine all tables
combined_table <- rbind(binary_summary, numerical_summary, categorical_summary)

# Ensure that "Barcharts" folder exists
if (!dir.exists("Barcharts")) {
  dir.create("Barcharts", recursive = TRUE)
}

# Save combined table
write.csv(combined_table, "Barcharts/Sociodemographic_Characteristics.csv", row.names = FALSE)

#=====================================================================
# Create Bar Charts
#=====================================================================

demographics <- list(
  Gender = df %>% select(S1) %>% 
    mutate(S1 = factor(S1, levels = c(2, 1, 3), labels = c("female", "male", "diverse"))) %>% na.omit(),
  
  Age = df %>% select(alter_kat) %>% 
    mutate(alter_kat = factor(alter_kat, 
                              levels = c(2, 3, 4, 5, 6),
                              labels = c("18-29 years", "30-39 years", "40-49 years", "50-64 years", "65 years and older"))) %>% na.omit(),
  
  Number_Persons_in_Household = df %>% select(S3),
  
  Education = df %>% select(S6) %>% 
    mutate(S6 = factor(S6, levels = 1:5, labels = c("No degree", "Vocational degree", "Master craftsman", "University degree", "Other"))) %>% na.omit(),
  
  Income = df %>% select(S8) %>%
    mutate(S8 = factor(S8, levels = 1:6, labels = c("below EUR 1,000", "EUR 1,000 to under EUR 2,000", "EUR 2,000 to under EUR 3,000", "EUR 3,000 to under EUR 4,000", "EUR 4,000 to under EUR 5,000", "EUR 5,000 and more"))) %>% na.omit(),
  
  Employment_status = df %>% select(F9.1) %>% 
    mutate(F9 = factor(F9.1, levels = 1:7, labels = c("Full-time", "Part-time", "Student/Trainee/Voluntary or Military Service", "Retired", "Unemployed", "Housekeeping or Parental/Caregiver Leave", "Permanently Incapacitated" ))) %>% na.omit(),
  
  Bundesland = df %>% select(S9) %>% 
    mutate(S9 = factor(S9, levels = 1:16, labels = c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"))) %>% na.omit(),
  
  Wohnhaus = df %>% select(S10) %>% 
    mutate(S10 = factor(S10, levels = c(1, 2, 3), labels = c("Detached House", "Terraced/Semi-detached House", "Apartment in Multi-family Building"))) %>% na.omit(),
  
  Wohnsituation = df %>% select(S11) %>% 
    mutate(S11 = factor(S11, levels = c(1, 2), labels = c("Renter", "Owner"))) %>% na.omit(),
  
  Wohnumgebung = df %>% select(S12) %>% 
    mutate(S12 = factor(S12, levels = 1:3, labels = c("Urban", "Suburban", "Rural"))) %>% na.omit()
)

# Create PDF file
pdf("Barcharts/Demographics.pdf", width=8, height=6)

# Create bar charts
for (var_name in names(demographics)) {
  data <- demographics[[var_name]]
  colnames(data) <- c("Kategorie")

  plot <- ggplot(data, aes(x = Kategorie)) +
    geom_bar(aes(y = ..count.. / 10), fill = "skyblue", color = "black") +
    xlab(NULL) +
    ylab("Count") +
    labs(title = var_name) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  print(plot)
}

# Close PDF file
dev.off()

