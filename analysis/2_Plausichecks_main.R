######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Plausibility check                             ###
### Output        : R-Tables                                       ###
### Date          : 31.01.2025                                     ###
######################################################################

#====================================
# Preparation
#====================================
df <- df_long

#===========================
# Environmental Awareness Score: Check for Plausibility
#===========================
#Check whether there is a large discrepancy between responses for similar questions
df_long <- df_long %>%
  mutate(env_awareness_score_plausibel = if_else(abs(F8.4_3_1 - F8.4_4_1) >= 3, "Unplausibel", "Plausibel"))

# Calculate Cronbach’s α
umwelt_items <- df_long %>% select(starts_with("F8.4_")) %>% drop_na()
alpha_result <- psych::alpha(umwelt_items)
print(alpha_result$total$raw_alpha)


#===========================
# Electricity Price and Consumption: Check for Plausibility
#===========================

df_long <- df_long %>%
  mutate(cost_per_kwh_cents = if_else(!is.na(F3.3 & F3.4A1) > 0,
                                                 round(F3.3 *100 / (F3.4A1 / 12), 2),
                                                 NA),
         cost_per_kwh_cents_plausibel = if_else(!is.na(cost_per_kwh_cents) & (cost_per_kwh_cents < 20 | cost_per_kwh_cents > 55), #price below 20 cents/kWh and price above 55cents/kWh seems unplausibel 
                                                "Unplausibel", 
                                                if_else(!is.na(cost_per_kwh_cents),"Plausibel",NA)),
         demand_per_person =  if_else(!is.na(S3 & F3.4A1) > 0,
                                     round(F3.4A1 / S3, 1),
                                     NA),
         demand_per_person_plausibel = if_else(!is.na(demand_per_person) & (demand_per_person < 350 | demand_per_person> 6000),
                                               "Unplausibel", 
                                               if_else(!is.na(demand_per_person),"Plausibel",NA))
                  )

# #====================================
# # 2. Summary Statistics for Cost per kWh
# #====================================

summary_stats_electrcity_costs <- df_long %>%
  summarise(
    count = n()/10,
    avg_cost_cents = round(mean(cost_per_kwh_cents, na.rm = TRUE), 2),
    median_cost_cents = round(median(cost_per_kwh_cents, na.rm = TRUE), 2),
    min_cost_cents = round(min(cost_per_kwh_cents, na.rm = TRUE), 2),
    max_cost_cents = round(max(cost_per_kwh_cents, na.rm = TRUE), 2),
    avg_yearly_consumption_kwh = round(mean(demand_per_person, na.rm = TRUE), 0),
    median_yearly_consumption_kwh = round(median(demand_per_person, na.rm = TRUE), 0),
    min_yearly_consumption_kwh = round(min(demand_per_person, na.rm = TRUE), 0),
    max_yearly_consumption_kwh = round(max(demand_per_person, na.rm = TRUE), 0),
    n_plausibel_cost = sum(cost_per_kwh_cents_plausibel == "Plausibel", na.rm = TRUE)/10
    )

print("Summary Statistics for Cost per kWh:")
print(summary_stats_electrcity_costs)

# #====================================
# # 3. Yearly Electricity Consumption by Household Size
# #====================================
# 
summary_stats_consumption_by_household <- df_long %>%
  summarise(
    count = n(),
    avg_consumption_pP_kwh = round(mean(demand_per_person, na.rm = TRUE), 0),
    median_consumption_pP_kwh = round(median(demand_per_person, na.rm = TRUE), 0),
    min_consumption_pP_kwh = round(min(demand_per_person, na.rm = TRUE), 0),
    max_consumption_pP_kwh = round(max(demand_per_person, na.rm = TRUE), 0),
    n_plausibel_demand = sum(demand_per_person_plausibel == "Plausibel", na.rm = TRUE)/10
  )

print("Yearly Electricity Consumption by Household Size:")
print(summary_stats_consumption_by_household)

