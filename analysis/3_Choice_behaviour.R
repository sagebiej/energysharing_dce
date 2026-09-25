#########################################################################
### Study         : DCE Energy Sharing                                ###
### Description   : Preferences vs. Choices Summary Table             ###
###                 & Descriptive Statistics for Attribute Choices.   ###
### Output        : R-Tables, R-Plots                                 ###
### Date          : 01.04.2025                                        ###
#########################################################################

# Load data
df <- df_long

#=====================================
# 1: How many respondents chose alternative 1 and alternative 2?
#====================================

# Summarize: how many people chose 2 exactly 0, 1, 2, ... times
summary_table <- df %>%
  group_by(i_NUMBER) %>%
  summarise(count_choice_2 = sum(choice == 2), .groups = "drop") %>%
  count(count_choice_2, name = "num_persons") %>%
  arrange(count_choice_2)

# Bar chart for number of people who choose 2 exactly X times
ggplot(summary_table, aes(x = factor(count_choice_2), y = num_persons)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = num_persons), vjust = -0.3, size = 4) +  # Add value labels
  labs(
    title = "How often people chose 'I stay'",
    x = "Number of times people chose 'I stay'",
    y = "Number of people"
  ) +
  theme_minimal()


total_people <- sum(summary_table$num_persons)

# Calculate relative frequency in percent
summary_table_pct <- summary_table %>%
  mutate(percent = round(100 * num_persons / total_people, 2))

print(summary_table_pct)

# Bar chart for number of people who choose 2 exactly X times
ggplot(summary_table_pct, aes(x = factor(count_choice_2), y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.3, size = 4) +
  labs(
    title = "Relative distribution: How often people chose option 'I stay'",
    x = "Number of times people chose 'I stay'",
    y = "Percentage of people"
  ) +
  theme_minimal()

#====================================
# 2a (linechart): How many respondents chose alternative 1 and alternative 2 depending on whether they are part of the new target group?
#====================================

#Choice behavior of people that have no low income, are not tenants, and do not live in a mfh building
summary_table_referencegroup <- df %>%
  filter(lowincome == 0, mfh == 0, tenant == 0) %>% 
  group_by(i_NUMBER) %>%
  summarise(count_choice_2 = sum(choice == 2), .groups = "drop") %>%
  count(count_choice_2, name = "num_persons") %>%
  arrange(count_choice_2)

#Choice behavior of people with low income
summary_table_lowincome <- df %>%
  filter(lowincome == 1) %>% 
  group_by(i_NUMBER) %>%
  summarise(count_choice_2 = sum(choice == 2), .groups = "drop") %>%
  count(count_choice_2, name = "num_persons") %>%
  arrange(count_choice_2)

print(summary_table_lowincome)

#Choice behaviour of people living in multi-family houses
summary_table_mfh <- df %>%
  filter(mfh == 1) %>% 
  group_by(i_NUMBER) %>%
  summarise(count_choice_2 = sum(choice == 2), .groups = "drop") %>%
  count(count_choice_2, name = "num_persons") %>%
  arrange(count_choice_2)

print(summary_table_mfh)

#Choice behavior of tenants
summary_table_tenant <- df %>%
  filter(tenant == 1) %>% 
  group_by(i_NUMBER) %>%
  summarise(count_choice_2 = sum(choice == 2), .groups = "drop") %>%
  count(count_choice_2, name = "num_persons") %>%
  arrange(count_choice_2)

print(summary_table_tenant)


combined_table <- summary_table %>%
  rename(all_respondents = num_persons) %>%
  full_join(summary_table_referencegroup %>% rename(Old_Target_Group = num_persons),
            by = "count_choice_2") %>%
  full_join(summary_table_lowincome %>% rename(low_income = num_persons),
            by = "count_choice_2") %>%
  full_join(summary_table_mfh %>% rename(mfh_residents = num_persons),
            by = "count_choice_2") %>%
  full_join(summary_table_tenant %>% rename(tenants = num_persons),
            by = "count_choice_2") %>%

  arrange(count_choice_2)

print(combined_table)


combined_plot <- combined_table %>%
  pivot_longer(cols = -count_choice_2, names_to = "group", values_to = "count")

ggplot(combined_plot, aes(x = count_choice_2, y = count, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  scale_x_continuous(breaks = 0:10) +
  scale_color_manual(
    values = c(
      "all_respondents" = "steelblue",
      "Old_Target_Group" = "lightblue",
      "low_income" = "darkorange",
      "mfh_residents" = "forestgreen",
      "tenants" = "firebrick"
    ),
    labels = c(
      "all_respondents" = "All Respondents",
      "Old_Target_Group" = "Old Target Group",
      "low_income" = "Low Income",
      "mfh_residents" = "MFH Residents",
      "tenants" = "Tenants"
    ),
    name = "Group"
  ) +
  labs(
    title = "Number of Persons by Group and Count Choice 2",
    x = "Number of times people chose 'I stay'",
    y = "Number of Persons",
    color = "Group"
  ) +
    theme_minimal()


# Convert data to long format
combined_plot <- combined_table %>%
  pivot_longer(cols = -count_choice_2, names_to = "group", values_to = "count")

# Calculate percentages
combined_plot_percent <- combined_plot %>%
  group_by(group) %>%
  mutate(
    total = sum(count, na.rm = TRUE),
    percent = (count / total) * 100
  ) %>%
  ungroup()

# Create line plot
ggplot(combined_plot_percent, aes(x = count_choice_2, y = percent, color = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  scale_x_continuous(breaks = 0:10) +
  
  scale_color_manual(
    values = c(
      "all_respondents" = "steelblue",
      "Old_Target_Group" = "lightblue",
      "low_income" = "darkorange",
      "mfh_residents" = "forestgreen",
      "tenants" = "firebrick"
    ),
    labels = c(
      "all_respondents" = "All Respondents",
      "Old_Target_Group" = "Old Target Group",
      "low_income" = "Low Income",
      "mfh_residents" = "MFH Residents",
      "tenants" = "Tenants"
    ),
    name = "Group"
  ) +
  
  labs(
    title = "Percentage of Persons by Group and Count Choice 2",
    x = "Number of times people chose 'I stay'",
    y = "Percentage of Persons (%)"
  ) +
  
  theme_minimal()

#====================================
# 2b (barchart): How many respondents chose alternative 1 and alternative 2 depending on task ID and block?
#====================================

# Define order of groups

combined_table_without_all_respondents <- combined_table %>%
  select(-all_respondents) 

combined_plot <- combined_table_without_all_respondents %>%
  pivot_longer(cols = -count_choice_2, names_to = "group", values_to = "count")

# combined_plot <- combined_table_without_all_respondents
# #   select(-all_respondents)  # entfernt die Spalte "all_respondents"
combined_plot$group <- factor(combined_plot$group, levels = c(
  "Old_Target_Group",
  "low_income",
  "mfh_residents",
  "tenants"
))


ggplot(combined_plot, aes(x = factor(count_choice_2), y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  scale_fill_manual(
    values = c(
      "Old_Target_Group" = "lightblue",
      "low_income" = "darkorange",
      "mfh_residents" = "forestgreen",
      "tenants" = "firebrick"
    ),
    labels = c(
      "Old_Target_Group" = "Old Target Group",
      "low_income" = "Low Income",
      "mfh_residents" = "MFH Residents",
      "tenants" = "Tenants"
    ),
    name = "Group"
  ) +
  
  labs(
    title = "Number of Persons by Group and Count Choice 2",
    x = "Number of times people chose 'I stay'",
    y = "Number of Persons"
  ) +
  
  theme_minimal()

# Set group order for percentage plot

# Calculate percentages
combined_plot_percent <- combined_plot %>%
  group_by(group) %>%
  mutate(
    total = sum(count, na.rm = TRUE),
    percent = (count / total) * 100
  ) %>%
  ungroup()

combined_plot_percent$group <- factor(combined_plot_percent$group, levels = c(
  "Old_Target_Group",
  "low_income",
  "mfh_residents",
  "tenants"
))

ggplot(combined_plot_percent, aes(x = factor(count_choice_2), y = percent, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  scale_fill_manual(
    values = c(
      "Old_Target_Group" = "lightblue",
      "low_income" = "darkorange",
      "mfh_residents" = "forestgreen",
      "tenants" = "firebrick"
    ),
    labels = c(
      "Old_Target_Group" = "Old target group",
      "low_income" = "Low income",
      "mfh_residents" = "MFH residents",
      "tenants" = "Tenants"
    ),
    name = "Group"
  ) +
  
  labs(
    #title = "Percentage of Persons by Group and Count Choice 2",
    x = "Number of times people chose 'I stay'",
    y = "Percentage of persons (%)"
  ) +
  
  theme_minimal()+
  theme(
    axis.title = element_text(size = 16),      # Achsentitel
    axis.text = element_text(size = 14),       # Achsenbeschriftung
    legend.title = element_text(size = 16),    # Legendentitel
    legend.text = element_text(size = 14)      # Legendeneinträge
  )

#====================================
# 2: How many respondents chose alternative 1 and alternative 2 depending on task ID and block?
#====================================

# 1. Absolute counts: how often was choice 1 and 2 selected per block and task
abs_counts <- df %>%
  filter(choice %in% c(1, 2)) %>%
  count(block, task_id, choice, name = "abs") %>%
  pivot_wider(
    names_from = choice,
    values_from = abs,
    names_prefix = "choice_",
    values_fill = 0
  ) %>%
  rename(
    choice_1_abs = choice_1,
    choice_2_abs = choice_2
  )

# 2. Relative counts: share of choices within each block and task
rel_counts <- df %>%
  filter(choice %in% c(1, 2)) %>%
  count(block, task_id, choice, name = "abs") %>%
  group_by(block, task_id) %>%
  mutate(rel = round(100 * abs / sum(abs), 2)) %>%
  ungroup() %>%
  select(block, task_id, choice, rel) %>%
  pivot_wider(
    names_from = choice,
    values_from = rel,
    names_prefix = "choice_",
    values_fill = 0
  ) %>%
  rename(
    choice_1_rel = choice_1,
    choice_2_rel = choice_2
  )

# 3. Join absolute and relative data into one combined table
combined_table <- abs_counts %>%
  left_join(rel_counts, by = c("block", "task_id")) %>%
  arrange(block, task_id)

# View result
print(combined_table)


#====================================
# 3: Approval rate depending on Attribute and Level
#====================================

table(df$Organisator, df$choice)
prop.table(table(df$Organisator, df$choice), margin = 1)

table(df$Participation, df$choice)
prop.table(table(df$Participation, df$choice), margin = 1)

table(df$Goal, df$choice)
prop.table(table(df$Goal, df$choice), margin = 1)

table(df$Contract, df$choice)
prop.table(table(df$Contract, df$choice), margin = 1)

table(df$Price, df$choice)
prop.table(table(df$Price, df$choice), margin = 1)


# Graphs prop tables/line graphs
##Organizer
prop_table <- prop.table(table(df$Organisator, df$choice), margin = 1)
prop_table_organisator <- as.data.frame(prop_table) %>% 
  filter(Var2 == 1) %>%
  select(-Var2) %>%
  rename(PA_ext = Var1)  %>%
  # mutate(PA_ext = as.numeric(as.character(PA_ext))) %>% 
  arrange(PA_ext)

organisator_pl <- ggplot(prop_table_organisator, aes(x = PA_ext, y = Freq)) +
  geom_line(color = "lightgreen") +          
  geom_point(color = "forestgreen", size = 3) + 
  labs(
    title = "Organisator",
    x = "Different Organisators",
    y = "Approval Rate for Change"
  ) +
  coord_cartesian(ylim = c(0.0, 1.0)) +
  theme_minimal()

##Participation
prop_table <- prop.table(table(df$Participation, df$choice), margin = 1)
prop_table_Participation <- as.data.frame(prop_table) %>% 
  filter(Var2 == 1) %>%
  select(-Var2) %>%
  rename(HNV_ext = Var1)  %>%
  arrange(HNV_ext) # Sort data by HNV_ext

Participation_pl <- ggplot(prop_table_Participation, aes(x = HNV_ext, y = Freq)) +
  geom_line(color = "peachpuff") +          
  geom_point(color = "darkorange", size = 3) +
  labs(
    title = "Participation",
    x = "Degree of Participation",
    y = "Approval Rate for Change"
  ) +
  coord_cartesian(ylim = c(0.0, 1.0)) +
  theme_minimal()

## Goal
prop_table <- prop.table(table(df$Goal, df$choice), margin = 1)
prop_table_Goal <- as.data.frame(prop_table) %>% 
  filter(Var2 == 1) %>%
  select(-Var2) %>%
  rename(Aggr_ext = Var1)  %>%
  arrange(Aggr_ext) 

Goal_pl <- ggplot(prop_table_Goal, aes(x = Aggr_ext, y = Freq)) +
  geom_line(color = "khaki") +          
  geom_point(color = "darkkhaki", size = 3) + 
  labs(
    title = "Goal",
    x = "Additional Goal",
    y = "Approval Rate for Change"
  ) +
  coord_cartesian(ylim = c(0.0, 1.0)) + # Set y-axis limits
  theme_minimal()

##Contract
prop_table <- prop.table(table(df$Contract, df$choice), margin = 1)
prop_table_Contract <- as.data.frame(prop_table) %>% 
  filter(Var2 == 1) %>%
  select(-Var2) %>%
  rename(Radius = Var1)  %>%
  arrange(Radius) 

Contract_pl <- ggplot(prop_table_Contract, aes(x = Radius, y = Freq)) +
  geom_line(color = "lavender") +          
  geom_point(color = "darkviolet", size = 3) + 
  labs(
    title = "Contract",
    x = "Contract design",
    y = "Approval Rate for Change"
  ) +
  coord_cartesian(ylim = c(0.0, 1.0)) + # Set y-axis limits
  theme_minimal()

#====================================
# 3: Approval rate depending on Attribute and level and price
#====================================

# Contract with Price
table(df$choice, df$Price, df$Contract)
round(prop.table(table(df$Price, df$choice, df$Contract), margin = c(1, 3)), 2)

average_pref_table <- tapply(df$choice, list(df$Price, df$Contract), mean, na.rm = TRUE)
average_pref_table_pa_check <- 1-(average_pref_table - 1)
average_pref_table_long <- melt(average_pref_table_pa_check)
colnames(average_pref_table_long) <- c("Relative_Costs", "Contract_Levels", "value")

pref_Contract_check_price <- ggplot(average_pref_table_long, aes(x = Relative_Costs, y = value, color = as.factor(Contract_Levels), group = Contract_Levels)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +  
  labs(title = 'Price/Contract', x = "Relative Costs", y = "Approval Rate for Change", color = "Contract:") +
  theme_minimal() +
  theme(legend.position = "bottom",    
        axis.title = element_text(size = 14),       
        axis.text = element_text(size = 12),                       
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  scale_color_brewer(palette = "Set2") +
  ylim(0, 1) +
  scale_x_continuous(breaks = seq(min(average_pref_table_long$Relative_Costs), max(average_pref_table_long$Relative_Costs), by = 3))


# Participation with Price
table(df$choice, df$Price, df$Participation)
round(prop.table(table(df$Price, df$choice, df$Participation), margin = c(1, 3)), 2)

average_pref_table <- tapply(df$choice, list(df$Price, df$Participation), mean, na.rm = TRUE)
average_pref_table_pa_check <- 1-(average_pref_table - 1)
average_pref_table_long <- melt(average_pref_table_pa_check)
colnames(average_pref_table_long) <- c("Relative_Costs", "Participation_Levels", "value")

pref_Partizipation_check_price <- ggplot(average_pref_table_long, aes(x = Relative_Costs, y = value, color = as.factor(Participation_Levels), group = Participation_Levels)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) + 
  labs(title = 'Price/Participation', x = "Relative Costs", y = "Approval Rate for Change", color = "Participation:") +
  theme_minimal() +
  theme(legend.position = "bottom",    
        axis.title = element_text(size = 14),       
        axis.text = element_text(size = 12),                       
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  scale_color_brewer(palette = "Set2") +
  ylim(0, 1)+
  scale_x_continuous(breaks = seq(min(average_pref_table_long$Relative_Costs), max(average_pref_table_long$Relative_Costs), by = 3))


# Goal with Price
table(df$choice, df$Price, df$Goal)
round(prop.table(table(df$Price, df$choice, df$Goal), margin = c(1, 3)), 2)


average_pref_table <- tapply(df$choice, list(df$Price, df$Goal), mean, na.rm = TRUE)
average_pref_table_pa_check <- 1-(average_pref_table - 1)
average_pref_table_long <- melt(average_pref_table_pa_check)
colnames(average_pref_table_long) <- c("Relative_Costs", "Goal", "value")

pref_Motiv_check_price <- ggplot(average_pref_table_long, aes(x = Relative_Costs, y = value, color = as.factor(Goal), group = Goal)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) + 
  labs(title = 'Price/Goal', x = "Relative Costs", y = "Approval Rate for Change", color = "Goal:") +
  theme_minimal() +
  theme(legend.position = "bottom",    
        axis.title = element_text(size = 14),       
        axis.text = element_text(size = 12),                       
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  scale_color_brewer(palette = "Set2") +
  ylim(0, 1)+
  scale_x_continuous(breaks = seq(min(average_pref_table_long$Relative_Costs), max(average_pref_table_long$Relative_Costs), by = 3))


# Organiser with Costs
table(df$choice, df$Price, df$Organisator)
round(prop.table(table(df$Price, df$choice, df$Organisator), margin = c(1, 3)), 2)


average_pref_table <- tapply(df$choice, list(df$Price, df$Organisator), mean, na.rm = TRUE)
average_pref_table_pa_check <- 1-(average_pref_table - 1)
average_pref_table_long <- melt(average_pref_table_pa_check)
colnames(average_pref_table_long) <- c("Relative_Costs", "Organisator", "value")

pref_Organisator_check_price <- ggplot(average_pref_table_long, aes(x = Relative_Costs, y = value, color = as.factor(Organisator), group = Organisator)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) + 
  labs(title = 'Price/Organsiator', x = "Relative Costs", y = "Approval Rate for Change", color = "Goal:") +
  theme_minimal() +
  theme(legend.position = "bottom",    
        axis.title = element_text(size = 14),       
        axis.text = element_text(size = 12),                       
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        plot.title = element_text(size = 16, hjust = 0.5)) +
  scale_color_brewer(palette = "Set2") +
  ylim(0, 1)+
  scale_x_continuous(breaks = seq(min(average_pref_table_long$Relative_Costs), max(average_pref_table_long$Relative_Costs), by = 3))


##Price
prop_table <- prop.table(table(df$Price, df$choice), margin = 1)
prop_table_Price <- as.data.frame(prop_table) %>%
  filter(Var2 == 1) %>%
  select(-Var2) %>%
  rename(Costs = Var1)  %>%
  mutate(Costs = as.numeric(as.character(Costs))) %>%
  arrange(Costs)

price_pl <- ggplot(prop_table_Price, aes(x = Costs, y = Freq)) +
  geom_line(color = "lightcoral") +
  geom_point(color = "darkred", size = 3) +
  labs(
    title = "Price",
    x = "Relative Costs",
    y = "Approval Rate for Change"
  ) +
  coord_cartesian(ylim = c(0.0, 1)) +
  theme_minimal()+
  scale_x_continuous(breaks = seq(min(average_pref_table_long$Relative_Costs), max(average_pref_table_long$Relative_Costs), by = 3))

# Combine all plots into one object
allcomb_pl <- ggarrange(
  organisator_pl,
  price_pl,
  Participation_pl,
  Goal_pl,
  Contract_pl,
  pref_Contract_check_price,
  pref_Partizipation_check_price,
  pref_Motiv_check_price,
  pref_Organisator_check_price,
  ncol = 1,     
  nrow = 1
)
#Display all graphs
allcomb_pl
#====================================
# 4: Approval rate depending on relative costs and target group
#    (x = relative costs as in the appendix figures,
#     y = approval rate, one line per target group as in Figure 2)
#    Addresses reviewer 1, question 4, point 4.
#====================================

# The four groups are the same as in section 2a and are NOT mutually
# exclusive: a respondent can be a low-income tenant living in a
# multi-family house. "Old target group" is the complement of all three.
target_groups <- list(
  Old_Target_Group = quote(lowincome == 0 & mfh == 0 & tenant == 0),
  low_income       = quote(lowincome == 1),
  mfh_residents    = quote(mfh == 1),
  tenants          = quote(tenant == 1)
)

approval_by_price <- function(data, condition) {
  data %>%
    filter(!!condition) %>%
    group_by(Price) %>%
    summarise(
      n_choices     = n(),
      n_respondents = n_distinct(i_NUMBER),
      approval      = mean(choice == 1, na.rm = TRUE),   # = 1 - (mean(choice) - 1)
      .groups = "drop"
    )
}

approval_groups <- bind_rows(lapply(names(target_groups), function(g) {
  approval_by_price(df, target_groups[[g]]) %>% mutate(group = g)
}))

approval_groups$group <- factor(approval_groups$group, levels = names(target_groups))

# Table behind the figure - the reviewer asked for a figure or a table,
# so both are produced.
approval_table <- approval_groups %>%
  mutate(approval_pct = round(100 * approval, 1)) %>%
  select(group, Price, n_respondents, n_choices, approval_pct) %>%
  arrange(group, Price)

print(as.data.frame(approval_table), row.names = FALSE)

approval_wide <- approval_groups %>%
  mutate(approval_pct = round(100 * approval, 1)) %>%
  select(group, Price, approval_pct) %>%
  pivot_wider(names_from = group, values_from = approval_pct) %>%
  arrange(Price)

print(as.data.frame(approval_wide), row.names = FALSE)

# Group sizes for the legend
group_n <- approval_groups %>%
  group_by(group) %>%
  summarise(n = max(n_respondents), .groups = "drop")

group_labels <- setNames(
  paste0(c("Old target group", "Low income", "MFH residents", "Tenants"),
         " (n = ", format(group_n$n[match(names(target_groups), group_n$group)],
                          big.mark = ","), ")"),
  names(target_groups)
)

approval_price_groups_pl <- ggplot(
    approval_groups,
    aes(x = Price, y = approval, color = group, group = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(
    values = c(
      "Old_Target_Group" = "lightblue",
      "low_income"       = "darkorange",
      "mfh_residents"    = "forestgreen",
      "tenants"          = "firebrick"
    ),
    labels = group_labels,
    name   = "Group"
  ) +
  scale_x_continuous(
    breaks = seq(min(approval_groups$Price), max(approval_groups$Price), by = 3)
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Relative costs (% above the current contract)",
    y = "Approval rate for change"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title      = element_text(size = 16),
    axis.text       = element_text(size = 14),
    legend.title    = element_text(size = 16),
    legend.text     = element_text(size = 14)
  )

print(approval_price_groups_pl)

# Export next to the other paper outputs
fig_dir <- "Hauptstudie/Paper"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(file.path(fig_dir, "Figure_approval_rate_by_price_and_group.png"),
       approval_price_groups_pl, width = 9, height = 5.5, dpi = 300)
write.csv(approval_wide,
          file.path(fig_dir, "Figure_approval_rate_by_price_and_group.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
