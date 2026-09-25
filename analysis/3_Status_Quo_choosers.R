######################################################################
### Study         : DCE Energy Sharing                             ###
### Description   : Status Quo Choice                              ###
### Output        : R-Tables                                       ###
### Date          : 01.04.2025                                     ###
######################################################################

# Load data
df <- df_long

# 1. Define the columns corresponding to the "status quo" reasons
grund_vars <- paste0("F7.2A", 1:12)

# English labels describing each reason for choosing the status quo option
grund_labels_en <- c(
  "I did not understand the proposed options.",
  "The effort involved in switching is too high for me.",
  "I am satisfied with my current electricity contract.",
  "I'm afraid of having more work to do if I buy electricity from community energy community",  # Optional: fill in the full phrase here
  "I want to wait until energy communities become more established.",
  "I want to be able to switch at any time.",
  "I want to get my electricity from an established supplier.",
  "I do not want to deal with electricity matters.",
  "I do not want to pay more than with my current provider.",
  "The €100 for a share or bond was too much for me.",
  "I did not want to commit for one year.",
  "It seemed too uncertain whether the energy community would reliably supply electricity."
)

# 2. Count how many respondents chose each reason exactly (value == 1)
grund_counts <- sapply(df[, grund_vars], function(x) sum(x == 1, na.rm = TRUE))

# 3. Combine labels and counts into a data frame for easier handling
results <- data.frame(
  Reason = grund_labels_en,
  Count = grund_counts
)

# 4. Additional count: number of respondents who selected "status quo" at least 7 times (test_defaultA1 >= 7)
test_defaultA1_count <- sum(df$test_defaultA1 >= 7, na.rm = TRUE)

# 5. Append this additional summary as a new row in the results table
results <- rbind(
  results,
  data.frame(
    Reason = "Respondents who selected 'status quo' at least 7 times status quo",
    Count = test_defaultA1_count
  )
)

# 6. Optionally sort the results by count in descending order to highlight the most common reasons
results <- results[order(-results$Count), ]

# 7. Print the final summary table without row names
print(results, row.names = FALSE)

# Define the groups by exact values of test_defaultA1: 7, 8, 9, 10
groups <- 7:10

# Prepare an empty list to store counts per group
counts_list <- list()

# Loop over each group and count how many respondents chose each reason (value == 1)
for (g in groups) {
  counts_list[[as.character(g)]] <- sapply(df[df$test_defaultA1 == g, grund_vars], function(x) sum(x == 1, na.rm = TRUE))
}

# Combine counts into a data frame
results_4groups <- data.frame(
  Times_respondend_picked_status_quo = grund_labels_en,
  "7" = counts_list[["7"]],
  "8" = counts_list[["8"]],
  "9" = counts_list[["9"]],
  "10" = counts_list[["10"]]
)

# Add total counts per reason
results_4groups$Count_total <- rowSums(results_4groups[, 2:5], na.rm = TRUE)

# Sort by total count descending
results_4groups <- results_4groups[order(-results_4groups$Count_total), ]

# Calculate number of respondents per group
num_respondents <- sapply(groups, function(g) sum(df$test_defaultA1 == g, na.rm = TRUE))

# Remove names from num_respondents to avoid mismatch
num_respondents <- as.numeric(num_respondents)

# Create a summary row ensuring all columns are numeric where needed
summary_row <- data.frame(
  Times_respondend_picked_status_quo = "Number of respondents",
  "7" = num_respondents[1],
  "8" = num_respondents[2],
  "9" = num_respondents[3],
  "10" = num_respondents[4],
  Count_total = sum(num_respondents)
)

# Append the summary row to the results table
results_4groups <- rbind(results_4groups, summary_row)

# Print the final table
print(results_4groups, row.names = FALSE)


# 2. Create the relative percentage table
results_pct <- data.frame(
  Reason = grund_labels_en,
  Pct_7 = round((counts_list[["7"]] / num_respondents[1]) * 100, 0),
  Pct_8 = round((counts_list[["8"]] / num_respondents[2]) * 100, 0),
  Pct_9 = round((counts_list[["9"]] / num_respondents[3]) * 100, 0),
  Pct_10 = round((counts_list[["10"]] / num_respondents[4]) * 100, 0)
)

# Add a summary row with 100% for each group (because it's 100% of respondents in that group)
summary_pct <- data.frame(
  Reason = "Number of respondents",
  Pct_7 = 100,
  Pct_8 = 100,
  Pct_9 = 100,
  Pct_10 = 100,
  PCT_Meant = 100
)

# Add mean value per reason
results_pct$PCT_Meant <- rowMeans(results_pct[, 2:5], na.rm = TRUE)
results_pct <- results_pct[order(-results_pct$PCT_Meant), ]
results_pct <- rbind(results_pct, summary_pct)

# Format percentages nicely as strings with %
results_pct_formatted <- results_pct
results_pct_formatted[, 2:5] <- lapply(results_pct[, 2:5], function(x) paste0(x, "%"))

# Print relative percentages table
cat("\nRelative percentages of reasons by test_defaultA1 groups:\n")
print(results_pct_formatted, row.names = FALSE)



#### Table with all respondents

# Step 1: Combine total counts across groups
results_summary <- data.frame(
  Reason = grund_labels_en,
  Count_total = rowSums(sapply(groups, function(g) {
    sapply(df[df$test_defaultA1 == g, grund_vars], function(x) sum(x == 1, na.rm = TRUE))
  }))
)

# Step 2: Calculate total number of respondents with test_defaultA1 >= 7
total_eligible_respondents <- sum(df$test_defaultA1 >= 7, na.rm = TRUE)

# Step 3: Calculate relative percentages per reason
results_summary$Pct_total <- round((results_summary$Count_total / total_eligible_respondents) * 100, 0)

# Step 4: Format percentages nicely (optional)
results_summary$Pct_total <- paste0(results_summary$Pct_total, "%")


# Step 5: Order percentages nicely (optional)
results_summary <- results_summary[order(-results_summary$Count_total), ]
# results_4groups <- results_4groups[order(-results_4groups$Count_total), ]


# Step 6: Add summary row (optional)
results_summary <- rbind(
  results_summary,
  data.frame(
    Reason = "Number of respondents (Chose Status Quo ≥ 7)",
    Count_total = total_eligible_respondents,
    Pct_total = "100%"
  )
)

# Step 7: Print final table
cat("Summary of total reasons among all respondents who chose status quo at least 7 times:\n")
print(results_summary, row.names = FALSE)



