library(dplyr)
library(lubridate)
library(tidyr)

# Read the CSV file
polling_data <- read.csv("2026_Senate_polling_data.csv", stringsAsFactors = FALSE)

# Clean the data - remove rows with missing essential information
polling_data <- polling_data %>%
  filter(!is.na(state) & state != "" & 
           !is.na(end_date) & end_date != "" & 
           !is.na(sample_size) & sample_size > 0)

# Convert end_date to Date format
polling_data$end_date <- as.Date(polling_data$end_date)

# Define election day
election_day <- as.Date("2026-11-03")

# Create sequence of 434 days preceding election day
days_sequence <- seq(from = election_day - 433, to = election_day, by = "day")

# Get unique states from the data
unique_states <- unique(polling_data$state)

# Create a data frame with all combinations of days and states
all_combinations <- expand.grid(
  day = days_sequence,
  state = unique_states,
  stringsAsFactors = FALSE
)

# Function to calculate median sample size for a given day and state
calculate_median_sample_size <- function(current_day, current_state, polling_df) {
  # Get polls from current day and previous 27 days
  window_start <- current_day - 27
  
  relevant_polls <- polling_df %>%
    filter(state == current_state & 
             end_date >= window_start & 
             end_date <= current_day)
  
  if (nrow(relevant_polls) == 0) {
    return(NA)
  } else {
    return(median(relevant_polls$sample_size))
  }
}

# Function to calculate weighted polling average
calculate_weighted_average <- function(current_day, current_state, polling_df, metric) {
  # Get polls from current day and previous 27 days
  window_start <- current_day - 27
  
  relevant_polls <- polling_df %>%
    filter(state == current_state & 
             end_date >= window_start & 
             end_date <= current_day &
             !is.na(get(metric)))
  
  if (nrow(relevant_polls) == 0) {
    return(NA)
  }
  
  # Calculate median sample size for this period
  median_sample_size <- median(relevant_polls$sample_size)
  
  # Calculate weights for each poll
  relevant_polls <- relevant_polls %>%
    mutate(
      # Sample size weight component
      sample_weight = sqrt(sample_size) / sqrt(median_sample_size),
      # Time decay weight component
      days_ago = as.numeric(current_day - end_date),
      time_weight = (1 - 0.15)^days_ago,
      # Combined weight
      total_weight = sample_weight * time_weight,
      # Weighted value
      weighted_value = get(metric) * total_weight
    )
  
  # Calculate weighted average
  weighted_avg <- sum(relevant_polls$weighted_value) / sum(relevant_polls$total_weight)
  
  return(weighted_avg)
}

# Calculate median sample size for each day-state combination
print("Calculating median sample sizes...")
all_combinations$median_sample_size <- mapply(
  calculate_median_sample_size,
  all_combinations$day,
  all_combinations$state,
  MoreArgs = list(polling_df = polling_data)
)

# Calculate weighted averages for Republican, Democrat, and Other percentages
print("Calculating weighted polling averages...")

# Republican averages
all_combinations$rep_weighted_avg <- mapply(
  calculate_weighted_average,
  all_combinations$day,
  all_combinations$state,
  MoreArgs = list(polling_df = polling_data, metric = "rep")
)

# Democrat averages
all_combinations$dem_weighted_avg <- mapply(
  calculate_weighted_average,
  all_combinations$day,
  all_combinations$state,
  MoreArgs = list(polling_df = polling_data, metric = "dem")
)

# Other averages
all_combinations$other_weighted_avg <- mapply(
  calculate_weighted_average,
  all_combinations$day,
  all_combinations$state,
  MoreArgs = list(polling_df = polling_data, metric = "other")
)

# Filter out rows where we have no polling data (all NA values)
final_results <- all_combinations %>%
  filter(!is.na(median_sample_size)) %>%
  arrange(state, day)

# Display summary
print("Summary of results:")
print(paste("States included:", paste(unique(final_results$state), collapse = ", ")))
print(paste("Date range:", min(final_results$day), "to", max(final_results$day)))
print(paste("Total day-state combinations with polling data:", nrow(final_results)))

# Show sample of results
print("\nSample of weighted polling averages (last 10 days with data):")
sample_results <- final_results %>%
  arrange(desc(day)) %>%
  head(10) %>%
  select(day, state, median_sample_size, rep_weighted_avg, dem_weighted_avg, other_weighted_avg)

print(sample_results)

# Save results to CSV
write.csv(final_results, "2026_Senate_polling_averages.csv", row.names = FALSE)
print("\nResults saved to '2026_Senate_polling_averages.csv'")

# Create a summary by state showing the most recent polling average
latest_by_state <- final_results %>%
  group_by(state) %>%
  filter(day == max(day)) %>%
  select(state, day, median_sample_size, rep_weighted_avg, dem_weighted_avg, other_weighted_avg) %>%
  ungroup()

print("\nMost recent weighted polling averages by state:")
print(latest_by_state)