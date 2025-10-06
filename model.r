# Senate Model with Correlated State Errors
# Based on the 2016 presidential model's correlation structure

library(tidyverse)
library(mgcv)
library(MASS)
library(boot)
library(dplyr)
library(lqmm)

# Load the correlation matrix from the 2016 model
# This captures demographic and regional similarities between states
state_data <- read.csv("potus_results_76_16.csv")
census <- read.csv('acs_2013_variables.csv')
urbanicity <- read.csv('urbanicity_index.csv')
white_evangel_pct <- read.csv('white_evangel_pct.csv')

# Build state correlation matrix (same as 2016 model)
build_state_correlation <- function() {
  # Combine state characteristics
  state_data_combined <- state_data %>% 
    dplyr::select(year, state, dem) %>%
    filter(year == 2016) %>%
    dplyr::select(state, value = dem)
  
  census_long <- census %>%
    filter(!is.na(state)) %>% 
    dplyr::select(-c(state_fips, pop_total, pop_density)) %>%
    gather(variable, value, -state)
  
  urbanicity_long <- urbanicity %>%
    dplyr::select(state, pop_density = average_log_pop_within_5_miles) %>%
    gather(variable, value, -state)
  
  evangel_long <- white_evangel_pct %>%
    gather(variable, value, -state)
  
  # Combine all features
  all_features <- bind_rows(
    census_long,
    urbanicity_long,
    evangel_long
  ) %>%
    group_by(variable) %>%
    mutate(value = (value - min(value, na.rm=T)) / 
             (max(value, na.rm=T) - min(value, na.rm=T))) %>%
    spread(state, value) %>%
    na.omit() %>%
    ungroup() %>%
    dplyr::select(-variable)
  
  # Compute correlation matrix
  C <- cor(all_features)
  C[C < 0] <- 0  # Set negative correlations to 0
  
  # Mix with matrix of 1s to increase baseline correlation
  lambda <- 0.75
  C_1 <- matrix(data = 1, nrow = ncol(C), ncol = ncol(C))
  state_correlation <- lambda * C + (1 - lambda) * C_1
  
  # Make positive definite
  state_correlation <- make.positive.definite(state_correlation)
  
  return(state_correlation)
}

# Create covariance matrix with specified national variance
create_state_covariance <- function(correlation_matrix, national_variance, state_weights) {
  n_states <- nrow(correlation_matrix)
  
  # Ensure weights are a column vector with correct dimensions
  if(length(state_weights) != n_states) {
    stop(sprintf("Weight vector length (%d) doesn't match correlation matrix dimension (%d)", 
                 length(state_weights), n_states))
  }
  
  weights_matrix <- matrix(state_weights, ncol = 1)
  
  # Scale to achieve desired national variance
  # This ensures the weighted average variance matches our target
  current_variance <- as.numeric(t(weights_matrix) %*% correlation_matrix %*% weights_matrix)
  
  # Ensure national_variance is a scalar
  national_variance <- as.numeric(national_variance)[1]
  
  # Calculate scaling factor as a single number
  scaling_factor <- as.numeric(sqrt(national_variance / current_variance))
  
  # Apply scalar multiplication
  covariance_matrix <- correlation_matrix * (scaling_factor^2)
  
  return(covariance_matrix)
}

# Load polling data and calculate errors
polling_data <- read.csv("previous_2week_averages_with_actuals.csv")

polling_data <- polling_data %>%
  mutate(
    rep_polling_error = rep_actual - weighted_rep,
    dem_polling_error = dem_actual - weighted_dem
  )

# Calculate national errors
polling_data <- polling_data %>%
  group_by(election_year, days_before_election) %>%
  mutate(
    rep_national_polling_error = mean(rep_polling_error, na.rm = TRUE),
    dem_national_polling_error = mean(dem_polling_error, na.rm = TRUE)
  ) %>%
  ungroup()

# Estimate time-varying error parameters
estimate_error_parameters <- function(polling_data) {
  # National error models
  national_errors <- polling_data %>%
    group_by(election_year, days_before_election) %>%
    slice(1) %>%
    ungroup() %>%
    filter(!is.na(rep_national_polling_error) & 
           !is.na(dem_national_polling_error))
  
  # Fit GAM models for mean
  rep_mean_model <- gam(rep_national_polling_error ~ s(days_before_election, k = 20), 
                        data = national_errors, method = "REML")
  dem_mean_model <- gam(dem_national_polling_error ~ s(days_before_election, k = 20), 
                        data = national_errors, method = "REML")
  
  # Estimate time-varying variance
  national_errors$rep_residual <- residuals(rep_mean_model)
  national_errors$dem_residual <- residuals(dem_mean_model)
  
  rep_var_model <- gam(I(abs(rep_residual)) ~ s(days_before_election, k = 15), 
                       data = national_errors, method = "REML")
  dem_var_model <- gam(I(abs(dem_residual)) ~ s(days_before_election, k = 15), 
                       data = national_errors, method = "REML")
  
  # State-level errors (beyond national)
  state_errors <- polling_data %>%
    mutate(
      rep_state_error = rep_polling_error - rep_national_polling_error,
      dem_state_error = dem_polling_error - dem_national_polling_error
    ) %>%
    filter(!is.na(rep_state_error) & !is.na(dem_state_error))
  
  # State error variance models - aggregate first, then model
  state_error_summary <- state_errors %>% 
    group_by(days_before_election) %>% 
    summarise(
      rep_state_sd = sd(rep_state_error, na.rm = TRUE),
      dem_state_sd = sd(dem_state_error, na.rm = TRUE),
      n_obs = n()
    ) %>%
    filter(!is.na(rep_state_sd) & !is.na(dem_state_sd) & n_obs >= 3)
  
  # Fit models on aggregated data
  rep_state_var_model <- gam(rep_state_sd ~ s(days_before_election, k = min(10, nrow(state_error_summary)-1)), 
                             data = state_error_summary,
                             method = "REML")
  dem_state_var_model <- gam(dem_state_sd ~ s(days_before_election, k = min(10, nrow(state_error_summary)-1)), 
                             data = state_error_summary,
                             method = "REML")
  
  return(list(
    rep_mean = rep_mean_model,
    dem_mean = dem_mean_model,
    rep_var = rep_var_model,
    dem_var = dem_var_model,
    rep_state_var = rep_state_var_model,
    dem_state_var = dem_state_var_model
  ))
}

# Load priors and 2026 polling
priors <- read.csv("state_priors_abbrev.csv")
polling_2026 <- read.csv("2026_Senate_polling_averages.csv")
polling_2026$day <- as.Date(polling_2026$day)

# Setup dates
election_day <- as.Date("2026-11-03")
today <- as.Date("2025-09-13")
days_until_election <- as.numeric(election_day - today)

# Get states with Senate races
senate_states <- unique(priors$state)
n_senate_states <- length(senate_states)

# Build correlation matrix for Senate states
state_correlation_full <- build_state_correlation()

# Build correlation matrix for Senate states
state_correlation_full <- build_state_correlation()

# Extract correlation for just Senate states
# Match state names to correlation matrix columns (need to handle case differences)
state_names_in_matrix <- colnames(state_correlation_full)

# Convert senate states to match correlation matrix format
# Try different formats: uppercase abbreviations first
senate_state_indices <- match(toupper(substr(senate_states, 1, 2)), state_names_in_matrix)

# If that doesn't work, try full state names
if(all(is.na(senate_state_indices))) {
  # Load state name to abbreviation mapping
  state_abbrev <- data.frame(
    name_lower = tolower(c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", 
                           "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho", 
                           "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", 
                           "maine", "maryland", "massachusetts", "michigan", "minnesota", 
                           "mississippi", "missouri", "montana", "nebraska", "nevada", 
                           "new hampshire", "new jersey", "new mexico", "new york", 
                           "north carolina", "north dakota", "ohio", "oklahoma", "oregon", 
                           "pennsylvania", "rhode island", "south carolina", "south dakota", 
                           "tennessee", "texas", "utah", "vermont", "virginia", "washington", 
                           "west virginia", "wisconsin", "wyoming")),
    abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", 
             "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
             "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", 
             "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
             "WI", "WY")
  )
  
  # Convert senate states to abbreviations
  senate_states_abbr <- state_abbrev$abbr[match(tolower(senate_states), state_abbrev$name_lower)]
  senate_state_indices <- match(senate_states_abbr, state_names_in_matrix)
}

# Remove any states not found
valid_indices <- senate_state_indices[!is.na(senate_state_indices)]
valid_states <- senate_states[!is.na(senate_state_indices)]

if(length(valid_indices) == 0) {
  stop("No Senate states could be matched to the correlation matrix. Check state name formats.")
}

if(length(valid_indices) < length(senate_states)) {
  warning(sprintf("Could not find correlation data for %d states out of %d", 
                  sum(is.na(senate_state_indices)), length(senate_states)))
  cat("Unmatched states:", senate_states[is.na(senate_state_indices)], "\n")
}

# Extract submatrix for Senate states
state_correlation_senate <- state_correlation_full[valid_indices, valid_indices]
colnames(state_correlation_senate) <- valid_states
rownames(state_correlation_senate) <- valid_states

# Calculate state weights (could use electoral votes or population)
# For now, use equal weights
state_weights_senate <- rep(1/length(valid_states), length(valid_states))
names(state_weights_senate) <- valid_states

# Estimate error models
error_models <- estimate_error_parameters(polling_data)

# Get error parameters for current day
get_error_parameters <- function(days_before, models) {
  # National error means
  rep_mean <- predict(models$rep_mean, 
                     newdata = data.frame(days_before_election = days_before))
  dem_mean <- predict(models$dem_mean, 
                     newdata = data.frame(days_before_election = days_before))
  
  # National error SDs
  rep_sd <- abs(predict(models$rep_var, 
                       newdata = data.frame(days_before_election = days_before))) * 1.5
  dem_sd <- abs(predict(models$dem_var, 
                       newdata = data.frame(days_before_election = days_before))) * 1.5
  
  # State error SDs (beyond national)
  rep_state_sd <- abs(predict(models$rep_state_var, 
                             newdata = data.frame(days_before_election = days_before)))
  dem_state_sd <- abs(predict(models$dem_state_var, 
                             newdata = data.frame(days_before_election = days_before)))
  
  # Handle potential NAs
  if(is.na(rep_state_sd)) rep_state_sd <- 0.02
  if(is.na(dem_state_sd)) dem_state_sd <- 0.02
  
  return(list(
    rep_national_mean = rep_mean,
    rep_national_sd = rep_sd,
    dem_national_mean = dem_mean,
    dem_national_sd = dem_sd,
    rep_state_sd = rep_state_sd,
    dem_state_sd = dem_state_sd
  ))
}

# Create Bayesian estimates for today
create_bayesian_estimates <- function(priors, polling_2026, target_date, election_day) {
  days_until <- as.numeric(election_day - target_date)
  
  estimates <- priors %>%
    mutate(
      other_prior = 100 - rep_prior - dem_prior,
      rep_estimate = rep_prior,
      dem_estimate = dem_prior,
      other_estimate = other_prior
    )
  
  # Update with available polling
  for(i in 1:nrow(estimates)) {
    poll_data <- polling_2026 %>%
      filter(day == target_date, state == estimates$state[i])
    
    if(nrow(poll_data) > 0) {
      sample_size <- poll_data$median_sample_size[1]
      
      # Weight polling more heavily as election approaches
      if(days_until <= 84) {
        prior_weight <- 100 * (days_until / 84)
      } else {
        prior_weight <- 500
      }
      poll_weight <- sample_size
      
      # Bayesian update
      estimates$rep_estimate[i] <- (estimates$rep_prior[i] * prior_weight + 
                                   poll_data$rep_weighted_avg[1] * poll_weight) / 
                                  (prior_weight + poll_weight)
      estimates$dem_estimate[i] <- (estimates$dem_prior[i] * prior_weight + 
                                   poll_data$dem_weighted_avg[1] * poll_weight) / 
                                  (prior_weight + poll_weight)
      estimates$other_estimate[i] <- (estimates$other_prior[i] * prior_weight + 
                                     poll_data$other_weighted_avg[1] * poll_weight) / 
                                    (prior_weight + poll_weight)
    }
  }
  
  # Normalize to 100%
  estimates <- estimates %>%
    mutate(
      total = rep_estimate + dem_estimate + other_estimate,
      rep_estimate = rep_estimate * 100 / total,
      dem_estimate = dem_estimate * 100 / total,
      other_estimate = other_estimate * 100 / total
    )
  
  return(estimates)
}

# Get today's estimates
today_estimates <- create_bayesian_estimates(priors, polling_2026, today, election_day)

# Ensure estimates are in same order as correlation matrix
today_estimates <- today_estimates %>%
  filter(state %in% valid_states) %>%
  arrange(match(state, valid_states))

# Run simulations with correlated errors
cat("\n===========================================\n")
cat("Running Senate Simulations with Correlated State Errors\n")
cat("===========================================\n")
cat(sprintf("Date: %s\n", today))
cat(sprintf("Days until election: %d\n", days_until_election))

# Get error parameters for today
error_params <- get_error_parameters(days_until_election, error_models)

cat("\nError parameters for simulation:\n")
cat(sprintf("National error means - Rep: %.3f, Dem: %.3f\n", 
           error_params$rep_national_mean, error_params$dem_national_mean))
cat(sprintf("National error SDs - Rep: %.3f, Dem: %.3f\n", 
           error_params$rep_national_sd, error_params$dem_national_sd))
cat(sprintf("State error SDs - Rep: %.3f, Dem: %.3f\n", 
           error_params$rep_state_sd, error_params$dem_state_sd))

# Create covariance matrices for state errors
rep_state_covariance <- create_state_covariance(
  state_correlation_senate,
  error_params$rep_state_sd^2,
  state_weights_senate
)

dem_state_covariance <- create_state_covariance(
  state_correlation_senate,
  error_params$dem_state_sd^2,
  state_weights_senate
)

# Run simulations
n_simulations <- 10000
set.seed(6202)

rep_seat_totals <- numeric(n_simulations)
state_win_matrix <- matrix(0, nrow = n_simulations, ncol = length(valid_states))
colnames(state_win_matrix) <- valid_states

for(sim in 1:n_simulations) {
  # Draw national errors
  rep_national_error <- rnorm(1, error_params$rep_national_mean, error_params$rep_national_sd)
  dem_national_error <- rnorm(1, error_params$dem_national_mean, error_params$dem_national_sd)
  
  # Draw correlated state errors
  rep_state_errors <- mvrnorm(1, rep(0, length(valid_states)), rep_state_covariance)
  dem_state_errors <- mvrnorm(1, rep(0, length(valid_states)), dem_state_covariance)
  
  # Apply errors to each state
  for(i in 1:length(valid_states)) {
    # Get base estimates
    base_rep <- today_estimates$rep_estimate[i]
    base_dem <- today_estimates$dem_estimate[i]
    base_other <- today_estimates$other_estimate[i]
    
    # Apply national and state-specific errors
    final_rep <- base_rep + rep_national_error + rep_state_errors[i]
    final_dem <- base_dem + dem_national_error + dem_state_errors[i]
    final_other <- base_other  # No error for other
    
    # Ensure non-negative and normalize
    final_rep <- max(0, final_rep)
    final_dem <- max(0, final_dem)
    final_other <- max(0, final_other)
    
    total <- final_rep + final_dem + final_other
    if(total > 0) {
      final_rep <- final_rep / total * 100
      final_dem <- final_dem / total * 100
    }
    
    # Determine winner
    if(final_rep > final_dem && final_rep > final_other) {
      state_win_matrix[sim, i] <- 1
      rep_seat_totals[sim] <- rep_seat_totals[sim] + 1
    }
  }
  
  if(sim %% 1000 == 0) {
    cat(sprintf("Completed %d simulations - P(GOP ≥19) = %.1f%%\n", 
               sim, mean(rep_seat_totals[1:sim] >= 19) * 100))
  }
}

# Calculate results
prob_rep_control <- mean(rep_seat_totals >= 19)
mean_rep_seats <- mean(rep_seat_totals)
median_rep_seats <- median(rep_seat_totals)
mode_rep_seats <- as.numeric(names(sort(table(rep_seat_totals), decreasing = TRUE)[1]))

# Results summary
cat("\n===========================================\n")
cat("SIMULATION RESULTS WITH CORRELATED ERRORS\n")
cat("===========================================\n")
cat(sprintf("Probability Republicans retain control (≥19 seats): %.1f%%\n", 
           prob_rep_control * 100))
cat(sprintf("Mean Republican seats: %.2f\n", mean_rep_seats))
cat(sprintf("Median Republican seats: %.0f\n", median_rep_seats))
cat(sprintf("Mode Republican seats: %d\n", mode_rep_seats))
cat(sprintf("95%% CI: [%.0f, %.0f]\n", 
           quantile(rep_seat_totals, 0.025), 
           quantile(rep_seat_totals, 0.975)))

# State-by-state probabilities
state_probs <- data.frame(
  state = valid_states,
  rep_win_prob = colMeans(state_win_matrix) * 100
) %>%
  arrange(desc(rep_win_prob))

cat("\nState-by-State Republican Win Probabilities:\n")
cat("============================================\n")
print(state_probs)

# Identify key correlations in outcomes
cat("\nKey State Correlations in Outcomes:\n")
cat("====================================\n")

# Pick a few swing states if they exist
swing_states <- state_probs %>%
  filter(rep_win_prob > 30 & rep_win_prob < 70) %>%
  pull(state)

if(length(swing_states) >= 2) {
  swing_indices <- match(swing_states[1:min(4, length(swing_states))], valid_states)
  outcome_correlations <- cor(state_win_matrix[, swing_indices])
  colnames(outcome_correlations) <- swing_states[1:min(4, length(swing_states))]
  rownames(outcome_correlations) <- swing_states[1:min(4, length(swing_states))]
  
  cat("\nOutcome correlations between key swing states:\n")
  print(round(outcome_correlations, 2))
}

# Save results
simulation_results <- list(
  date = today,
  days_until_election = days_until_election,
  n_simulations = n_simulations,
  prob_rep_control = prob_rep_control,
  mean_seats = mean_rep_seats,
  median_seats = median_rep_seats,
  seat_distribution = table(rep_seat_totals),
  state_probabilities = state_probs,
  correlation_matrix = state_correlation_senate,
  error_parameters = error_params
)

saveRDS(simulation_results, "senate_2026_correlated_simulation.rds")
cat("\nResults saved to 'senate_2026_correlated_simulation.rds'\n")

# Create visualization
library(ggplot2)

hist_data <- data.frame(seats = rep_seat_totals)

p <- ggplot(hist_data, aes(x = seats)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, 
                fill = "red", alpha = 0.6, color = "darkred") +
  geom_vline(xintercept = 19, linetype = "dashed", size = 1.2) +
  geom_vline(xintercept = mean_rep_seats, linetype = "solid", color = "blue") +
  annotate("text", x = 19, y = 0.08, label = "Control\nThreshold", hjust = -0.2) +
  labs(
    title = "Senate 2026: Distribution of Republican Seats",
    subtitle = sprintf("With Correlated State Errors | P(GOP Control) = %.1f%%", 
                      prob_rep_control * 100),
    x = "Republican Seats",
    y = "Density"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(10, 30, 2))

print(p)

ggsave("senate_2026_correlated_histogram.png", p, width = 10, height = 6, dpi = 300)
cat("Histogram saved to 'senate_2026_correlated_histogram.png'\n")