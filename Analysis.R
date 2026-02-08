library(data.table)
library(ggplot2)
library(MCMCpack)
library(reshape2)

#### QUESTION 1:Penalty conversion difference between men and women ####
penaltyDT <- fread("./data/penalty_dataset.csv")

### A chisq test
# Filter penaltyDT for relevant event types only
penalty_success <- penaltyDT[event_type %in% c("PG", "PSG", "PM", "PSM")]

# Summarize penalties
penalties_summary <- penaltyDT[, .(Made = sum(event_type %in% c("PG", "PSG")),
                                   Missed = sum(event_type %in% c("PM", "PSM"))), by = .(gender)]

# Generate the HTML table
html_table <- knitr::kable(penalties_summary, format = "html", 
                           table.attr = "style='width:50%;' class='table table-bordered table-hover'", 
                           caption = "Penalty conversion rates by gender")

# Save this table as an HTML file
cat(html_table, file = "summary_table.html")

# Summarize to get counts of made and missed penalties for each gender
conversion_summary <- penaltyDT[, .(Made = sum(event_type %in% c("PG", "PSG")),
                                    Total = .N), by = .(gender)]

# Calculate conversion rates
conversion_summary[, ConversionRate := Made / Total]

# Create the bar plot
conversion_rate_plot <- ggplot(conversion_summary, aes(x = gender, y = ConversionRate, fill = gender)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("men" = "blue", "women" = "red")) + # Make sure gender names match your dataset
  labs(title = "Penalty Conversion Rates by Gender", x = "Gender", y = "Conversion Rate")+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold", color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 17, color = "black", hjust = 0.5),
    plot.caption = element_text(size = 14, hjust = 0.5),
    legend.position = "bottom",
    # legend.text = element_text(size=14), # Adjust legend text size if needed
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 14, color = "black")
    # panel.grid = element_blank()#,
    #plot.margin = unit(rep(-1,1), "cm")
  )


# Create a success column, where 1 = success (PG, PSG) and 0 = fail (PM, PSM)
penalty_success[, success := ifelse(event_type %in% c("PG", "PSG"), 1, 0)]

# Create a contingency table
contingency_table <- dcast(penalty_success, gender ~ success, fun.aggregate = length)

# Perform Chi-square test
chi_test_result <- chisq.test(contingency_table[,-1]) # Exclude the gender column for the test

# Chi-square test results
chi_summary <- data.table(
  Statistic = "X-squared",
  Value = round(chi_test_result$statistic, 3),
  DF = chi_test_result$parameter,
  P_Value = round(chi_test_result$p.value, 4)
)

# Generate the HTML table
html_table_chi <- knitr::kable(chi_summary, format = "html", 
                           table.attr = "style='width:50%;' class='table table-bordered table-hover'", 
                           caption = "Chi-square Test Summary")

# Save this table as an HTML file
cat(html_table_chi, file = "chi_table.html")


### B cramer v metric
n <- sum(contingency_table[,-1]) # Total sample size, excluding the first column which is usually the grouping variable
chi_squared <- chi_test_result$statistic

# For a 2x2 table, k = 2 but since (k - 1) = 1, it simplifies the formula
cramers_v <- sqrt(chi_squared / n)

# Cramer's V test results
cramers_v <- data.table(
  Metric = c("X-squared", "Degrees of Freedom", "P-Value", "Cramer's V"),
  Value = c(
    round(chi_test_result$statistic, 3), 
    chi_test_result$parameter, 
    round(chi_test_result$p.value, 4), 
    round(cramers_v, 3)
  )
)

# Now, generate the HTML table using knitr::kable
html_cramers_v <- knitr::kable(cramers_v, format = "html", 
                                   table.attr = "style='width:50%;' class='table table-bordered table-hover'", 
                                   caption = "Chi-square Test and Cramer's V Summary")

# Save this table as an HTML file
cat(html_cramers_v, file = "html_cramers_v.html")

### Bootstrap method
set.seed(123) # For reproducibility
bootstrap_differences <- replicate(10000, {
  # Create a bootstrap sample with replacement
  sample_indices <- sample(nrow(penaltyDT), replace = TRUE)
  bootstrap_sample <- penaltyDT[sample_indices]
  
  # Calculate conversion rates for men and women in this sample
  conversion_rates <- bootstrap_sample[, .(
    TotalPenalties = .N,
    SuccessfulPenalties = sum(event_type %in% c("PG", "PSG"))
  ), by = .(gender, event_type)][, .(ConversionRate = SuccessfulPenalties / TotalPenalties), by = gender]
  
  # Compute the difference in conversion rates between genders
  diff(conversion_rates$ConversionRate)
})


# bootstrap analysis results
bootstrap_summary <- data.table(
  Metric = c("Mean Difference", "Standard Deviation", "Confidence Interval Lower", "Confidence Interval Upper"),
  Value = c(
    round(mean(bootstrap_differences), 5), 
    round(sd(bootstrap_differences), 5), 
    round(quantile(bootstrap_differences, probs = c(0.025, 0.975))[1], 5), 
    round(quantile(bootstrap_differences, probs = c(0.025, 0.975))[2], 5)
  )
)

# Now, generate the HTML table using knitr::kable
html_bootstrap_summary <- knitr::kable(bootstrap_summary, format = "html", 
                                       table.attr = "style='width:50%;' class='table table-bordered table-hover'", 
                                       caption = "Bootstrap Analysis Summary")

# Save this table as an HTML file
cat(html_bootstrap_summary, file = "html_bootstrap_summary.html")

#### QUESTION 2:Best penalty takers ####

# Calculate successes and attempts
player_stats <- penaltyDT[, .(Successes = sum(event_type %in% c("PG", "PSG")),
                            Attempts = .N), by = .(player_id, player)]

# Calculate failures as Attempts - Successes
player_stats[, Failures := Attempts - Successes]


# Calculate the number of successes and attempts for each player
player_stats[, `:=` (Successes = as.integer(Successes),
                     Attempts = as.integer(Attempts),
                     Failures = Attempts - Successes)]

#filter dataset keep only values on tope 20% of attempts
quantile0.8 <- quantile(player_stats$Attempts,0.8)


####BOOTSTRAP####
# Calculate overall success rate to determine pseudo-counts (example)
overall_success_rate <- sum(player_stats$Successes) / sum(player_stats$Attempts)
pseudo_successes <- overall_success_rate * 2
pseudo_failures <- (1 - overall_success_rate) * 2

# Add pseudo-counts to each player's record
player_stats[, `:=` (AdjustedSuccesses = Successes + pseudo_successes,
                     AdjustedAttempts = Attempts + pseudo_successes + pseudo_failures)]

set.seed(1213)  # For reproducibility

bootstrap_simulation <- function(successes, attempts, n_bootstrap = 1000) {
  bootstrapped_rates <- numeric(n_bootstrap)
  
  for (i in 1:n_bootstrap) {
    # Simulate successes from a binomial distribution based on adjusted attempts and success rates
    simulated_successes <- rbinom(1, size = attempts, prob = successes / attempts)
    bootstrapped_rates[i] <- simulated_successes / attempts
  }
  
  return(list(
    Mean = mean(bootstrapped_rates),
    CI_Lower = quantile(bootstrapped_rates, probs = 0.025),
    CI_Upper = quantile(bootstrapped_rates, probs = 0.975)
  ))
}

# Apply the bootstrap simulation to each player
player_stats[, `:=`(BootstrapResults = lapply(seq_len(.N), function(i) bootstrap_simulation(AdjustedSuccesses[i], AdjustedAttempts[i])))]

# Extracting the mean, lower CI, and upper CI from the bootstrap results
player_stats[, `:=` (
  MeanRateBootstrap = sapply(BootstrapResults, function(x) x$Mean),
  LowerCIBootstrap = sapply(BootstrapResults, function(x) x$CI_Lower),
  UpperCIBootstrap = sapply(BootstrapResults, function(x) x$CI_Upper)
)]

# Removing the BootstrapResults column for clarity
player_stats[, BootstrapResults := NULL]

#bootstrap player results
penaltyTakersBootstrap <- player_stats[Attempts >= quantile0.8]

# Ranking players by mean rate
penaltyTakersBootstrap <- penaltyTakersBootstrap[order(-MeanRateBootstrap)]
penaltyTakersBootstrap <- penaltyTakersBootstrap[1:10]

# Add a new column for player names with attempts
penaltyTakersBootstrap[, player_label := paste(player, " (", Attempts, " attempts)", sep = "")]

# Plot with updated aesthetics
ggplot(penaltyTakersBootstrap, aes(x = reorder(player_label, MeanRateBootstrap), y = MeanRateBootstrap)) +
  geom_point(color = "firebrick2", size = 3) +
  geom_errorbar(aes(ymin = LowerCIBootstrap, ymax = UpperCIBootstrap), width = 0.2, color = "royalblue4") +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "#383838"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_line(color = "#cbcbcb"),
    panel.grid.minor.y = element_line(color = "#e7e7e7"),
    panel.background = element_rect(fill = "white"),
    axis.ticks.y = element_line(color = "#cbcbcb")
  ) +
  labs(
    title = "Top 10 Penalty Takers: Success Rates with Bootstrap Confidence Intervals",
    x = "Player (Number of Adjusted Attempts)",
    y = "Success Rate"
  )

player_stats[, ]

####BAYESIAN####

# Assuming a Beta(1,1) prior, which is a non-informative prior
# Loop through each player and perform the Bayesian update
results <- lapply(seq_len(nrow(player_stats)), function(i) {
  row <- player_stats[i, ]
  
  # Perform Bayesian update using MCMCbinomialbeta from MCMCpack
  post_sample <- MCMCpack::MCbinomialbeta(row$Successes, row$Attempts, 1, 1, mcmc = 10000)
  
  # Calculate posterior mean as the estimate
  post_mean <- mean(post_sample)
  
  return(c(post_mean, quantile(post_sample, probs = c(0.025, 0.975))))
})

# Convert results to a readable format
player_stats$MeanRateBayesian <- sapply(results, function(x) x[1])
player_stats$LowerCIBayesian  <- sapply(results, function(x) x[2])
player_stats$UpperCIBayesian  <- sapply(results, function(x) x[3])

#evaluating model's performance
player_stats[, actual_conversion := Successes/Attempts]
brier_score <- mean((player_stats$MeanRateBayesian - player_stats$actual_conversion)^2)

# Ranking players by mean rate
penaltyTakersBayesian <- player_stats[Attempts >= quantile0.8]
penaltyTakersBayesian <- penaltyTakersBayesian[order(-MeanRateBayesian)]
penaltyTakersBayesian <- penaltyTakersBayesian[1:10]

# Add a new column for player names with attempts
penaltyTakersBayesian[, player_label := paste(player, " (", Attempts, " attempts)", sep = "")]

# Plot with updated aesthetics
ggplot(penaltyTakersBayesian, aes(x = reorder(player_label, MeanRateBayesian), y = MeanRateBayesian)) +
  geom_point(color = "firebrick2", size = 3) +
  geom_errorbar(aes(ymin = LowerCIBayesian, ymax = UpperCIBayesian), width = 0.2, color = "royalblue4") +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "#383838"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, face = "bold"),
    panel.grid.major.y = element_line(color = "#cbcbcb"),
    panel.grid.minor.y = element_line(color = "#e7e7e7"),
    panel.background = element_rect(fill = "white"),
    axis.ticks.y = element_line(color = "#cbcbcb")
  ) +
  labs(
    title = "Top 10 Penalty Takers: Success Rates with Bayesian Confidence Intervals",
    x = "Player (Number of Adjusted Attempts)",
    y = "Success Rate"
  )

#### CUSTOM METRIC ####
player_stats$CR <- player_stats$Successes / player_stats$Attempts
player_stats$SE <- sqrt(player_stats$CR * (1 - player_stats$CR) / player_stats$Attempts)
player_stats$UA <- 1 / (1 + player_stats$SE)
player_stats$PS_norm <- player_stats$Successes / max(player_stats$Successes)

# Assuming equal weights
w1 <- w2 <- w3 <- 1/3

# Calculate the custom metric
player_stats$RateCustomMetric <- w1 * player_stats$CR + 
  w2 * player_stats$UA + 
  w3 * player_stats$PS_norm

# Ranking players by the custom metric
penaltyTakersCustom <- player_stats[Attempts >= quantile0.8]
penaltyTakersCustom <- penaltyTakersCustom[order(-RateCustomMetric)]
topPenaltyTakersCustom <- head(penaltyTakersCustom, 10)

# Add a new column for player names with attempts
topPenaltyTakersCustom[, player_label := paste(player, " (", Attempts, " attempts)", sep = "")]

# Create the plot
ggplot(topPenaltyTakersCustom, aes(x = reorder(player_label, RateCustomMetric), y = RateCustomMetric)) +
  geom_point(color = "firebrick", size = 3) +
  geom_errorbar(aes(ymin = RateCustomMetric - SE, ymax = RateCustomMetric + SE), 
                width = 0.2, color = "royalblue") +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "sans", color = "#383838"),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "bold"),
        panel.grid.major = element_line(color = "#cbcbcb"),
        panel.grid.minor = element_line(color = "#e7e7e7"),
        panel.background = element_rect(fill = "white"),
        axis.ticks = element_line(color = "#cbcbcb")) +
  labs(title = "Top 10 Penalty Takers: Custom Metric Scores",
       x = "Player (Number of Adjusted Attempts)",
       y = "Custom Metric Score")

#### FINAL ANSWER ####
# Remove the specified columns by setting them to NULL
player_stats[, `:=` (
  AdjustedSuccesses = NULL,
  AdjustedAttempts = NULL,
  CR = NULL,
  SE = NULL,
  UA = NULL,
  PS_norm = NULL,
  actual_conversion = NULL
)]

# Normalize 'MeanRateBayesian' and 'RateCustomMetric' to a 0-100 scale
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}


player_stats$NormalizedMeanRateBayesian <- normalize(player_stats$MeanRateBayesian) * 100
player_stats$NormalizedRateCustomMetric <- normalize(player_stats$RateCustomMetric) * 100

# Calculate the average of the two normalized metrics
player_stats$CombinedMetric <- rowMeans(player_stats[, c('NormalizedMeanRateBayesian', 'NormalizedRateCustomMetric')], na.rm = TRUE)

# Select the top 10 players based on the combined metric
top_players <- player_stats[order(-player_stats$CombinedMetric), ]

# Add labels with the number of attempts for the players
player_stats$player_label <- paste(player_stats$player, " (", player_stats$Attempts, " attempts)", sep = "")

# Filter for the top 5 players based on the combined metric
top_5_players <- head(player_stats[order(-player_stats$CombinedMetric), ], 5)

# Plot for the top 5 players based on the combined metric
ggplot(top_5_players, aes(x = reorder(player_label, CombinedMetric), y = CombinedMetric, fill = CombinedMetric)) +
  geom_col(width = 0.5) +
  scale_fill_gradient(name = "Penalty Taking Skill", low = "blue", high = "red") +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "#383838"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.grid.major.y = element_line(color = "#cbcbcb"),
    panel.grid.minor.y = element_line(color = "#e7e7e7"),
    panel.background = element_rect(fill = "white"),
    axis.ticks.y = element_line(color = "#cbcbcb")
  ) +
  labs(
    title = "Top 5 Penalty Takers full dataset: Penalty Taking Skill",
    x = "",
    y = "Penalty Taking Skill (0-100 scale)"
  )

# Plot for the top 5 players in the top 99% percentile by attempts
ggplot(top_5_players_99_percentile, aes(x = reorder(player_label, CombinedMetric), y = CombinedMetric, fill = CombinedMetric)) +
  geom_col(width = 0.5) +
  scale_fill_gradient(name = "Penalty Taking Skill", low = "blue", high = "red") +
  coord_flip() +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "#383838"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    panel.grid.major.y = element_line(color = "#cbcbcb"),
    panel.grid.minor.y = element_line(color = "#e7e7e7"),
    panel.background = element_rect(fill = "white"),
    axis.ticks.y = element_line(color = "#cbcbcb")
  ) +
  labs(
    title = "Top 5 Penalty Takers in the 99th Percentile: Penalty Taking Skill",
    x = "",
    y = "Penalty Taking Skill (0-100 scale)"
  )


#### QUESTION 3:Penalty takers shootout ####

#use MAN UTD for the example
manchesterUTDPenalties <- penaltyDT[team_id==123  & season_name == "2020/2021"]

#check number of penalties
nrow(manchesterUTDPenalties)

#check unique number of penalty takers
length(unique(manchesterUTDPenalties$player_id))

#keep only important columns
manchesterUTDPenaltiesReduced <- unique(manchesterUTDPenalties[,.(player_id, player,team_id,"team" = "Manchester United",season_name)])

manchesterUTDPenaltiesReduced <- merge(manchesterUTDPenaltiesReduced, 
                                player_stats[,.(player_id,Attempts, Successes, Failures, "PenaltyTakingSkill"=CombinedMetric)],
                                by = c("player_id"))

# create extra columns for identifying anxious penalties taken for these players
extraColsDT <- penaltyDT[player_id %in% manchesterUTDPenaltiesReduced$player_id]

# shootout success percentage:
extraColsDT[, `:=`(
  ShootoutAttempt = sum(event_type %in% c("PSG", "PSM")), 
  ShootoutSuccess = sum(event_type %in% c("PSG"))
), by = player_id]

extraColsDT[, ShootoutSuccessPercentage := ShootoutSuccess / ShootoutAttempt]

# Calculate the ratio for success under pressure
extraColsDT[, UnderPressure := (event_type %in% c("PSG", "PSM")) | 
                    (team_id == home_team_id & home_score <= away_score) | 
                    (team_id == away_team_id & away_score <= home_score)]

extraColsDT[, `:=`(
  UnderPressureAttempt = sum(UnderPressure),
  UnderPressureSuccess = sum(event_type %in% c("PG","PSG") & UnderPressure)
),by = player_id]

extraColsDT[, UnderPressureSuccessRatio := UnderPressureSuccess / UnderPressureAttempt]

extraColsDT <- unique(extraColsDT[,.(player_id, ShootoutAttempt, ShootoutSuccess, ShootoutSuccessPercentage, UnderPressureAttempt, UnderPressureSuccess, UnderPressureSuccessRatio)])

#add the extra columns regarding the pressure
manchesterUTDPenaltiesFinal <- merge(manchesterUTDPenaltiesReduced, extraColsDT, by = "player_id")

manchesterUTDPenaltiesFinal[is.na(manchesterUTDPenaltiesFinal)] <- 0

manchesterUTDPenaltiesFinal <- manchesterUTDPenaltiesFinal[order(-Attempts,-PenaltyTakingSkill)]

# Generate the HTML table
html_table_manutd <- knitr::kable(manchesterUTDPenaltiesFinal, format = "html", 
                           table.attr = "style='width:50%;' class='table table-bordered table-hover'", 
                           caption = "Comprehensive Penalty Performance Analysis of Manchester United Players (2020/2021 Season)")

# Save this table as an HTML file
cat(html_table_manutd, file = "man_utd.html")

manchesterUTDPenaltiesFinal[, "ConversionRatio" := Successes/Attempts]

# Filter the top 5 players by attempts
top_players <- manchesterUTDPenaltiesFinal[order(-Attempts)][1:5]

# Order the players as specified
top_players <- top_players[order(match(top_players$player, c("Edinson Cavani", "Bruno Fernandes", "Marcus Rashford", "Anthony Martial", "Juan Mata")))]

# Melting the data
melted_data <- melt(top_players, id.vars = 'player', 
                    measure.vars = c("Attempts", "ConversionRatio","PenaltyTakingSkill", 
                                     "ShootoutAttempt", "ShootoutSuccessPercentage", 
                                     "UnderPressureAttempt", "UnderPressureSuccessRatio"))

# Normalize the values column by each measured variable
melted_data[, value := normalize(value), by = variable]

# Ensure the player order
player_order <- c(  "Juan Mata",  "Anthony Martial","Marcus Rashford", "Bruno Fernandes","Edinson Cavani")

# Generate the heatmap plot
ggplot(melted_data, aes(x = variable, y = factor(player, levels = player_order), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5, name = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, family = "sans", color = "#383838"),
        axis.text.y = element_text(size = 12, family = "sans"),
        plot.title = element_text(size = 16, face = "bold", family = "sans"),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
  labs(
    title = "Manchester United Penalty Shootout Strategy: Top 5 Takers Ranked",
    subtitle = "Color Scale: Blue (Low) — White (Medium) — Red (High)",
    caption = "Note: The color scale represents normalized performance metrics."
  )



