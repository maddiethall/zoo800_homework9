######################
##### HOMEWORK 9 #####
#### Maddie Thall ####
######################

library(dplyr)
library(ggplot2)

## Objective 1

set.seed(123)

# parameters
alpha = 10
beta = 8
sigma_values = c(1, 10, 25)

x = runif(100, min = 0, max = 10)

# generate data
sim_data = lapply(sigma_values, function(s) {
  error = rnorm(100, mean = 0, sd = s)
  y = alpha + beta * x + error
  data.frame(x = x, y = y, sigma = s)
}) %>%
  bind_rows()

# convert to factors for plotting
sim_data$sigma = factor(sim_data$sigma, levels = sigma_values,
                        labels = paste("σ =", sigma_values))
# plot data
ggplot(sim_data, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ sigma, nrow = 1) +
  labs(title = "Effects of Increasing Sigma Values on Linear Regression",
       x = "Predictor (x)",
       y = "Response (y)") +
  theme_minimal()

## Objective 2

set.seed(123)

# parameters
prob = 0.55
n_sim = 100
n_trials = 1:20
alpha_2 = 0.05


# generate data
power_55 = sapply(n_trials, function(n_flips) {
  sig_count = sum(replicate(n_sim, {
    heads = rbinom(n_flips, size = 1, prob = prob)
    test = binom.test(sum(heads), n_flips, p = 0.5, alternative = "greater")
    test$p.value < alpha_2
  }))
  sig_count / n_sim
})

df_55 = data.frame(
  flips = n_trials,
  power = power_55
)

# plot results
ggplot(df_55, aes(x = flips, y = power)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Statistical Power vs. Number of Coin Flips",
       x = "Number of Coin Flips",
       y = "Statistical Power") +
  theme_minimal()

# simulation for p = 0.6 

prob2 = 0.6
power_60 = sapply(n_trials, function(n_flips) {
  sig_count = sum(replicate(n_sim, {
    heads = rbinom(n_flips, size = 1, prob = prob2)
    test = binom.test(sum(heads), n_flips, p = 0.5, alternative = "greater")
    test$p.value < alpha_2
  }))
  sig_count / n_sim
})

df_60 = data.frame(
  flips = n_trials,
  power = power_60
)

# simulation for p = 0.65
prob3 = 0.65
power_65 = sapply(n_trials, function(n_flips) {
  sig_count = sum(replicate(n_sim, {
    heads = rbinom(n_flips, size = 1, prob = prob3)
    test = binom.test(sum(heads), n_flips, p = 0.5, alternative = "greater")
    test$p.value < alpha_2
  }))
  sig_count / n_sim
})

df_65 = data.frame(
  flips = n_trials,
  power = power_65
)
# combine dataframes
combined_power = rbind(
  data.frame(df_55, p = factor(prob)),
  data.frame(df_60, p = factor(prob2)),
  data.frame(df_65, p = factor(prob3))
)

# plot combined results
ggplot(combined_power, aes(x = flips, y = power, color = p)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Detection Power for Biased Coins",
       x = "Number of Coin Flips",
       y = "Proportion of Significant Tests",
       color = "True p") +
  theme_minimal()
