library(dplyr)
library(ggplot2)

## Objective 1

set.seed(123)

# parameters
alpha = 10
beta = 8
sigma_values = c(1, 10, 25)

x_true = runif(100, min = 0, max = 10)

# generate data with observation error in x
sim_data = lapply(sigma_values, function(s) {
  x_obs_error = rnorm(100, mean = 0, sd = s*0.1)
  x_obs = x_true + x_obs_error
  y_error = rnorm(100, mean = 0, sd = s)
  y = alpha + beta * x_obs + y_error
  data.frame(x = x_obs, y = y, sigma = s)
}) %>%
  bind_rows()

fit_data = sim_data %>%
  group_by(sigma) %>%
  do({
    fit = lm(y ~ x, data = .)
    data.frame(
      x = .$x,
      y = .$y,
      sigma = .$sigma,
      alpha_hat = coef(fit)[1],
      beta_hat = coef(fit)[2]
    )
  })

ggplot(fit_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = alpha, slope = beta, color = "blue", linetype = "dashed", size = 1.2) + 
  geom_abline(aes(intercept = alpha_hat, slope = beta_hat), color = "red") +          
  facet_wrap(~ sigma, scales = "free") +
  labs(
    title = "True vs Estimated Regression Lines with Increasing Observation Error",
    subtitle = "Blue dashed = True line | Red = Estimated line",
    x = "Observed X",
    y = "Y"
  ) +
  theme_minimal()



