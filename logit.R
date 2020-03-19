# Load necessary libraries and set up multi-core processing for Stan
options(warn=-1, message =-1)
library(dplyr)
library(ggplot2)
library(rstan)
library(reshape2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# Generate a matrix of random numbers and values for beta
set.seed(42)
N <- 1000 # number of observations
P <- 10 # number of covariates
X <- matrix(rnorm(N*P), N, P) # generate a N*P covariate matrix of random data
beta <- rnorm(P+1) # generate random coefficients


# Draw a vector of random numbers for known Xs and beta
pr <- inverse_logit( X = X, beta = beta )
y_sim <- rbinom(N, 1, pr)
head(y_sim)
head(pr)

# n = 1000
data_list_2 <- list(X = X, N = N, y = y_sim, P = P)
logit_fit <- stan(model_code = logit_model, data = data_list_2, cores = 1, chains = 2, iter = 2000)
probit_fit <- stan(model_code = probit_model, data = data_list_2, cores = 1, chains = 2, iter = 2000, save_warmup = FALSE)
cloglog_fit <- stan(model_code = cloglog_model, data = data_list_2, cores = 1, chains = 2, iter = 2000, init_r = 0.1, save_warmup = FALSE)
gev_fit <- stan(model_code = gev_model, data = data_list_2, cores = 1, chains = 2, iter = 2000, init_r = 0.1)

# n = 200 
X_1 <- matrix(rnorm(200*P), 200, P)
pr_1 <- inverse_logit(X = X_1, beta = beta)
y_1 <- rbinom(200, 1, pr_1)

data_list_1 <- list(X = X_1, N = 200, y = y_1, P = P)
logit_fit_1 <- stan(model_code = logit_model, data = data_list_1, cores = 1, chains = 2, iter = 2000)
probit_fit_1 <- stan(model_code = probit_model, data = data_list_1, cores = 1, chains = 2, iter = 2000)
cloglog_fit_1 <- stan(model_code = cloglog_model, data = data_list_1, cores = 1, chains = 2, iter = 2000, init_r = 0.1)
gev_fit_1 <- stan(model_code = gev_model, data = data_list_1, cores = 1, chains = 2, iter = 2000, init_r = 0.1)

# n = 5000
X_3 <- matrix(rnorm(5000*P), 5000, P)
pr_3 <- inverse_logit(X = X_3, beta = beta)
y_3 <- rbinom(5000, 1, pr_3)

data_list_3 <- list(X = X_3, N = 5000, y = y_3, P = P)
logit_fit_3 <- stan(model_code = logit_model, data = data_list_3, cores = 1, chains = 2, iter = 2000)
probit_fit_3 <- stan(model_code = probit_model, data = data_list_3, cores = 1, chains = 2, iter = 2000)
cloglog_fit_3 <- stan(model_code = cloglog_model, data = data_list_3, cores = 1, chains = 2, iter = 2000, init_r = 0.1)
gev_fit_3 <- stan(model_code = gev_model, data = data_list_3, cores = 1, chains = 2, iter = 2000, init_r = 0.1)

# Check model fit with shinystan
library(shinystan)
shinystan::launch_shinystan(logit_fit)
shinystan::launch_shinystan(probit_fit)
shinystan::launch_shinystan(cloglog_fit)
shinystan::launch_shinystan(gev_fit)

# summarize the draws from each parameter
print(logit_fit, pars = "beta") # beta[1] is the constant
print(probit_fit, pars = "beta")
print(cloglog_fit, pars = "beta")
print(gev_fit, pars = c("beta","xi"))


# plot the parameter estimates against the known values
# Decalre a data frame that contains the known parameter names in one column 'variable' and their known values
known_parameter <- data_frame(variable = paste0("beta[",1:(P+1),"]"), real_value = beta)
# extract params as a (draws * number of chains * number of params) array
extract(gev_fit_3, permute = F, pars = "beta") %>%
  # stack the chains on top of one another and drop the chains label
  plyr::adply(2) %>%
  dplyr::select(-chains) %>%
  # converting from wide form to long form
  melt() %>%
  # perform a left join with the known parameters
  left_join(known_parameter, by = "variable") %>%
  # generate the plot
  ggplot(aes(x = value)) +
  geom_density(fill = "orange", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  geom_vline(aes(xintercept = real_value), colour = "red") +
  ggtitle("Actual parameter and estimates")


# approximate model's leave-one-out(LOO) cross validation error
library(loo)
# extract the log likelihood of the model
llik_logit <- extract_log_lik(logit_fit, parameter_name = "log_lik")
# estimate the leave-one-out cross validation error
loo_logit <- loo(llik_logit)
# print the loo statistics
print("Logit model")
print(loo_logit)
waic_logit <- waic(llik_logit)
print(waic_logit)

llik_probit <- extract_log_lik(probit_fit, parameter_name = "log_lik")
loo_probit <- loo(llik_probit)
print("Probit model")
print(loo_probit)
waic_probit<- waic(llik_probit)
print(waic_probit)

llik_cloglog <- extract_log_lik(cloglog_fit, parameter_name = "log_lik")
loo_cloglog <- loo(llik_cloglog)
print("Cloglog model")
print(loo_cloglog)
waic_cloglog <- waic(llik_cloglog)
print(waic_cloglog)

llik_gev <- extract_log_lik(gev_fit_3, parameter_name = "log_lik")
loo_gev <- loo(llik_gev)
print("GEV model")
print(loo_gev)
waic_gev <- waic(llik_gev)
print(waic_gev)


# generating posterior predictions for our model
known_pr <- data_frame(variable = paste0("pr[",1:5000,"]"), real_pr = pr_3)
# Extract params as a (draws * number of chains * number of params) array
plot_data <- extract(gev_fit3, permuted = F, pars = c("pr")) %>%
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Stack the chains on top of one another and drop the chains label
  melt() %>% 
  left_join(known_pr, by = "variable") %>% # Join the known parameter table
  # Convert from wide form to long form (stack the columns on one another)
  # Write out the plot
  group_by(variable) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            actual = first(real_pr)) 

plot_data %>%
  ggplot(aes(x = median)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(y = median)) +
  geom_point(aes(y = actual)) 
  #ggtitle("Actual outcomes and 95% posterior predictive interval\n") 
# for the dots, x-axis is the posterior, y-axis is the true probability
plot_data %>% summarize(proportion_within_95pc = mean(actual>=lower & actual<=upper))


known_pr <- data_frame(variable = paste0("pr[",1:200,"]"), real_pr = pr_1)
# Extract params as a (draws * number of chains * number of params) array
plot_data <- extract(gev_fit_1, permuted = F, pars = c("pr")) %>%
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Stack the chains on top of one another and drop the chains label
  melt() %>% 
  left_join(known_pr, by = "variable") %>% # Join the known parameter table
  # Convert from wide form to long form (stack the columns on one another)
  # Write out the plot
  group_by(variable) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975),
            actual = first(real_pr)) 

plot_data %>%
  ggplot(aes(x = median)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.5) + 
  geom_line(aes(y = median)) +
  geom_point(aes(y = actual)) 
#ggtitle("Actual outcomes and 95% posterior predictive interval\n") 
# for the dots, x-axis is the posterior, y-axis is the true probability
plot_data %>% summarize(proportion_within_95pc = mean(actual>=lower & actual<=upper))