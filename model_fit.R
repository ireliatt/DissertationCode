options(warn=-1, message =-1)
library(dplyr)
library(ggplot2)
library(rstan)
library(reshape2)
library(shinystan)
library(loo)
library("bayesplot")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

subX <- scaled.X[seq(1, nrow(scaled.X), 10), ]
suby <- y[seq(1, nrow(y), 10)]

N <- dim(subX)[1]

data_list1 <- list(X = subX, N = N, y = suby, P = 12)
logit_fit <- stan(model_code = logit_model, data = data_list1, cores = 1, chains = 2, iter = 2000, save_warmup = FALSE)
probit_fit <- stan(model_code = probit_model, data = data_list1, cores = 1, chains = 2, iter = 2000, save_warmup = FALSE)


print(logit_fit, pars = "beta")
traceplot(logit_fit, pars = c("beta"), inc_warmup = TRUE)
pp_check(logit_fit)

posterior_vs_prior(logit_fit, group_by_parameter = TRUE, pars=c("beta"))

stan_diag(logit_fit, info = c("sample","stepsize", "treedepth","divergence"))
stan_dens(logit_fit, pars = c("beta[1]"), fill = "orange", color="blue")
stan_hist(logit_fit, pars = c("beta[1]"), fill = "orange", color="skyblue")+
  xlab(c("1"))

posterior <- as.matrix(logit_fit)
plot_title <- ggtitle("Posterior distributions","with medians and 80% intervals")
mcmc_areas(posterior, pars = c("beta[1]"),prob = 0.8) + plot_title

shinystan::launch_shinystan(logit_fit)


llik_logit <- extract_log_lik(logit_fit, parameter_name = "log_lik")
# estimate the leave-one-out cross validation error
loo_logit <- loo(llik_logit)
# print the loo statistics
print("Logit model")
print(loo_logit)
waic_logit <- waic(llik_logit)
print(waic_logit)


known_parameter <- data_frame(variable = paste0("beta[",1:(P+1),"]"), real_value = beta)
# extract params as a (draws * number of chains * number of params) array
extract(logit_fit, permute = F, pars = "beta") %>%
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



known_pr <- data_frame(variable = paste0("pr[",1:200,"]"), real_pr = pr_1)
# Extract params as a (draws * number of chains * number of params) array
plot_data <- extract(probit_fit, permuted = F, pars = c("pr")) %>%
  plyr::adply(2) %>% 
  dplyr::select(-chains) %>% 
  # Stack the chains on top of one another and drop the chains label
  melt() %>% 
#  left_join(known_pr, by = "variable") %>% # Join the known parameter table
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
