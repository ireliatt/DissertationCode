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

scaled.X <- read.csv("~/Desktop/Mortgage/cloglog/scaled.X.csv", header = TRUE)
y <- read.csv("~/Desktop/Mortgage/cloglog/y.csv", header = TRUE)

subX <- scaled.X[seq(1, nrow(scaled.X), 10), ]
suby <- y[seq(1, nrow(y), 10),3]

N <- dim(subX)[1]
P <- 12

data_list <- list(X = subX, N = N, y = suby, P = P)

cloglog_fit <- stan(model_code = cloglog_model, data = data_list, cores = 1, chains = 2, iter = 2000, init_r = 0.5)

gev_fit <- stan(model_code = gev_model, data = data_list, cores = 1, chains = 2, iter = 2000, init_r=0.5)

print(cloglog_fit, pars = "beta")

llik_cloglog <- extract_log_lik(cloglog_fit, parameter_name = "log_lik")
loo_cloglog <- loo(llik_cloglog)
print("Cloglog model")
print(loo_cloglog)
waic_cloglog <- waic(llik_cloglog)
print(waic_cloglog)
