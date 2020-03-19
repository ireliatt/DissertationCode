logit_data <- "
functions{
/**
* @param X Data matrix (N x P)
* @param beta Coefficient vector (P +1 x 1)
* @return Return an N-vector of draws from the model
*/
vector inverse_logit(matrix X, vector beta){
vector[rows(X)] pr; 
//Fill it in
for (n in 1:rows(X))
pr[n] <- exp(beta[1] + X[n,] * beta[2:(cols(X)+1)]) / (1+exp(beta[1]+X[n,]*beta[2:(cols(X)+1)]));
return pr;
}
}
data {
//define the data inputs here
}
parameters {
//parameters we want to estimate
}
model {
//the probability model we want to estimate
}
"

logit_model <- "
functions{
  vector inverse_logit(matrix X, vector beta){
    vector[rows(X)] pr; 
    //Fill it in
    for (n in 1:rows(X))
      pr[n] <- exp(beta[1] + X[n,] * beta[2:(cols(X)+1)]) / (1+exp(beta[1]+X[n,]*beta[2:(cols(X)+1)]));
    return pr;
  }
}
data {
  int N; // number of observations
  int P; // number of covariates
  matrix[N, P] X; // covariate matrix
  int y[N]; // outcome vector
}
parameters {
  vector[P+1] beta;
}
model {
  // define the prior
  beta ~ normal(0,5);
  y ~ bernoulli_logit_glm(X, beta[1], beta[2:(P+1)]);
}
generated quantities{
  vector[N] log_lik;
  vector[N] y_sim;
  vector[N] pr;
  pr = inverse_logit(X,beta);
  for(i in 1:N){
   log_lik[i] <- y[i]*log(pr[i]) + (1 - y[i])*log(1 - pr[i]);
   y_sim[i] <- bernoulli_rng(pr[i]);
 }
}
"

probit_data <- "
functions{
/**
* @param X Data matrix (N x P)
* @param beta Coefficient vector (P +1 x 1)
* @return Return an N-vector of draws from the model
*/
vector inverse_probit(matrix X, vector beta){
vector[rows(X)] pr; 

//Fill it in
for (n in 1:rows(X))
pr[n] <- normal_cdf((beta[1] + X[n,] * beta[2:(cols(X)+1)]), 0, 1);
return pr;
}
}
data {
//define the data inputs here
}
parameters {
//parameters we want to estimate
}
model {
//the probability model we want to estimate
}
"

probit_model <- "
functions{
  vector inverse_probit(matrix X, vector beta){
    vector[rows(X)] pr; 
    //Fill it in
   for (n in 1:rows(X)){
     pr[n] <- normal_cdf((beta[1] + X[n,] * beta[2:(cols(X)+1)]), 0, 1);
   }
   return pr;
   }
}
data {
  int N; // number of observations
  int P; // number of covariates
  matrix[N, P] X; // covariate matrix
  int y[N]; // outcome vector
}
parameters {
  vector[P+1] beta;
}
model {
  vector[N] pr;
  beta ~ normal(0,5);
  pr <- inverse_probit(X, beta);
  y ~ bernoulli(pr);
}
generated quantities{
  vector[N] log_lik;
  vector[N] y_sim;
  vector[N] pr;
  pr = inverse_probit(X,beta);
  for(i in 1:N){
   log_lik[i] <- y[i]*log(pr[i]) + (1 - y[i])*log(1 - pr[i]);
    y_sim[i] <- bernoulli_rng(pr[i]);
}
}
"

gev_data <- "
functions{
/**
* @param X Data matrix (N x P)
* @param beta Coefficient vector (P+1)
* @param xi Shape parameter
* @return Return an N-vector of draws from the model
*/
vector inverse_gev(matrix X, vector beta, real xi){
vector[rows(X)] pr; 
  //Fill it in
  for (n in 1:rows(X)){
    if(xi != 0){pr[n] <- 1 - exp(-(1 - (xi*(beta[1] + X[n,]*beta[2:(cols(X)+1)])))^(-1/xi));}
    else{pr[n] <- 1 - exp(-exp(beta[1] + X[n,]*beta[2:(cols(X)+1)]));}
}
  return pr;
  }
}
data {
  //define the data inputs here
}
parameters {
  //parameters we want to estimate
}
model {
  //the probability model we want to estimate
}
"

gev_model <- "
functions{
/**
* @param X Data matrix (N x P)
* @param beta Coefficient vector (P +1 x 1)
* @return Return an N-vector of draws from the model
*/
  vector inverse_gev(matrix X, vector beta, real xi){
  vector[rows(X)] pr; 
  for (n in 1:rows(X)){
     pr[n] <- 1 - exp(-max([(1 - xi*(beta[1] + X[n,]*beta[2:(cols(X)+1)])),0])^(-1/xi));
      }
  return pr;
  }
}
data {
  int N; // number of observations
  int P; // number of covariates
  matrix[N, P] X; // covariate matrix
  int y[N]; // outcome vector
}
parameters {
  vector[P+1] beta;
  real xi;
}
model {
  vector[N] pr;
  beta ~ normal(0,5);
  -xi ~ lognormal(log(0.5),0.125);
  pr <- inverse_gev(X, beta, xi);
  y ~ bernoulli(pr);
}
generated quantities{
  vector[N] log_lik;
  vector[N] y_sim;
  vector[N] pr;
  pr = inverse_gev(X, beta, xi);
  for(i in 1:N){
    if(pr[i] == 0){log_lik[i]=0;}
    else if(pr[i]==1){log_lik[i]=0;}
    else{log_lik[i] <- y[i]*log(pr[i]) + (1 - y[i])*log(1 - pr[i]);}
    y_sim[i] <- bernoulli_rng(pr[i]);
  }
}
"

cloglog_data <- "
functions{
    /**
  * @param X Data matrix (N x P)
  * @param beta Coefficient vector (P +1 x 1)
  * @return Return an N-vector of draws from the model
  */
    vector inverse_cloglog(matrix X, vector beta){
      vector[rows(X)] pr; 
      //Fill it in
      for (n in 1:rows(X))
        pr[n] <- 1 - exp(-exp(beta[1] + X[n,]*beta[2:(cols(X)+1)]));
      return pr;
  }
}
data {
  //define the data inputs here
}
parameters {
  //parameters we want to estimate
}
model {
  //the probability model we want to estimate
}
"

cloglog_model <- "
functions{
    /**
  * @param X Data matrix (N x P)
  * @param beta Coefficient vector (P +1 x 1)
  * @return Return an N-vector of draws from the model
  */
    vector inverse_cloglog(matrix X, vector beta){
      vector[rows(X)] pr; 
     //Fill it in
     for (n in 1:rows(X))
       pr[n] <- 1 - exp(-exp(beta[1] + X[n,]*beta[2:(cols(X)+1)]));
     return pr;
  }
}
data {
  int N; // number of observations
  int P; // number of covariates
  matrix[N, P] X; // covariate matrix
  int y[N]; // outcome vector
}
parameters {
  vector[P+1] beta;
}
model {
  vector[N] pr;
  beta ~ normal(0,5);
  pr <- inverse_cloglog(X, beta);
  y ~ bernoulli(pr);
}
generated quantities{
  vector[N] log_lik;
  vector[N] y_sim;
  vector[N] pr;
  pr = inverse_cloglog(X,beta);
  for(i in 1:N){
    if(pr[i] == 0){log_lik[i]=0;}
    else if(pr[i]==1){log_lik[i]=0;}
    else{log_lik[i] <- y[i]*log(pr[i]) + (1 - y[i])*log(1 - pr[i]);}
    y_sim[i] <- bernoulli_rng(pr[i]);
  }
}
"


compiled_function <- stan_model(model_code = logit_data)
expose_stan_functions(compiled_function)

compiled_function <- stan_model(model_code = probit_data)
expose_stan_functions(compiled_function)

compiled_function <- stan_model(model_code = cloglog_data)
expose_stan_functions(compiled_function)

compiled_function <- stan_model(model_code = gev_data)
expose_stan_functions(compiled_function)
