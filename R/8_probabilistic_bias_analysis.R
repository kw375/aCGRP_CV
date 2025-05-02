
### Probabilistic Bias Analysis for Unmeasured Confounding ###
library(tidyverse)
library(trapezoid)


# fun to print result
RR_adj_output <- function(x) {
  print(paste("RR_adj (95% CI) = ",
              round(mean(x), 2), 
              " (", 
              round(quantile(x, probs = 0.025), 2), 
              " - ", 
              round(quantile(x, probs = 0.975), 2), 
              ")",
              sep = ""))
  }

###################################################################
# Negative-binomial distribution for headache days
bias_nb <- function(obs_mu = obs_mu, 
                    obs_se = obs_se, 
                    cd_mu = cd_mu,
                    cd_se = cd_se) {
  
  # RR_obs distribution for random error
  rr_obs <- exp(rnorm(1, obs_mu, obs_se))
  
  # RR_cd from literature
  rr_cd <- exp(rnorm(1, cd_mu, cd_se))
  
  # nb to binary
  count1 <- rnbinom(n = 25635, size = 1/0.2, mu = 10)
  count0 <- rnbinom(n = 135740, size = 1/0.2, mu = 6)
  
  p1 <- mean(as.integer(count1 >= 5))
  p0 <- mean(as.integer(count0 >= 5))
  
  # calculate bias-adjusted RR
  RR_adj <- rr_obs*((rr_cd*p0+(1-p0))/rr_cd*p1+(1-p1))
  
  return(RR_adj)
  
}

## Scenario 1, MMDs >= 5 with RR_cd = 1.39 (0.32-6.01)

# if RR_obs < 1, use 1/RR_obs; get coef and SE
obs_mu <- log(1/0.80)
obs_se <- (log(1.02) - log(0.62))/(2*1.96)

cd_mu <- log(1.39)
cd_se <- (log(6.01) - log(1.29))/(2*1.96)

set.seed(82)
sim1 <- replicate(50000, bias_nb(obs_mu, obs_se, cd_mu, cd_se))

# if RR_obs < 1, also use 1/estimates
RR_adj_output(1/sim1)


## Scenario 2, MHDs >= 5 with RR_cd = 1.63 (1.29-2.06)
obs_mu <- log(1/0.80)
obs_se <- (log(1.02) - log(0.62))/(2*1.96)

cd_mu <- log(1.63)
cd_se <- (log(2.06) - log(1.29))/(2*1.96)

set.seed(82)
sim2 <- replicate(50000, bias_nb(obs_mu, obs_se, cd_mu, cd_se))

# if RR_obs < 1, also use 1/estimates
RR_adj_output(1/sim2)



###################################################################
# trapezoidal distribution for proportion of aura

bias_tpz <- function(obs_mu = obs_mu, 
                     obs_se = obs_se, 
                     cd_mu = cd_mu,
                     cd_se = cd_se) {
  
  # RR_obs distribution for random error
  rr_obs <- exp(rnorm(1, obs_mu, obs_se))
  
  # RR_CD distribution
  rr_cd <- exp(rnorm(1, cd_mu, cd_se))

  # triapezoid distribution
  p1 <- rtrapezoid(1, 0.35, 0.4, 0.5, 0.55)
  p0 <- rtrapezoid(1, 0.2, 0.25, 0.35, 0.4)
  
  RR_adj <- rr_obs*((rr_cd*p0+(1-p0))/rr_cd*p1+(1-p1))

  return(RR_adj)

}


## Scenario 3, aura with RR_cd 1.51 (1.21-2.04)
obs_mu <- log(1/0.80)
obs_se <- (log(1.02) - log(0.62))/(2*1.96)

cd_mu <- log(1.51)
cd_se <- (log(2.04) - log(1.12))/(2*1.96)


tstart <- Sys.time()
set.seed(23)

sim3 <- replicate(50000, bias_tpz(obs_mu, obs_se, cd_mu, cd_se))

print(Sys.time() - tstart)

# if RR_obs < 1, also use 1/estimates
RR_adj_output(1/sim3)


######################################################################
# Uniform distribution for the proportion of unknown CV-risk factor

bias_uni <- function(obs_mu = obs_mu, 
                     obs_se = obs_se, 
                     cd_mu = cd_mu,
                     cd_se = cd_se) {
  
  # RR_obs distribution for random error
  rr_obs <- exp(rnorm(1, obs_mu, obs_se))
  
  # RR_CD distribution
  rr_cd <- exp(rnorm(1, cd_mu, cd_se))
  
  # uniform distribution
  p1 <- runif(1, 0.7, 1.0)
  p0 <- runif(1, 0.1, 0.4)
  
  RR_adj <- rr_obs*((rr_cd*p0+(1-p0))/rr_cd*p1+(1-p1))
  
  return(RR_adj)
  
}

obs_mu <- log(2.75)
obs_se <- (log(3.77) - log(2.01))/(2*1.96)

# let RR_cd 1.40 (1.20-1.64); 1.60 (1.37-1.87); 2.00 (1.71-2.24); 4.00 (3.42-4.68)
cd_mu <- log(4.00)
cd_se <- (log(4.68) - log(3.42))/(2*1.96)
sim4 <- replicate(50000, bias_uni(obs_mu, obs_se, cd_mu, cd_se))

RR_adj_output(sim4)


