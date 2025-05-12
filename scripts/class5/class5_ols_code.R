# Title: Class 5 code - OLS explained
# Author: Giuseppe Carteny
# Last update: 10.05.2025

## 1.1 Packages # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

want <- c('dplyr', 'magrittr', 'here', 'labelled', 'ggplot2', 'ggeffects', 
          'tidyr', 'ggridges')
have <- want %in% installed.packages()[,1]            
if (any(!have)) { install.packages(want[!have]) }     
for(pckg in want) {library(pckg, character.only = T)} 
options(scipen=99)


## 1.2 Clean the environment # - - - - - - - - - - - - - - - - - - - - - - - - -
rm(list=ls())

# 2. Functions  # --------------------------------------------------------------

asn <- function(x) {as.numeric(x)} 
se <- function(gino) {sd(gino, na.rm = F)/sqrt(length(gino[!is.na(gino)]))}

resc_foo <- 
  function(x, type = 'empirical', theo_min = NA, theo_max = NA) {
    
    if (type=='empirical') {
      res_x <- (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))  
      
    } else if (type=='theoretical') {
      res_x <- (x-theo_min)/(theo_max-theo_min)  
      
    }
    return(res_x)
  }

std_foo <- function(x) {(x-mean(x,na.rm=T))/sd(x,na.rm=T)}
std2_foo <- function(x) {(x-mean(x,na.rm=T))/(2*sd(x,na.rm=T))}

# 3. source the data # ---------------------------------------------------------

source(here('scripts', 'class5', 'class5_dataprep_code.R'))

# 4. OLS with simulated data # -------------------------------------------------

set.seed(20200222)

X <- matrix(c(rep(1, 5), 
              rnorm(5, mean = 10, sd = 1), 
              rnorm(5, mean = 50, sd = 2)), 
            nrow = 5, 
            ncol = 3)

X

# we define the true coefficients (beta) and add errors (epsilon).
beta0 <- 2
beta1 <- 1.5
beta2 <- 0.2
epsilon <- rnorm(5, 0, 0.01)

y <- beta0 * X[, 1] + beta1 * X[, 2] + beta2 * X[, 3] + epsilon

y

# With t() we can transpose a matrix
t(X)

# With %*% we can multiply two matrices (if the dimensions are correct).
t(X) %*% X

# solve() takes the inverse of a matrix (if it is invertible).

# solve(X)

solve(t(X) %*% X)

# We already know %*% and t()

solve(t(X) %*% X) %*% t(X)

# And finally we add another %*% which should give us b_hat

solve(t(X) %*% X) %*% t(X) %*% y

# 4. wrangle the data again # --------------------------------------------------
# Most of the times, data needs to be reshaped for statistical modelling. 

# When it comes to continuous variables (or variables trated as such), 
# one of the most common things to do is to bring most of the variables 
# on a common scale (usually, from 0 to 1). 

# Another approach is the standardisation of continuos variables. 
# standardisation means that you substract the mean from the variable 
# and then you divide the difference it by the variable standard deviation. 

# A slightly modified version is the standardisation proposed by an american 
# statistician, Andrew Gelman, who shows that by dividing the score by 
# two (and not only one) standard deviation you can then compare the 
# magnitude of the continuois variables coefficients with the magnitude of 
# categorical variable coefficients - which otherwise would not be directly 
# comparable. 

# Let's filter first the data - we work only with the German sample of the EES
ees2019_short_de <- ees2019_short %>% filter(countryname=='Germany')

# Then we can factorize the categorical variables of interest. 

ees2019_short_de <- 
  ees2019_short_de %>% 
  mutate(across(c('gender', 'edu_rec'), ~as.factor(.)))


# 5. OLS betas with actual data # ----------------------------------------------
# Let's wrap in a function and apply it to our data 

ols_point <- function(X, y) {
  
  b_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  
  return(b_hat)
}



ols_point(X, y)

# 5. OLS with actual data # ----------------------------------------------------

ols_se <- function(X, y) {
  
  # use the ols_point function to get estimates of beta
  b_hat <- ols_point(X, y)
  
  # Residuals
  e <- y - X %*% b_hat
  
  # Variance Estimates
  n <- nrow(X)
  k <- ncol(X)
  sigma_sq <- t(e) %*% e / (n - k) # constant is already in
  sigma_sq <- as.numeric(sigma_sq)
  
  # Variance Covariance matrix
  var_cov <- sigma_sq * solve(t(X) %*% X)
  
  # Standard Errors
  std_err <- sqrt(diag(var_cov))
  return(std_err)
}

ols_se(X = iv, y = dv)

# let's fit the model with lm 
lm(y ~ X[,1] + X[,2] + X[,3])
coef(summary(m1))[, 2]