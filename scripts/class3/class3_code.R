# Title: Class 3 code
# Author: Giuseppe Carteny
# Last update: 2025.05.05

# Preliminary notes:
# This is the code to replicate most of the visualizations and explore the 
# concepts presented in class 3. 

# The data used here are simulated

# 1. Load the packages # -------------------------------------------------------

want <- c('ggplot2', 'ggridges', 'dplyr', 'magrittr', 'tidyr')
have <- want %in% installed.packages()[,1] 
if(any(want[!have])) {
  install.packages(want[!have])
}
for(pckgs in want) {library(pckgs, character.only = T)}
rm(list = ls())

# 2. simulate population and show central limit theroem # ----------------------

## 2.0 Use always a seed! < - - - - - - - - - - - - - - - - - - - - - - - - - - - - # !!!

# Before doing any operation that implies random numbers 
# you must include a "seed" that allows reproducibilty 
# For instance, run several times this line
mean(rnorm(100,2,3)) # Everytime the value is different

# but if you put the seed, and run the two lines together
set.seed(1234) # can be whatever number
mean(rnorm(100,2,3)) # The value remains the same


## 2.1. let's simulate a population # - - - - - - - - - - - - - - - - - - - - - 

# R allows you to simulate variables from a number of distributions 
# All functions share one argument: you need to specify the length of the output
# Do you want 10, 100, 1000 values as an output?
# Here we set it as 1000
n <- 1000
# Each distribution has its own parameters! 

# The normal distribution has a mean and a standard deviation parameters
# The binomial distribution has the trial size, and the probability of each trial
# The uniform distribution just requires a minimum and max value

set.seed(1234)
simudistributions <- 
  data.frame(
  norm = rnorm(n,1,2),
  binom = rnorm(n,1,.5),
  uniform = runif(n,0,2)
)

simudistributions %>% 
  pivot_longer(everything())


# we can simulate the population from any kind of distribution
# Check the rnorm, rbinom, runif,.... functions
N = 10000
population <- runif(n=N, min = 0, max = 10)


# Now we do 1000 times the following
# - we get a sample (without replacement)
# - we take the mean
# - we store the mean 

mean_vals <- c()

for(i in 1:1000) {
  n <- sample(population, size=100, replace=F)
  x <- mean(n)
  mean_vals <- c(x, mean_vals)
}

# Create a dataframe for ggplot
df <- data.frame(x=mean_vals)
# Let's get mean and sd of the mean! For checking whether dist is normal
df %<>% 
  mutate(
    x_mean = mean(x),
    x_sd = sd(x)
  )

x_mean <- mean(df$x)
x_sd <- sd(df$x)


# Correct
df %>% 
  ggplot(aes(x=x)) +
  geom_density() + # the distribution
  stat_function(fun = rnorm, args = c(x_mean, x_sd)) 
  




# 3. Work just with the sample # -----------------------------------------------

# Now, often we don't work with the populatio, but we have only a sample

n <- 100                                # The sample size 
smpl_dist <- rnorm(n, mean=45, sd=15)   # The (assumed) distribution of the variable
smpl_mean <- mean(smpl_dist)            # The mean of the sample distribution
smpl_sd <- sd(smpl_dist)                # The SD of the sample distribution
smpl_se <- smpl_sd/sqrt(n)              # The standard error of the mean -

smpl_68ci <- c(smpl_mean-smpl_se,smpl_mean+smpl_se)                 # 68% confidence interval of the sample mean distribution
smpl_95ci <- c(smpl_mean-(1.96*smpl_se),smpl_mean+(1.96*smpl_se))   # 95% ...
smpl_99ci <- c(smpl_mean-(2.56*smpl_se),smpl_mean+(2.56*smpl_se))   # 99% ...



# Let's plot it 
range <- (smpl_mean-(smpl_se * 3)):(smpl_mean + (smpl_se * 3))


ggplot(data.frame(x = range), aes(x)) +
  stat_function(fun = dnorm, args = c(smpl_mean, smpl_se), geom = 'area',
                xlim = smpl_99ci, fill = '#efb7b3') +
  stat_function(fun = dnorm, args = c(smpl_mean, smpl_se), geom = 'area',
                xlim = smpl_95ci, fill = 'indianred') +
  stat_function(fun = dnorm, args = c(smpl_mean, smpl_se), geom = 'area',
                xlim = smpl_68ci, fill = 'indianred4') +
  geom_vline(xintercept=smpl_mean) +
  stat_function(fun = dnorm, args = c(smpl_mean, smpl_se), geom = 'line', linewidth=2) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )

# 4. testing mean differences # ------------------------------------------------


set.seed(124365)
n <- 1000

smpl1_dist <- rnorm(n, mean=45, sd=15)
smpl2_dist <- rnorm(n, mean=45, sd=14.5)

smpl1_mean <- mean(smpl1_dist)
smpl1_sd <- sd(smpl1_dist)
smpl1_se <- smpl1_sd/sqrt(n)

smpl1_68ci <- c(smpl1_mean-smpl1_se,smpl1_mean+smpl1_se)
smpl1_95ci <- c(smpl1_mean-(1.96*smpl1_se),smpl1_mean+(1.96*smpl1_se))
smpl1_99ci <- c(smpl1_mean-(2.56*smpl1_se),smpl1_mean+(2.56*smpl1_se))


smpl2_mean <- mean(smpl2_dist)
smpl2_sd <- sd(smpl2_dist)
smpl2_se <- smpl2_sd/sqrt(n)

smpl2_68ci <- c(smpl2_mean-smpl2_se,smpl2_mean+smpl2_se)
smpl2_95ci <- c(smpl2_mean-(1.96*smpl2_se),smpl2_mean+(1.96*smpl2_se))
smpl2_99ci <- c(smpl2_mean-(2.56*smpl2_se),smpl2_mean+(2.56*smpl2_se))

# distributions of the mean
smpl1_mean_dist <- rnorm(n, mean=smpl1_mean, sd=smpl1_se)
smpl2_mean_dist <- rnorm(n, mean=smpl2_mean, sd=smpl2_se)


df <- 
  rbind(
    data.frame(x = smpl1_mean_dist) %>% mutate(smpl = '1'),
    data.frame(x = smpl2_mean_dist) %>% mutate(smpl = '2')
  )


df %>% 
  ggplot(aes(x=x, y=smpl)) +
  geom_density_ridges(quantile_lines=T, quantiles=c(.5), fill='grey',rel_min_height=.01) +
  theme_classic() + 
  annotate("text", x = 45.5, y = 3, label = "Are they different?", family='Segoe UI', size=8) +
  theme(
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )    


# Are they different? Let's check 
test <- t.test(smpl1_dist,smpl2_dist)

test$statistic
test$p.value

df %>% 
  ggplot(aes(x=x, y=smpl, fill= factor(after_stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = T,
    quantile_lines = T,
    quantiles=c(.025, .5, .975), rel_min_height=.01) +
  # geom_density_ridges(quantile_lines=T, quantiles=c(.025, .5, .975), rel_min_height=.01) +
  theme_classic() + 
  # scale_fill_viridis_d() +
  scale_fill_manual(values=c('white', 'indianred','indianred','white')) +
  annotate("text", x = 45.5, y = 3, label = "Not really...", family='Segoe UI', size=8) +
  annotate("text", x = 45.5, y = 2.5, label = paste0('Test P-value=',round(test$p.value,3)), family='Segoe UI', size=7) +
  theme(
    legend.position = 'none',
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.line.x = element_blank(),
    # axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )  
