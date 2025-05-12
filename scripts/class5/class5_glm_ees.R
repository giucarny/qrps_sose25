# Title: Class 5 code - estimation
# Author: Giuseppe Carteny
# Last update: 10.05.2025


# 1. Admin 

## 1.1 Packages # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

want <- c('dplyr', 'magrittr', 'here', 'labelled', 'ggplot2', 'ggeffects', 
          'tidyr', 'ggridges', 'marginaleffects')
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
here()

list.files(here('scripts'))

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

# Then we can rescale and standardize the continuous variables of interest. 

# Rescaling examples # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Rescaling is achieved by using this approach 
x <- runif(1000, 1,11)

# here we use the "empirical" max and min - those that actually appear in the data
x_rescaled <- (x-min(x, na.rm=T))/(max(x, na.rm=T)-min(x, na.rm=T)) 

# But if we know the theoretical max and min of the scale 
# we can use those 
x_theoretical_min <- 1
x_theoretical_max <- 11
x_rescaled_2 <- (x-x_theoretical_min)/(x_theoretical_max-x_theoretical_min) 


# strandardisation
x_std <- (x-mean(x, na.rm=T)) / (sd(x,na.rm = T))
x_std2 <- (x-mean(x, na.rm=T)) / (2*sd(x,na.rm = T))



# Compare the scales

# Scales are different
plot1 <- 
  cbind(x,x_rescaled,x_rescaled_2,x_std,x_std2) %>% 
  as.data.frame() %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x=value,y=name)) +
  ggridges::geom_density_ridges()

# But distributions... the same!
plot2 <- 
  cbind(x,x_rescaled,x_rescaled_2,x_std,x_std2) %>% 
  as.data.frame() %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x=value)) +
  geom_density() +
  facet_wrap(~name, scales='free')


ggpubr::ggarrange(plot1,plot2)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# we create new variables
# In this case we use the "across" function, that allows to perform the same 
# operation across more than one variable
ees2019_short_de <- 
  ees2019_short_de %>% 
  mutate(
    across(
      c('age', 'self_lr', 'immigr', 'redistr', 'regulation'),
      list(
        res = ~ resc_foo(.), # this will create vrbls named with the suffix "_res"
        z = ~ std_foo(.),    # this will create vrbls named with the suffix "_z"
        z2 = ~ std2_foo(.)   # this will create vrbls named with the suffix "_z2"
      )
    )
  )


ees2019_short_de[,c('age', 'age_z', 'age_res')]

# # Then we can factorize the categorical variables of interest # [Chunk muted, for example]
# ees2019_short_de <- 
#   ees2019_short_de %>% 
#   mutate(across(c('gender', 'edu_rec'), ~as.factor(.)))


# 5.1 Analysis: First model # --------------------------------------------------

# Let's fit our first model # - - - - - - - - - - - - - - - - - - - - - - - - -

data_fit_1 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, age, gender, edu_rec) %>% 
  na.omit

frml_fit_1 <- 'eu_int ~ age + gender + edu_rec'

fit_1 <- lm(formula = frml_fit_1, data = data_fit_1)

summary(fit_1)

# Mmmh edu is weird... it has three categories but only one coefficient? 
# Should be treated as a factor variable. 
# Same for gender.

# Let's fit our first model, again # - - - - - - - - - - - - - - - - - - - - - -

# Let's wrangle the data
ees2019_short_de <- 
  ees2019_short_de %>% 
  mutate(across(c('gender', 'edu_rec'), ~as.factor(.)))

# now again 
data_fit_1 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, age, gender, edu_rec) %>% 
  na.omit

frml_fit_1 <- 'eu_int ~ age + gender + edu_rec'

fit_1 <- lm(formula = frml_fit_1, data = data_fit_1)

summary(fit_1) 

# Better

# Now let's check the model assumptions # - - - - - - - - - - - - - - - - - - - -
par(mfrow = c(2, 2))
plot(fit_1)

# # The assumptions
# Linearity of the data. The relationship between the predictor (x) and the outcome (y) is assumed to be linear.
# Normality of residuals. The residual errors are assumed to be normally distributed.
# Homogeneity of residuals variance. The residuals are assumed to have a constant variance (homoscedasticity)
# Independence of residuals error terms.

dev.off()

# QoIs of the first model # - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Now let's see the "predicted value" of the model variables # - - - - - - - - - 
# The `ggpredict` function of the `ggeffects` package 
# allows you to compute the predicted probabilities of one 
# of the variables of interest. 

# w/ [all] it returns all the values of the variable. Without it only part of those
fit_1_age_pred <- ggpredict(fit_1, 'age [all]') %>% as_tibble 
fit_1_age_pred

# x: is your variable (age)
# predicted: are the predicted values 
# std.error: is the standard error of the estimate 
# conf.low: 95% confidence interval lower bound
# conf.higher: 95% confidence interval higher bound

# You have everything you need to plot the predicted probabilities
fit_1_age_pred %>% 
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(colour='indianred4') +
  geom_ribbon(alpha=.25, fill='indianred') +
  xlab('Age') +
  ylab('Predicted values (EU Integration attitudes)') +
  theme_bw() 

# Let's repeat it with gender 
ggpredict(fit_1, 'gender') %>% plot() 
# Yes, you can get a plot also just like this :)
# And you can actually modify it, a bit - is a ggplot object
# For instance: 
ggpredict(fit_1, 'gender') %>% 
  plot() + 
  ylab('Predicted values (EU Integration attitudes)') 

# Nonetheless, if you want to have more control on the plotting 
# then you should do as with 'age' a few lines above. 

# let's check the last predictor:
ggpredict(fit_1, 'edu_rec') %>% plot()



# 5.2 Analysis: Second model # -------------------------------------------------

# Age however is an interesting variable, because a lot of times its correlation 
# with the DV is not linear 

# For instance, you can model these using a squared term - which is what we 
# might call an *interaction*, even though a peculiar one
# now again 
data_fit_2 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, age_z2, gender, edu_rec) %>% 
  na.omit

frml_fit_2 <- 'eu_int ~ age_z2 + I(age_z2^2) + gender + edu_rec'

fit_2 <- lm(formula = frml_fit_2, data = data_fit_2)

summary(fit_2) 

ggpredict(fit_2, 'age_z2') %>% plot()

# Interesting right? What this plot tells you?


# 5.3 Analysis: Third model # --------------------------------------------------

# However, even a quadratic specification can still cover 
# more complex relationships. 

# So, you might check whether the quadratic relationship is reasonable
# by creating categorical variable of age to check for non-linear correlations

# Let's check the values of age
ees2019_short_de$age %>% summary

# Ok, based on those values we can build a variable with 5 years intervals
ees2019_short_de <-
  ees2019_short_de %>% 
  mutate(
    age_cat = cut(
      age,
      breaks = seq(16, 100, by = 5),
      right = FALSE,
      ordered_result = FALSE,
      labels = paste(seq(16, 90, by = 5),seq(21, 98, by = 5), sep = "–")
    )    
  ) 


# Fit the third model # - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
data_fit_3 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, age_cat, gender, edu_rec) %>% 
  na.omit

frml_fit_3 <- 'eu_int ~ age_cat + gender + edu_rec'

fit_3 <- lm(formula = frml_fit_3, data = data_fit_3)

summary(fit_3) 

ggpredict(fit_3, 'age_cat') %>% plot()

# see? Even the quadratic form cannot fully capture the relationship
# nonetheless, 

# 5.4 Analysis: fourth model # -------------------------------------------------

# Let's add some substantively interesting 


# Fit the fourth model # - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
data_fit_4 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, immigr_z2, age_z2, gender, edu_rec) %>% 
  na.omit

frml_fit_4 <- 'eu_int ~ immigr_z2 + age_z2 + gender + edu_rec'

fit_4 <- lm(formula = frml_fit_4, data = data_fit_4)

summary(fit_4) 
ggpredict(fit_4, 'immigr_z2') %>% plot()


# Interesting. Attitudes toward immigration are NOT correlated with attitudes 
# toward the EU... wait, what? 

# There's something missing here. 
# Let's check using the categorical approach

# wrangle
ees2019_short_de$immigr_cat <- as.factor(ees2019_short_de$immigr)

# fit again
data_fit_4 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, immigr_cat, age_z2, gender, edu_rec) %>% 
  na.omit

frml_fit_4 <- 'eu_int ~ immigr_cat + age_z2 + gender + edu_rec'

fit_4 <- lm(formula = frml_fit_4, data = data_fit_4)

summary(fit_4) 
ggpredict(fit_4, 'immigr_cat') %>% plot()

# now let's try the quadratic version

data_fit_4 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, immigr_z2, age_z2, gender, edu_rec) %>% 
  na.omit

frml_fit_4 <- 'eu_int ~ immigr_z2 + I(immigr_z2^2) + age_z2 + gender + edu_rec'

fit_4 <- lm(formula = frml_fit_4, data = data_fit_4)

summary(fit_4) 
ggpredict(fit_4, 'immigr_z2') %>% plot()


# 5.5 Analysis: fifth model # -------------------------------------------------

# Let's add another variable instead of immigration
data_fit_5 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, self_lr_z2, age_z2, gender, edu_rec) %>% 
  na.omit

frml_fit_5 <- 'eu_int ~ self_lr_z2 + age_z2 + gender + edu_rec'

fit_5 <- lm(formula = frml_fit_5, data = data_fit_5)

summary(fit_5) 
ggpredict(fit_5, 'self_lr_z2') %>% plot()

stargazer::stargazer(fit_5, type='text')

# 5.6 Analysis: sixth model # -------------------------------------------------

# Let's add another variable instead of immigration
data_fit_6 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_int, self_lr_z2, immigr_z2, age_z2, gender, edu_rec) %>% 
  na.omit

frml_fit_6 <- 'eu_int ~ self_lr_z2 + immigr_z2 + I(immigr_z2^2) + age_z2 + gender + edu_rec'

fit_6 <- lm(formula = frml_fit_6, data = data_fit_6)

summary(fit_6) 
ggpredict(fit_6, 'self_lr_z2') %>% plot()
ggpredict(fit_6, 'immigr_z2') %>% plot()

stargazer::stargazer(fit_6, type='text')



# Marginal effects # -----------------------------------------------------------
# Let's use another model with another DV to explain this 


# Let's add another variable instead of immigration
data_fit_7 <- 
  ees2019_short_de %>% 
  dplyr::select(self_lr, redistr_z2, regulation_z2, age_z2, gender, edu_rec) %>% 
  na.omit

frml_fit_7 <- 'self_lr ~ redistr_z2 + regulation_z2 + age_z2 + gender + edu_rec'

fit_7 <- lm(formula = frml_fit_7, data = data_fit_7)

summary(fit_7) 


# Let's see the expected/predicted values 
rbind(
  ggpredict(fit_7, 'regulation_z2 [-1,1]') %>% as_tibble %>% mutate(group='regulation'),
  ggpredict(fit_7, 'redistr_z2 [-1,1]') %>% as_tibble %>% mutate(group='redistribution')
) %>% 
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, colour=group,fill=group)) +
  geom_ribbon(alpha=.25) +
  geom_line() +
  scale_colour_viridis_d(option='plasma', direction=-1, begin=.1,end=.6, name = 'Attitudes toward') +
  scale_fill_viridis_d(option='inferno', direction=-1, begin=.1,end=.6, name = 'Attitudes toward') +
  xlab('Attitude scale') +
  ylab('Predicted values (Left-Right self-placement)') +
  theme_bw() +
  theme(legend.position = 'top')
  

# stargazer::stargazer(fit_7, type='text')



# Now let's check the marginal effects # - - - - - - - - - - - - - - - - - - - -
# Which is the strongest?

rbind(
  marginaleffects::slopes(fit_7, variables = c('regulation_z2')) %>% 
    as_tibble %>% 
    summarise(
      predicted=mean(estimate), 
      conf.lo = mean(conf.low), 
      conf.hi = mean(conf.high)) %>% 
    mutate(group='regulation')
  ,
  marginaleffects::slopes(fit_7, variables = c('redistr_z2')) %>% 
    as_tibble %>% 
    summarise(predicted=mean(estimate), 
              conf.lo = mean(conf.low), 
              conf.hi = mean(conf.high)) %>% 
    mutate(group='redistribution')
) %>% 
  ggplot(aes(y=predicted, x=group, ymin=conf.lo, ymax=conf.hi, colour=group,fill=group)) +
  geom_hline(yintercept = 0, linetype='dotted', colour='indianred4') +
  geom_errorbar(width=0, linewidth=3) +
  geom_point(size=1, colour='white') +
  scale_colour_viridis_d(option='plasma', direction=-1, begin=.1,end=.6, name = 'Attitudes toward') +
  scale_fill_viridis_d(option='inferno', direction=-1, begin=.1,end=.6, name = 'Attitudes toward') +
  ylab('AME') +
  xlab('Attitudes toward') +
  theme_bw() +
  theme(legend.position = 'none')



# What happens with categorical variables? # - - - - - - - - - - - - - - - - - -

# now let's check the marginal effects
marginaleffects::slopes(fit_7, variables = c('edu_rec')) %>% 
  as_tibble %>% 
  summarise(.by = contrast, predicted=mean(estimate), conf.lo = mean(conf.low), conf.hi = mean(conf.high)) %>% 
  ggplot(aes(y=predicted, x=contrast, ymin=conf.lo, ymax=conf.hi)) +
  geom_hline(yintercept = 0, linetype='dotted', colour='indianred4') +
  geom_errorbar(width=0, linewidth=3) +
  geom_point(size=1, colour='white') +
  ylab('Edu AME') +
  theme_bw()
