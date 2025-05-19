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

source(here('scripts', 'class6', 'class6_dataprep_code.R'))

# 4. wrangle the data again # --------------------------------------------------

# Let's filter first the data - we work only with the German sample of the EES
ees2019_short_de <- ees2019_short %>% filter(countryname=='Germany')

ees2019_short_de <-  
  ees2019_short_de %>% 
  mutate(
    eu_mem_dic = ifelse(eu_mem!=2,0,1),
    gender_dic = ifelse(gender==3,1,gender)
  ) %>% 
  mutate(
    across(
      c('age', 'self_lr', 'immigr', 'redistr', 'regulation', 'eu_int'),
      list(
        res = ~ resc_foo(.), # this will create vrbls named with the suffix "_res"
        z = ~ std_foo(.),    # this will create vrbls named with the suffix "_z"
        z2 = ~ std2_foo(.)   # this will create vrbls named with the suffix "_z2"
      )
    ),
    across(c('gender', 'gender_dic', 'edu_rec', 'eu_mem', 'eu_mem_dic'), ~as.factor(.))
  ) 



# 5.1 Analysis: First model # --------------------------------------------------

# Let's add another variable instead of immigration
data_fit_8 <- 
  ees2019_short_de %>% 
  dplyr::select(eu_mem_dic, eu_int_z2, regulation_z2, age_z2, gender_dic, edu_rec) %>% 
  na.omit 

frml_fit_8 <- 'eu_mem_dic ~ eu_int_z2 + age_z2 + gender_dic + edu_rec'

fit_8 <-  
  glm( # < - - - - - - - - - - not "lm" but "glm"
  formula = frml_fit_8, 
  data = data_fit_8, 
  family = binomial() # < - - - - - - - - HERE you state that this is a logit
  )

# Here "binomial" is actually a Bernoulli distribution, 

summary(fit_8) 



# QoIs of the first model # - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Now let's see the "predicted value" of the model variables # - - - - - - - - - 
# The `ggpredict` function of the `ggeffects` package 
# allows you to compute the predicted probabilities of one 
# of the variables of interest. 

# w/ [all] it returns all the values of the variable. Without it only part of those
fit_8_eu_int_pred <- ggpredict(fit_8, 'eu_int_z2 [all]') %>% as_tibble 
fit_8_eu_int_pred

# x: is your variable (age)
# predicted: are the predicted values 
# std.error: is the standard error of the estimate 
# conf.low: 95% confidence interval lower bound
# conf.higher: 95% confidence interval higher bound

# You have everything you need to plot the predicted probabilities
fit_8_eu_int_pred %>% 
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_line(colour='indianred4') +
  geom_ribbon(alpha=.25, fill='indianred') +
  xlab('EU Integration Attitudes') +
  ylab('Pr(EU membership is good)') +
  theme_bw() 

# Let's try with the marginaleffects package 
fit_8_eu_int_pred_me <- 
  marginaleffects::predictions(fit_8, variables = 'eu_int_z2') %>% 
  as_tibble %>% 
  summarise(
    .by = c(eu_int_z2),
    predicted=mean(estimate), conf.lo = mean(conf.low), conf.hi = mean(conf.high)) 
fit_8_eu_int_pred_me


# Marginal effects # -----------------------------------------------------------
# Let's use another model with another DV to explain this 


# Let's see the expected/predicted values 

ggpredict(fit_8, terms=c('eu_int_z2 [all]')) %>% 
  as_tibble %>% 
  ggplot(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) + # colour=group,fill=group
  geom_ribbon(alpha=.25) +
  geom_line() +
  scale_colour_viridis_d(option='plasma', direction=-1, begin=.1,end=.6, name = 'Attitudes toward') +
  scale_fill_viridis_d(option='inferno', direction=-1, begin=.1,end=.6, name = 'Attitudes toward') +
  xlab('Attitude scale') +
  ylab('Predicted values (Left-Right self-placement)') +
  theme_bw() +
  theme(legend.position = 'top')
  

