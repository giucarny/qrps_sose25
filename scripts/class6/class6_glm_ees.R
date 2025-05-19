# Title: Class 6 code - estimation
# Author: Giuseppe Carteny
# Last update: 19.05.2025


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

marginaleffects::slopes(fit_8, variable = 'eu_int_z2') %>% 
  as_tibble() %>% 
  summarise(
    estimate = mean(estimate),
    conf.low = mean(conf.low),
    conf.high = mean(conf.high)
  ) %>% 
  ggplot(aes(x=0, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0.1, colour = 'dodgerblue4') +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'indianred4')


# Authoritarian model # --------------------------------------------------------


ees2019_short_de %>% count(civ_lib)
ees2019_short_de %>% count(immigr)
ees2019_short_de %>% count(strongleader)

cor_test_civlib_immigr <- 
  ees2019_short_de %>% 
  dplyr::select(civ_lib, immigr) %>% 
  na.omit()

cor.test(cor_test_civlib_immigr[[1]], cor_test_civlib_immigr[[2]])

cor_test_civlib_immigr %>% 
  ggplot(aes(x=civ_lib, y=immigr)) +
  geom_point() +
  geom_smooth()

cor_test_strongleader_immigr <- 
  ees2019_short_de %>% 
  dplyr::select(strongleader, immigr) %>% 
  na.omit()

cor.test(cor_test_strongleader_immigr[[1]], cor_test_strongleader_immigr[[2]])

cor_test_strongleader_immigr %>% 
  ggplot(aes(x=strongleader, y=immigr)) +
  geom_point() +
  geom_smooth()

cor_test_strongleader_civ_lib <- 
  ees2019_short_de %>% 
  dplyr::select(strongleader, civ_lib) %>% 
  na.omit()

cor.test(cor_test_strongleader_civ_lib[[1]], cor_test_strongleader_civ_lib[[2]])

cor_test_strongleader_civ_lib %>% 
  ggplot(aes(x=strongleader, y=civ_lib)) +
  geom_point() +
  geom_smooth(method='lm')

# Strong leader LM 

df_strongleader_lm <- 
  ees2019_short_de %>% 
  dplyr::select(strongleader, edu_rec, gender_dic, age_z2) %>% 
  na.omit()

frml_strongleader_lm <- 'strongleader ~ edu_rec + gender_dic + age_z2'

fit_strongleader_lm <- 
  lm(
    formula = frml_strongleader_lm,
    data = df_strongleader_lm
  )

summary(fit_strongleader_lm)

ggpredict(fit_strongleader_lm, terms = 'edu_rec') %>% plot()

# Strong leader logit

df_strongleader_logit <- 
  ees2019_short_de %>%
  mutate(strongleader_dic = ifelse(strongleader>=4, 1, 0)) %>% 
  dplyr::select(strongleader_dic, edu_rec, gender_dic, age_z2) %>% 
  na.omit()

frml_strongleader_logit <- 'strongleader_dic ~ edu_rec + gender_dic + age_z2'

fit_strongleader_logit <- 
  glm(
    formula = frml_strongleader_logit,
    data = df_strongleader_logit,
    family = binomial()
  )

summary(fit_strongleader_logit)

ggpredict(fit_strongleader_logit, terms = 'edu_rec') %>% plot()

# Satisfaction w/ democracy # -------------------------------------------------

ees2019 %>% count(q3, satwithdem)


# sat. with dem LM 

df_satwithdem_lm <- 
  ees2019_short_de %>% 
  dplyr::select(satwithdem, self_lr_z2, edu_rec, gender_dic, age_z2) %>% 
  na.omit()

frml_satwithdem_lm <- 'satwithdem ~ self_lr_z2 + edu_rec + gender_dic + age_z2'

fit_satwithdem_lm <- 
  lm(
    formula = frml_satwithdem_lm,
    data = df_satwithdem_lm
  )

summary(fit_satwithdem_lm)

ggpredict(fit_satwithdem_lm, terms = 'self_lr_z2') %>% plot()

marginaleffects::slopes(fit_satwithdem_lm, variables = 'self_lr_z2') %>% 
  as_tibble() %>% 
  summarise(
    estimate = mean(estimate),
    conf.low = mean(conf.low),
    conf.high = mean(conf.high)
  )
 
# sat. with dem logit

df_satwithdem_logit <- 
  ees2019_short_de %>%
  mutate(satwithdem_dic = ifelse(satwithdem>=3, 1, 0)) %>% 
  dplyr::select(satwithdem_dic, self_lr_z2, edu_rec, gender_dic, age_z2) %>% 
  na.omit()

frml_satwithdem_logit <- 'satwithdem_dic ~ self_lr_z2 + edu_rec + gender_dic + age_z2'

fit_satwithdem_logit <- 
  glm(
    formula = frml_satwithdem_logit,
    data = df_satwithdem_logit,
    family = binomial()
  )

summary(fit_satwithdem_logit)

ggpredict(fit_satwithdem_logit, terms = 'self_lr_z2') %>% plot()
