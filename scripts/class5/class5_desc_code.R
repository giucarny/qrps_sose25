# Title: Class 5 code - descriptive analysis
# Author: Giuseppe Carteny
# Last update: 10.05.2025


# 1. Admin 

## 1.1 Packages # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

want <- c('dplyr', 'magrittr', 'here', 'labelled', 'ggplot2', 'ggeffects')
have <- want %in% installed.packages()[,1]            
if (any(!have)) { install.packages(want[!have]) }     
for(pckg in want) {library(pckg, character.only = T)} 

# * class4_ees_supplinfo.R

## 1.2 Clean the environment # - - - - - - - - - - - - - - - - - - - - - - - - -
rm(list=ls())

## 1.3 code variables # - - - - - - - - - - - - - - - - - - - - - - - - - - - -

desc_analysis <- F

# 2. Functions  # --------------------------------------------------------------

asn <- function(x) {as.numeric(x)} 
se <- function(gino) {sd(gino, na.rm = F)/sqrt(length(gino[!is.na(gino)]))}

# 3. source the data # ---------------------------------------------------------

source(here('scripts', 'class5', 'class5_dataprep_code.R'))

# 5.1 EU integration: univariate analysis # -----------------------------------


eu_int_df <- 
  ees2019_short %>% 
  dplyr::select(eu_int) %>% 
  mutate(
    eu_int_mean = mean(eu_int, na.rm=T), 
    eu_int_sd = sd(eu_int, na.rm=T),
    eu_int_n = length(eu_int[!is.na(eu_int)]),
    eu_int_se = eu_int_sd/sqrt(eu_int_n),
    eu_int_68dist_upper = eu_int_mean+(1*eu_int_sd),
    eu_int_68dist_lower = eu_int_mean-(1*eu_int_sd),
    eu_int_mean_95ci_upper = eu_int_mean+(1.96*eu_int_se),
    eu_int_mean_95ci_lower = eu_int_mean-(1.96*eu_int_se),
  ) 

eu_int_df %>% 
  ggplot(aes(x=eu_int)) +
  geom_bar(alpha=.5) +
  geom_vline(aes(xintercept=eu_int_mean), colour='indianred4') +
  geom_vline(aes(xintercept=eu_int_68dist_upper), colour='dodgerblue4') +
  geom_vline(aes(xintercept=eu_int_68dist_lower), colour='dodgerblue4') + 
  geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred') +
  geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred') 


# The standard error is SUPER SMALL! See here: 

eu_int_df %>% 
  ggplot(aes(x=eu_int)) +
  # geom_bar(alpha=.5) +
  geom_vline(aes(xintercept=eu_int_mean), colour='indianred4') +
  geom_vline(aes(xintercept=eu_int_68dist_upper), colour='dodgerblue4') +
  geom_vline(aes(xintercept=eu_int_68dist_lower), colour='dodgerblue4') + 
  geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred') +
  geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred') +
  coord_cartesian(xlim=c(5.3,5.45))


# But this is because we have 26k+ observations!               # <- - - - - - - # Check supplemental info script, A.3, and class3 
# But this is because we're not considering differences among countries, or groups, 
# etc... 
# When you start accounting for these things the standard error of your 
# estimates will shrink because: 
# - you have less observations 
# - the distribution might change across groups


# 5.2 EU integration: bivariate analysis (means difference) # ------------------
# let's see:

# the "by" argument in mutate/summarise allows to create stats by group(s)!
eu_int_sum_df_by <-              
  ees2019_short %>% 
  mutate(
    .by = c(countryname, gender), # <- the "by" argument 
    eu_int_mean = mean(eu_int, na.rm=T), 
    eu_int_sd = sd(eu_int, na.rm=T),
    eu_int_n = length(eu_int[!is.na(eu_int)]),
    eu_int_se = eu_int_sd/sqrt(eu_int_n),
    eu_int_68dist_upper = eu_int_mean+(1*eu_int_sd),
    eu_int_68dist_lower = eu_int_mean-(1*eu_int_sd),
    eu_int_mean_95ci_upper = eu_int_mean+(1.96*eu_int_se),
    eu_int_mean_95ci_lower = eu_int_mean-(1.96*eu_int_se),
  ) 

eu_int_sum_df_by


eu_int_sum_df_by_plot <- 
  eu_int_sum_df_by %>% 
  filter(gender!='Other') %>% 
  ggplot(aes(x=eu_int)) +
  geom_bar(alpha=.5) +
  geom_vline(aes(xintercept=eu_int_mean), colour='indianred4') +
  geom_vline(aes(xintercept=eu_int_68dist_upper), colour='dodgerblue4') +
  geom_vline(aes(xintercept=eu_int_68dist_lower), colour='dodgerblue4') + 
  geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred') +
  geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred') +
  facet_grid(countryname~gender)              

eu_int_sum_df_by_plot

# Ok definitely too many countries for one visualization!
# Let's select 4/5

eu_int_sum_df_by_plot <- 
  eu_int_sum_df_by %>% 
  filter(
    gender!='Other',
    countryname %in% c('Austria', 'Germany', 'Italy', 'Estonia')
  ) %>% 
  ggplot(aes(x=eu_int)) +
  geom_bar(alpha=.3) +
  geom_vline(aes(xintercept=eu_int_mean), colour='indianred4') +
  geom_vline(aes(xintercept=eu_int_68dist_upper), colour='dodgerblue4') +
  geom_vline(aes(xintercept=eu_int_68dist_lower), colour='dodgerblue4') + 
  geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred') +
  geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred') +
  facet_grid(countryname~gender)

eu_int_sum_df_by_plot

# Returning to the previous plot, the picture conveys some interesting info: 
# there seems to be a statistical significant difference (mean + c.i. 95%) 
# between Germany and Estonia, irrespective of gender

# Let's check it out 
# Create an empty list
vec_list <- list()

# Fill the list with the vectors
# Naming each list with the countryname
countries2test <- c('Estonia','Germany', 'Austria', 'Italy')
for(cntry in countries2test) {
  condition <- ees2019_short$countryname==cntry
  eu_int_vector <- ees2019_short$eu_int[condition] 
  eu_int_vector <- eu_int_vector[!is.na(eu_int_vector)] 
  vec_list[[cntry]] <- eu_int_vector 
}

ttest_estonia_germany <- t.test(vec_list[['Estonia']], vec_list[['Germany']]) 
ttest_italy_germany <- t.test(vec_list[['Italy']], vec_list[['Germany']]) 

options(scipen=99) 
ttest_estonia_germany$p.value # This is very small, means that there's a 
# statistically significant difference
ttest_italy_germany$p.value # Not statistically significant 

# This implies that in the first case there's a substantially different 
# average opinion about EU integration between Estonian and German respondents, 
# whereas Germans and Italians have a pretty 
# similar average position
# But it's big? 

# 5.3 EU integration: correlation analysis # -----------------------------------

euint_lr_df <-
  ees2019_short %>% 
  dplyr::select(respid, countryname, eu_int, self_lr)

# Let's drop any row with missing values with `na.omit`
euint_lr_df_naomit <- euint_lr_df %>% na.omit
euint_lr_df_filtered <- euint_lr_df %>% filter(!is.na(eu_int) & !is.na(self_lr))
# in our case, same result as before.
all.equal(euint_lr_df_filtered, euint_lr_df_naomit) # TRUE


euintvec <- euint_lr_df_naomit[['eu_int']]
lrvec <- euint_lr_df_naomit[['self_lr']]

cor.test(euintvec, lrvec) # Statistically significant negative correlation! 
# But the effect is VERY small! 

# Let's inspect it graphically with `geom_smooth`
euint_lr_df_naomit %>% 
  ggplot(aes(x=self_lr,y=eu_int)) +
  geom_point() + 
  geom_smooth(method='lm') 

# Doesn't look great... 
euint_lr_df_naomit %>% 
  ggplot(aes(x=self_lr,y=eu_int)) +
  # geom_point() + 
  geom_jitter(alpha=.01) + 
  geom_smooth(method='lm')

# Slightly better... but with these scale we can just drop the points/jitter
euint_lr_df_naomit %>% 
  ggplot(aes(x=self_lr,y=eu_int)) +
  geom_smooth(method='lm')

# Yes, better. 
# WHat happens if you change the scale? 
euint_lr_df_naomit %>% 
  ggplot(aes(x=self_lr,y=eu_int)) +
  geom_smooth(method='lm') +
  scale_y_continuous(limits = c(2,8)) # see? 


# Let's see austria
euintvec_austria <- euint_lr_df_naomit[['eu_int']][euint_lr_df_naomit$countryname=='Austria']
lrvec_austria <- euint_lr_df_naomit[['self_lr']][euint_lr_df_naomit$countryname=='Austria']
cor.test(euintvec_austria, lrvec_austria) 
# Statistically significant, and relatively strong, negative correlation! 

# Let's check graphically
countries2test <- c('Estonia','Germany', 'Austria', 'Italy')

euint_lr_df_naomit %>% 
  filter(countryname %in% countries2test) %>% 
  ggplot(aes(x=self_lr,y=eu_int)) +
  geom_smooth(method='lm') +
  scale_y_continuous(limits = c(2,8)) +
  facet_wrap(~countryname)

# There's quite some variability :) But the correlations are anything but great
# Your time to explore

