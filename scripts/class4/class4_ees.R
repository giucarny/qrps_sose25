# Title: Class 4 code
# Author: Giuseppe Carteny
# Last update: 05.05.2025

# When going through the code check the supplemental information marks here and there
# And then check the `~/scripts/class4/class4_ees_supplinfo.R` script

# It would be very useful if you take a look at the third class code, namely 
# `~/scripts/class3/class3_code.R`

# 1. Admin # -------------------------------------------------------------------

## 1.1 Packages # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# As usual, first thing install and load the packages 

# You can install them manually, one by one... for instance: 
run <- F
if(run) { # <- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - # Check supplemental info script, A.1                                       
  install.packages('dplyr')
  install.packages('magrittr')
  install.packages('here')
  library('dplyr')
  library('magrittr')
  library('here')  
}


# But like this you would have to reinstall the packages everytime
# and you load them, you'll have to restart the R session


# To avoid this you can use a slightly more refined approach... 

# .a you select the packages you want 
want <- c('dplyr', 'magrittr', 'here', 'labelled', 'ggplot2')

# .b then you check whether those you want are already among the installed
#    packages
#      Note: .installed.packages() returns a matrix
#      we get the first column of the matrix, which are the package names 
#      and compare it with the list I want using the %in% operator      # <- - - - - - - # Check supplemental info script, A.2
have <- want %in% installed.packages()[,1] 

# .c If some of the packages are not installed, then you install them sequentially
if (any(!have)) {  # Literally: If I don't have any of these packages then... 
  install.packages(want[!have]) # ... install the packages that I want but I don't have
}

# .d when all the required packages are installed, you load them 
#    in a loop - first the first one, then the second, etc. 
for(pckg in want) {library(pckg, character.only = T)}                  # <- - - - - - - # Check supplemental info script, A.3



## 1.2 Clean the environment # - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Before starting is always good to tidy up the environment. 
# You can do that using a sort of "reset" command line, which eliminates all 
# the objects in the environment

env_objs <- ls() # The list of objects in the environment
rm(list=env_objs) # Remove them 

# More compact: 
rm(list=ls())
 
# This is feasible If you don't have huge amount of data - e.g. datasets with 
# millions of observations. 
# In the latter case, then you need to try to avoid to reset the data everytime 
# but only when strictly needed.

# If your dataset is not particularly big and you can load it quickly, 
# then it's good praxi to reset the environent somtimes. 

# Note: 
# This is particularly good if you often change the objects you are workign with 
# without creating hundreds of copies

# For instance 
## Base R
df1 <- data.frame(x=rnorm(100,5,2))
print(df1)
x_mean <- mean(df1$x)
df1 <- data.frame(x_mean = x_mean)

## Tidyverse
df1 <- tibble(x=rnorm(100,5,2))
df1 <- df1 %>% summarise(x_mean = mean(x))

# This is not a good way to code, but let's assume it is fine.
#  The original df1 now has been modified, and you cannot retrieve it in the 
#  current enviroment. Therefore, you must restart from the beginning.
#  To avoid headaches, you can always tidy up everything 


# 2. Functions  # ----------------------------------------------------------------

# Custom functions are super-useful
# Some of them can be prepared before you start 
# For instance: 

# a shorter form of `as.numeric()`. Just useful for typing less
asn <- function(x) {as.numeric(x)} 

vrbl <- c('Hello', '1.2')
asn(vrbl)

# the standard error function - there's no inbuilt function in R
se <- function(gino) {sd(gino, na.rm = F)/sqrt(length(gino[!is.na(gino)]))}

x <- c(rnorm(100, 0,1), rep(NA,10))
x_mean <- mean(x, na.rm=T)
x_sd <- sd(x, na.rm=T)
x_n <- length(x[!is.na(x)])
x_se <- x_sd/sqrt(x_n)
x_se <- se(x)
# Others can be built to solve some specific issues you might encounter


# 3. Load the data # -------------------------------------------------------------

## 3.1 File path # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# To load the data you must identify the path
# Some people directly code (i.e., hardcode) the directory to their files 
hardcoded_directory <- 'C:/Users/giuse/Documents/GIT/qrps_sose25'

# But this is sub-optimal/very dangerous!
# If you change your directory, or the directory of a specific file, for 
# whatever reason, you might have to manually change all the directory or 
# file paths manually, one by one

# With the `here` package we can avoid most of these problems!
# The here package anchors your paths to your R project.
# In other word, the starting directory is the folder in which your R project is
here_directory <- here()

# same? 
here_directory==hardcoded_directory
# Let's remove the hard-coded one
rm(hardcoded_directory)

# Get the file directory
file_dir <- here('data', 'ees', '2019', 'ZA7581_v1-0-0_full.sav')

## 3.2 Load the file # - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# The file we are loading is a 'sav' file, namely a dataset for the 
# IBM SPSS (Statistical Package for the Social Sciences) - a software still 
# used today, but much less flexible and powerful than R.

# We use then the `haven` package, that allows us data in various formats - See [here for more info](https://haven.tidyverse.org/)

ees2019 <- 
  haven::read_sav(
    file = file_dir, 
    encoding = 'latin1' # this is just to prperly read some some string variables in the dataset that use non-latin characters
  ) 


## 3.3 Inspect the data # - - - - - - - - - - - - - - - - - - - - - - - -
# Let's take a preliminary look at the data 

# For taking a quick peak at the data there are several functions you can use

# The ees is built in a TIDY format... so the number of rows is...
obs_n <- nrow(ees2019) # the number of observations
# And the number of column is...
vrbls_n <- ncol(ees2019) # the number of variables

# How many observations do we have? 

# The `count` function from the dplyr package is an handy one, that allows you 
# to get the number of observations according to a grouping variable
# For instance, we want to know the sample size in all the countries of the 
# ees 
# The function will return a reduced form of the dataset - that if you want, 
# you can assign to an object - 

obs_x_country <- 
  ees2019 %>% 
  count(countrycode) %>% 
  print(n=Inf) 
# Notew: `print(n=Inf)` means: print everything in the console.
# Very usesful but dangerous! It will print EVERYTHING!
# With big datasets don't do it, it will freeze your session/computer!

# 4  Wrangle the data # --------------------------------------------------------

# Now it's time to wrangle a bit what we have here. 

# 4.1 Basic wrangling # - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Before moving to the substantive analyses you always need to think about what 
# you would need/make your life easier

# 4.1.1 First thing: change all the variable names to lower letters. 

names(ees2019)[1:20] # first 20 names
names(ees2019) <- tolower(names(ees2019))
names(ees2019)[1:20] # first 20 names

# 4.1.2 Then, we check the variables that identify our observations
ees2019 %>% 
  dplyr::select(
    respid,          # code for each unique responder
    countrycode,     # code for each country
    meta_start_date, # identifier of the start date of the interview
    meta_end_date    # identifier of the end date of the interview
  )

# We might want to check or change something. 

# For instance, we have the countrycodes of the EES, but don't have a variable 
# with the country names - which are much more informative

# But... the columns of the dataset, in sav format, are labelled!
# This means that theare labels attached to each value.
# Usually R doens't work well with labels, and it is good practice to remove them 
# before working with the data for something else

# Get the named vector
codes_and_labels <- ees2019$countrycode %>% val_labels 
# From the vector extract the labels
labels <- attr(codes_and_labels, 'names')
# From the vector extract the codes 
codes <- asn(codes_and_labels)
# Create a data.frame/tibble
country_df <- data.frame(countrycode = codes, countryname = labels)

# You can do this also with a more compact code and avoiding assignments
country_df <-
  data.frame(
    countrycode = ees2019$countrycode %>% val_labels(.) %>% as.numeric(.),
    countryname = ees2019$countrycode %>% val_labels(.) %>% attr(., 'names')
  )

# and then... we can join the countrynames with the whole dataset. 
# The ees2019 dataframe and the country_df one share one column - "countrycode"
# Then we merge the country_df with the ees2019 using this column 
ees2019 <- 
  merge(ees2019, country_df, by.x = "countrycode", by.y = "countrycode") %>%
  as_tibble

ees2019 %>% count(countrycode, countryname) %>% print(n=Inf)

# Now you are probably thinking: what the... why should I do this? 
# Of course, you could assign all the countrynames to a vector or 
# even changing the dataset one by one. It's possible, and sometimes 
# it might even be the only option available. 
# But the method above 
# (a) saves a lot of time 
# (b) saves from error deriving by typing everything
# (c) helps you to understand a basic operation that is super-useful
#     when you start merging different datasets together 
#     e.g.: individual-level data with country- or party-level data... 



# 4.3. Variables of interest: attitudes toward EU integration 
# First let's say we are interested in individual attitudes toward european 
# integration. The 0variable is the "q23". 

# Let's first assign a new variable, or rename it, so that we can remember it 
# without checking everytime the questionnaire/codebook 

ees2019$eu_int <- ees2019$q23

# Let's take a quick look at it

# You can use base R to check at the distribution...
hist(ees2019$eu_int, breaks=100)

# ..Or ggplot2
# You first define the "coordinates" of the plot, and initialize it.
# Then you add "+" layers of on top of each other - the "geoms" 
# or you modify the coordinates
# in this case is simple
ees2019 %>% 
  ggplot(aes(x=eu_int)) +  
  geom_bar()


ees2019 %>% count(eu_int)
# weird
# Acccording to the codebook this variable should go from 0 to 10 
# So what's wrong with it? The missing values! 
# Let's see it 

# Now, let's stop one second: missing va # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - # Focus missing values
# In individual-level surveys there are several possible missing values
# Don't know: the respondent doesn't know what to reply and declares it
# No reply: the respondent skipped the question 
# Doesn't apply: the question was not asked because dependent on another one 

# For instance... electoral participation
# Respondents can reply "yes I went to vote", "No...", "I prefer not to say"
# Value 2 indicates those that didn't go to vote. 
# This implies than on q9 (the vote choice variable) we (should)
# have a specific missing value

ees2019 %>% 
  dplyr::select(q6, q9) %>% 
  filter(q6==2)

# Indeed, but as you se... it's not always that linear and tidy :)

# There are for instance some respondents that say "I didn't go to vote" 
# And then.... they choose a party when asked which party they voted for (...)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Let's return to the EU integration 
# we can change these values using some conditions

# In base R....
# Option 1 - just conditions and assignment
condition <- ees2019$eu_int>10 # Identify the positions of the values that are greater than 10
ees2019$eu_int[condition] <- NA

# Option 2 - the "ifelse" function
ees2019$eu_int <- 
  ifelse(
    ees2019$eu_int > 10, # condition, 
    NA,                      # value to assign if condition is TRUE 
    ees2019$eu_int       # value to assign if condition is FALSE
  )      

# ... or using dplyr
ees2019 <- 
  ees2019 %>% 
  # `mutate()` will add columns to your existing ones, preserving the original
  mutate(eu_int = ifelse(eu_int > 10, NA, eu_int))

# Then you can check again the new variable against the original one
# using `count()`

ees2019 %>% count(q23, eu_int)


# Let's check again the frequencies
ees2019 %>% 
  ggplot(aes(x=eu_int)) +  
  geom_bar()

# Ok, it looks fine 

# 4.4. Variables of interest: others # - - - - - - - - - - - - - - - - - - - - - -

# Now let's wrangle other variables (I won't explain the details here)

ees2019 <- 
  ees2019 %>% 
  mutate(
    age = 2019 - d4_1, # birth year
    gender = d3,
    urbrur = d8,
    marital = case_when(d5>14 ~ NA_real_,
                        d5>=1 & d5<=8 ~ 1, 
                        d5>=9 & d5<=14 ~ 0),
    self_lr = ifelse(q11>10,NA,q11),
    regulation = ifelse(q14_1>10,NA,q14_1),
    redistr = ifelse(q14_2>10,NA,q14_2),
    ss_marriage = ifelse(q14_3>10,NA,q14_3),
    civ_lib = ifelse(q14_4>10,NA,q14_4),
    immigr = ifelse(q14_5>10,NA,q14_5),
    envir = ifelse(q14_6>10,NA,q14_6),
  )


# 4.5 shorter dataframe? # - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#
# You can always opt to then work with a reduced dataframe to make your work faster 
# and or your console output slightly more readable
# In this case, remember to include all the basic variables 
# But likely you'll change it the more you work on the data and analysis

vrbls2keep <- 
  c(# Identifiers
    'respid', 'countryname', 
    # Sociodemograpic variables
    'age','gender', 'urbrur', 'marital',
    # Variables of interest
    'self_lr','eu_int', 'regulation', 'redistr', 
    'ss_marriage', 'civ_lib', 'immigr', 'envir'
    )

# NOTE: when you "select" with an external vector always use `all_of()` to wrap the vector...
# If you don't do it, for now, it happens almost nothing, it just pops out a warning.
# But you never know,... a new package update might transform it in an error - stopping your code 
ees2019_short <- ees2019 %>% dplyr::select(all_of(vrbls2keep)) 
ees2019_short

# 5. Analyses # -------------------------------------------------------------------

# Let's have some descriptive analysis of the EUintegration variable 
# And let's work using (almost) only data.frames.

# So rather than doing this: 
eu_int_mean <- mean(ees2019_short$eu_int, na.rm=T)

# we do this: 
ees2019_short <-
  ees2019_short %>%
  mutate(eu_int_mean = mean(eu_int, na.rm=T))

# or this: 
eu_int_sum_df <- 
  ees2019_short %>% 
  summarise(mean = mean(eu_int, na.rm=T))


# Check the difference above between mutate and summarise! 

## 5.1 Univariate analyses: EU integration # - - - - - - - - - - - - - - - - - -
### 5.1.1 prepare your data for the analyses #

# Now, we can get all the needed information. 
# The data.frame you need depends on what kind of information you want to include 
# in your analyses. 

# You are interested in preserving the original variable distribution? 
# Then go for mutate: 
eu_int_df <- 
  ees2019_short %>% 
  dplyr::select(eu_int) %>% # We select this variable to make the dataset tidier
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

# You are interested in just getting the descriptive statistics? 
# Then go for summarise: 
eu_int_sum_df <-              # here "sum" means "summary" 
  ees2019_short %>% 
  # dplyr::select(eu_int) %>% # here is not needed, because summarise will drop the other columns
  summarise(
    eu_int_mean = mean(eu_int, na.rm=T), 
    eu_int_sd = sd(eu_int, na.rm=T),
    eu_int_n = length(eu_int[!is.na(eu_int)]),
    eu_int_se = eu_int_sd/sqrt(eu_int_n),
    eu_int_68dist_upper = eu_int_mean+(1*eu_int_sd),
    eu_int_68dist_lower = eu_int_mean-(1*eu_int_sd),
    eu_int_mean_95ci_upper = eu_int_mean+(1.96*eu_int_se),
    eu_int_mean_95ci_lower = eu_int_mean-(1.96*eu_int_se),
  ) 

# Let's use the dataframe created using "mutate"
# This is because we want to plot also the distribution of the original 
# variable. 

eu_int_df %>% 
  ggplot(aes(x=eu_int)) +
  geom_bar(alpha=.5) +
  geom_vline(xintercept=eu_int_mean, colour='red4') +
  geom_vline(aes(xintercept=eu_int_68dist_upper), colour='blue4') +
  geom_vline(aes(xintercept=eu_int_68dist_lower), colour='blue4') + 
  geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred4') +
  geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred4') 

# The standard error is SUPER SMALL! See here: 

eu_int_df %>% 
  ggplot(aes(x=eu_int)) +
  # geom_bar(alpha=.5) +
  geom_vline(xintercept=eu_int_mean, colour='red4') +
  geom_vline(aes(xintercept=eu_int_68dist_upper), colour='blue4') +
  geom_vline(aes(xintercept=eu_int_68dist_lower), colour='blue4') + 
  geom_vline(aes(xintercept=eu_int_mean_95ci_upper), colour='indianred4') +
  geom_vline(aes(xintercept=eu_int_mean_95ci_lower), colour='indianred4') +
  coord_cartesian(xlim=c(5.3,5.45))


# But this is because we have 26k+ observations!                   # <- - - - - - - # Check supplemental info script, A.3, and class3 
# In our case, however, you have such a huge number of observations because 
# you're not considering differences among countries, or groups, etc... 
# When you start accounting for these things the standard error of your 
# estimates will shrink because: 
# - you have less observations 
# - the distribution might change across groups










