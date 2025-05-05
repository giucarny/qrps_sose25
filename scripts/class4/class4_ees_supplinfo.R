# Title: Class 4 code - Supplemental information
# Author: Giuseppe Carteny
# Last update: 05.05.2025

# A.1 the `if` statement # -----------------------------------------------------

# "if" is a function which is ubiquitous. It is available in any programmi language

# The logic is: 
# You provide a condition/requirement/...
# The if statement(s) check whether the condition/requirement/... is true
# if it's true, then R evaluates the code between the following {}
# if not, then R will skip that chunk of code

peanut_allergy <- T
if(peanut_allergy) {
  verb <- 'is'
} else {
  verb <- 'is not'
}

sentence <- paste0("Peanut butter ", verb, " a red flag!")
print(sentence)

# You can use logical statements (T,F) but also other conditions, with more than 
# two possible values

fruit_like <- "plum"

if(fruit_like == 'apple') {
  colour <- ' red'
} else if (fruit_like == 'plum') {
  colour <- ' purple'
} else if (fruit_like == 'peer') {
  colour <- ' green' 
} else {                          # If all the previous conditions are not met
  fruit_like <- 'fruit'
  colour <- ' colorless'
}

sentence <- paste0('This ', fruit_like, ' is', colour)
print(sentence)

# A.2 the `%in%` operator # ----------------------------------------------------

# It allows you to compare vectors with more than one element
# for instance: 

x <- c(1)
y <- c(2)

# with one element you use "==" or related operators: "!=",">", "<", "<=",...
x==y
x!=y
x>y
x<y

# But if you have vectors like those below you need %in% 
w <- c(1,5,4,6,8)
z <- c(2,5,4,7,8)

w %in% z # This tells you which vectors in w satisfy the condition in z

# if you want to take only the values that are TRUE, namely those of w that 
# are in z
w[w %in% z] # Here in brackets is your CONDITION



# A.3 the `for` loop # ---------------------------------------------------------

# Sometimes you need to perform the same operation several times, 
# just slightly changing one feature 
# We can see this for exploring, for instance, the relationship between 
# standard error and sample size. 

# Do you remember the formula of the standard error? # < - - - - - - - - - - - - # Check also ~/scripts/class3/class3_code.R
# It is the standard deviation of the sample on the sample size, squared

# Let's create a population 
set.seed(1245)
population <- rnorm(n=10000,1,2)

# let's create a function, that we will apply iteratively with the `for` loop
se_foo <- 
  function(x) {
    x_sd <- sd(x, na.rm=T) # na.rm=T: removes the missing values from the vector
    x_n <- length(x[!is.na(x)])
    x_n_sqrt <- sqrt(x_n)
    x_se <- x_sd/x_n_sqrt
  }

# Let's get all the possible sample sizes from 10 to 1000, by 10
sample_size <- seq(from=10,to=1000,by=10) 
sample_size

# Now let's loop

# First, initialize an empty vector for storing your estimates
se_vec <- c()

# Literally, for each `ss` value in the vector `sample_size` perform the 
# following operations {...}

for(ss in sample_size) { 
  x <- sample(x = population, size=ss)
  se_estimate <- se_foo(x) 
  se_vec <- c(se_vec, se_estimate)
}

# For each sample size we computed the standar error estimates, and we 
# stored it sequentially in the vector `se_vec`

# Let's put together estimates and sample sizes it in a data.frame
sample_size_df <- data.frame(sample_size=sample_size, se=se_vec)

# Let's plot this 
sample_size_df %>% 
  ggplot(aes(x=se_vec, y=sample_size)) + 
  geom_point(alpha=.8) +
  ylab('Sample size') +
  xlab('Standard error')


# So, the larger n gets, the smaller is the estimate of the se! 


# Clearly you can use the for loop for a number of tasks. But for now the 
# important is that you understand just the logic 
