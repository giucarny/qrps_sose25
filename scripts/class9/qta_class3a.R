# load the necessary packages
library(tidyverse)
library(quanteda)

# install and load a new package for today
install.packages("openxlsx")
library(openxlsx)



# Dealing with messy files and incomplete data
# import tweets from Dropbox folder
twt_de <- read.xlsx("https://www.dropbox.com/scl/fi/2gcyo0f8i4pdrcoqzxyn7/mdb_twt.xlsx?rlkey=t3yycjvxzhc39l98xjm5nle4g&st=ov9y8aaf&dl=1")


# view available variables
ls(twt_de)

# if we are interested in polarisation,
# what useful information is missing?


# import a new file
pol_de <- read.csv("https://www.dropbox.com/scl/fi/4st89lqg8soj9gj3z4lj0/de_all.csv?rlkey=h163w1p8dy1t8p6mzvtcn36ve&st=fzqc309o&dl=1",
                   encoding = "UTF-8")

# view the available variables
ls(pol_de)

# using code, how can we quickly view the datasets?
# you can also create some of the descriptive stats we used before

# now we need to merge the files
# how can we do this?

# rename variables for merging
pol_de <- pol_de %>%
  rename(username = Twitter)

# rename and select variables for merging
twt_sub <- twt_de %>%
  select(AUTHOR_USERNAME, TEXT) %>%
  rename(username = AUTHOR_USERNAME,
         text = TEXT)

# merge politician details and tweets
twt_all <- merge(twt_sub, pol_de, by = "username", all.x = TRUE)

# the newly created shared "username" variable 
# allows us to merge the politician details with the tweets
# note the "all.x" argument


# note the different row numbers between twt_sub and twt_all
# why is this?

sum(is.na(twt_all$party))

# we need to add some details to fill the gaps...
twt_all$party <- ifelse(twt_all$username == "Jan_Nolte_AfD", 
                        "AfD", 
                        twt_all$party)

# did that work?
sum(is.na(twt_all$party))


# select name and text for the corpus file
twt_selected <- twt_all %>%
                select(name, text)


# create the corpus, tokens, and dfm
twt_corp <- corpus(twt_selected)

# rename the rows
docnames(twt_corp) <- paste0(1:nrow(twt_selected),"_",
                                twt_selected$name)
twt_toks <- tokens(twt_corp)

twt_dfm <- dfm(twt_toks)

# check the head
head(twt_dfm)


# Pre-processing

# Pre-processing has three different aspects
# restricting feature definitions
# removing uninformative features
# uniting features

# The structure of your dfm

# View the number of tokens

ntoken(twt_toks) %>% 
  head()

# View the number of features in a dfm

nfeat(twt_dfm)


# View the most frequent features

topfeatures(twt_dfm)

# View the number of documents

ndoc(twt_dfm)

# Names of features as a vector

featnames(twt_dfm) %>% 
  head()

# frequency of features as a vector

featfreq(twt_dfm) %>% 
  head()

# You can use many of these commands to check the results 
# of your pre-processing in the following steps

## Restricting feature definitions

# We can restrict features by removing punctuation, 
# numbers and symbols

# check feature number
nfeat(twt_dfm)

# restrict feature definition
twt_toks_nostop <- tokens(twt_corp,  
                   remove_punct=T,
                   remove_numbers=T, 
                   remove_symbols=T)


# create the dfm again
twt_dfm <- dfm(twt_toks_nostop)

# check new feature number
nfeat(twt_dfm)
# compare this with the previous number
# what has changed and why?


## Removing uninformative features
# we call these stopwords
# check the examples in English and then in German
stopwords("en")

stopwords("de")

# we can remove them as follows
twt_toks_nostop <- tokens_remove(twt_toks, 
                                   pattern = stopwords("de"))

# as before, create the dfm and check its size


# dfm_trim() is an alternative that selects words 
# based on their frequency.

# Additional Exercises
# try the different options of `dfm_trim()`
# remove features that occur less than 10 times in total
# remove features occurring in less than 2 documents
# remove features occurring more than 50 times

# *Tip: If you specify verbose=T in your command, 
# you see how much you remove.*



# We can also stem the features of the dfm
# this means removing the end of words
# so for example go and going become the same
# try it with the Tweets
twt_toks_stemmed <- tokens_wordstem(twt_toks_nostop, 
                                      language = "german")

# compare the before and after tokens
head(twt_toks_nostop, 1)
head(twt_toks_stemmed, 1)



# Transformations

# We have already used some commands to transform dfms for pre-processing. 
# 
# dfm_subset() : Selection based on docvars
# dfm_group() : grouping of documents based on docvars
# dfm_select(): Selection of features
# dfm_trim(): Selection of features based on frequency
# dfm_weight() & dfm_tfidf(): weighting the feature counts
# dfm_lookup(): Looking up dictionaries


## Grouping

# First, group your dataset by name. How did the number of documents change?
  
## Subsetting

## Weighting

# Finally, weigh your dfm: `dfm_weight()` allows you to weigh your dfm by different characteristics. You can consult the documentation to see the different options.
# 
# See how the `topfeatures()` change when you use binary weighting and relative frequency.
# 
# A frequently applied weight is [*term-frequency-inverse-document-frequency*](https://en.wikipedia.org/wiki/Tf%E2%80%93idf). It is also implemented in quanteda with `dfm_tfidf()`. Look at the results of this weighting for your first documents. Is this more or less meaningful to you?
