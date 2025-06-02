# note: you need to complete last week's code first
# activate the required packages for this week
# this will only work after running the first steps of class 1
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

# also add a new package for today
install.packages("newsmap")
library(newsmap)


#####IMPORT THE DATA#####
# as last week, import the dataset from GitHub
first_debate <- read_csv("https://raw.githubusercontent.com/AlexDataPlatz/qrps_sose25/main/data/us_election_2020_1st_presidential_debate.csv")

# alternatively, you can download the csv 
# and import it from your working directory
# getwd() or setwd() will be useful for this 
# can be more reliable, also quicker with larger datasets


# examine the file
head(first_debate)


# edit the speaker variable with regular expressions
first_debate <- first_debate %>%
  mutate(speaker=str_extract(speaker,"[A-z]*$")) 

# examine the file again
head(first_debate)
# notice the change to the speaker variable



#####CREATE THE NECESSARY OBJECTS#####

# fist, create a corpus
debate_corp <- corpus(first_debate)


# let's rename the documents
docnames(debate_corp) <- paste0(1:nrow(first_debate),"_",
                                first_debate$speaker)


# next, create a tokens object
# write the correct command to do this
debate_toks <- _ _ _(debate_corp)

# examine the object to check your code worked
head(_ _ _ _ _ _ _)

# then, create a dfm
# write the code to do this
debate_dfm <- _ _ _ _ _ _ _ 

# examine the object to check that your code worked
_ _ _ _ _ _ _


###### WORKING WITH DICTIONARIES ######

# import the "newsmap" dictionary in yaml format
newsmap_dict <- dictionary(data_dictionary_newsmap_en, 
                           file = "english.yml",
                           format = "YAML")

# look at the whole dictionary
newsmap_dict

# can you import the same dictionary in German?
# (if you do this, make sure you give the object a new name. 
# We need the English version for the next steps...)

# apply the dictionary to the dfm
dict_dfm_results <- dfm_lookup(debate_dfm,
                               newsmap_dict)

# look at some of the results
dict_dfm_results[650:655,111:113]

# what are we looking at?
# what do the numbers 650:655 and 111:113 refer to?
# try with other numbers in the same format


# apply dictionary at tokens level
dict_toks_results <- tokens_lookup(debate_toks,newsmap_dict)

# we can run the same analysis at the token level
dict_toks_results[650:655]

# note the similarities and differences in the results format


## Differences between tokens and dfm
debate_dfm %>% 
  dfm_lookup(newsmap_dict) %>% 
  textstat_frequency() %>% 
  head(4)

debate_toks %>% 
  tokens_lookup(newsmap_dict) %>% 
  dfm() %>% 
  textstat_frequency() %>%
  head(4)

# Why do we get different results? 
# try to guess first, then run the following code

# Look back at the full dictionary for America North US
newsmap_dict$AMERICA$NORTH$US

# look at the tokens for doc 12
tokens_select(debate_toks,newsmap_dict)[12]

# look at the dfm for doc 12
debate_toks[12] %>% 
  dfm() %>% 
  dfm_select(newsmap_dict)

# so, why did we get different frequencies earlier?


# Let's go back to the slides to review what we did...


#####WEIGHTING#####
# what can we do if person A speaks/ writes/ tweets etc a lot more than person B?
# weighting can help us to control for this
# which kind of weighting to use depends on our research question

geography_dfm <- debate_toks %>%
  tokens_lookup(newsmap_dict) %>%
  dfm()

# we can measure the total frequency
geography_dfm %>% 
  textstat_frequency() %>% 
  head(2)

# or frequency per text
geography_dfm %>% 
  dfm_weight("boolean") %>%
  textstat_frequency() %>% 
  head(2)

# if person A and person B create (roughly) the same number of texts,
# but person A's texts are longer, we control for this

# we can also weight word frequencies relative to all words
tokens_lookup(debate_toks, newsmap_dict,nomatch =
                "NN") %>%
  dfm() %>% 
  dfm_group(speaker) %>%
  dfm_weight(
    "prop"
  ) %>% 
  textstat_frequency(group=speaker) %>%
  head()

# this allows us to compare relative frequencies across speakers
# if person A creates more words than person B, we control for this



######CREATE AND APPLY A NEW DICTIONARY#####
# for some operations or packages, it is easier to work with a data frame (df)
# we can convert the dfm to a df for statistical analysis

dfm_lookup(debate_dfm,newsmap_dict) %>%
  convert(
    "data.frame"
  ) %>%
  head()

# we can create our own dictionary and use it to compare groups
# here is a simple example comparing Trump and Biden
# who talks more about economic issues?
# who talks more about cultural issues?

pol_space_dict <- dictionary(list(economy = c(
  'tax*',
  'compet*',
  'dereg*',
  'effici*',
  'job*',
  '*employ*'),
  culture = c(
    '^gun*',
    '*migr*',
    'gender',
    'climate',
    'abort*',
    'marriage*'
  )))

# take a look
pol_space_dict


# useful RegEx for creating dictionaries
# * captures any word with this pattern
# ? captures one additional letter
# ^ captures no additional letters
# [] captures one or other letter (e.g. r[au]n) will return ran and run


# this is only a simple example
# but what results do you expect?
# let's calculate the scores for Biden and Trump
dict_dfm <- tokens_lookup(
  debate_toks,
  dictionary = pol_space_dict,
  valuetype = "glob",  # since you're using RegEx terms
  levels = 1
) %>%
  dfm()

# group dfm by speaker
speaker_dfm <- dfm_group(dict_dfm, groups = first_debate$speaker)

# show the scores for the two candidates
convert(speaker_dfm, to = "data.frame") %>%
  filter(doc_id %in% c("Trump", "Biden"))

# create a plot of the results
# group by speaker and convert to data frame
scores_df <- dfm_group(dict_dfm, groups = first_debate$speaker) %>%
  convert(to = "data.frame") %>%
  filter(doc_id %in% c("Trump", "Biden"))

# reshape to long format for ggplot
scores_long <- scores_df %>%
  pivot_longer(cols = -doc_id, names_to = "category", values_to = "count")

# plot the results
ggplot(scores_long, aes(x = category, y = count, fill = doc_id)) +
  geom_col(position = "dodge") +
  labs(
    title = "Economic vs Cultural Salience by Speaker",
    x = "",
    y = "Frequency",
    fill = "Speaker"
  ) +
  theme_minimal()




# Exercises to try at home

# Try the following tasks with the same corpus or a new one

# e.g. Parlspeech (https://search.gesis.org/research_data/SDN-10.7802-2824)
# e.g. Party Manifestos (https://manifesto-project.wzb.eu/datasets/MPDS2024a)
# Collection of example datasets:(https://www.ics.uci.edu/~smyth/courses/cs175/text_data_sets.html)


# Try applying a different dictionary

# e.g. Lexicoder Sentiment Dictionary in quanteda dictionary: `data_dictionary_LSD2015`
# or look here (https://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/), most for free
# Moral foundations dictionary(https://moralfoundations.org/other-materials/)
# you could translate wordlist / thesaurus you find into a dictionary, 
# e.g.: http://gender-decoder.katmatfield.com/about#masculine


## Exercise: Grouping

# You can also apply dictionaries by group, using docvars to form these groups. 
# You can group the dfm before applying the dictionary with `dfm_group()` 
# or specify the group inside the `textstat_frequency()` command.


## Exercise: Levels

# the newsmap dictionary has multiple levels - continents, regions and countries.

# you can also apply each level separately by specifying `levels=1` 
# (or a different number) inside the `dfm_lookup()` command.

# try applying only the first level of the newsmap dictionary


## Exercise: Weighting

# `dfm_weight()` allows you to weigh your dfm by different characteristics. 
# You can consult the documentation to see the different options.

# Try relative frequency and a binary weighting (all non-zeros coded as 1) 
# How do the results for textstat_frequency differ?


# Exercise: Dictionary Creation

# create a new dictionary and apply it to the presidential debate 
# or another corpus from the above list (or check kaggle.com for others)



# Other Options

# try a ***targeted dictionary analysis***
# see (https://tutorials.quanteda.io/advanced-operations/targeted-dictionary-analysis/) 
# you can apply a dictionary to words surrounding a concept 
# e.g. to find sentiment about a topic 
# or the way media talk about male and female politicians