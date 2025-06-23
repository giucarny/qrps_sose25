library(tidyverse)
library(quanteda)
library(quanteda.textmodels)


# new package for today's class
# caret is useful for measuring performance
install.packages("caret")
library(caret)

# MACHINE LEARNING

##########GETTING STARTED###########
# Let's practice with an example
# first, load Quanteda's movie review corpus
reviews_corp <- data_corpus_moviereviews

# take a look
head(reviews_corp)
docvars(reviews_corp)

# make a tokens object, removing punctuation
reviews_toks <- tokens(reviews_corp, 
                       remove_punct=T)

# make a dfm
reviews_dfm <- dfm(reviews_toks)

head(reviews_dfm)

# as discussed, we can use this to train a classifier

# remember the workflow
# first, create train and test subsets
reviews_train <- dfm_sample(reviews_dfm,
                            0.8*ndoc(reviews_corp))

reviews_test <- dfm_subset(reviews_dfm,
!(docnames(reviews_dfm) %in% docnames(reviews_train)))

# training data is usually larger than testing data
# (here it is an 80-20 split)
# why?

# make sure features match
# only use features which are in the training data

# select the training features
reviews_train <- reviews_train %>% 
                 dfm_trim(1)


# remove features not in training data
reviews_test <- dfm_match(reviews_test, 
                          featnames(reviews_train))

# let's go back to the slides to discuss the next steps

###########


#######NAIVE BAYES CLASSIFIER###########

# apply the NB classifier to our training data via Quanteda
nb_model <- textmodel_nb(reviews_train,docvars(reviews_train,
                                             "sentiment"
))

# view the model
nb_model

# this is the model ("classifier") we will now use
# to classify reviews as positive or negative
# notice how fast it is

# now apply the trained classifier to the test set
test_predictions <- predict(nb_model,
                            newdata=reviews_test)

# view the results
head(test_predictions, 5)

# how did our model do?
table(docvars(reviews_test,
              "sentiment"),test_predictions)

# should see that many/ most predictions are correct
# % of incorrect classifications gives us some idea of error rate

# EVALUATION
# create a "Confusion Matrix" via the caret package
confusionMatrix(as.factor(docvars(reviews_test,
                                  "sentiment")), test_predictions)



# Let's work through another example
# First, we load the House of Commons Corpus from the ParlSpeech Dataset
# (https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/L4OAKN/W2SVMF&version=1.0)

# download and save in your working directory
# (it is 2.3GB and may take time to download and import!)
# (similar datasets are available via GESIS)

# then import into your R environment
Corp_HouseOfCommons_V2 <- readRDS("Corp_HouseOfCommons_V2.rds")


# We keep only speeches where the speaker is not the chair and is a member of the largest party.

# We also transform the party variable into a factor and transform the date (including the creation of a year variable) since we will need both later.

corp_hoc <- Corp_HouseOfCommons_V2 %>%
  filter(chair==F) %>%
  filter(party %in% c("Con","Lab")) %>%
  mutate(date=as.Date(date)) %>% 
  mutate(year=lubridate::year(date)) %>%
  mutate(party=as.factor(party))

corpus_hoc <- corpus(corp_hoc)

# Overview: How much data do we have over time?
  
corp_hoc %>% 
  ggplot()+
  geom_histogram(aes(x=date),stat="count")+
  facet_wrap(~party)

# To make the task more manageable on a normal computer, 
# we draw a sample of 4000 speeches per year. 

sample_corp <- corpus_hoc %>%
  corpus_sample(4000, by = docvars(corpus_hoc, "year"))

# save(sample_corp,file="../data/sample_corp.RData")

# Now, we extract data for one year from our dfm.

corpus_hoc_2019 <- corpus_subset(sample_corp,year==2019)


# now create a dfm 
# can also e.g. remove punctuation or similar pre-processing.

dfm_hoc <- corpus_hoc_2019 %>%
  tokens(remove_punct = TRUE) %>%
  dfm()

hoc_train <- dfm_sample(dfm_hoc,0.8*ndoc(dfm_hoc))
hoc_test <- dfm_subset(dfm_hoc,!(docnames(dfm_hoc) %in% docnames(hoc_train)))

# we may remove rare or frequent features

hoc_train <- dfm_trim(hoc_train,
                        min_docfreq=0.01,
                        max_docfreq=0.8,
                        docfreq_type="prop",
                        verbose=T)

# Adjust the features of the test dfm to the training dfm

hoc_test <- dfm_match(hoc_test,featnames(hoc_train))



# train the model, predict & see how well we did.

nb_model <- textmodel_nb(hoc_train,docvars(hoc_train,
                                         "party"))
nb_model

preds <- predict(nb_model,hoc_test)
table(preds,docvars(hoc_test,"party"))

# how did it perform?

# accuracy calculation

sum(docvars(hoc_test,"party")==preds)/length(preds)

# confusion matrix:
caret::confusionMatrix(preds,docvars(hoc_test, "party"))

# The effect of pre-processing

# Repeat what we have done above but use different trimming thresholds
# look at both the accuracy and the speed to get an idea of what works