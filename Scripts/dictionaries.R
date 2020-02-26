# Text Preprocessing

# clear out environment
rm(list = ls())

# preliminaries, make sure we have the right packages downloaded
# install.packages("quanteda", dependencies = TRUE)

# https://tutorials.quanteda.io/advanced-operations/targeted-dictionary-analysis/
# https://quanteda.io/articles/pkgdown/quickstart.html

# lets load in quanteda
require(quanteda)
require(ggplot2)

# load in example corpus of inaugural speeches
example <- corpus(data_corpus_inaugural)

# take a look at the documents
summary(example)

# tokenize the documents:
toks <- tokens(example, remove_punct = TRUE)

# Lexicoder Sentiment Dictionary:
summary(data_dictionary_LSD2015)

# lets look at the different term classes:
data_dictionary_LSD2015$positive[1:100]
data_dictionary_LSD2015$negative[1:100]
data_dictionary_LSD2015$neg_positive[1:100]
data_dictionary_LSD2015$neg_negative[1:100]

# now we code our speeches using the LSD dictionary
coded <- tokens_lookup(
    toks,
    dictionary =  data_dictionary_LSD2015)
head(coded, 3)

# now we make a document_term matrix out of the coded terms:
dfm_lsd <- dfm(coded)
# and convert it to a data.frame:
valences_by_speech<- convert(dfm_lsd, to = "data.frame")

# pull out year
valences_by_speech$year <- as.numeric(stringr::str_replace_all(valences_by_speech$document,"-.*",""))

# get sum of term counts
all_words <- dfm(toks)
valences_by_speech$total_words <- rowSums(all_words)

# calculate Y&S measure:
valences_by_speech$valence <- (valences_by_speech$positive/valences_by_speech$total_words) - (valences_by_speech$negative/valences_by_speech$total_words)

# take a look at valence over time:
ggplot(valences_by_speech, aes(x = year, y = valence)) +
    geom_point() +
    geom_smooth()


# another example
recent_corpus <- corpus_subset(data_corpus_inaugural, Year > 1991)

# create a custom dictionary
my_dict <- dictionary(list(terror = c("terrorism",
                                      "terrorists",
                                      "threat"),
                           economy = c("jobs",
                                       "business",
                                       "grow",
                                       "work")))

# create a dfm out of coded words:
by_pres_mat <- dfm(recent_corpus, dictionary = my_dict)
# take a look:
by_pres_mat


#### lets actually pull out the positive and negative terms ####
# create a custom dictionary which starts as a list:
temp <- as.list(c(data_dictionary_LSD2015$positive,
                  data_dictionary_LSD2015$negative))

# set the names equal to the word stems:
names(temp) <- stringr::str_replace_all(
    c(data_dictionary_LSD2015$positive,
      data_dictionary_LSD2015$negative),
    "[^a-zA-Z]",
    "")

# now we create a dfm with only positive and negative terms:
only_keep_pos_neg <- dfm(example,
                         dictionary = dictionary(temp))

# look at the top features:
topfeatures(only_keep_pos_neg, n = 100)

