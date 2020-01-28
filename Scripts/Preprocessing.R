# Text Preprocessing

# clear out environment
rm(list = ls())

# preliminaries, make sure we have the right packages downloaded
# install.packages("quanteda", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE)
# install.packages("preText", dependencies = TRUE)
# install.packages("stopwords", dependencies = TRUE)

# load packages
require(stringr)
require(quanteda)
require(preText)
require(stopwords)

# set working directory (you will need to change this for your computer)
setwd("~/Desktop/Bill_Text")

# lets start with the basics of reading in some documents and generating a
# document term matrix:

# read in documents
documents <- rep("", length = 100)
# loop over documents
for (i in 1:100) {
    cat("currently working on bill:",i,"\n")
    # set the current bill number
    ind <- 97000 + i
    # get the text of the bill
    text <- readLines(paste("Bill_",ind,".txt", sep = ""))
    # collapse it together into a string and store it in a vector
    documents[i] <- paste0(text,collapse = " ")
}

# we can create a quanteda corpus object out of this character vector:
my_corp <- corpus(documents,
                  metacorpus = list(source = "100 Example congressional bills."))
summary(my_corp)

# We could also create it out of a data.frame:
my_corpus_df <- data.frame(doc_name = paste("Bill",1:100),
                           text = documents,
                           continuous_var = rnorm(100),
                           categorical_var = c(rep("cat1",50),
                                               rep("cat2",50)),
                           stringsAsFactors = FALSE)

# note that we need to specify a docid field and a text field
my_corp <- corpus(my_corpus_df,
                  docid_field = "doc_name",
                  text_field = "text",
                  metacorpus = list(source = "100 Example congressional bills with covariate data."))
summary(my_corp)

# The common goal of most text preprocessing is to generate a document-term
# matrix, where each row represents a document, and each column represents the
# count of a vocabulary term in the current document.
doc_term_matrix <- quanteda::dfm(my_corp,
                                 tolower = TRUE,
                                 remove_numbers = TRUE,
                                 remove_punct = TRUE,
                                 remove_separators = TRUE,
                                 remove_twitter = FALSE,
                                 stem = FALSE)

# look at some of the vocabulary
head(doc_term_matrix@Dimnames$features, n = 100)

# get column sums
word_counts <- colSums(doc_term_matrix)

# order word counts
word_counts <- word_counts[order(word_counts, decreasing = TRUE)]

# top words
head(word_counts,n = 100)

# bottom words
tail(word_counts,n = 100)

# another way to look at top terms:
topfeatures(doc_term_matrix,40)

# we can also get the docvars back out as a data.frame:
docvars(doc_term_matrix)

# Now lets move on to working with a builtin corpus in quanteda and try some
# more advanced functionality

# Lets load in some example data:
corp <- quanteda::data_corpus_inaugural

summary(corp)

# Let's look at one document (Washington's first inaugural)
texts(corp)[1]

# Let's look at the contexts in which a couple of words have been used
#  kwic = "key words in context"

# set the line width a bit wider:
options(width=120)

# first we will try the word "humble":
kwic(corp, "humble", window=4)

# now lets try tombstones:
kwic(data_corpus_inaugural, "tombstones", window=4)

# set the line width back to its original value:
options(width=80)

# lets try another example with forming a document-term matrix:
doc_term_matrix <- quanteda::dfm(corp,
                                 tolower = TRUE,
                                 stem = FALSE,
                                 remove_punct = TRUE,
                                 remove = stopwords("english"),
                                 ngrams = 1)

# What kind of object is doc_term_matrix?
class(doc_term_matrix)

# How big is it? How sparse is it?
doc_term_matrix

# Let's look inside it a bit:
doc_term_matrix[1:5,1:5]

# We can also change the settings of our document term matrix generating
# function:
doc_term_matrix <- quanteda::dfm(corp,
                                 tolower = FALSE,
                                 stem = TRUE,
                                 remove_punct = FALSE,
                                 remove = stopwords("english"),
                                 ngrams = 1)

# How big is it now? How sparse is it now?
doc_term_matrix

# Or try adding longer n-grams
doc_term_matrix <- quanteda::dfm(corp,
                                 tolower = TRUE,
                                 stem = FALSE,
                                 remove_punct = TRUE,
                                 remove = stopwords("english"),
                                 ngrams = 2)

# How big is it now? How sparse is it now?
doc_term_matrix

# We can also play around with stopword lists:
options(max.print = 2000)
stopwords_getsources()
stopwords("en", source = "snowball")
stopwords("en", source = "stopwords-iso")
stopwords("en", source = "smart")

# and other languages (check out https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes)
# for other lanugage ISO codes and try one:
stopwords("de")


# now an example using preText:
# http://www.mjdenny.com/getting_started_with_preText.html


# load in U.S. presidential inaugural speeches from Quanteda example data.
corp <- quanteda::data_corpus_inaugural
# use first 10 documents for example
documents <- corp[1:10,]
# take a look at the document names
print(names(documents))

#generate factorial preprocessing specifications
preprocessed_documents <- factorial_preprocessing(
    documents,
    use_ngrams = TRUE,
    infrequent_term_threshold = 0.2,
    verbose = FALSE)

# look at the fields in the list object:
names(preprocessed_documents)
# see the different specifications:
head(preprocessed_documents$choices)

# generate pretext scores:
preText_results <- preText(
    preprocessed_documents,
    dataset_name = "Inaugural Speeches",
    distance_method = "cosine",
    num_comparisons = 20,
    verbose = FALSE)

# create a preText plot ranking specifications
preText_score_plot(preText_results)

# see which features "matter"
regression_coefficient_plot(preText_results,
                            remove_intercept = TRUE)





