# Text Preprocessing

# clear out environment
rm(list = ls())

# preliminaries, make sure we have the right packages downloaded
# install.packages("quanteda", dependencies = TRUE)
# install.packages("phrasemachine", dependencies = TRUE)

library(phrasemachine)
library(quanteda)

# exerpt from Obama's 2016 press corps dinner:
# https://www.washingtonpost.com/news/reliable-source/wp/2016/05/01/the-complete-transcript-of-president-obamas-2016-white-house-correspondents-dinner-speech/
example_text <- "Anyway, here we are, my eighth and final appearance at this unique event. And I am excited. If this material works well, I’m going to use it at Goldman Sachs next year. Earn me some serious Tubmans. That’s right. That’s right. My brilliant and beautiful wife Michelle is here tonight. She looks so happy to be here. It’s called practice. It’s like learning to do three-minute planks. She makes it look easy now."

# we are going to use the phrasemachine() function.
?phrasemachine()

# lets try a simple example with the default POS tag pattern and ngrams
# from length 1-3. This will get us back simple noun phrases:
phrasemachine(example_text,
              regex = "(A|N)*N(PD*(A|N)*N)*",
              maximum_ngram_length = 3,
              minimum_ngram_length = 1)

# now lets try it with longer phrases:
phrasemachine(example_text,
              regex = "(A|N)*N(PD*(A|N)*N)*",
              maximum_ngram_length = 8,
              minimum_ngram_length = 2)

# lets do the same thing, but now allowing for noun phrases with coordination and verb phrases:
# "((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)", # NP
# "(((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)(P(CP)*)*(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*(D(CD)*)*((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)+|((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)(P(CP)*)*((M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*(D(CD)*)*((A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*(C(D(CD)*)*(A(CA)*|N)*N((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)*)*)|(M(CM)*|V)*V(M(CM)*|V)*(C(M(CM)*|V)*V(M(CM)*|V)*)*((P(CP)*)+(D(CD)*)*(A(CA)*|N)*N)+))") # VP
phrasemachine(example_text,
              regex = "Phrases",
              maximum_ngram_length = 4,
              minimum_ngram_length = 2)

# now lets return the POS tag patterns as well:
output <- phrasemachine(
    example_text,
    regex = "Phrases",
    return_tag_sequences = TRUE)

# turn into a data frame
example_data <- data.frame(text = output[[1]]$phrases,
                           tag_sequence = output[[1]]$tags)

# take a look at some tag patterns:
example_data[which(example_data$tag_sequence == "AN"),]

example_data[which(example_data$tag_sequence == "NVM"),]


# Lets look inside phrasemachine to see how it does POS tagging under the hood:
# get rid of extra spaces.
document <- stringr::str_replace_all(example_text,"[\\s]+"," ")
document <- stringr::str_replace_all(document,"[\\s]$","")

document <- NLP::as.String(document)

# tokenize document into words:
wordAnnotation <- NLP::annotate(
    document,
    list(openNLP::Maxent_Sent_Token_Annotator(),
         openNLP::Maxent_Word_Token_Annotator()))
# annotate words with POS tags:
POSAnnotation <- NLP::annotate(
    document,
    openNLP::Maxent_POS_Tag_Annotator(),
    wordAnnotation)

# lets take a look at the output:
POSAnnotation[1:30]

# extract the tagged words so we can get the tokens
POSwords <- subset(POSAnnotation, type == "word")

# extract the tokens and tags
tags <- sapply(POSwords$features, '[[', "POS")
tokens <- document[POSwords][1:length(tags)]

# store everything in a list object
tagged_document <- data.frame(tokens = tokens,
                              tags = tags,
                              stringsAsFactors = FALSE)

# tag interpretation:
# https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html



# Lets load in some example data:
corp <- quanteda::data_corpus_inaugural

summary(corp)

# Let's look at one document (Washington's first inaugural)
documents <- texts(corp)

# now we POS tag the documents:
phrase_documents <- phrasemachine(
    documents,
    regex = "Phrases",
    maximum_ngram_length = 4,
    minimum_ngram_length = 2,
    return_phrase_vectors = TRUE,
    return_tag_sequences = FALSE,
    memory = "-Xmx512M")

# turn from a list of character vectors of tokens into a character vector of space separated words:
temp <- unlist(lapply(phrase_documents,paste0,collapse = " "))

# assign back into our corpus object to retain metadata
texts(corp) <- temp

# now we tokenize only on whitespaces
phrase_tokens <- tokens(corp,
                        what = "fastestword")


# and create a document term matrix
doc_term_matrix <- quanteda::dfm(phrase_tokens,
                                 tolower = TRUE,
                                 stem = FALSE,
                                 remove_punct = FALSE)

doc_term_matrix

# take a look at the top 100 most freuent terms:
topfeatures(doc_term_matrix,100)

# now look at least frequent terms:
topfeatures(doc_term_matrix,
            n =40,
            decreasing = FALSE)

# can also look by document frequency
topfeatures(doc_term_matrix,
            n = 100,
            scheme = "docfreq")

# grouping by president last name
topfeatures(doc_term_matrix,
            n = 5,
            groups = "President")









