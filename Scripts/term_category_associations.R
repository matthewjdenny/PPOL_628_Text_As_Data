# Term-category associations

# Preliminaries
rm(list = ls())

# load in packages
library(quanteda)
library(SpeedReader)
library(slam)

# optionally install the quanteda.corpora package for access to additional
# corpora:
#devtools::install_github("quanteda/quanteda.corpora")
library(quanteda.corpora)

# load in the example data that I uploaded to the github for this course:
load("~/Desktop/Example_Bills_Corpus_Object.RData")

# take a look at our data:
summary(bills)

# create a dtm with 1-4gram features:
dtm <- dfm(bills,
           remove_punct = TRUE,
           remove_numbers = TRUE,
           ngrams = 1:4)

# now we are going to convert this into a simple triplet matrix format:
dtm_triplet <- SpeedReader::convert_quanteda_to_slam(dtm)

# extract the document features so we can use them to create a
# contingency table:
document_covariates <- docvars(dtm)

# now we create a contingency table over topics and parties. Note that the order
# we input the variables_to_use vetor will change the order of the rows in the
# contingency table, even though we will get the same results either way:
topic_party_table <- contingency_table(
    metadata = document_covariates,
    document_term_matrix = dtm_triplet,
    variables_to_use = c("topic","party"),
    threshold = 10
    )

# and now with grouping variables reversed, and a lower threshold on a
# combination of covariates occuring for inclusion in the contingency table:
topic_party_table2 <- contingency_table(
    metadata = document_covariates,
    document_term_matrix = dtm_triplet,
    variables_to_use = c("party","topic"),
    threshold = 1
)

# look at rownames to get their numeric indices:
rownames(topic_party_table)
rownames(topic_party_table2)

# set the prior as the average number of terms in each row in the contingency
# table. Note that the choice of prior will have a significant effect on results
avg_terms_per_category <- mean(slam::row_sums(topic_party_table))

# first let's  experiment with ranking by z-scores:
top_features <- feature_selection(topic_party_table,
                                  rows_to_compare = c(2,1),
                                  alpha = avg_terms_per_category,
                                  method = "informed Dirichlet",
                                  rank_by_log_odds = FALSE)

# and now we reduce alpha significantly:
top_features <- feature_selection(topic_party_table,
                                  rows_to_compare = c(2,1),
                                  alpha = .001*avg_terms_per_category,
                                  method = "informed Dirichlet",
                                  rank_by_log_odds = FALSE)

# now lets try ranking by log-odds and see how the ranking changes:
top_features <- feature_selection(topic_party_table,
                                  rows_to_compare = c(2,1),
                                  alpha = avg_terms_per_category,
                                  method = "informed Dirichlet",
                                  rank_by_log_odds = TRUE)

# and now we reduce alpha significantly and look at log-odds ratios:
top_features <- feature_selection(topic_party_table,
                                  rows_to_compare = c(2,1),
                                  alpha = .001*avg_terms_per_category,
                                  method = "informed Dirichlet",
                                  rank_by_log_odds = TRUE)

# Now lets generate some plots with different input rows, and generate some
# funnel plots to illustrate the top terms:
rownames(topic_party_table)
top_features <- feature_selection(topic_party_table,
                                  rows_to_compare = c(6,5),
                                  alpha = avg_terms_per_category,
                                  method = "informed Dirichlet",
                                  rank_by_log_odds = FALSE)

# output a pdf with the plot included:
pdf(file = "~/Desktop/Example_Fightin_Words_Plot_1.pdf",
    width = 10,
    height = 8.5)
fightin_words_plot(top_features,
                   positive_category = "Democrat",
                   negative_category = "Republican",
                   max_terms_to_display = 100000)
dev.off()


# we can also try to generate a plot with subsumed n-grams as top terms:
top_features <- feature_selection(topic_party_table,
                                  rows_to_compare = c(6,5),
                                  document_term_matrix = dtm_triplet,
                                  alpha = avg_terms_per_category,
                                  method = "informed Dirichlet",
                                  rank_by_log_odds = FALSE,
                                  subsume_ngrams = TRUE)
# and output the pdf
pdf(file = "~/Desktop/Example_Fightin_Words_Plot_2.pdf",
    width = 10,
    height = 8.5)
fightin_words_plot(top_features,
                   positive_category = "Democrat",
                   negative_category = "Republican",
                   use_subsumed_ngrams = TRUE,
                   max_terms_to_display = 200000)
dev.off()


# now try log odds ranking. Note that we can turn on n-gram subsumption in the
# feature selection function but still output the non-subsumed terms as well:
top_features <- feature_selection(topic_party_table,
                                  rows_to_compare = c(6,5),
                                  document_term_matrix = dtm_triplet,
                                  alpha = avg_terms_per_category,
                                  method = "informed Dirichlet",
                                  rank_by_log_odds = TRUE,
                                  subsume_ngrams = TRUE)

# output a PDF:
pdf(file = "~/Desktop/Example_Fightin_Words_Plot_3.pdf",
    width = 10,
    height = 8.5)
fightin_words_plot(top_features,
                   positive_category = "Democrat",
                   negative_category = "Republican",
                   use_subsumed_ngrams = FALSE,
                   max_terms_to_display = 200000)
dev.off()

# and now try the same plot but with subsumed n-grams:
pdf(file = "~/Desktop/Example_Fightin_Words_Plot_4.pdf",
    width = 10,
    height = 8.5)
fightin_words_plot(top_features,
                   positive_category = "Democrat",
                   negative_category = "Republican",
                   use_subsumed_ngrams = TRUE,
                   max_terms_to_display = 200000)
dev.off()


############ ACMI #############

# Remember that we can calculate the mutual information of the joint
# distribution implied by the normalization of a contingency table:
mutual_information(topic_party_table[1:2,])

# lets look at some terms
colnames(topic_party_table)[1:50]

# lets try removing a stop-term like "the":
mutual_information(topic_party_table[1:2,-12])

# now we can calcualte term ACMI as a way of assessing how much information each
# terms gives us about category, on average.
acmi_contribs <- ACMI_contribution(topic_party_table)

# now lets look at terms by their ACMI:
term_acmi <- data.frame(
    term = colnames(topic_party_table),
    acmi = acmi_contribs$average_contribution,
    count = slam::col_sums(topic_party_table),
    stringsAsFactors = FALSE)
# and order by acmi score:
term_acmi <- term_acmi[order(term_acmi$acmi, decreasing = F),]

# null out the rownames for display:
rownames(term_acmi) <- NULL

# lets look at the number of terms in each broad category:
length(which(term_acmi$acmi < 0))
length(which(term_acmi$acmi == 0))
length(which(term_acmi$acmi > 0))

# and look at the terms with the highest and lowest ACMI scores:
head(term_acmi,n = 50)
tail(term_acmi,n = 50)

