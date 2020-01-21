# Preliminaries
rm(list = ls())
# Set your working directory to some place you can find
setwd("~/Desktop")

# First we will need to install the packages we plan to use for this exercise (
# if they are not already installed on your computer).
# install.packages("httr", dependencies = TRUE)
# install.packages("stringr", dependencies = TRUE)

# httr is a package for downloading html
library(httr)
# A package for manipulating strings
library(stringr)

# Lets start by downloading an example web page:
url <- "http://www.mjdenny.com/Rcpp_Intro.html"

# We start by using the httr package to download the source html
page <- httr::GET(url)

# As we can see, this produces a great deal of information
str(page)

# To get at the actual content of the page, we use the content() function:
page_content <- httr::content(page, "text")

# Now lets print it out
cat(page_content)

# and write it to a file for easier viewing
write.table(x = page_content,
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE,
            file = "Example_1.html")

# lets try a more complicated example page for a peice of legislation in the
# U.S. Congress
url <- "https://www.congress.gov/bill/103rd-congress/senate-bill/486/text"

# we start by using the httr package to download the source html
page <- httr::GET(url)

# as we can see, this produces a great deal of information
str(page)

# to get at the actaul content of the page, we use the content() function
page_content <- httr::content(page, "text")

# now lets print it out
cat(page_content)

# and write it to a file for easier viewing
write.table(x = page_content,
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE,
            file = "Example_2.html")



### Web Scraping Example, Part 1 ###

url <- "https://scholar.google.com/scholar?hl=en&q=laurel+smith-doerr"

# we start by using the httr package to download the source html
page <- httr::GET(url)

# as we can see, this produces a great deal of information
str(page)

# to get at the actaul content of the page, we use the content() function
page_content <- httr::content(page, "text")

# now lets print it out
cat(page_content)

# and write it to a file for easier viewing
write.table(x = page_content,
            col.names = FALSE,
            row.names = FALSE,
            quote = FALSE,
            file = "Example_3.html")


####### Full Example #########
# Lets try a real example from: https://www.govinfo.gov/bulkdata/BILLSUM/115/hr

library(XML)
# download the page
page <- httr::GET("https://www.govinfo.gov/bulkdata/BILLSUM/115/hr")

# get the text
page <- httr::content(page, "text")

# get the urls out of the page:
urls <- stringr::str_extract_all(
    page,
    "<link>[^<]+</link>")[[1]]

urls <- stringr::str_replace_all(urls,"<(l|/l)ink>","")

data <- data.frame(url = urls,
                   measure_number = rep(0,length(urls)),
                   title = rep("",length(urls)),
                   final_stage = rep("",length(urls)),
                   text = rep("",length(urls)),
                   stringsAsFactors = FALSE)
# loop through urls, download them, extract information:
for (i in 1:10) {
    cat("currently working on bill",i,"of",length(urls),"\n")
    bill <- httr::GET(urls[i])
    text <- httr::content(bill, "text")
    bill_list <- XML::xmlToList(text)

    text <- stringr::str_extract_all(text,"<summary-text>.*</summary-text>")[[1]][1]
    text <- stringr::str_replace_all(text,"<summary-text><!\\[CDATA\\[","")
    text <- stringr::str_replace_all(text,"\\]\\]></summary-text>","")

    data$text[i] <- text

    data$measure_number[i] <- bill_list$item$.attrs[3]
    data$title[i] <- bill_list$item$title
    data$final_stage[i] <- bill_list$item$summary$`action-desc`

    #make sure we are not downloading data too fast
    Sys.sleep(2)
}


# Scraping Twitter (if we have time)
# based on a tutorial available: https://github.com/pablobarbera/streamR

# Before you do anything with this tutorial, make sure you have a Twitter
# account. You will need one to get the proper access to download tweets.



### Letting R access the Twitter API ###

# Lets start by downloading the R packages we will need:
install.packages(c("ROAuth","devtools","ggplot2","maps", "streamR"),
                 repos = "http://cran.r-project.org")

# Alternatively we can get the latest version from Pablo's github:
# devtools::install_github("pablobarbera/streamR/streamR", force = TRUE)

# Before you can scrape Twitter, you will need to follow the directions here:
# https://github.com/SMAPPNYU/smappR#b-creating-your-own-twitter-oauth-token
# to create an authorization token which can be used access the Twitter API.

library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"

# You will need to fill in these two as suggested in the directions given at the
# webpage above:
consumerKey <- "XXXXXXXXXXX"
consumerSecret <- "XXXXXXXXXXXXXXXX"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

# Now run this line of code and go to the URL it prints out (it may also just
# take you directly to the webpage).
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem",
                                        package = "RCurl"))

# Now save your credentials for future use!
setwd("~/Dropbox/Credentials/")
save(my_oauth, file = "my_oauth.RData")



### Twitter Scraping Example, Part 1 ###

# Now lets walk through a simple example collecting tweets containing the
# terms "Trump" and "POTUS" in the next sixty seconds. This example follows
# along with: https://github.com/pablobarbera/streamR

# Load the streamR library
library(streamR)

# Load in your access credential we created above
setwd("~/Dropbox/Credentials/")
load("my_oauth.Rdata")

# Now set your working directory to the location where you would like to save
# the tweets you are about to collect:
setwd("~/Desktop")

# Here we are going to use the filter function which uses dome sort of criteria
# for determining which tweets should be saved.
filterStream("tweets2.json", # name of file that we want to save tweets to.
             track = c("Trump","POTUS"), # key terms to collect.
             timeout = 60, # amount of time to collect data in seconds.
             oauth = my_oauth) # your token!

# Load in the tweets from the tweets.json file where they were stored and turn
# them into a data.frame:
tweets.df <- parseTweets("tweets.json",
                         simplify = FALSE)

# We can also just collect a random (approximately 1%) sample of all tweets on
# twitter. I suggest only doing this for a short period of time on your
# computer:

# Capture the tweets:
sampleStream("tweetsSample.json",
             timeout = 120,
             oauth = my_oauth,
             verbose = FALSE)

# Load them into a data.frame and take a look:
tweets.df2 <- parseTweets("tweetsSample.json",
                          verbose = TRUE)
