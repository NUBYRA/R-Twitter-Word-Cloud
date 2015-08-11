library(devtools)
library(twitteR) ## install_github("twitteR", username="geoffjentry")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# First go to https://dev.twitter.com/ and log in with your Twitter Account and create an App and obtain the following information
consumer_key <- "Your Consumer Key"
consumer_secret <- "Your Consumer Secret"
access_token <- "Your Access Token"
access_secret <- "Your Access Secret"
 
# authenticate in R
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

# Collecting tweets only written in english from 07/31/15
tweets <- searchTwitter("#healthyliving", since="2015-07-31", until="2015-08-01", lang="en")
tweets <- twListToDF(tweets) # converting to a dataframe for simplicity

# A mini function to clean up texts
clean.txt<-function(txt){   
    txt <- sapply(txt,function(row) iconv(row, "latin1", "ASCII", sub=""))
    txt <- tolower(txt)     # convert to lower case
    txt <- gsub("rt","",txt)    # remove "rt"
    txt <- gsub("@\\S+","",txt) # remove @user
    txt <- gsub("http\\S+\\s*", "", txt)  # remove urls 
    txt <- gsub("[[:punct:]]","",txt) # remove punctuations 
    txt <- gsub('[[:cntrl:]]', "", txt) # remove Controls and special characters
    txt <- gsub("[[:digit:]]", "", txt) #remove digits
    txt <- gsub("^[[:space:]]*","",txt) # remove trailing whitespaces in the beginning 
    txt <- gsub("[[:space:]]*$","",txt) # remove trailing whitespcaes in the end
    txt <- gsub(' +'," ",txt) # remove extra whitespaces
}

# Cleaned texts 
tweets.txt<-clean.txt(tweets$text)

# Create corpus
txt.corpus <- Corpus(VectorSource(tweets.txt))

# Create a term-document matrix from a corpus and remove stopwords
text.doc <- TermDocumentMatrix(txt.corpus, control=list(stopwords=c("healthyliving", "healthy", "health",stopwords("english"))))
        
# Word count 
word.count <- sort(rowSums(as.matrix(text.doc)), decreasing=TRUE)

# Create a dataframe with words and its frequencies
df <- data.frame(word = names(word.count), count=word.count)

# Create Word Cloud
wordcloud(df$word, df$count, random.color = TRUE, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words = 200)