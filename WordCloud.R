library(devtools)
library(twitteR)  # install_github("twitteR", username="geoffjentry")
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(plyr)

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
preprocess.text<-function(txt){	
	txt	<- sapply(txt,function(row) iconv(row, "latin1", "ASCII", sub="")) # remove non-ASCII characters
	txt <- tolower(txt) # convert to lower case
	txt <- gsub("rt","",txt) # remove "rt"
	txt <- gsub("@\\S+","",txt) # remove @user
	txt <- gsub("http\\S+\\s*", "", txt) # remove urls  
	txt	<- gsub("[[:punct:]]","",txt) # remove punctuations  
	txt <- gsub('[[:cntrl:]]', "", txt) # remove Controls and special characters
	txt <- gsub("[[:digit:]]", "", txt) #remove digits
	txt <- gsub("([[:alpha:]])\\1\\1+", "\\1\\1", txt) #replace characters that appear three or more times consecutively to two
	pattern <- paste0("\\b(", paste0(c("healthyliving", "healthy", "health", stopwords("english")), collapse="|"), ")\\b")	
	txt <- gsub(pattern,"",txt)	#remove stopwords
	txt <- gsub("^[[:space:]]*","",txt)	# remove trailing whitespaces in the beginning 
	txt <- gsub("[[:space:]]*$","",txt) # remove trailing whitespcaes in the end
	txt <- gsub(' +'," ",txt) # remove extra whitespaces
	txt
}

# Clean texts 
cleaned.txt<-preprocess.text(tweets$text)

# Count words 
words <- unlist(strsplit(cleaned.txt," "))
word.count<-count(words)
colnames(word.count) <- c("wods", "count")

# Sort frequncy of words from highest to lowest 
df <- counts[order(-word.count$count),]

# Create Word Cloud 
wordcloud(df$word, df$count, random.color = TRUE, random.order=FALSE, colors=brewer.pal(8, "Dark2"), max.words = 200)