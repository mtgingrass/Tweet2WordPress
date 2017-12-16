### Libraries
library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(plyr)
library(RColorBrewer)

### Set API Keys
api_key <- "XXXXXXXXXXXXXXXXXXXX"
api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxx"

### Authorize Twitter API and Grab Latest Tweets
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Grab latest tweets
tweets_trump = searchTwitter('#NetNeutrality', n = 1000)
#today_trends = getTrends(period = "daily", date=Sys.Date())

today_trends = getTrends(2364559)


tweets.text = twListToDF(tweets_trump)
tweets.text = tweets.text[,1]

### Create a corpus
tweet.corpus = Corpus(VectorSource(tweets.text))

##### Remove Certain Characters and Words
#https://github.com/raredd/regex
#http://www.gnu.org/software/grep/manual/html_node/Character-Classes-and-Bracket-Expressions.html
tweet.removeURL = function(x) gsub("http[^[:space:]]*","",x)
tweet.removeATUser = function(x) gsub("@[a-z,A-Z]*","",x)
tweet.removeEmoji = function(x) gsub("\\p{So}|\\p{Cn}", "", x, perl = TRUE)
tweet.removeSpecialChar = function(x) gsub("[^[:alnum:]///' ]", "", x)

tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeURL))

tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeATUser))

tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeEmoji))
tweet.corpus = tm_map(tweet.corpus, content_transformer(tweet.removeSpecialChar))
tweet.corpus = tm_map(tweet.corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
tweet.corpus = tm_map(tweet.corpus, content_transformer(tolower))

#words like "And" or "the" are removed.
tweet.corpus=tm_map(tweet.corpus, removeWords, c(stopwords("english"), "NetNeutrality", "RT", "rt"))
tweet.corpus=tm_map(tweet.corpus, removeNumbers)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus=tm_map(tweet.corpus, removeWords, c(stopwords("english"), "NetNeutrality","netneutrality", "RT", "rt"))
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
#converts things like "learns" to "learn" or "running" to "run" -Omitting for now
#tweet.corpus = tm_map(tweet.corpus, stemDocument)


ap.tdm <- TermDocumentMatrix(tweet.corpus)
ap.m <- as.matrix(ap.tdm)

ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
freqterms = findFreqTerms(ap.tdm, 15)

#https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf
pal2 <- brewer.pal(8,"Dark2")
png("realdonaldtrump.png", width=1920,height=1080)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=15,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

