# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stopwords")

text <- readLines(file.choose(),encoding="UTF-8")

#filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
#text <- readLines(filePath)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "●")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
#stopwords("hr", source = "stopwords-iso")
docs <- tm_map(docs, removeWords,stopwords("hr", source = "stopwords-iso"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("lip","koje","ožu","će","vlj","svi","slika","kol","l","–","stu","lis","srp","pro","tra","koji","kako","sij","što","koja","engl","kao","prema","odnosu","–"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
d
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 20,
          max.words=30, random.order=F, rot.per=0.5,
          colors=brewer.pal(8, "Dark2"),scale = c(2.5,1))
