setwd("~/Projects/NewsSentiment")

library(tm)
library(SnowballC)
library(dplyr)
library(wordcloud)

data <- read.csv('rawdata.csv')

# Clean the text
corpus <- VCorpus(VectorSource(data$content))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stripWhitespace)

# Create Model
dtm <- corpus %>% DocumentTermMatrix

# Filter non-frequent words
dtm <- dtm %>% removeSparseTerms(.992)

dataset <- dtm %>% as.matrix %>% as.data.frame
dataset$article.subject <- data$subject
dataset$article.source <- data$source.name

# Get a word count for each subject/source
agg.data <- dataset %>% group_by(article.subject, article.source) %>% summarise_all(sum)

# Remove unnecessary words
agg.data['chars'] <- 0
agg.data['yearold'] <- 0
agg.data['amp'] <- 0
agg.data['th…'] <- 0
agg.data['scho…'] <- 0
agg.data['trailer'] <- 0

# Remove each subject's own name to avoid being trivial
agg.data[agg.data$article.subject == '"Bernie Sanders"', ]['bernie'] <- 0
agg.data[agg.data$article.subject == '"Bernie Sanders"', ]['sanders'] <- 0

agg.data[agg.data$article.subject == '"Donald Trump"', ]['donald'] <- 0
agg.data[agg.data$article.subject == '"Donald Trump"', ]['trump'] <- 0
agg.data[agg.data$article.subject == '"Donald Trump"', ]['trump’s'] <- 0

agg.data[agg.data$article.subject == 'Kamala', ]['kamala'] <- 0
agg.data[agg.data$article.subject == 'Kamala', ]['harris'] <- 0

agg.data[agg.data$article.subject == 'Beto', ]['beto'] <- 0
agg.data[agg.data$article.subject == 'Beto', ]['orourke'] <- 0
agg.data[agg.data$article.subject == 'Beto', ]['o’rourke'] <- 0

agg.data[agg.data$article.subject == 'Biden', ]['joe'] <- 0
agg.data[agg.data$article.subject == 'Biden', ]['biden'] <- 0
agg.data[agg.data$article.subject == 'Biden', ]['biden’s'] <- 0
agg.data[agg.data$article.subject == 'Biden', ]['bidens'] <- 0

agg.data[agg.data$article.subject == '"Elizabeth Warren"', ]['elizabeth'] <- 0
agg.data[agg.data$article.subject == '"Elizabeth Warren"', ]['warren'] <- 0

agg.data[agg.data$article.subject == 'Buttigieg', ]['buttigieg'] <- 0
agg.data[agg.data$article.subject == 'Buttigieg', ]['pete'] <- 0

# Calculate the total use of each word in all articles
agg.data.total <- dataset %>% subset(select=-c(article.subject, article.source)) %>% colSums %>% as.data.frame
agg.data.total <- agg.data.total %>% t

# Calculate word use in each subject/source as percentage of total
agg.avg <- agg.data
agg.avg[-c(1,2)] <- agg.data[-c(1,2)] %>% apply(1, function(x) x/agg.data.total) %>% t

# Multiply percentages by 1000 to get good spread for word cloud
word.data <- agg.avg %>% as.data.frame
word.data[,-c(1:2)] <- word.data[,-c(1:2)] * 1000

# Transpose data frame and set article subject and source as column names
word.data <- word.data %>% t %>% as.data.frame(stringsAsFactors = F)
colnames(word.data) <- paste(gsub('"', '', word.data['article.subject',]), word.data['article.source',])
word.data <- word.data[-c(1:2),]

# Create "words" column with row names
word.data$words <- word.data %>% row.names

# Plot word clouds
wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Bernie Sanders Breitbart News`),
          max.words = 40,
          scale=c(2.9,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "OrRd")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Donald Trump Breitbart News`),
          max.words = 40,
          scale=c(2.5,.01),
          rot.per=0.25, 
          colors=brewer.pal(5, "OrRd")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Elizabeth Warren Breitbart News`),
          max.words = 40,
          scale=c(2.4,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "OrRd")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Beto Breitbart News`),
          max.words = 40,
          scale=c(3.0,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "OrRd")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Biden Breitbart News`),
          max.words = 40,
          scale=c(2.5,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "OrRd")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Buttigieg Breitbart News`),
          max.words = 40,
          scale=c(2.8,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "OrRd")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Bernie Sanders RT`),
          max.words = 40,
          scale=c(2.5,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuGn")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Donald Trump RT`),
          max.words = 40,
          scale=c(3.0,.3),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuGn")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Elizabeth Warren RT`),
          max.words = 40,
          scale=c(2.7,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuGn")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Beto RT`),
          max.words = 40,
          scale=c(2.6,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuGn")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Biden RT`),
          max.words = 40,
          scale=c(2.9,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuGn")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Buttigieg RT`),
          max.words = 40,
          scale=c(2.4,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuGn")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Bernie Sanders Fox News`),
          max.words = 40,
          scale=c(2.8,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "RdPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Donald Trump Fox News`),
          max.words = 40,
          scale=c(3.4,.3),
          rot.per=0.25, 
          colors=brewer.pal(5, "RdPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Elizabeth Warren Fox News`),
          max.words = 40,
          scale=c(2.8,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "RdPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Beto Fox News`),
          max.words = 40,
          scale=c(2.5,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "RdPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Biden Fox News`),
          max.words = 40,
          scale=c(2.5,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "RdPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Buttigieg Fox News`),
          max.words = 40,
          scale=c(3.0,.3),
          rot.per=0.25, 
          colors=brewer.pal(5, "RdPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Bernie Sanders The New York Times`),
          max.words = 40,
          scale=c(3.0,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "YlGnBu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Donald Trump The New York Times`),
          max.words = 40,
          scale=c(3.0,.4),
          rot.per=0.25, 
          colors=brewer.pal(5, "YlGnBu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Elizabeth Warren The New York Times`),
          max.words = 40,
          scale=c(2.8,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "YlGnBu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Beto The New York Times`),
          max.words = 40,
          scale=c(3.0,.2),
          rot.per=0.25, 
          colors=brewer.pal(5, "YlGnBu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Biden The New York Times`),
          max.words = 40,
          scale=c(3.0,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "YlGnBu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Buttigieg The New York Times`),
          max.words = 40,
          scale=c(2.7,.01),
          rot.per=0.25, 
          colors=brewer.pal(5, "YlGnBu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Bernie Sanders The Washington Post`),
          max.words = 40,
          scale=c(2.8,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Donald Trump The Washington Post`),
          max.words = 40,
          scale=c(2.6,.01),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Elizabeth Warren The Washington Post`),
          max.words = 40,
          scale=c(3.0,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Beto The Washington Post`),
          max.words = 40,
          scale=c(3.0,.1),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Biden The Washington Post`),
          max.words = 40,
          scale=c(2.5,.01),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuPu")
)

wordcloud(words = word.data$words,
          freq = as.numeric(word.data$`Buttigieg The Washington Post`),
          max.words = 40,
          scale=c(3.4,.3),
          rot.per=0.25, 
          colors=brewer.pal(5, "BuPu")
)
