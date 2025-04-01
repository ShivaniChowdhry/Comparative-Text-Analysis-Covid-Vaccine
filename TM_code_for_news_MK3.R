#Dohyo
#Oct.28.2021
# Text analysis for Newspapers

rm(list = ls())

#read 1st data (Jan~March. 2021)
twtext<-read.csv("your 1st file name", 
                 header = TRUE, 
                 sep = ",")

#loading library
# Import libraries
library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(tidytext)

# URL removal
removeURL <- function(x){
  gsub("http[^[:space:]]*", "", x)
}
# Mention removal
removeMention <- function(x){
  gsub("@\\w+", "", x)
}
# Hashtag removal
removeHashtag <- function(x){
  gsub("#\\S+", "", x)
}
# Carriage removal
removeCarriage <- function(x){
  gsub("[\r\n]", "", x)
}
# Emoticon removal
removeEmoticon <- function(x){
  gsub("[^\x01-\x7F]", "", x)
}
# Retweet removal
removeRT <- function(x){
  gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)
}
# Invoice removal
removeInvoice <- function(x){
  gsub("inv/[0-9]+/+[xvi]+/[xvi]+/[0-9]+", "", x, ignore.case = T)
}
# HTML removal
unescapeHTML <- function(str) {
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}


# Read the data
data_tweet <- twtext$text
data_tweet <- unique(data_tweet)
# Work with corpus
tweet_corpus <- VCorpus(VectorSource(data_tweet))
# Case folding
tweet_corpus <- tm_map(tweet_corpus,content_transformer(tolower))

#######Execute the necessary functions according to your data#########
# Retweet removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(removeRT))
# Hashtag removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(removeHashtag))
# URL removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(removeURL))
# HTML removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(removeMention))
# Carriage removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(removeCarriage))
# Emoticon removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(removeEmoticon))
# Invoice removal
tweet_corpus <- tm_map(tweet_corpus,content_transformer(removeInvoice))
# Remove additional symbols to white space
# punctuation
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:punct:]]")
# numbers
tweet_corpus = tm_map(tweet_corpus,toSpace,"[[:digit:]]")
# Eliminate extra white spaces
tweet_corpus = tm_map(tweet_corpus,stripWhitespace)


# Remove english common stopwords
tweet_corpus <- tm_map(tweet_corpus, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#########Include the necessary words according to your data#####
tweet_corpus <- tm_map(tweet_corpus, removeWords, c("rt", "will", "can","la","el","en","ud"))######## 
# Remove punctuations
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
# Eliminate extra white spaces
tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)
# Text stemming - which reduces words to their root form
tweet_corpus <- tm_map(tweet_corpus, stemDocument)

# Check the final result
inspect(tweet_corpus[[1]])

# Save as data
df_clean <- data.frame(text = sapply(tweet_corpus,as.character),
                       stringsAsFactors = FALSE)



# change the data structure to "tibble"
twtext_df<-as_tibble(df_clean$text)
twtext_df


#unnest_tokens Function
twtext_token<-twtext_df %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")


# Delete single-letter words
twtext_token2<-twtext_token2 %>%
  filter(str_count(word)>1)

# Frequency analysis
word_space<-twtext_token %>%
  count(word, sort = TRUE)
word_space

# top 20 words
top20<-word_space %>%
  mutate(word=ifelse(word == "se", "side effect", word)) %>%
  filter(word !="vaccin") %>%
  head(20)
top20

#make bar graph
library(ggplot2)

ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01) +
  labs(title = "Frequency about Vaccine Tweets",
       x="n", y=NULL) +
  theme(title=element_text(size=12))


#-------------------------------------------------------------------
#read 2nd data (April to June 2021)
twtext2<-read.csv("your 2nd file name", 
                  header = TRUE, 
                  sep = ",")

# Read the data
data_tweet2 <- twtext2$text
data_tweet2 <- unique(data_tweet2)
# Work with corpus
tweet_corpus2 <- VCorpus(VectorSource(data_tweet2))
# Case folding
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(tolower))

#######Execute the necessary functions according to your data#########
# Retweet removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(removeRT))
# Hashtag removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(removeHashtag))
# URL removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(removeURL))
# HTML removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(removeMention))
# Carriage removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(removeCarriage))
# Emoticon removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(removeEmoticon))
# Invoice removal
tweet_corpus2 <- tm_map(tweet_corpus2,content_transformer(removeInvoice))
# Remove additional symbols to white space
# punctuation
tweet_corpus2 = tm_map(tweet_corpus2,toSpace,"[[:punct:]]")
# numbers
tweet_corpus2 = tm_map(tweet_corpus2,toSpace,"[[:digit:]]")
# Eliminate extra white spaces
tweet_corpus2 = tm_map(tweet_corpus2,stripWhitespace)


# Remove english common stopwords
tweet_corpus2 <- tm_map(tweet_corpus2, removeWords, stopwords("english"))
# Remove your own stop word
#######Include the necessary words according to your data#########
# specify your custom stopwords as a character vector
tweet_corpus2 <- tm_map(tweet_corpus2, removeWords, c("rt", "will", "can","la","el","en","ud"))######## 
# Remove punctuations
tweet_corpus2 <- tm_map(tweet_corpus2, removePunctuation)
# Eliminate extra white spaces
tweet_corpus2 <- tm_map(tweet_corpus2, stripWhitespace)
# Text stemming - which reduces words to their root form
tweet_corpus2 <- tm_map(tweet_corpus2, stemDocument)

# Check the final result
inspect(tweet_corpus2[[1]])

# Save as data
df_clean2 <- data.frame(text = sapply(tweet_corpus2,as.character),
                        stringsAsFactors = FALSE)


# change the data structure to "tibble"
twtext_df2<-as_tibble(df_clean2$text)
twtext_df2


#unnest_tokens Function
twtext_token2<-twtext_df2 %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# Delete single-letter words
twtext_token2<-twtext_token2 %>%
  filter(str_count(word)>1)

# Frequency analysis
word_space2<-twtext_token2 %>%
  count(word, sort = TRUE)
word_space2

# top 20 words
top20se<-word_space2 %>%
  mutate(word=ifelse(word == "se", "side effect", word)) %>%
  filter(word !="vaccin") %>%
  head(20)
top20se

#make bar graph
library(ggplot2)

ggplot(top20se, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01) +
  labs(title = "Frequency about Vaccine Tweets",
       x="n", y=NULL) +
  theme(title=element_text(size=12))

#---------------------------------------------------------------
#read delta data (July to Step 2021)
twtext3<-read.csv("your 3rd file name", 
                  header = TRUE, 
                  sep = ",")

# Read the data
data_tweet3 <- twtext3$text
data_tweet3 <- unique(data_tweet3)
# Work with corpus
tweet_corpus3 <- VCorpus(VectorSource(data_tweet3))
# Case folding
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(tolower))

#######Execute the necessary functions according to your data#########
# Retweet removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(removeRT))
# Hashtag removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(removeHashtag))
# URL removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(removeURL))
# HTML removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(unescapeHTML))
# Mention removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(removeMention))
# Carriage removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(removeCarriage))
# Emoticon removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(removeEmoticon))
# Invoice removal
tweet_corpus3 <- tm_map(tweet_corpus3,content_transformer(removeInvoice))
# Remove additional symbols to white space
# punctuation
tweet_corpus3 = tm_map(tweet_corpus3,toSpace,"[[:punct:]]")
# numbers
tweet_corpus3 = tm_map(tweet_corpus3,toSpace,"[[:digit:]]")
# Eliminate extra white spaces
tweet_corpus3 = tm_map(tweet_corpus3,stripWhitespace)


# Remove english common stopwords
tweet_corpus3 <- tm_map(tweet_corpus3, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#######Include the necessary words according to your data#########
tweet_corpus3 <- tm_map(tweet_corpus3, removeWords, c("rt", "will", "can","la","el","en","ud"))######## 
# Remove punctuations
tweet_corpus3 <- tm_map(tweet_corpus3, removePunctuation)
# Eliminate extra white spaces
tweet_corpus3 <- tm_map(tweet_corpus3, stripWhitespace)
# Text stemming - which reduces words to their root form
tweet_corpus3 <- tm_map(tweet_corpus3, stemDocument)

# Check the final result
inspect(tweet_corpus3[[1]])

# Save as data
df_clean3 <- data.frame(text = sapply(tweet_corpus3,as.character),
                        stringsAsFactors = FALSE)


# change the data structure to "tibble"
twtext_df3<-as_tibble(df_clean3$text)
twtext_df3


#unnest_tokens Function
twtext_token3<-twtext_df3 %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# Delete single-letter words
twtext_token3<-twtext_token3 %>%
  filter(str_count(word)>1)

# Frequency analysis
word_space3<-twtext_token3 %>%
  count(word, sort = TRUE)
word_space3

# top 20 words
top20de<-word_space3 %>%
  mutate(word=ifelse(word == "se", "side effect", word)) %>%
  filter(word !="vaccin") %>%
  head(20)
top20de

#make bar graph
library(ggplot2)

ggplot(top20de, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01) +
  labs(title = "Frequency about Vaccine Tweets",
       x="n", y=NULL) +
  theme(title=element_text(size=12))

#---------------------------------------------------------------
#compare with three period

keyword1<-df_clean %>%
  as_tibble() %>%
  mutate(period="before")

keyword2<-df_clean2 %>%
  as_tibble() %>%
  mutate(period="after")

keyword3<-df_clean3 %>%
  as_tibble() %>%
  mutate(period="delta")

#bind rows
bind_keywords<- bind_rows(keyword1, keyword2, keyword3) %>%
  select(text, period)
head(bind_keywords)

#unnest_tokens Function
bind_token<-bind_keywords %>%
  unnest_tokens(input = text,
                output = word,
                token = "words")
bind_token

# Frequency each keywords
frequency<-bind_token %>%
  count(period, word) %>%
  filter(str_count(word)>1)
head(frequency)

keytop20 <- frequency %>%
  mutate(word=ifelse(word == "se", "side effect", word)) %>%
  filter(word !="vaccin") %>%
  filter(word !="cov19") %>%
  filter(word !="get") %>%
  filter(word !="just") %>%
  filter(word !="isnt") %>%
  filter(word !="now") %>%
  filter(word !="like") %>%
  filter(word !="new") %>%
  filter(word !="know") %>%
  filter(word !="one") %>%
  group_by(period) %>%
  slice_max(n, n=20, with_ties = F)
keytop20

# make compare Freq plot
ggplot(keytop20, aes(x=reorder_within(word, n, period),
                     y=n,
                     fill= period)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~period, scales = "free_y") +
  scale_x_reordered() +
  labs(x=NULL) +
  theme(text = element_text(size = 10))

#--------------------------------------------------------------
# TF-IDF
library(tidytext)
keytop20<- keytop20 %>%
  bind_tf_idf(term = word,
              document = period,
              n = n) %>%
  arrange(-tf_idf)
keytop20

# tf_idf for "before"
keytop20 %>% filter(period == "before")
# tf_idf for "after"
keytop20 %>% filter(period == "after")
# tf_idf for "delta"
keytop20 %>% filter(period == "delta")

#make order the graph
TFbar<- keytop20 %>%
  group_by(period) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

TFbar$period <- factor(TFbar$period,
                       levels = c("before", "after", "delta"))

# make bar plot
ggplot(TFbar, aes(x=reorder_within(word, tf_idf, period),
                  y=tf_idf,
                  fill = period)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~period, scales = "free", ncol = 3) +
  scale_x_reordered() +
  labs(x=NULL)

#----------weighted log odds
library(tidylo)
keytop20

bigram_log_odds <- keytop20 %>%
  bind_log_odds(period, word, n) 

bigram_log_odds %>%
  arrange(-log_odds_weighted)

# make plot by using log odds
bigram_log_odds %>%
  group_by(period) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, log_odds_weighted)) %>%
  ggplot(aes(word, log_odds_weighted, fill = period)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~period, scales = "free") +
  coord_flip() +
  labs(title = "Weighted log-odds comparison by period", x = NULL)

#---------------------------------------------------------------