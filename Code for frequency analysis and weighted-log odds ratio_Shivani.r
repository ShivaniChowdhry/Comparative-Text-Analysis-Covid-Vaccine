install.packages("remotes")
remotes::install_github("JBGruber/LexisNexisTools")
library("LexisNexisTools")

LNToutput1 <- lnt_read("C:/Users/shiva/OneDrive/Documents/UTD/Fall 2021 sem/Content analysis/Group project/Data files/NYTJan-Mar(98).docx")
meta_df <- LNToutput1@meta
articles_df <- LNToutput1@articles
paragraphs_df <- LNToutput1@paragraphs
head(meta_df, n = 3)

Corpus <- lnt_convert(LNToutput1, to = "tm")


library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(tidytext)

##Cleaning
#Change to lower case
corpus <- tm_map(Corpus,content_transformer(tolower))
# Remove punctuations
corpus <- tm_map(Corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(Corpus,stripWhitespace)
# Remove english common stopwords
corpus <- tm_map(Corpus, removeWords, stopwords("english"))

# Check the final result
inspect(corpus[[1]])

#Making a DTM matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

#Finding the most frequent items
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

#Making a word cloud of most common words
install.packages("wordcloud")
library(wordcloud)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

# Save as data
df_clean <- data.frame(text = sapply(corpus,as.character),
                       stringsAsFactors = FALSE)

#change the data structure to "tibble"
twtext_df<-as_tibble(df_clean$text)
twtext_df

#unnest_tokens Function
twtext_token<-twtext_df %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# Delete single-letter words
twtext_token2<-twtext_token %>%
  filter(str_count(word)>1)

# Frequency analysis
word_space<-twtext_token %>%
  count(word, sort = TRUE)
word_space

# top 20 words
top20<-word_space %>%
  filter(word !="get") %>%
  filter(word !="just") %>%
  filter(word !="i") %>%
  filter(word !="s") %>%
  filter(word !="but") %>%
  filter(word !="one") %>%
  filter(word !="like") %>%
  filter(word !="can") %>%
  filter(word !="get") %>%
  filter(word !="also") %>%
  filter(word !="will") %>%
  filter(word !="and") %>%
  filter(word !="the") %>%
  filter(word !="now") %>%
  head(20)
top20


#make bar graph
library(ggplot2)

ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01) +
  labs(title = "Frequency of Words in Covid Vaccine Reporting",
       x="n", y=NULL) +
  theme(title=element_text(size=12))


#-------------------------------------------------------------------
#read 2nd data (April to June 2021)
LNToutput2 <- lnt_read("C:/Users/shiva/OneDrive/Documents/UTD/Fall 2021 sem/Content analysis/Group project/Data files/Data 2nd period")
meta_df2 <- LNToutput2@meta
articles_df2 <- LNToutput2@articles
paragraphs_df2 <- LNToutput2@paragraphs
head(meta_df, n = 3)

Corpus2 <- lnt_convert(LNToutput2, to = "tm")

##Cleaning
#Change to lower case
corpus2 <- tm_map(Corpus2,content_transformer(tolower))
# Remove punctuations
corpus2 <- tm_map(Corpus2, removePunctuation)
# Eliminate extra white spaces
corpus2 <- tm_map(Corpus2,stripWhitespace)
# Remove english common stopwords
corpus2 <- tm_map(Corpus2, removeWords, stopwords("english"))

# Check the final result
inspect(corpus2[[1]])

#Making a DTM matrix
dtm <- DocumentTermMatrix(corpus2)
dtm2 <- as.matrix(dtm)

#Finding the most frequent items
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

#Making a word cloud of most common words
install.packages("wordcloud")
library(wordcloud)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

# Save as data
df_clean2 <- data.frame(text = sapply(corpus2,as.character),
                       stringsAsFactors = FALSE)

#change the data structure to "tibble"
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
  filter(word !="the") %>%
  filter(word !="and") %>%
  filter(word !="will") %>%
  filter(word !="get") %>%
  filter(word !="but") %>%
  filter(word !="one") %>%
  filter(word !="like") %>%
  filter(word !="can") %>%
  filter(word !="also") %>%
  filter(word !="just") %>%
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

#-------------------------------------------------------------------
#read 3rd data (July to Sept 2021)
LNToutput3 <- lnt_read("C:/Users/shiva/OneDrive/Documents/UTD/Fall 2021 sem/Content analysis/Group project/Data files/Data 3rd period")
meta_df3 <- LNToutput3@meta
articles_df3 <- LNToutput3@articles
paragraphs_df3 <- LNToutput3@paragraphs
head(meta_df3, n = 3)

Corpus3 <- lnt_convert(LNToutput3, to = "tm")

##Cleaning
#Change to lower case
corpus3 <- tm_map(Corpus3,content_transformer(tolower))
# Remove punctuations
corpus3 <- tm_map(Corpus3, removePunctuation)
# Eliminate extra white spaces
corpus3 <- tm_map(Corpus3,stripWhitespace)
# Remove english common stopwords
corpus3 <- tm_map(Corpus3, removeWords, stopwords("english"))

# Check the final result
inspect(corpus3[[1]])

#Making a DTM matrix
dtm <- DocumentTermMatrix(corpus3)
dtm2 <- as.matrix(dtm)

#Finding the most frequent items
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)

#Making a word cloud of most common words

words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

# Save as data
df_clean3 <- data.frame(text = sapply(corpus3,as.character),
                        stringsAsFactors = FALSE)

#change the data structure to "tibble"
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
  filter(word !="the") %>%
  filter(word !="and") %>%
  filter(word !="will") %>%
  filter(word !="get") %>%
  filter(word !="but") %>%
  filter(word !="one") %>%
  filter(word !="like") %>%
  filter(word !="can") %>%
  filter(word !="also") %>%
  filter(word !="just") %>%
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

#-------------------------------------------------------------------
#compare all three periods

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
  filter(word !="the") %>%
  filter(word !="will") %>%
  filter(word !="get") %>%
  filter(word !="just") %>%
  filter(word !="but") %>%
  filter(word !="now") %>%
  filter(word !="like") %>%
  filter(word !="new") %>%
  filter(word !="know") %>%
  filter(word !="one") %>%
  filter(word !="also") %>%
  filter(word !="can") %>%
  filter(word !="and") %>%
  filter(word !="in") %>%
  filter(word !="re") %>%
  filter(word !="said") %>%
  filter(word !="many") %>%
  filter(word !="19") %>%
  filter(word !="going") %>%
  filter(word !="think") %>%
  filter(word !="so") %>%
  filter(word !="may") %>%
  filter(word !="say") %>%
  filter(word !="coronavirus") %>%
  filter(word !="pandemic") %>%
  filter(word !="year") %>%
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