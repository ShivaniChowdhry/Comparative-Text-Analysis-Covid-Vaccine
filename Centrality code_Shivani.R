rm(list = ls())
install.packages("remotes")
remotes::install_github("JBGruber/LexisNexisTools")
library("LexisNexisTools")


LNToutput4 <- lnt_read("C:/Users/shiva/OneDrive/Documents/UTD/Fall 2021 sem/Content analysis/Group project/Data files/Data combined")
meta_df4 <- LNToutput4@meta
articles_df4 <- LNToutput4@articles
paragraphs_df4 <- LNToutput4@paragraphs
head(meta_df4, n = 3)

CorpusT <- lnt_convert(LNToutput4, to = "tm")

library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(tidytext)

# Read the data
data_T <- CorpusT$content
data_T <- unique(data_T)
# Work with corpus
News_corpusT <- VCorpus(VectorSource(data_T))
##Cleaning
# Case folding
News_corpusT <- tm_map(News_corpusT,content_transformer(tolower))
# Remove english common stopwords
News_corpusT <- tm_map(News_corpusT, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
News_corpusT <- tm_map(News_corpusT, removeWords, c("will", "can", "vaccinated", "vaccines", "covid", "coronavirus"))
# Remove punctuations
News_corpusT <- tm_map(News_corpusT, removePunctuation)
# Eliminate extra white spaces
News_corpusT <- tm_map(News_corpusT, stripWhitespace)
# Text stemming - which reduces words to their root form
News_corpusT <- tm_map(News_corpusT, stemDocument)

# Check the final result
inspect(News_corpusT[[1]])

# Save as data
df_cleanT <- data.frame(text = sapply(News_corpusT,as.character),
                        stringsAsFactors = FALSE)

# centality figure
networkd<-df_cleanT %>%
  select(text) %>%
  mutate(text = str_replace_all(text, "[^a-z']"," "),
         text = str_squish(text),
         id = row_number())

net_token <- networkd %>%
  unnest_tokens(input = text,
                output = word,
                token = "words",
                drop = FALSE)
net_token %>%
  select(word, text)

net_token<-net_token %>%
  filter(str_count(word)>1) %>%
  filter(word !="get") %>%
  filter(word !="just") %>%
  filter(word !="now") %>%
  filter(word !="like") %>%
  filter(word !="new") %>%
  filter(word !="know") %>%
  filter(word !="one") 

library(tidyr)
library(widyr)
pair <- net_token %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = TRUE)
head(pair)

library(tidygraph)

graph_news <- pair %>%
  filter(n>=200) %>%
  as_tbl_graph()
graph_news

library(ggraph)
ggraph(graph_news) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

#------------------------
library(widyr)
pair <- net_token %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = TRUE)
head(pair)

pair %>% filter(item1 == "vaccin")

library(tidygraph)
graph_news <- pair %>%
  filter(n>=250) %>%
  as_tbl_graph(directed = FALSE) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))
graph_news

library(ggraph)
set.seed(1234)
ggraph(graph_news, layout = "fr") +
  geom_edge_link(color = "gray50",
                 alpha = 0.5) +
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = FALSE) +
  theme(legend.position="none")+
  scale_size(range = c(5, 15)) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 5) +
  labs(title = "Co-occurrence Network",
       x=NULL, y=NULL) +
  theme(title=element_text(size=12)) +
  theme_graph()

