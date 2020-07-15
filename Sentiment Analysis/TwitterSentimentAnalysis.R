# ---
# title: "Twitter Sentiment Analysis"
# author: "Dhrubasattwata Roy Choudhury"
# ---

## Import the required packages
library("rtweet")
library("dplyr")
library("tidyr")
library("tidyverse")
library("httpuv")
library("tidytext")
library("textdata")

## You need a twitter developer accound: https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

## store api keys
app_name <<- "RSentimentAnalysis_dsrc"
api_key <<- xxx
api_secret_key <<- xxx
access_token <<- xxx
access_token_secret <<- xxx

## authenticate via web browser
token <- create_token(
  app = app_name,
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

get_token()

## Accessing Tweets

tweets1 <- search_tweets("#HagiaSophia", n=10000,include_rts = FALSE, lang="en")
tweets2 <- search_tweets("#Hagia_Sophia", n=10000,include_rts = FALSE, lang="en")
tweets3 <- search_tweets("#AyaSophia", n=10000,include_rts = FALSE, lang="en")
tweets4 <- search_tweets("#Aya_Sophia", n=10000,include_rts = FALSE, lang="en")
tweets <- rbind(tweets1, tweets2, tweets3, tweets4)

## Processing the Data
tweets.all<- tweets %>% select(screen_name,text)

## Pre-processing to clean up tweets
tweets.all$stripped_text1 <- gsub("http\\S+","",tweets.all$text)
#vuse the unnest_tokens() function to convert to lowercase, remove punctuation and add ID
tweets.all_stem <- tweets.all %>% select(stripped_text1) %>% tidytext::unnest_tokens(word,stripped_text1)
cleaned_tweets.all <- tweets.all_stem %>% anti_join(stop_words) 

## Let us also remove words like hagiasophia, ayasophia, sophia, hagia_sophia because this will not add any value to the analysis
x <- c("hagiasophia", "ayasophia", "sophia", "hagia_sophia", "ayasofya", "hagiasofia")
remove_words <- stop_words[1:6,]
remove_words[,1] <- x

cleaned_tweets.all <- cleaned_tweets.all %>% anti_join(remove_words) 

#BASIC_Analysis

cleaned_tweets.all %>%
  count(word,sort=TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x=word,y=n)) + geom_col() + coord_flip() +theme_classic() +
  labs(x="Count",y="Unique Words", title= "Unique Words in tweets")

### Sentiment Analysis

## Getting Sentiments
tidytext::get_sentiments("bing") %>% filter(sentiment=="positive")
tidytext::get_sentiments("bing") %>% filter(sentiment=="negative")

# Analysis
bing_all <- cleaned_tweets.all %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort=TRUE) %>%
  ungroup()

bing_all %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales="free_y")+
  labs(title="Tweets for HagiaSophia",y="Contribution to Sentiment",x=NULL)+
  coord_flip() + theme_bw()


### Sentiment Score for each tweet

sentiment_bing <- function(twt){
  #Step 1: perform basic cleaning of tweet
  twt_tbl <- tibble(text=twt) %>%
    mutate(stripped_text = gsub("http\\S+","",text)) %>%
    unnest_tokens(word,stripped_text) %>%
    anti_join(stop_words) %>% #remove stop_words
    inner_join(get_sentiments("bing")) %>% #merge with bing sentiments
    count(word,sentiment,sort=TRUE)%>%
    ungroup() %>%
    mutate( # Calculate Score : 1 for positive word, -1 for negatives
      score= case_when(
        sentiment == "negative" ~ n*(-1),
        sentiment == "postive" ~ n*1
      )
    ) #Calculate Total Score
  sent.score= case_when(
    nrow(twt_tbl) == 0~0,
    nrow(twt_tbl) >0~sum(twt_tbl$score)
  )
  zero.type = case_when(
    nrow(twt_tbl) == 0~"Type 1", #no words
    nrow(twt_tbl) >0~"Type 2" #sum of words is 0
  )
  list(score=sent.score,type=zero.type,twt_tbl=twt_tbl)
}

all_sentiment <- lapply(tweets$text,function(x){sentiment_bing(x)})
all_sentiment <- sentiment_bing(tweets$text)
# all_sentiment

## Plotting Histogram
sentiment <- bind_rows(
  tibble(
    about = "#Hagia Sophia",
    score = unlist(map(all_sentiment,'score')),
    type = unlist(map(all_sentiment,'type'))
  )
)

ggplot(sentiment, aes(x=score)) + geom_histogram(bins=15, alpha = 1, color = "midnightblue", fill = "darkolivegreen") + ggtitle("Score of the tweets about Hagia Sophia") +  theme_bw()
