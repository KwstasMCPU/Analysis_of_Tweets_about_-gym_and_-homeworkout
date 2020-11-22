library(rtweet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(jsonlite)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tidyr)
library(maps)
library(scales)


#setting the working directory
setwd("C:/Users/kwsta/master_projects/Math513/CRW_presentation")

###### NO NEED TO RUN IF THE JSON FILES ARE HERE #######
# making request to the tweeter API
my_token <- create_token(
  app = "KTweeter_analysis_app",
  consumer_key = "gcwB0SC0FfKAz6YfyTIw3wUGd",
  consumer_secret = "0HeRgRxBIfVJVxSmjjaJdFZtIs9nMM1lVpVHv2c3mtnOtp0qoG",
  access_token = "1315266158052990977-uWAIGpx2rlbGPyuICYwmGrj1UtO7Vc",
  access_secret = "7JVOj6aVqvWy2lC6ehN7VHHrmB5RwFsr0Xopmtd6ABRQO")
###########

########## 
# we filter our search to receive tweets only with English language,  
# also we excluded the re-tweets in order to minimize bias,
# since can affect the word count as well as sentimental analysis and the analysis in general
hash_homeworkout_tweets <- search_tweets(q = "#homeworkout",
                        n = 1000, lang = "en", include_rts = FALSE)

hash_gym_tweets <- search_tweets(q = "#gym",
                                 n = 1000, lang = "en", include_rts = FALSE)

homeworkout_tweets <- search_tweets(q = "home workout OR homeworkout",
                                    n = 1000, lang = "en", include_rts = FALSE)

gym_tweets <- search_tweets(q = "gym",
                            n = 1000, lang = "en", include_rts = FALSE)

########## THIS WILL REWRITE THE JSON FILES ########################################
## As before, these tweets can be saved for future use, for example, as a json file
hash_homeworkout_tweets %>% toJSON() %>% write_lines("hash_homeworkout_tweets.json")
hash_gym_tweets %>% toJSON() %>% write_lines("hash_gym_tweets.json")
homeworkout_tweets %>% toJSON() %>% write_lines("homeworkout_tweets.json")
gym_tweets %>% toJSON() %>% write_lines("gym_tweets.json")

######### RUN ONLY IF U WANT TO LOAD FROM THE JSON FILE!!!!!############################
# makes a dataframe out of a json file
# Read in the data
library(jsonlite)
#df <- fromJSON(temp) %>% as.data.frame
hash_homeworkout_tweets <- stream_in(file("hash_homeworkout_tweets.json"))
hash_gym_tweets <- stream_in(file("hash_gym_tweets.json"))
homeworkout_tweets <- stream_in(file("homeworkout_tweets.json"))
gym_tweets <- stream_in(file("gym_tweets.json"))

# inspect some tweets
####### for hash_homeworkout_tweets##########
View(hash_homeworkout_tweets)
names(hash_homeworkout_tweets)
head(hash_homeworkout_tweets$text)
head(hash_homeworkout_tweets$screen_name)

# VISUALS countplots
# top users
hash_homeworkout_tweets %>% 
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name_r = reorder(screen_name, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Twitter userss",
       y = "Number of tweets per user.",
       title = "Who tweeted the most about #homeworkout") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

## top locations
# dealing with blank locations
hash_homeworkout_tweets$location[hash_homeworkout_tweets$location==""] <- NA
# joining similar locations
hash_homeworkout_tweets_recoded <- hash_homeworkout_tweets %>% mutate(location_rec = 
                                              recode(location, 'United States' = 'USA', 
                                                     'US' = 'USA', 'Chicago' = 'Chicago, IL', 
                                                     "London, England" = "London", 
                                                     "London, UK" = "London", 
                                                     "South East, England" = 'United Kingdom',
                                                     "UK" = "United Kingdom", "u.k." = "United Kingdom",
                                                     "United Kingdom, EU" = "United Kingdom",
                                                     "England, United Kingdom" = "United Kingdom",
                                                     "united kingdom" = "United Kingdom",
                                                     "EU" = "European Union"
                                              ))

hash_homeworkout_tweets_recoded %>% 
  count(location_rec, sort = TRUE) %>%
  mutate(screen_name_r = reorder(location_rec, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Location",
       y = "Number of tweets",
       title = "Where twitter users using '#homeworkout' are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

####
################## for hash_gym_tweets ######################
View(hash_gym_tweets)
names(hash_gym_tweets)
head(hash_gym_tweets$text)
head(hash_gym_tweets$screen_name)
## top users ######
hash_gym_tweets %>% 
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name_r = reorder(screen_name, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Twitter users",
       y = "Number of tweets per user",
       title = "Who tweeted the most about #gym") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

## top locations ######
# dealing with blank locations
hash_gym_tweets$location[hash_gym_tweets$location==""] <- NA
# joining similar locations
hash_gym_tweets_recoded <- hash_gym_tweets %>% mutate(location_rec = 
                                                            recode(location, 'United States' = 'USA', 
                                                                              'US' = 'USA', 'Chicago' = 'Chicago, IL', 
                                                                              "London, England" = "London", 
                                                                              "London, UK" = "London", 
                                                                              "South East, England" = 'United Kingdom',
                                                                              "UK" = "United Kingdom", "u.k." = "United Kingdom",
                                                                              "United Kingdom, EU" = "United Kingdom",
                                                                              "England, United Kingdom" = "United Kingdom",
                                                                              "united kingdom" = "United Kingdom",
                                                                              "EU" = "European Union"
                                                                        ))

hash_gym_tweets_recoded %>% 
  count(location_rec, sort = TRUE) %>%
  mutate(screen_name_r = reorder(location_rec, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Location",
       y = "Number of tweets",
       title = "Where twitter users using '#gym' are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))


###### for homeworkout_tweets #########
View(homeworkout_tweets)
names(homeworkout_tweets)
head(homeworkout_tweets$text)
head(homeworkout_tweets$screen_name)

##### top users #######
homeworkout_tweets %>% 
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name_r = reorder(screen_name, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Twitter users",
       y = "Number of tweets per user",
       title = "Who tweeted the most about 'home workout'") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

## top locations
# dealing with blank locations
View(homeworkout_tweets)
homeworkout_tweets$location[homeworkout_tweets$location==""] <- NA
# joining similar locations
homeworkout_tweets_recoded <- homeworkout_tweets %>% mutate(location_rec = 
                                                        recode(location, 'United States' = 'USA', 
                                                               'US' = 'USA', 'Chicago' = 'Chicago, IL', 
                                                               "London, England" = "London", 
                                                               "London, UK" = "London", 
                                                               "South East, England" = 'United Kingdom',
                                                               "UK" = "United Kingdom", "u.k." = "United Kingdom",
                                                               "United Kingdom, EU" = "United Kingdom",
                                                               "England, United Kingdom" = "United Kingdom",
                                                               "united kingdom" = "United Kingdom",
                                                               "EU" = "European Union"
                                                        ))

homeworkout_tweets_recoded %>% 
  count(location_rec, sort = TRUE) %>%
  mutate(screen_name_r = reorder(location_rec, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Location",
       y = "Number of tweets",
       title = "Where twitter users using 'home workout' are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

#### for gym #######
View(gym_tweets)
names(gym_tweets)
head(gym_tweets$text)
head(gym_tweets$screen_name)
head(gym_tweets$text)
### top users ####
gym_tweets %>% 
  count(screen_name, sort = TRUE) %>%
  mutate(screen_name_r = reorder(screen_name, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Twitter users",
       y = "Number of tweets per user",
       title = "Who tweeted the most about 'gym'") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

## top locations
# dealing with blank locations
gym_tweets$location[gym_tweets$location==""] <- NA
# joining similar locations
gym_tweets_recoded <- gym_tweets %>% mutate(location_rec = 
                              recode(location, 'United States' = 'USA', 'US' = 'USA', 'Chicago' = 'Chicago, IL', "London, England" = "London", "London, UK" = "London", 
                                     "UK" = "United Kingdom", "u.k." = "United Kingdom",
                                     "United Kingdom, EU" = "United Kingdom",
                                     "England, United Kingdom" = "United Kingdom",
                                     "united kingdom" = "United Kingdom",
                                     "EU" = "European Union"
                              ))

gym_tweets_recoded %>% 
  count(location_rec, sort = TRUE) %>%
  mutate(screen_name_r = reorder(location_rec, n)) %>%
  na.omit() %>%
  head(10) %>%
  ggplot(aes(x = screen_name_r, y = n))+
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Location",
       y = "Number of tweets",
       title = "Where twitter users using 'gym' are from") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

####


#####################
#####
# get tweets from companies employed in fitness sector


tmls <- get_timeline(
  c("AnytimeFitness", "PureGym"),
  n = 1000
)

# See the most recent tweets posted by the gym francises figures
tmls %>% 
  arrange(desc(created_at)) %>% 
  group_by(screen_name) %>%
  select(created_at, screen_name, text) 

########

# stream_tweets(
#   q = "#homeworkout",
#   timeout = 60, # stream for 60 seconds
#   file_name = "homeworkout_stream.json", # file where the data are saved
#   lang = "en", # tweets written in english
#   parse = FALSE
# )
# #
# ## read in the data as a data frame
# homeworkout_stream_tweets <- parse_stream("homeworkout_stream.json")
# # 
# ## look at the data
# homeworkout_stream_tweets
# # 
# homeworkout_stream_tweets$text


### 
### MAPS
#
#
# create basemap of the globe
# the theme_map() function cleans up the look of your map.
world_basemap <- ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map()
world_basemap

## create variables indicating latitude and longitude using all available 
## tweet and profile geo-location data
hash_homeworkout_tweets_map <- lat_lng(hash_homeworkout_tweets)
hash_gym_tweets_map <- lat_lng(hash_gym_tweets)
#
# create new data frame with just the tweet texts, usernames and location data
#homeworkout
hash_homeworkout_tweets_map_s <- data.frame(date_time = hash_homeworkout_tweets_map$created_at,
                       username = hash_homeworkout_tweets_map$screen_name,
                       tweet_text = hash_homeworkout_tweets_map$text,
                       long = hash_homeworkout_tweets_map$lng,
                       lat = hash_homeworkout_tweets_map$lat)
#gym
hash_gym_tweets_map_s <- data.frame(date_time = hash_gym_tweets_map$created_at,
                                            username = hash_gym_tweets_map$screen_name,
                                            tweet_text = hash_gym_tweets_map$text,
                                            long = hash_gym_tweets_map$lng,
                                            lat = hash_gym_tweets_map$lat)




#
#
#
# remove na values
hash_homeworkout_locations  <- hash_homeworkout_tweets_map_s %>%
  na.omit()
head(hash_homeworkout_locations)
#
#
hash_gym_locations  <- hash_gym_tweets_map_s %>%
  na.omit()
head(hash_gym_locations)
#
#
# 
# round latitude and longitude and group close tweets
hash_homeworkout_locations_grp <- hash_homeworkout_locations %>%
  mutate(long_round = round(long, 2),
         lat_round = round(lat, 2)) %>%
  group_by(long_round, lat_round) %>%
  summarise(total_count = n()) %>%
  ungroup() 
hash_homeworkout_locations_grp
#
#
hash_gym_locations_grp <- hash_gym_locations %>%
  mutate(long_round = round(long, 2),
         lat_round = round(lat, 2)) %>%
  group_by(long_round, lat_round) %>%
  summarise(total_count = n()) %>%
  ungroup() 
hash_gym_locations_grp
#
#
#
#
# Plot tweet data on #homeworkout and #gym grouping close tweets and 
# using larger points to show higer frequency
# 
world_basemap + 
  geom_point(data = hash_homeworkout_locations_grp,
             aes(long_round, lat_round, size = total_count),
             color = "purple", alpha = .5) + 
  geom_point(data = hash_gym_locations_grp,
             aes(long_round, lat_round, size = total_count),
             color = "red", alpha = .5) +
  coord_fixed() +
  labs(title = "Twitter Activity and locations of #homeworkout vs #gym",
       size = "Number of Tweets")
#
#
#
#
#
##############################################################
##############################################################
#
### ***
### *** Sentiment Analysis and Text Mining of Twitter Data 
### ***
#
### Data Clean-Up #######
###
#
# REMOVE URLS
hash_homeworkout_tweets$stripped_text <- gsub("http.*","",  hash_homeworkout_tweets$text)
hash_homeworkout_tweets$stripped_text <- gsub("https.*","", hash_homeworkout_tweets$stripped_text)
hash_homeworkout_tweets$stripped_text <- gsub("amp","", hash_homeworkout_tweets$stripped_text)
#
#
# REMOVE URLS
hash_gym_tweets$stripped_text <- gsub("http.*","",  hash_gym_tweets$text)
hash_gym_tweets$stripped_text <- gsub("https.*","", hash_gym_tweets$stripped_text)
hash_gym_tweets$stripped_text <- gsub("amp","", hash_gym_tweets$stripped_text)
#
# Then, you can clean up your text. If you are trying to create a list of unique words 
# in your tweets, words with capitalization will be different from words that are all 
# lowercase. Also you don't need punctuation to be returned as a unique word
#
# You can use the unnest_tokens() function in the tidytext package to  
# clean up your text. When you use this function the following things will be cleaned up 
# in the text:
#
# 1. Convert text to lowercase: each word found in the text will be converted to lowercase 
# so ensure that you don't get duplicate words due to variation in capitalization.
# 
# 2. Punctuation is removed: all instances of periods, commas etc will be removed from your 
# list of words, and
#
# 3. Unique id associated with the tweet: will be added for each occurrence of the word
#
# The unnest_tokens() function takes two arguments:
#  
# 1) The name of the column where the unique word will be stored and
# 
# 2) The column name from the data.frame that you are using that you want to pull unique 
# words from.
#
# In your case, you want to use the stripped_text column which is where you have your 
# cleaned up tweet text stored.
#
# Let's remove punctuation, convert to lowercase, add id for each tweet:
hash_homeworkout_tweets_clean <- hash_homeworkout_tweets %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% # create new variable denoting the tweet number
  unnest_tokens(word, stripped_text)
head(hash_homeworkout_tweets_clean)
#
hash_gym_tweets_clean <- hash_gym_tweets %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% # create new variable denoting the tweet number
  unnest_tokens(word, stripped_text)
head(hash_gym_tweets_clean)
#
#common words
# converting the striped text columns to a dataframe so we can use the rbind()
# then the colnames() is used to set the same column name in order to bind the together
df_stripped_text_home <- as.data.frame(hash_homeworkout_tweets$stripped_text)
colnames(df_stripped_text_home) <- 'text'
#
df_stripped_text_gym <- as.data.frame(hash_gym_tweets$stripped_text)
colnames(df_stripped_text_gym) <- 'text'
# binding them together
common_words_together <- rbind(df_stripped_text_home, df_stripped_text_gym)
View(common_words_together)
#
#
common_words_together_clean <- common_words_together %>%
  select(text) %>% 
  mutate(tweetnumber = row_number()) %>% # create new variable denoting the tweet number
  unnest_tokens(word, text)
head(common_words_together_clean)
#
# clean stop words
data("stop_words")
#
my_stop_words <- data.frame(word = c("30","day")) # the word 30 appears a lot
#
#
hash_homeworkout_tweets_clean  <- hash_homeworkout_tweets_clean %>%
  anti_join(stop_words)
#
hash_homeworkout_tweets_clean  <- hash_homeworkout_tweets_clean %>%
  anti_join(my_stop_words)
#
#
hash_gym_tweets_clean  <- hash_gym_tweets_clean %>%
  anti_join(stop_words)
#
hash_gym_tweets_clean  <- hash_gym_tweets_clean %>%
  anti_join(my_stop_words)
#
#
common_words_together_clean <- common_words_together_clean %>%
  anti_join(stop_words)
#
common_words_together_clean <- common_words_together_clean %>%
  anti_join(my_stop_words)
#
# plotting the top 10 words
hash_homeworkout_tweets_clean %>%
  count(word, sort = TRUE) %>% # count of number of occurrences of each word and sort according to count
  head(10) %>% # extract top 10 words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Count of unique words found in tweets with #homeworkout") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#
#
hash_gym_tweets_clean %>%
  count(word, sort = TRUE) %>% # count of number of occurrences of each word and sort according to count
  head(10) %>% # extract top 10 words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Count of unique words found in tweets with #gym") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#
#
common_words_together_clean %>%
  count(word, sort = TRUE) %>% # count of number of occurrences of each word and sort according to count
  head(10) %>% # extract top 10 words
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "pink", color = "red") +
  coord_flip() +
  labs(x = "Unique Words",
       y = "Frequency",
       title = "Count of unique common words") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#
##
########################################
#
#
###
### Wordclouds
###
hash_homeworkout_tweets_clean_2 <- hash_homeworkout_tweets_clean %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))
head(hash_homeworkout_tweets_clean_2)
#
hash_gym_tweets_clean_2 <- hash_gym_tweets_clean %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))
head(hash_gym_tweets_clean_2)
#
#
#check color paletes
display.brewer.all()

with(hash_homeworkout_tweets_clean_2, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 50,
               random.order = FALSE, 
               colors = brewer.pal(8, "Set3"), 
               scale = c(4.5, 0.1)))+
  title(main = "Wordcloud for Tweets containing #homeworkout", 
        cex.main = 2)

wordcloud2(hash_homeworkout_tweets_clean_2)
wordcloud2(hash_gym_tweets_clean_2)
#####################################################
#
###
### Sentiment analysis
###
#
#
#
bing_home_word_counts <- hash_homeworkout_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#
bing_gym_word_counts <- hash_gym_tweets_clean %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 
#
# plot top words
#
bing_home_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Most common Positive and Negative words in tweets on #homeworkout",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 15))
#
#
bing_gym_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Most common Positive and Negative words in tweets on #gym",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 15))
#
bing_home_word_counts_m <-bing_home_word_counts %>%
                            mutate(hash = 'homeworkout')
bing_home_word_counts_m
#
bing_gym_word_counts_m <- bing_gym_word_counts %>%
                            mutate(hash = 'gym')
#
#added together
#
bing_together <- rbind(bing_home_word_counts_m, bing_gym_word_counts_m)
bing_together
#
bing_together %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(hash~sentiment, scales = "free_y") +
  labs(title = "Most common Positive and Negative words in tweets on #homeworkout vs #gym",
       y = "Sentiment",
       x = NULL) +
  theme(axis.text = element_text(size = 14, color = "black"), 
        axis.title = element_text(size = 14, color = "black"),
        title = element_text(size = 15))
