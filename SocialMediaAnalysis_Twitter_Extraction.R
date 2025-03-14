#loading required packages
library(vosonSML)
library(magrittr)
library(igraph)
library(tidyr)
library(tidytext)
library(stopwords)

# Set up Twitter authentication variables

my_app_name <- "7230ICT_Omkar"
my_api_key <- "zNlART4QyzZUeKOx6qa8VepMW"
my_api_secret <- "5gh4YlcwfkLi0kQByBheqwzFSQdfhWhHSZjiz0VjhEu5JsMA8R"
my_access_token <- "1635880137337241600-Z7BSJlPrBMeOW2ktAjd55YiKx9bi4c"
my_access_token_secret <- "lbvvYTV6H6MbjPlTVC9W6DX9rspMiLXlP6BkNnyIeeyVa"

# Q 1.2: Data Retrival
# Authenticate to Twitter and collect data

twitter_data2 <- Authenticate("twitter",
                             appName = my_app_name,
                             apiKey = my_api_key,
                             apiSecret = my_api_secret,
                             accessToken = my_access_token,
                             accessTokenSecret = my_access_token_secret) %>%
  Collect(searchTerm = "drake",
          searchType = "recent",
          numTweets = 8000,
          lang = "en",
          includeRetweets = TRUE,
          writeToFile = TRUE,
          verbose = TRUE) # use 'verbose' to show download progress


# View collected Twitter data

View(twitter_data2$tweets)

# ----------
# Create actor network and graph from the data

twitter_actor_network <- twitter_data %>% Create("actor")
twitter_actor_graph <- twitter_actor_network %>% Graph()

# Write graph to file
# Make sure to set your working directory to where you want to save the file
# before you execute the next line

write.graph(twitter_actor_graph, file = "TwitterActor.graphml", format = "graphml")

# Run Page Rank algorithm to find important users

rank_twitter_actor <- sort(page_rank(twitter_actor_graph)$vector, decreasing=TRUE)
head(rank_twitter_actor, n=5)

# Q 1.3: Finding top 5 most influential users
# Overwrite the 'name' attribute in your graph with the 'screen name' attribute
# to replace twitter IDs with more meaningful names,
# then run the Page Rank algorithm again

V(twitter_actor_graph)$name <- V(twitter_actor_graph)$screen_name

rank_twitter_actor <- sort(page_rank(twitter_actor_graph)$vector, decreasing = TRUE)
head(rank_twitter_actor, n = 5)

# ----------
# Create semantic network and graph from the data

twitter_semantic_network <- twitter_data %>% Create("semantic")
twitter_semantic_graph <- twitter_semantic_network %>% Graph()

# Write graph to file

write.graph(twitter_semantic_graph, file = "TwitterSemantic.graphml", format = "graphml")

# Q 1.4: Top 10 important terms
# Run Page Rank algorithm to find important terms/hashtags

rank_twitter_semantic <- sort(page_rank(twitter_semantic_graph)$vector, decreasing = TRUE)
head(rank_twitter_semantic, n = 10)

# Create the network and graph again, but this time: 
# - with 25% of the most frequent terms (before was the default of 5%)
# - with 75% of the most frequent hashtags (before was the default of 50%)
# - removing the actual search term ("drake")

tw_sem_nw_more_terms <- twitter_data %>%
  Create("semantic",
         termFreq = 25,
         hashtagFreq = 75,
         removeTermsOrHashtags = c("drake"))

tw_sem_graph_more_terms <- tw_sem_nw_more_terms %>% Graph()


# Write graph to file

write.graph(tw_sem_graph_more_terms, 
            file = "TwitterSemanticMoreTerms.graphml",
            format = "graphml")

# Q.1.5
# Finding the unique users of keyword 'drake' dataset

library('dplyr')
library("data.table")

datainquestion <- read.csv("C:/Users/omkar/OneDrive/Documents/BD&SM_Milestone/DrakeRetrievedData.csv")

results <- datainquestion %>%
  group_by(v_screen_name) %>%
  summarize(count = n_distinct(v_screen_name))

#view results
results

length(unique(results$count))

#view graph of results
plot.default(results)