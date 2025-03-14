#Spotify Artist Analysis
# Load packages required for this session into library
library(Rspotify)
library(spotifyr)
library(magrittr)
library(igraph)
library(dplyr)
library(knitr)
library(ggplot2)
library(ggridges)
library(httpuv)

# Configure application to store Spotify authentication data in cache

options(httr_oauth_cache = TRUE)


# Set up authentication variables

app_id <- "cf58bbe7efe843e29b35e9c383778e5d"
app_secret <- "bde0a847bd2841e49894b616ff649f51"
token <- "1"


# Authentication for Rspotify package:

keys <- spotifyOAuth(token, app_id, app_secret) 

# Get Spotify data on 'Drake'

find_my_artist <- searchArtist("Drake", token = keys)
View(find_my_artist)

#2.1
# Retrieve information about artist

my_artist <- getArtist("3TVXtAsR1Inumwj472S9r4", token = keys)
View(my_artist)

# Retrieve album data of artist

albums <- getAlbums("3TVXtAsR1Inumwj472S9r4", token = keys)
View(albums)
unique(albums$name)

# Authentication for spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

#Audio features
audio_features <- get_artist_audio_features("Drake")
View(audio_features)

audio_features <- audio_features[!duplicated(audio_features$track_name), ]
View(audio_features)

unique(audio_features$track_name)

#Song features across albums
ggplot(audio_features, aes(x = speechiness, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Speechiness in Drake Albums",
          subtitle = "Based on speechiness from Spotify's Web API")

ggplot(audio_features, aes(x = danceability, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Danceability in Drake Albums",
          subtitle = "Based on danceability from Spotify's Web API")

ggplot(audio_features, aes(x = energy, y = album_name)) +
  geom_density_ridges() +
  theme_ridges() +
  ggtitle("Energy in Drake Albums",
          subtitle = "Based on energy from Spotify's Web API")

# Retrieve information about related artists

related_bm <- getRelated("Drake", token = keys)
View(related_bm)

#Corr Plot of All audio features
af_df <- data.frame(audio_features$album_release_year, audio_features$duration_ms, audio_features$danceability, audio_features$energy, audio_features$loudness, audio_features$speechiness,
                    audio_features$acousticness, audio_features$instrumentalness, audio_features$liveness, audio_features$valence, audio_features$tempo, audio_features$time_signature)

library(corrplot)
cormat = cor(af_df)
corrplot((cor(af_df)))

library(reshape2)
melted_data <- melt(cor(af_df))

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_data, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "cyan", mid = "violet", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Audio\nFeatures") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

# Set up YouTube authentication variables 

api_key <- "AIzaSyD1nYqwlbEL_GJ_-D4j2koreCgQNuy11bE"
client_id <- "1057465214837-q53tkljli63n67l68do5le9gpt7dfpdo.apps.googleusercontent.com"
client_secret <- "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"

library(tuber)

# Authenticate to YouTube using the tuber package

yt_oauth(app_id = client_id, app_secret = client_secret)

#2.2

# Search YouTube

video_search <- yt_search("Drake Official Music Video")
View(video_search)

chan <- get_all_channel_video_stats(channel_id = "UCQznUf1SjfDqx65hX3zRDiA")

#Top 5 most viewed
library(sqldf)

top_5V <- sqldf("SELECT id,title,viewCount,likeCount,url FROM chan 
      ORDER BY CAST(viewCount AS numeric) DESC
      LIMIT 5", row.names = TRUE)

View(top_5V)

#Top 5 mot liked
top_5L <- sqldf("SELECT id,title,viewCount,likeCount,url FROM chan 
      ORDER BY CAST(likeCount AS numeric) DESC
      LIMIT 5", row.names = TRUE)
View(top_5L)

#Making a Data Frame for above 2 results and then plotting a graph 
df <- data.frame(SortedBy=rep(c("Likes", "Views"), each=5),
                  Views=c(1929552454, 1490332372, 507785195, 440935088, 430373338, 1490332372, 1929552454, 287923761, 360484852, 440935088),
                  Likes=c(10597918, 15449108, 2683277, 4048613, 2068505, 15449108, 10597918, 4247348, 4234106, 4048613))

# Plotting
ggplot(df, aes(x=Views, y=Likes, group=SortedBy)) +
  geom_line(aes(linetype=SortedBy))+
  geom_point(aes(shape=SortedBy))

#2.3 

# Load packages for Text pre-processing
library(vosonSML)
library(magrittr)
library(tidyr)
library(tidytext)
library(stopwords)
library(textclean)
library(qdapRegex)
library(tm)
library(SnowballC)
library(ggplot2)

# Clean the tweet text

clean_text <- twitter_data2$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()


# Convert clean_text vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content

# Perform further pre-processing 

text_corpus <- text_corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

text_corpus[[1]]$content
text_corpus[[5]]$content

# Transform corpus into a Document Term Matrix

doc_term_matrix <- DocumentTermMatrix(text_corpus)


# Sort words by total frequency across all documents

dtm_df <- as.data.frame(as.matrix(doc_term_matrix))
View(dtm_df)

freq <- sort(colSums(dtm_df), decreasing = TRUE)

head(freq, n = 10)

#Text preprocessing for milestone 1 data

clean_text2 <- twitter_data$tweets$text %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_emoji() %>% 
  replace_emoticon()

text_corpus2 <- VCorpus(VectorSource(clean_text2))

text_corpus2[[1]]$content
text_corpus2[[5]]$content

text_corpus2 <- text_corpus2 %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords(kind = "SMART")) %>% 
  tm_map(stemDocument) %>% 
  tm_map(stripWhitespace)

text_corpus2[[1]]$content
text_corpus2[[5]]$content

doc_term_matrix2 <- DocumentTermMatrix(text_corpus2)

dtm_df2 <- as.data.frame(as.matrix(doc_term_matrix2))
View(dtm_df2)

freq2 <- sort(colSums(dtm_df2), decreasing = TRUE)

head(freq2, n = 10)

# Plot word frequency

word_frequ_df <- data.frame(word = names(freq), freq)
View(word_frequ_df)

ggplot(subset(word_frequ_df, freq > 1000), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency") + 
  xlab("Words") + 
  ylab("Frequency")


word_frequ_df2 <- data.frame(word = names(freq2), freq2)
View(word_frequ_df2)

ggplot(subset(word_frequ_df2, freq2 > 67), aes(x = reorder(word, -freq2), y = freq2)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Word Frequency for Milestone 1 Data") + 
  xlab("Words") + 
  ylab("Frequency")

#2.4
library(igraph)

# Create twomode (bimodal) network

twomode_network <- twitter_data2 %>% Create("twomode", 
                                           removeTermsOrHashtags = c("Drake", "drake", "#Drake", "#drake"))
twomode_graph <- twomode_network %>% Graph()


# Write graph to file

write.graph(twomode_graph, file = "TwitterTwomode.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph))
V(twomode_graph)$name


# Find all maximum components that are weakly connected

twomode_comps <- components(twomode_graph, mode = c("weak"))

twomode_comps$no
twomode_comps$csize
head(twomode_comps$membership, n = 30)


# Get sub-graph with most members

largest_comp <- which.max(twomode_comps$csize)

twomode_subgraph <- twomode_graph %>% 
  induced_subgraph(vids = which(twomode_comps$membership == largest_comp))


# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph, directed = FALSE), decreasing = TRUE)[1:20]

#Same analysis for DJ Khaled
twomode_network2 <- twitter_data6 %>% Create("twomode", 
                                             removeTermsOrHashtags = c("DJ Khaled"))
twomode_graph2 <- twomode_network2 %>% Graph()


# Write graph to file

write.graph(twomode_graph2, file = "TwitterTwomode2.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph2))
V(twomode_graph2)$name


# Find all maximum components that are weakly connected

twomode_comps2 <- components(twomode_graph2, mode = c("weak"))

twomode_comps2$no
twomode_comps2$csize
head(twomode_comps2$membership, n = 30)


# Get sub-graph with most members

largest_comp2 <- which.max(twomode_comps2$csize)

twomode_subgraph2 <- twomode_graph2 %>% 
  induced_subgraph(vids = which(twomode_comps2$membership == largest_comp2))


# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph2, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph2, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph2, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph2, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph2, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph2, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph2, directed = FALSE), decreasing = TRUE)[1:20]

#for third artist Justin Bieber
twomode_network3 <- twitter_data_JB %>% Create("twomode", 
                                             removeTermsOrHashtags = c("Justin Beiber", "jb", "JB"))
twomode_graph3 <- twomode_network3 %>% Graph()


# Write graph to file

write.graph(twomode_graph3, file = "TwitterTwomode3.graphml", format = "graphml")


# Inspect the graph object

length(V(twomode_graph3))
V(twomode_graph3)$name


# Find all maximum components that are weakly connected

twomode_comps3 <- components(twomode_graph3, mode = c("weak"))

twomode_comps3$no
twomode_comps3$csize
head(twomode_comps3$membership, n = 30)


# Get sub-graph with most members

largest_comp3 <- which.max(twomode_comps3$csize)

twomode_subgraph3 <- twomode_graph3 %>% 
  induced_subgraph(vids = which(twomode_comps3$membership == largest_comp3))


# Display top 20 nodes from the sub-graph ordered by degree centrality

sort(degree(twomode_subgraph3, mode = "in"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph3, mode = "out"), decreasing = TRUE)[1:20]
sort(degree(twomode_subgraph3, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by closeness centrality

sort(closeness(twomode_subgraph3, mode = "in"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph3, mode = "out"), decreasing = TRUE)[1:20]
sort(closeness(twomode_subgraph3, mode = "total"), decreasing = TRUE)[1:20]


# Display top 20 nodes from the sub-graph ordered by betweenness centrality

sort(betweenness(twomode_subgraph3, directed = FALSE), decreasing = TRUE)[1:20]

#2.5 
#Collecting YT data for Drake
# Search YouTube

video_search2 <- yt_search("Drake")
View(video_search2)


# Pick a video from video_search and get some info 

get_stats(video_id = "xpVfcZ0ZcFM")

# Choose some videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids <- as.vector(video_search$video_id[1:3])

yt_data <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data)

yt_actor_network <- yt_data %>% Create("actor")
yt_actor_graph <- Graph(yt_actor_network)


# Transform into an undirected graph

undir_yt_actor_graph <- as.undirected(yt_actor_graph, mode = "collapse")

# Run Louvain algorithm

louvain_yt_actor <- cluster_louvain(undir_yt_actor_graph)
# See sizes of communities

sizes(louvain_yt_actor)


# Visualise the Louvain communities

plot(louvain_yt_actor, 
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)
write.graph(undir_yt_actor_graph, file = "UndirYTActorforL.graphml", format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor <- cluster_edge_betweenness(undir_yt_actor_graph)


# See sizes of communities

sizes(eb_yt_actor)


# Visualise the edge-betweenness communities

plot(eb_yt_actor,
     undir_yt_actor_graph, 
     vertex.label = V(undir_yt_actor_graph)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph2 <- yt_actor_graph
V(yt_actor_graph2)$name <- V(yt_actor_graph2)$screen_name
undir_yt_actor_graph2 <- as.undirected(yt_actor_graph2, mode = "collapse")
eb_yt_actor2 <- cluster_edge_betweenness(undir_yt_actor_graph2)

is_hierarchical(eb_yt_actor2)
as.dendrogram(eb_yt_actor2)
plot_dendrogram(eb_yt_actor2)

plot_dendrogram(eb_yt_actor2, mode = "dendrogram", xlim = c(1,20))

#Data Collection for related artists
#DJ Khaled
video_search3 <- yt_search("DJ Khaled")
View(video_search3)


# Pick a video from video_search and get some info 

get_stats(video_id = "3CxtK7-XtE0")

# Choose some videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids2 <- as.vector(video_search3$video_id[1:3])

yt_data2 <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids2,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data2)

#21 Savage
video_search4 <- yt_search("21 Savage")
View(video_search4)


# Pick a video from video_search and get some info 

get_stats(video_id = "I4DjHHVHWAE")

# Choose some videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids3 <- as.vector(video_search4$video_id[1:3])

yt_data3 <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids3,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data3)

#Future
video_search5 <- yt_search("Future")
View(video_search5)


# Pick a video from video_search and get some info 

get_stats(video_id = "l0U7SxXHkPY")

# Choose some videos and store their video IDs,
# for which we want to collect comments
# and build an actor network

video_ids4 <- as.vector(video_search5$video_id[1:3])

yt_data4 <- Authenticate("youtube", apiKey = api_key) %>%
  Collect(videoIDs = video_ids4,
          writeToFile = TRUE,
          maxComments = 500,
          verbose = TRUE)

View(yt_data4)

#for second artist
yt_actor_network2 <- yt_data2 %>% Create("actor")
yt_actor_graph2 <- Graph(yt_actor_network2)


# Transform into an undirected graph

undir_yt_actor_graph2 <- as.undirected(yt_actor_graph2, mode = "collapse")

# Run Louvain algorithm

louvain_yt_actor2 <- cluster_louvain(undir_yt_actor_graph2)
# See sizes of communities

sizes(louvain_yt_actor2)


# Visualise the Louvain communities

plot(louvain_yt_actor2, 
     undir_yt_actor_graph2, 
     vertex.label = V(undir_yt_actor_graph2)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)
write.graph(undir_yt_actor_graph2, file = "UndirYTActorforDJK.graphml", format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor2 <- cluster_edge_betweenness(undir_yt_actor_graph2)


# See sizes of communities

sizes(eb_yt_actor2)


# Visualise the edge-betweenness communities

plot(eb_yt_actor2,
     undir_yt_actor_graph2, 
     vertex.label = V(undir_yt_actor_graph2)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph3 <- yt_actor_graph2
V(yt_actor_graph3)$name <- V(yt_actor_graph3)$screen_name
undir_yt_actor_graph3 <- as.undirected(yt_actor_graph3, mode = "collapse")
eb_yt_actor3 <- cluster_edge_betweenness(undir_yt_actor_graph2)

is_hierarchical(eb_yt_actor2)
as.dendrogram(eb_yt_actor2)
plot_dendrogram(eb_yt_actor2)

plot_dendrogram(eb_yt_actor2, mode = "dendrogram", xlim = c(1,20))

#For third artist

yt_actor_network3 <- yt_data3 %>% Create("actor")
yt_actor_graph3 <- Graph(yt_actor_network3)


# Transform into an undirected graph

undir_yt_actor_graph3 <- as.undirected(yt_actor_graph3, mode = "collapse")

# Run Louvain algorithm

louvain_yt_actor3 <- cluster_louvain(undir_yt_actor_graph3)
# See sizes of communities

sizes(louvain_yt_actor3)


# Visualise the Louvain communities

plot(louvain_yt_actor3, 
     undir_yt_actor_graph3, 
     vertex.label = V(undir_yt_actor_graph3)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)
write.graph(undir_yt_actor_graph3, file = "UndirYTActorfor21.graphml", format = "graphml")

# Run Girvan-Newman (edge-betweenness) algorithm

eb_yt_actor3 <- cluster_edge_betweenness(undir_yt_actor_graph3)


# See sizes of communities

sizes(eb_yt_actor3)


# Visualise the edge-betweenness communities

plot(eb_yt_actor3,
     undir_yt_actor_graph3, 
     vertex.label = V(undir_yt_actor_graph3)$screen_name,
     vertex.size = 4,
     vertex.label.cex = 0.7)


# Visualise the edge-betweenness hierarchy

yt_actor_graph4 <- yt_actor_graph3
V(yt_actor_graph4)$name <- V(yt_actor_graph4)$screen_name
undir_yt_actor_graph4 <- as.undirected(yt_actor_graph4, mode = "collapse")
eb_yt_actor4 <- cluster_edge_betweenness(undir_yt_actor_graph3)

is_hierarchical(eb_yt_actor3)
as.dendrogram(eb_yt_actor3)
plot_dendrogram(eb_yt_actor3)

plot_dendrogram(eb_yt_actor3, mode = "dendrogram", xlim = c(1,20))

#Loading Libraries for ML
library(vosonSML)
library(magrittr)
library(tidytext)
library(textclean)
library(qdapRegex)
library(syuzhet)
library(ggplot2)

# Clean the tweet text

clean_text_ML <- twitter_data2$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Assign sentiment scores to tweets

sentiment_scores <- get_sentiment(clean_text_ML, method = "afinn") %>% sign()

sentiment_df <- data.frame(text = clean_text, sentiment = sentiment_scores)
View(sentiment_df)

# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df$sentiment <- factor(sentiment_df$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df)

#2.6
# Plot sentiment classification

ggplot(sentiment_df, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")
# Assign emotion scores to tweets

emo_scores <- get_nrc_sentiment(clean_text)[ , 1:8]

emo_scores_df <- data.frame(clean_text, emo_scores)
View(emo_scores_df)


# Calculate proportion of emotions across all tweets

emo_sums <- emo_scores_df[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums)[1] <- "Proportion" 
View(emo_sums)

# Plot emotion classification

ggplot(emo_sums, aes(x = reorder(rownames(emo_sums), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets")

#Emotional analysis for second artist Justin Bieber

# Clean the tweet text

clean_text_ML2 <- twitter_data_JB$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Assign sentiment scores to tweets

sentiment_scores2 <- get_sentiment(clean_text_ML2, method = "afinn") %>% sign()

sentiment_df2 <- data.frame(text = clean_text_ML2, sentiment = sentiment_scores2)
View(sentiment_df2)

# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df2$sentiment <- factor(sentiment_df2$sentiment, levels = c(1, 0, -1),
                                 labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df2)

# Plot sentiment classification

ggplot(sentiment_df2, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")
# Assign emotion scores to tweets

emo_scores2 <- get_nrc_sentiment(clean_text_ML2)[ , 1:8]

emo_scores_df2 <- data.frame(clean_text_ML2, emo_scores2)
View(emo_scores_df2)


# Calculate proportion of emotions across all tweets

emo_sums2 <- emo_scores_df2[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df) 

names(emo_sums2)[1] <- "Proportion" 
View(emo_sums2)

# Plot emotion classification

ggplot(emo_sums2, aes(x = reorder(rownames(emo_sums2), Proportion),
                     y = Proportion,
                     fill = rownames(emo_sums2))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets")

#Now, for third artist DJ Khaled

# Clean the tweet text

clean_text_ML3 <- twitter_data6$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Assign sentiment scores to tweets

sentiment_scores3 <- get_sentiment(clean_text_ML3, method = "afinn") %>% sign()

sentiment_df3 <- data.frame(text = clean_text_ML3, sentiment = sentiment_scores3)
View(sentiment_df3)

# Convert sentiment scores to labels: positive, neutral, negative

sentiment_df3$sentiment <- factor(sentiment_df3$sentiment, levels = c(1, 0, -1),
                                  labels = c("Positive", "Neutral", "Negative")) 
View(sentiment_df3)

# Plot sentiment classification

ggplot(sentiment_df3, aes(x = sentiment)) +
  geom_bar(aes(fill = sentiment)) +
  scale_fill_brewer(palette = "RdGy") +
  labs(fill = "Sentiment") +
  labs(x = "Sentiment Categories", y = "Number of Tweets") +
  ggtitle("Sentiment Analysis of Tweets")
# Assign emotion scores to tweets

emo_scores3 <- get_nrc_sentiment(clean_text_ML3)[ , 1:8]

emo_scores_df3 <- data.frame(clean_text_ML3, emo_scores3)
View(emo_scores_df3)


# Calculate proportion of emotions across all tweets

emo_sums3 <- emo_scores_df3[,2:9] %>% 
  sign() %>% 
  colSums() %>% 
  sort(decreasing = TRUE) %>% 
  data.frame() / nrow(emo_scores_df3) 

names(emo_sums3)[1] <- "Proportion" 
View(emo_sums3)

# Plot emotion classification

ggplot(emo_sums3, aes(x = reorder(rownames(emo_sums3), Proportion),
                      y = Proportion,
                      fill = rownames(emo_sums3))) +
  geom_col() +
  coord_flip()+
  guides(fill = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Emotion Categories", y = "Proportion of Tweets") +
  ggtitle("Emotion Analysis of Tweets")



app_id <- "cf58bbe7efe843e29b35e9c383778e5d"
app_secret <- "bde0a847bd2841e49894b616ff649f51"
token <- "1"


# Authenticate to Spotify using the spotifyr package:

Sys.setenv(SPOTIFY_CLIENT_ID = app_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = app_secret)
access_token <- get_spotify_access_token()

#2.7

# Get songs from Drake and their audio features

drake_features <- get_artist_audio_features("Drake")
View(drake_features)

data.frame(colnames(drake_features))

drake_features_subset <- drake_features[ , 9:20]
View(drake_features_subset)

# Get top 100 songs and their audio features

top100_features <- get_playlist_audio_features("spotify", "4hOKQuZbraPDIfaGbM3lKI")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'isDrake' column (class variable) to each data frame
# to indicate which songs are by Drake and which are not

top100_features_subset["isDrake"] <- 0
drake_features_subset["isDrake"] <- 1

# Remove any songs by Drake that appear in the top 100
# and combine the two data frames into one dataset

top100_features_nodrake <- anti_join(top100_features_subset,
                                     drake_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_nodrake, drake_features_subset)

# Format the dataset so that we can give it as input to a model:
# change the 'isDrake' column into a factor
# and remove the 'track_id' column

comb_data$isDrake <- factor(comb_data$isDrake)
comb_data <- select(comb_data, -track_id)

# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]

# Train the decision tree model

dt_model <- train(isDrake~ ., data = training_set, method = "C5.0")
# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}

# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isDrake)


#Another playlist
top100_features <- get_playlist_audio_features("spotify", "37i9dQZF1DX9wC1KY45plY")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'isDrake' column (class variable) to each data frame
# to indicate which songs are by Drake and which are not

top100_features_subset["isDrake"] <- 0
drake_features_subset["isDrake"] <- 1

# Remove any songs by Drake that appear in the top 100
# and combine the two data frames into one dataset

top100_features_nodrake <- anti_join(top100_features_subset,
                                     drake_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_nodrake, drake_features_subset)

# Format the dataset so that we can give it as input to a model:
# change the 'isDrake' column into a factor
# and remove the 'track_id' column

comb_data$isDrake <- factor(comb_data$isDrake)
comb_data <- select(comb_data, -track_id)

# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]

# Train the decision tree model

dt_model <- train(isDrake~ ., data = training_set, method = "C5.0")
# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}

# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isDrake)

#third playlist
top100_features <- get_playlist_audio_features("spotify", "37i9dQZF1DX1H4LbvY4OJi")
View(top100_features)

data.frame(colnames(top100_features))

top100_features_subset <- top100_features[ , 6:17]
View(top100_features_subset)

top100_features_subset <- top100_features_subset %>% rename(track_id = track.id)


# Add the 'isDrake' column (class variable) to each data frame
# to indicate which songs are by Drake and which are not

top100_features_subset["isDrake"] <- 0
drake_features_subset["isDrake"] <- 1

# Remove any songs by Drake that appear in the top 100
# and combine the two data frames into one dataset

top100_features_nodrake <- anti_join(top100_features_subset,
                                     drake_features_subset,
                                     by = "track_id")
comb_data <- rbind(top100_features_nodrake, drake_features_subset)

# Format the dataset so that we can give it as input to a model:
# change the 'isDrake' column into a factor
# and remove the 'track_id' column

comb_data$isDrake <- factor(comb_data$isDrake)
comb_data <- select(comb_data, -track_id)

# Randomise the dataset (shuffle the rows)

comb_data <- comb_data[sample(1:nrow(comb_data)), ]


# Split the dataset into training and testing sets (80% training, 20% testing)

split_point <- as.integer(nrow(comb_data)*0.8)
training_set <- comb_data[1:split_point, ]
testing_set <- comb_data[(split_point + 1):nrow(comb_data), ]

# Train the decision tree model

dt_model <- train(isDrake~ ., data = training_set, method = "C5.0")
# Sample a single prediction (can repeat)

prediction_row <- 1 # MUST be smaller than or equal to training set size

if (tibble(predict(dt_model, testing_set[prediction_row, ])) ==
    testing_set[prediction_row, 12]){
  print("Prediction is correct!")
} else {
  ("Prediction is wrong")
}

# Analyse the model accuracy with a confusion matrix

confusionMatrix(dt_model, reference = testing_set$isDrake)

#2.8

clean_text <- twitter_data2$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Convert clean tweet vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content

# Remove stop words

text_corpus <- text_corpus %>%
  tm_map(removeWords, stopwords(kind = "SMART")) 

text_corpus[[1]]$content
text_corpus[[5]]$content

# Transform corpus into a Document Term Matrix and remove 0 entries

doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]

# Optional: Remove objects and run garbage collection for faster processing

save(dtm, file = "doc_term_matrix.RData")
rm(list = ls(all.names = TRUE))
gc() 
load("doc_term_matrix.RData")

# Create LDA model with k topics

lda_model <- LDA(dtm, k = 6)

# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

tweet_topics <- tidy(lda_model, matrix = "beta")

# Visualise the top 10 terms per topic

top_terms <- tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


#milestone1 data
clean_text <- twitter_data$tweets$text  %>% 
  rm_twitter_url() %>% 
  replace_url() %>% 
  replace_hash() %>% 
  replace_tag() %>% 
  replace_internet_slang() %>% 
  replace_emoji() %>% 
  replace_emoticon() %>% 
  replace_non_ascii() %>% 
  replace_contraction() %>% 
  gsub("[[:punct:]]", " ", .) %>% 
  gsub("[[:digit:]]", " ", .) %>% 
  gsub("[[:cntrl:]]", " ", .) %>% 
  gsub("\\s+", " ", .) %>% 
  tolower()

# Convert clean tweet vector into a document corpus (collection of documents)

text_corpus <- VCorpus(VectorSource(clean_text))

text_corpus[[1]]$content
text_corpus[[5]]$content

# Remove stop words

text_corpus <- text_corpus %>%
  tm_map(removeWords, stopwords(kind = "SMART")) 

text_corpus[[1]]$content
text_corpus[[5]]$content

# Transform corpus into a Document Term Matrix and remove 0 entries

doc_term_matrix <- DocumentTermMatrix(text_corpus)
non_zero_entries = unique(doc_term_matrix$i)
dtm = doc_term_matrix[non_zero_entries,]

# Optional: Remove objects and run garbage collection for faster processing

save(dtm, file = "doc_term_matrix2.RData")
rm(list = ls(all.names = TRUE))
gc() 
load("doc_term_matrix2.RData")

# Create LDA model with k topics

lda_model <- LDA(dtm, k = 6)

# Generate topic probabilities for each word
# 'beta' shows the probability that this word was generated by that topic

tweet_topics <- tidy(lda_model, matrix = "beta")

# Visualise the top 10 terms per topic

top_terms <- tweet_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#Converting data frames to JSON files for Q2.10
library(rjson)
library(jsonlite)

drake_features_subset2 <- data.frame(drake_features$album_release_year, drake_features[ , 9:19], drake_features$duration_ms)
View(drake_features_subset2)

toJSON(x = drake_features_subset2, dataframe = 'values', pretty = T)
json_aud_feat2 <- toJSON(drake_features_subset2)
write(json_aud_feat2, "json_aud_feat2.json")

#Generating geo data for tweets for Q2.10

world_bbox <- st_bbox(c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs = 4326)
world_bbox

map <- leaflet() %>%
  addTiles() %>%
  fitBounds(-180, -90, 180, 90)
map

geo_tweets_loc <- lat_lng(twitter_data2$tweets)

geo_tweets_loc <- subset(geo_tweets_loc, lat != "NA")

geo_tweets_loc <- geo_tweets_loc[c("text", "lat", "lng")]

geo_tweets_grouped <- geo_tweets_loc %>%
  group_by(lat, lng) %>%
  mutate(concat_text = paste0(text, collapse = "<br/> <br/>")) %>%
  select(-text)

geo_tweets_grouped <- unique(geo_tweets_grouped)
View(geo_tweets_grouped)

# Project tweets to map

geo_tweets_grouped$longitude = as.numeric(as.character(geo_tweets_grouped$lng))
geo_tweets_grouped$latitude = as.numeric(as.character(geo_tweets_grouped$lat))

map <- leaflet() %>%
  addTiles() %>%
  fitBounds(-180, -90, 180, 90) %>%
  addCircleMarkers(geo_tweets_grouped$longitude, 
                   geo_tweets_grouped$latitude, 
                   popup = geo_tweets_grouped$concat_text)
map

geo_df = data.frame(geo_tweets_grouped$longitude, geo_tweets_grouped$latitude)
View(geo_df) 

#converting it to JSON file

toJSON(x = geo_df, dataframe = 'values', pretty = T)
geo_df <- toJSON(geo_df)
write(geo_df, "geo_df.json")

#converting YT data into JSON file for Q2.10

toJSON(x = yt_data, dataframe = 'values', pretty = T)
yt_data <- toJSON(yt_data)
write(yt_data, "yt_data.json")

toJSON(x = geo_tweets_grouped, dataframe = 'values', pretty = T)
geo_tweets_grouped <- toJSON(geo_tweets_grouped)
write(geo_tweets_grouped, "geo_tweets.json")

toJSON(x = twitter_data2, dataframe = 'values', pretty = T)
twitter_data2J <- toJSON(twitter_data2)
write(twitter_data2J, "DrakeTweets.json")
# Remove special characters from location field
twitter_data2$users$location <- gsub("[^[:alnum:] ]", "", twitter_data2$users$location)