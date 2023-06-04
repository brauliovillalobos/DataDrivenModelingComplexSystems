rm(list = ls())
# Importing packages ------------------------------------------------------
# Data Wrangling
library(tidyverse)
library(dplyr)
# For graph and visualization
library(tidygraph)
library(ggraph)
library(igraph)
library(rtweet)
library(rjson)
library(jsonlite)
library(stringr)
library(gridExtra)
library(ggplot2)

# JSON (don't run) --------------------------------------------------------------------

#Trying to parse the data 
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets")

# 1. First row manually extracted
first_row <- fromJSON(txt = "/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/first_row.json")
print(data)

# 2. First row Fabri - manually extracted 
first_row_fabri <- fromJSON(txt = "/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/first_row_fabri.json")

# 3. Joining Charlie 
joining_charlie <- fromJSON(txt = "/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/test_joining_charlie.json")

data <- fromJSON(txt = "first_row.json")
# create an empty list to store the parsed JSON objects
parsed_json <- list()

# open the file and read it line by line
con <- file('CarlosAlvQ.json', 'r')
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  # parse the line as a JSON object and append it to the list
  parsed_json <- c(parsed_json, fromJSON(line))
}
close(con)

# combine the list of JSON objects into one big JSON object
combined_json <- toJSON(parsed_json)


stream_in(file("FabriAlvarado.json"))


joining_test <- fromJSON(file = "/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/test_joining_charlie.json")

json_lines <- readLines("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/test_joining_charlie.json")

# parse the JSON string
data <- fromJSON(paste(json_lines, collapse = "\n"))


joining_test <- fromJSON(txt = "combined_1.json")



# Choosing sections to use (don't run) -----------------------------------------------
library(jsonlite)
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets")

myData <- read_json("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/first_row.json")  

# There's an inconsistency between the number of tweets in "data" and the ones in includes-->tweets
# We have to decide which of the two to take. We compare which tweets are in includes that aren't present
# in data

smaller <- c()
for(i in 1:length(myData$data)){
  smaller <- c(smaller,myData$data[[i]]$conversation_id)
}

larger <- c()
for(i in 1:length(myData$includes$tweets)){
  larger <- c(larger,myData$includes$tweets[[i]]$conversation_id)
}

larger[!(larger%in%smaller)]
# These are tweets that mention the account that we're monitoring. For this reason, 
# we should take the tweets from data and not from includes$tweets. 

# We keep only the data within "data" 
myData <- myData$data



# Used queries (don't run) ------------------------------------------------------------

#twarc2 timeline --use-search --start-time "2017-10-03T12:31:04" --end-time "2018-04-08T12:31:04" 'fabrialvarado7' FabriAlvarado_filtered.jsonl
#twarc2 timeline --use-search --start-time "2017-10-03T12:31:04" --end-time "2018-04-08T12:31:04" 'CarlosAlvQ' CarlosAlvQ_filtered.jsonl

# Importing data (run) ----------------------------------------------------------
#Trying to parse the data 
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets")

fabri_a <- stream_in(file("FabriAlvarado_filtered.json"),pagesize = 10000)
fabri_a <- fabri_a$data

carlos_a <- stream_in(file("CarlosAlvQ_filtered.json"),pagesize = 10000)
carlos_a <- carlos_a$data
# Extraction functions (run) ----------------------------------------------------

# Function to extract the first level information like (1) tweets_id, (2) language
# (3) creation of the tweet, (4) author_id and (5) tweet text.
first_level_extraction <- function(large_list){

  first_level_df <- data.frame()
  for(i in 1:length(large_list)){
    tweet_id_v <- c()
    lang_v <- c()
    date_creation_v <- c()
    author_id_v <- c()
    text_v <- c()
    
    tweet_id_v      <- c(tweet_id_v,large_list[[i]]$id)
    lang_v          <- c(lang_v,large_list[[i]]$lang)
    date_creation_v <- c(date_creation_v,large_list[[i]]$created_at)
    author_id_v     <- c(author_id_v,large_list[[i]]$author_id)
    text_v          <- c(text_v,large_list[[i]]$text)
    
    first_level_df <- rbind(first_level_df, data.frame(tweet_id_v,lang_v,date_creation_v,author_id_v,text_v))
  }
  return(first_level_df)
}

# Function to extract the mentions, annotations and hashtags, which are multilevel information and 
# refer to the mentions, if any, done in the tweet. We extract, if any, the (1) mentioned username, 
# (2) id of mentioned user, (3) type of annotations, (4) the annotations and (5) used hashtags.
multilevel_extraction <- function(large_list){
  mentions_df <- data.frame()
  for(t in 1:length(large_list)){
    username_v <- c()
    id_mentioned <- c()
    annotations_type <- c()
    annotations <- c()
    hashtags <- c()
    
    for(y in 1:length(large_list[[t]]$entities$mentions)){
      username_v <- c(username_v,
                      ifelse(length(large_list[[t]][["entities"]][["mentions"]][[y]]) == 0, 
                             NA, 
                             large_list[[t]]$entities$mentions[[y]]$username))
      id_mentioned <- c(id_mentioned,
                        ifelse(length(large_list[[t]][["entities"]][["mentions"]][[y]]) == 0,
                               NA,
                               large_list[[t]]$entities$mentions[[y]]$id))
      annotations_type <- c(annotations_type,
                      ifelse(length(large_list[[t]][["entities"]][["annotations"]][[y]]) == 0, 
                             NA, 
                             large_list[[t]]$entities$annotations[[y]]$type))
      annotations <- c(annotations,
                            ifelse(length(large_list[[t]][["entities"]][["annotations"]][[y]]) == 0, 
                                   NA, 
                                   large_list[[t]]$entities$annotations[[y]]$normalized_text))
      hashtags <- c(hashtags,
                       ifelse(length(large_list[[t]][["entities"]][["hashtags"]][[y]]) == 0, 
                              NA, 
                              large_list[[t]]$entities$hashtags[[y]]$tag))
    }
    mentions_df <- rbind(mentions_df, data.frame(username_v,id_mentioned,annotations_type,annotations,hashtags))
  }
return(mentions_df)
}

# Function to extract the metrics as retweets, replies, likes and quotes. 
metrics_extraction <- function(large_list){
  metrics <- data.frame()
  for(i in 1:length(large_list)){
    retweet_v <- c()
    reply_v   <- c()
    like_v    <- c()
    quote_v   <- c()
    
    retweet_v <- c(large_list[[i]]$public_metrics$retweet_count)
    reply_v   <- c(large_list[[i]]$public_metrics$reply_count)
    like_v    <- c(large_list[[i]]$public_metrics$like_count)
    quote_v   <- c(large_list[[i]]$public_metrics$quote_count)
    
    metrics <- rbind(metrics, data.frame(retweet_v,reply_v,like_v,quote_v))
  }
  return(metrics)
}

# Function to extract all the information of interest
parse_info <- function(large_list){
  
  parsed_df <- first_level_extraction(large_list)
  parsed_df <- cbind(parsed_df,multilevel_extraction(large_list))
  parsed_df <- cbind(parsed_df,metrics_extraction(large_list))
  
  return(parsed_df)
}

extract_liking_users <- function(large_list){
  liking_users <- data.frame()
  for(i in 1:length(large_list)){
    username_v <- c()
    user_id_v <- c()
    verified_v <- c()
    followers_count_v <- c()
    following_count_v <- c()
    tweet_count_v <- c()
    date_creation_v <- c()
    
    username_v <- c(username_v, large_list[[i]]$username)
    user_id_v <- c(user_id_v,large_list[[i]]$id)
    verified_v <- c(verified_v,large_list[[i]]$verified)
    followers_count_v <- c(followers_count_v,large_list[[i]]$public_metrics$followers_count)
    following_count_v <- c(following_count_v,large_list[[i]]$public_metrics$following_count)
    tweet_count_v <- c(tweet_count_v,large_list[[i]]$public_metrics$tweet_count)
    date_creation_v <- c(date_creation_v,large_list[[i]]$created_at)
    
    liking_users <- rbind(liking_users, data.frame(username_v,user_id_v,verified_v,
                                                   followers_count_v, following_count_v,
                                                   tweet_count_v, date_creation_v))
  }
  return(liking_users)
}

extract_liking_users_short_version <- function(large_list){
  liking_users <- data.frame()
  for(i in 1:length(large_list)){
    user_id_v <- c()
    
    user_id_v <- c(user_id_v,large_list[[i]]$id)
    
    liking_users <- rbind(liking_users, data.frame(user_id_v))
  }
  return(liking_users)
}

dict_likes_count <- function(file_list_i){
  user_dict <- list()
  
  for(file_name in file_list_i){
    test_list <- stream_in(file(file_name),pagesize = 10000)
    test_list <- test_list$data
    
    test_list <- extract_liking_users_short_version(test_list)
    
    user_ids <- unique(test_list$user_id_v)
    
    for (user_id in user_ids) {
      
      # Check if the user ID is already in the dictionary
      if (user_id %in% names(user_dict)) {
        # If so, increment the count for that user ID
        user_dict[[user_id]] <- user_dict[[user_id]] + sum(test_list$user_id_v == user_id)
      } else {
        # If not, add the user ID to the dictionary with a count of 1
        user_dict[[user_id]] <- sum(test_list$user_id_v == user_id)
      }
    }
  }
  return(user_dict)
}

classifying_fraction_network <- function(data_frame_i){
  classified_network <- data_frame_i %>% 
    summarise(
      fam_polarized = mean(polariz_index < -0.5) * 100,
      caq_polarized = mean(polariz_index > 0.5) * 100,
      not_polarized = mean(polariz_index == 0) * 100,
      not_engaged = mean(polariz_index > -0.5 & polariz_index < 0.5 & polariz_index != 0) * 100
    )
  return(classified_network)
}

following_parsing <- function(large_list){
  
  first_df <- data.frame()
  for(t in 1:length(following_pol_caq)){
    user_id <- c(large_list[[t]]$id)
    username <- c(large_list[[t]]$username)
    name <- c(large_list[[t]]$name)
    created_at <- c(large_list[[t]]$created_at)
    location <- c(large_list[[t]]$location)
    followers_count <- c(large_list[[t]]$public_metrics$followers_count)
    following_count <- c(large_list[[t]]$public_metrics$following_count)
    tweet_count <- c(large_list[[t]]$public_metrics$tweet_count)
    
    first_df <- rbind(first_df, data.frame(user_id, username, name, created_at,
                                           location, followers_count,
                                           following_count, tweet_count))
  }
  return(first_df)
}

following_parsing_fam <- function(large_list){
  
  first_df <- data.frame()
  for(t in 1:length(following_pol_fam)){
    user_id <- c(large_list[[t]]$id)
    username <- c(large_list[[t]]$username)
    name <- c(large_list[[t]]$name)
    created_at <- c(large_list[[t]]$created_at)
    location <- c(large_list[[t]]$location)
    followers_count <- c(large_list[[t]]$public_metrics$followers_count)
    following_count <- c(large_list[[t]]$public_metrics$following_count)
    tweet_count <- c(large_list[[t]]$public_metrics$tweet_count)
    
    first_df <- rbind(first_df, data.frame(user_id, username, name, created_at,
                                           location, followers_count,
                                           following_count, tweet_count))
  }
  return(first_df)
}
# Parsing the data (run) --------------------------------------------------------
fabri_a <- parse_info(fabri_a)
carlos_a <- parse_info(carlos_a)

#First we notice that within the time limits we imposed, we retrieve 341 tweets from Fabricio
#Alvarado and 3601 tweets from Carlos Alvarado. This means that for Fabricio Alvarado
#we have less than 10% of the total number of tweets we have for Carlos Alvarado. 
#This, evidently, allow us make a first conclusion: Carlos Alvarado was considerably more
#active, by number of tweets, than Fabricio Alvarado. 

#Now, it isn't possible by using the liking-users function of twarc to retrieve the users
#that liked a retweeted tweet. This allows us to see, at least visually, that a lot of
#tweets from Carlos Alvarado are indeed retweets. For this reason, we carry out the following analysis
# Obtaining users that liked each post (run) ------------------------------------

#We tried by several ways to extract the liking users of the retweeted tweets, but it wasn't possible.
#I tried the GET, academictwitteR, twarc and tweepy but none gave results.
#For this reason, we will unfortunately have to drop all the retweeted tweets, as we can't
#extract which users liked them. 
carlos_a_f  <- carlos_a[substr(carlos_a$text_v, 1, 4) != "RT @", ]
fabri_a_f   <- fabri_a[substr(fabri_a$text_v, 1, 4) != "RT @", ]

# Academic Twitter (not run) --------------------------------------------------------
library(academictwitteR)
set_bearer()
start_time <- Sys.time()
hhh = get_liking_users("982311726744940544", bearer_token = get_bearer(), verbose = TRUE)
end_time <- Sys.time()
#16 seconds
time.taken <- end_time - start_time
# .bash creation (not run) ----------------------------------------------------------

# Carlos Alvarado
#liked_tweets_carlos <- as.numeric(carlos_a_f$tweet_id_v)
#setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_carlos_alv")
#write.table(liked_tweets_carlos, "user_ids.txt", sep = "\n", row.names = FALSE, col.names = FALSE)

# Fabricio Alvarado 
#liked_tweets_fabri <- as.numeric(fabri_a_f$tweet_id_v)
#setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_fabricio_alv")
#write.table(liked_tweets_fabri, "user_ids.txt", sep = "\n", row.names = FALSE, col.names = FALSE)


# Create dictionary of liking count based on info extracted from (run) . --------

# Carlos Alvarado Quesada (caq)
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_carlos_alv_2")
file_dir_caq <- "/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_carlos_alv_2"
file_list_caq <- list.files(file_dir_caq, pattern = ".json", full.names = TRUE)
file_list_caq <- sapply(strsplit(file_list_caq, "/"), tail, 1)
# Count how many likes each user has given to the candidate
start <- Sys.time()
count_likes_dict_caq <- dict_likes_count(file_list_caq)
end <- Sys.time()

count_likes_dict_caq <- data.frame(unlist(count_likes_dict_caq))
count_likes_dict_caq$user_id <- rownames(count_likes_dict_caq)
count_likes_dict_caq <- count_likes_dict_caq[order(count_likes_dict_caq$unlist.count_likes_dict_caq.,decreasing = TRUE),]
head(count_likes_dict_caq)

# Fabricio Alvarado Munoz (fam)
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_fabricio_alv_2")
file_dir_fam <- "/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_fabricio_alv_2"
file_list_fam <- list.files(file_dir_fam, pattern = ".json", full.names = TRUE)
file_list_fam <- sapply(strsplit(file_list_fam, "/"), tail, 1)
# Count how many likes each user has given to the candidate
start <- Sys.time()
count_likes_dict_fam <- dict_likes_count(file_list_fam)
end <- Sys.time()

count_likes_dict_fam <- data.frame(unlist(count_likes_dict_fam))
count_likes_dict_fam$user_id <- rownames(count_likes_dict_fam)
count_likes_dict_fam <- count_likes_dict_fam[order(count_likes_dict_fam$unlist.count_likes_dict_fam.,decreasing = TRUE),]
head(count_likes_dict_fam)

# Understanding missing tweets that weren't extracted(not run) ---------------------

#In the case of CAQ, we extracted in previous sections a total of 3601 tweets
#that were created within the timeframe of the present study. We discovered that it wasn't
#possible to extract the users that liked the tweets that were retweeted. For this reason, 
#we removed these tweets, which meant we ended up with 2051 tweets for which we extracted
#the users that liked them. However, after running the .bash we ended up with just 779 files
#out of the 2051 that were consulted. In this section, we tried to understand what happened
#with the missing 1272 tweets, for which we couldn't download the liking users. 

caq_filtered_tweets_id <- carlos_a_f$tweet_id_v
caq_extracted_liking_users_id <- sapply(strsplit(file_list_caq,"_"),head,1)
length(caq_filtered_tweets_id) #2051 tweets id to be explored and its liking users retrieved
length(caq_extracted_liking_users_id) #779 tweets for which the liking users where effectively retrieved


non_extracted_tweets <- caq_filtered_tweets_id[!(caq_filtered_tweets_id %in% caq_extracted_liking_users_id)]
head(non_extracted_tweets)
non_extracted_tweets <- as.integer64(non_extracted_tweets)
head(non_extracted_tweets)
#We found out the root of the problem. The tweets id are very large numbers and the numeric
#precision wasn't enough to keep the original number when we converted them to numeric values. 
#For that reason, the tweets ids were rounded and clearly they didn't match the desired tweets. 
#We proceed therefore to create a new .bash

#liked_tweets_carlos <- as.integer64(carlos_a_f$tweet_id_v)
#setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_carlos_alv_2")
#write.table(liked_tweets_carlos, "user_ids.txt", sep = "\n", row.names = FALSE, col.names = FALSE)

#liked_tweets_fabri <- as.integer64(fabri_a_f$tweet_id_v)
#setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_fabricio_alv_2")
#write.table(liked_tweets_fabri, "user_ids.txt", sep = "\n", row.names = FALSE, col.names = FALSE)


# Creation of polarization index (run) ------------------------------------------
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/liking_users_fabricio_alv_2")

head(count_likes_dict_fam)
rownames(count_likes_dict_caq) <- NULL
colnames(count_likes_dict_caq) <- c("num_likes","user_id")
head(count_likes_dict_caq)
rownames(count_likes_dict_fam) <- NULL
colnames(count_likes_dict_fam) <- c("num_likes","user_id")
head(count_likes_dict_fam)
  
# merge the two data frames based on the user_id column
merged_df <- merge(count_likes_dict_caq, count_likes_dict_fam, by = "user_id", all = TRUE)

# display the resulting data frame
merged_df[is.na(merged_df)] <- 0
colnames(merged_df) <- c("user_id","caq_likes","fam_likes")
merged_df$polariz_index <- ((merged_df$caq_likes)-(merged_df$fam_likes))/((merged_df$caq_likes)+(merged_df$fam_likes))

plot(density(merged_df$polariz_index))

df <- data.frame(polariz_plot = merged_df$polariz_index)
ggplot(data = df, aes(x = polariz_plot)) + 
  geom_density(color = "#000000", fill = "#F85700", alpha = 0.6) +
  scale_fill_gradient(low = "red", high = "blue") +
  labs(title = "User polarization", x = "Polarization Index", y = "PDF")

# Retrieving the network of followers (not run) -------------------------------------

#users_to_explore_caq <- as.integer64(count_likes_dict_caq$user_id)
#setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/network_carlos_avl")
#write.table(users_to_explore_caq, "user_ids.txt", sep = "\n", row.names = FALSE, col.names = FALSE)


#most_polarized_fam <- merged_df %>% filter(cumsum_fam<0.30)
#users_to_explore_fam <- as.integer64(most_polarized_fam$user_id)
#setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/network_fabricio_alv")
#write.table(users_to_explore_fam, "user_ids.txt", sep = "\n", row.names = FALSE, col.names = FALSE)

#What happened here? To make the plot of polarization index vs engagement, I first tried and tested
#with CAQ and didn't do the same for FAM. The way in which I did it for CAQ wasn't efficient. 
#Since I put ALL the users that liked tweets of CAQ into analysis by adding their users in the 
#user_idx.txt file, that was then used to run the "following" comand on twarc to extract
#the users followed by each of the users in analysis. This was inefficient because we don't need
#to analyze all the users that liked a tweet of CAQ but only those that were more active 
#in liking tweets on CAQ. 
#For this reason, when I replicated the analysis for FAM, instead of putting all the users that liked
#his tweets into the "user_ids.txt" file, I only put the highest 30% of users that liked the most
#FAM's tweets. Why 30%? To have a higher number than needed in case it is useful. However, since FAM has
#such a small number of tweets and CAQ such a high number, we can't use the 30%. 
#Then, this is the reason why I put within the user_idx.txt only the id of users that are within
#the 30% of highest liking rate to FAM and NOT all the users that liked his tweets. 
#This is the right decision in terms of efficient (to cop with Twitter's restrictions) but also
#because it isn't necessary to have this information as we won't be using it. 

# Exploring activity of candidates (run) ----------------------------------------

daily_tweet_activity <- function(data_frame_i){
  # Extract the day-month-year when the tweet was made
  data_frame_i$ymd_activity <- as.Date(substr(data_frame_i$date_creation_v,1,10))
  # Grouping and counting tweets for each day 
  tweets_count_by_dat <- data_frame_i %>% 
                          group_by(ymd_activity) %>% 
                          summarise(total_likes = sum(like_v), n = n())
  # Vector containing all days of the analyzed period
  range_completion <-  seq(as.Date("2017-10-03"),
                           as.Date("2018-04-08"),
                           by = "day")
  # Dataframe of zeros for days within the period of analysis that don't have tweets
  completing_dates <- data.frame(ymd_activity= range_completion[!(range_completion %in% tweets_count_by_dat$ymd_activity)],
                                 total_likes =0, n = 0)
  # Join both dataframes: days with tweets and days without tweets
  tweets_count_by_dat <- rbind(tweets_count_by_dat,completing_dates)
  # Sort dataframe based on date
  tweets_count_by_dat <- tweets_count_by_dat[order(as.Date(tweets_count_by_dat$ymd_activity, 
                                                           format="%Y/%m/%d")),]
  # Create variable of average daily likes
  tweets_count_by_dat$avg_likes_daily <- (tweets_count_by_dat$total_likes)/(ifelse(is.na(tweets_count_by_dat$n), 0, tweets_count_by_dat$n))
  return(tweets_count_by_dat)
}

tweets_count_day_caq <- daily_tweet_activity(carlos_a_f)
tweets_count_day_fam <- daily_tweet_activity(fabri_a_f)

# Create a time series plot

court_ruling <- as.Date("2018-01-09")
first_elections <- as.Date("2018-02-04")
second_elections <- as.Date("2018-04-01")
par(mfrow = c(2,2), col.axis = "white", col.lab = "white", tck = 0)
tweets_activity_plot <- ggplot(tweets_count_day_caq, aes(x = ymd_activity, y = n)) +
  geom_line() +
  geom_vline(xintercept = court_ruling, linetype = "dashed", color = "red") +
  geom_vline(xintercept = first_elections, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = second_elections, linetype = "dashed", color = "cyan") +
  scale_x_date(date_breaks = "7 day") + #date_labels = "%a") +
  labs(title = "Tweet count - CAQ", x = "Date", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

daily_avg_liking_plot <- ggplot(tweets_count_day_caq, aes(x = ymd_activity, y = avg_likes_daily)) +
  geom_line() +
  geom_vline(xintercept = court_ruling, linetype = "dashed", color = "red") +
  geom_vline(xintercept = first_elections, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = second_elections, linetype = "dashed", color = "cyan") +
  scale_x_date(date_breaks = "7 day") + #date_labels = "%a") +
  labs(title = "Daily Avg Likes - CAQ", x = "Date", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(tweets_activity_plot, daily_avg_liking_plot, ncol=2)

# Fabricio Alvarado 

court_ruling <- as.Date("2018-01-09")
first_elections <- as.Date("2018-02-04")
second_elections <- as.Date("2018-04-01")
par(mfrow = c(2,2), col.axis = "white", col.lab = "white", tck = 0)
tweets_activity_plot <- ggplot(tweets_count_day_fam, aes(x = ymd_activity, y = n)) +
  geom_line() +
  geom_vline(xintercept = court_ruling, linetype = "dashed", color = "red") +
  geom_vline(xintercept = first_elections, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = second_elections, linetype = "dashed", color = "cyan") +
  scale_x_date(date_breaks = "7 day") + #date_labels = "%a") +
  labs(title = "Tweet count - FAM", x = "Date", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

daily_avg_liking_plot <- ggplot(tweets_count_day_fam, aes(x = ymd_activity, y = avg_likes_daily)) +
  geom_line() +
  geom_vline(xintercept = court_ruling, linetype = "dashed", color = "red") +
  geom_vline(xintercept = first_elections, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = second_elections, linetype = "dashed", color = "cyan") +
  scale_x_date(date_breaks = "7 day") + #date_labels = "%a") +
  labs(title = "Daily Avg Likes - FAM", x = "Date", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(tweets_activity_plot, daily_avg_liking_plot, ncol=2)
par(mfrow = c(1,1))
dev.off()

# Compute user engagement (run) -------------------------------------------------

#max_num_likes_caq <- max(count_likes_dict_caq$num_likes)
#count_likes_dict_caq$engagement <- count_likes_dict_caq$num_likes/max_num_likes_caq

max_num_likes_caq <- max(merged_df$caq_likes)
max_num_likes_fam <- max(merged_df$fam_likes)
merged_df$engagement_caq <- merged_df$caq_likes/max_num_likes_caq
merged_df$engagement_fam <- merged_df$fam_likes/max_num_likes_fam
  
# Implementing Frequency Distribution (don't run) -------------------------------------
library(tm)
tweets_caq <- Corpus(VectorSource(c(unlist(carlos_a_f$text_v))))
tweets <- tm_map(tweets_caq, content_transformer(tolower))
tweets <- tm_map(tweets, removePunctuation)
tweets <- tm_map(tweets, removeNumbers)
tweets <- tm_map(tweets, stripWhitespace)

stopwords_regex = paste(stopwords('spanish'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
tweets = stringr::str_replace_all(tweets, stopwords_regex, '')

dtm <- DocumentTermMatrix(tweets)
freq <- colSums(as.matrix(dtm))
freq <- sort(freq, decreasing = TRUE)

ert = data.frame(freq)

tweets_fam <- Corpus(VectorSource(c(unlist(fabri_a_f$text_v))))
tweets_fam <- tm_map(tweets_fam, content_transformer(tolower))
tweets_fam <- tm_map(tweets_fam, removePunctuation)
tweets_fam <- tm_map(tweets_fam, removeNumbers)
tweets_fam <- tm_map(tweets_fam, stripWhitespace)

dtm_f <- DocumentTermMatrix(tweets_fam)
freq_f <- colSums(as.matrix(dtm_f))
freq_f <- sort(freq_f, decreasing = TRUE)

ert_f = data.frame(freq_f)


# Filtering only the most polarized users (run) ---------------------------------

# Identifying most porlarized users of CAQ - extracting 5% of users with the most likes given
merged_df <- merged_df %>% arrange(desc(caq_likes))
merged_df$cumsum_caq <- cumsum(merged_df$caq_likes)/sum(merged_df$caq_likes)
most_polarized_caq <- merged_df %>% filter(cumsum_caq<0.10)
# Identifying most polarized users of FAM
merged_df <- merged_df %>% arrange(desc(fam_likes))
merged_df$cumsum_fam <- cumsum(merged_df$fam_likes)/sum(merged_df$fam_likes)
most_polarized_fam <- merged_df %>% filter(cumsum_fam<0.20)


# Creating plot of Engagement vs Polarization - Carlos Alvarado Quesada (run)-----------------------------
# Do a loop to read different users from a list
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/network_carlos_avl")

# Dataframe that collects everything??
total_data_frame <- data.frame()
# Dataframe that collects the analyzed polarized users and finds the percentage of its following network
# that is polarized towards one or the other candidate or that isn't polarized or engaged.
fraction_network <- data.frame()

#for each user in the polarized users that we have decided to analyze
for(user_t in 1:length(most_polarized_caq$user_id)){
  # Identify the source user, who is the one for whom we're gonna analyze its network
  source_user <- most_polarized_caq$user_id[user_t]
  # Extract the following network retrieved for this user
  doc_to_extract <- paste0(source_user,"_user_following.jsonl")
  following_pol_caq <- stream_in(file(doc_to_extract),pagesize = 10000)
  following_pol_caq <- following_pol_caq$data
  
  # Parse the data
  following_parsed <- following_parsing(following_pol_caq)
  # Identify users that 1) are part of the users' network and 2) have liked at least
  # 1 tweet of one of the two candidates.
  following_present_in_merged <- following_parsed[following_parsed$user_id %in% merged_df$user_id,]
  # Paste the information of these users
  following_present_in_merged <- left_join(following_present_in_merged,merged_df, by = "user_id")
  
  # Creating a new column with the source user, IN CASE IT IS USEFUL
  following_present_in_merged$source_user <- source_user
  # Updating a dataframe with ALL the network of ALL the analyzed users, IN CASE IT IS USEFUL
  total_data_frame <- rbind(total_data_frame,following_present_in_merged)
  
  # Classifying the following network of the user being analyzed
  classified_following_network <- classifying_fraction_network(following_present_in_merged)
  # Pasting the source user and its engagement
  classified_following_network$source_user_id <- source_user
  #classified_following_network$source_user_engagement <- 
  # Building the final dataframe
  fraction_network <- rbind(fraction_network,classified_following_network)
}

eng_pol_caq <- left_join(fraction_network,merged_df,by = c("source_user_id" = "user_id"))

# Testing that we actually took all the users in the folder
dim(total_data_frame)


library(ggplot2)

# Create example data
set.seed(123) # For reproducibility
n_users <- dim(eng_pol_caq)[1]
engagement_index <- runif(n_users, 0, 1)
not_engaged <- runif(n_users, 0, 0.5)
not_polarized <- runif(n_users, 0, 0.3)
fam_polarized <- runif(n_users, 0, 0.1)
caq_polarized <- 1 - not_engaged - not_polarized - fam_polarized

df <- data.frame(engagement_index = eng_pol_caq$engagement_caq,
                 not_engaged = eng_pol_caq$not_engaged,
                 not_polarized = eng_pol_caq$not_polarized,
                 fam_polarized = eng_pol_caq$fam_polarized,
                 caq_polarized = eng_pol_caq$caq_polarized)

# Reshape data to long format
df_long <- tidyr::gather(df, key = "network_category", value = "percentage", 
                         not_engaged:caq_polarized)

# Define custom order of network categories
custom_order <- c("not_engaged", "not_polarized", "fam_polarized", "caq_polarized")

# Convert network_category to a factor with custom order
df_long$network_category <- factor(df_long$network_category, levels = custom_order)


ggplot(df_long, aes(x = engagement_index, y = percentage, fill = network_category)) +
  geom_area() +
  scale_fill_manual(values = c("#999999", "#AAAAAA", "red", "#336699")) +  # Update colors to coordinated shades of gray and blue
  labs(x = "Engagement Index", y = "Percentage of Network", fill = "") +  # Remove legend title
  ggtitle("CAQ - Engagement vs Polarization")  # Add plot title

# Creating plot of Engagement vs Polarization - Fabricio Alvarado Monge (run)-----------------------------
# Do a loop to read different users from a list
setwd("/Users/brauliovillalobos/Documents/Data_Science_Master_Degree_Sapienza_2021_2023/III_Semester/ModelingComplexSystems/FinalProject/scraped_tweets/network_fabricio_alv")

# Dataframe that collects everything??
total_data_frame_fam <- data.frame()
# Dataframe that collects the analyzed polarized users and finds the percentage of its following network
# that is polarized towards one or the other candidate or that isn't polarized or engaged.
fraction_network_fam <- data.frame()

#for each user in the polarized users that we have decided to analyze
for(user_t in 1:length(most_polarized_fam$user_id)){
  # Identify the source user, who is the one for whom we're gonna analyze its network
  source_user <- most_polarized_fam$user_id[user_t]

  # Extract the following network retrieved for this user
  doc_to_extract <- paste0(source_user,"_user_following.jsonl")
  following_pol_fam <- stream_in(file(doc_to_extract),pagesize = 10000)
  following_pol_fam <- following_pol_fam$data
  
  # Parse the data
  following_parsed_fam <- following_parsing_fam(following_pol_fam)
  # Identify users that 1) are part of the users' network and 2) have liked at least
  # 1 tweet of one of the two candidates.
  following_present_in_merged_fam <- following_parsed_fam[following_parsed_fam$user_id %in% merged_df$user_id,]
  
  if(nrow(following_present_in_merged_fam) == 0){
    next
  }
  # Paste the information of these users
  following_present_in_merged_fam <- left_join(following_present_in_merged_fam,merged_df, by = "user_id")
  
  # Creating a new column with the source user, IN CASE IT IS USEFUL
  following_present_in_merged_fam$source_user <- source_user
  # Updating a dataframe with ALL the network of ALL the analyzed users, IN CASE IT IS USEFUL
  total_data_frame_fam <- rbind(total_data_frame_fam,following_present_in_merged_fam)
  
  # Classifying the following network of the user being analyzed
  classified_following_network_fam <- classifying_fraction_network(following_present_in_merged_fam)
  # Pasting the source user and its engagement
  classified_following_network_fam$source_user_id <- source_user
  # Building the final dataframe
  fraction_network_fam <- rbind(fraction_network_fam,classified_following_network_fam)
}

eng_pol_fam <- left_join(fraction_network_fam,merged_df,by = c("source_user_id" = "user_id"))

library(ggplot2)

# Create example data
set.seed(123) # For reproducibility
n_users <- dim(eng_pol_fam)[1]
engagement_index <- runif(n_users, 0, 1)
not_engaged <- runif(n_users, 0, 0.5)
not_polarized <- runif(n_users, 0, 0.3)
fam_polarized <- runif(n_users, 0, 0.1)
caq_polarized <- 1 - not_engaged - not_polarized - fam_polarized

df <- data.frame(engagement_index = eng_pol_fam$engagement_fam,
                 not_engaged = eng_pol_fam$not_engaged,
                 not_polarized = eng_pol_fam$not_polarized,
                 fam_polarized = eng_pol_fam$fam_polarized,
                 caq_polarized = eng_pol_fam$caq_polarized)

# Reshape data to long format
df_long <- tidyr::gather(df, key = "network_category", value = "percentage", 
                         not_engaged:caq_polarized)

# Define custom order of network categories
custom_order <- c("not_engaged", "not_polarized","caq_polarized", "fam_polarized")

# Convert network_category to a factor with custom order
df_long$network_category <- factor(df_long$network_category, levels = custom_order)


ggplot(df_long, aes(x = engagement_index, y = percentage, fill = network_category)) +
  geom_area() +
  scale_fill_manual(values = c("#999999", "#AAAAAA", "#336699","red")) +  # Update colors to coordinated shades of gray and blue
  labs(x = "Engagement Index", y = "Percentage of Network", fill = "") +  # Remove legend title
  ggtitle("FAM - Engagement vs Polarization")  # Add plot title
  
