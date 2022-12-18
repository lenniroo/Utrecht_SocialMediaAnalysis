if (!("tuber" %in% installed.packages())) {
  install.packages("tuber", dependencies = T)}
if (!("youtubecaption" %in% installed.packages())) {
  install.packages("youtubecaption", dependencies = T)}
if (!("tidyverse" %in% installed.packages())) {
  install.packages("tidyverse", dependencies = T)}
if (!("dplyr" %in% installed.packages())) {
  install.packages("dplyr", dependencies = T)}
if (!("stringi" %in% installed.packages())) {
  install.packages("stringi", dependencies = T)}
if (!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2", dependencies = T)}
if (!("tm" %in% installed.packages())) {
  install.packages("tm", dependencies = T)}
if (!("RTextTools" %in% installed.packages())) {
  install.packages("RTextTools", dependencies = T)}
library(tuber)
library(youtubecaption)
library(dplyr)
library(tidyverse)
library(stringi)
library(ggplot2)
library(tm)
library(RTextTools)

#setwd("C:/Users/Lennart/Documents/Studium/applications_PhD/Utrecht/SocialMedia/presentation")

#set up Googles' YouTube API
app_id <- 'Your-YT_API-Client-ID' 
app_key <- "Your-YT_API-Client-Key"
yt_oauth(app_id, app_key)


#################################### Data Collection ###################################################################


#Influencers' YT channel IDs, Usernames:
#PewDiePie, MrBeast, LikeNastya, 
#DudePerfect, KidsDianaShow, Markiplier, 
#RyansWorld, SSSniperWolf, RomanAtwookVlogs,
#Smosh, BrailleSkateboarding, JonnyGiger,
#SamPilgrim, MattJones, MagnusMidtbo,
#AMereCreator, HarryMain, RyanTaylor,
#CloeTing, GrowingAnnanas
influencer <- c("UC-lHJZR3Gqxm24_Vd_AJ5Yw", "UCX6OQ3DkcsbYNE6H8uQQuVA", "UCJplp5SjeGSdVdwsfb9Q7lQ",
                "UCRijo3ddMTht_IHyNSNXpNQ", "UCk8GzjMOrta8yxDcKfylJYw", "UC7_YxT-KID8kRbqZo7MyscQ",
                "UChGJGhZ9SOOHvBB0Y4DOO_w", "UCpB959t8iPrxQWj7G6n0ctQ", "UC-SV8-bUJfXjrRMnp7F8Wzw",
                "UCY30JRSgfhYXA6i6xX1erWg", "UC9PgszLOAWhQC6orYejcJlw", "UCVooVnzQxPSTXTMzSi1s6uw",
                "UC-WMwOzgFdvvGVLB1EZ-n-w", "UCXEuQ_O6BUWwhVhcMyXNHgg", "UC_gSotrFVZ_PiAxo3fTQVuQ",
                "UCnFxFSv7_31uPDqaxtghyaQ", "UCoZ4Dwbzbzp7e5ERGtmaWbw", "UClTG5vjtyA-FwiYjkr8bzPA",
                "UCCgLoMYIyP0U56dEhEL1wXQ", "UCsLF0qPTpkYKq81HsjgzhwQ")

#scraping channel information of the 20 influencers
#usage of a for-loop to automatise the scraping process over all 20 influencers
#otherwise the first two commands within the loop has be run 20 times
#scraping command (get_channel_stats) is used from the 'tuber' package
l_channel <- list()

for (i in influencer) {
  data <- get_channel_stats(channel_id = i)
  info <- data.frame(title = data$snippet$title, #channel name
                     description = data$snippet$description, #channel information/description
                     customUrl = data$snippet$customUrl, #username '@userXXX'
                     viewCount = as.numeric(data$statistics$viewCount), #number of times the channel was viewed
                     subscriberCount = as.numeric(data$statistics$subscriberCount), #number of subscriber
                     videoCount = as.numeric(data$statistics$videoCount), #number of published videos
                     channel_id = i) #channel ID (see vector 'influencer' for actual ID)
  l_channel[[i]] <- info
  #binding the dataset of every channel information in one data frame
  channel_info <- bind_rows(l_channel)
}

#scraping video information of every video the channels have published so far (retrived 13th December 2022)
#usage of a for-loop for same reasons as above
#scraping command (list_channel_videos) is also used from the 'tuber' package
#video IDs are relevant to identify the exact video
#the video IDs are allocated to the publishing channel (name & channel ID) within the outcome
l_vid <- list()
row <- 1
 
for (i in channel_info[,7]) {
   cat(row, "\n")
   vid_data <- list_channel_videos(channel_id = i, max_results = channel_info[row,6])
   vid_info <- data.frame(vid_id = vid_data$contentDetails.videoId,
                          published_at = vid_data$contentDetails.videoPublishedAt,
                          channel = channel_info[row,1],
                          channel_id = channel_info[row,7])
   
   l_vid[[i]] <- vid_info
   video_info <- bind_rows(l_vid)
   
   row <- row + 1
}

#write.csv(video_info, "./video_info.csv", row.names = F)

#Due to limited time resources only the newest 50 videos of each channel get analyzed
#transcribing (scraping subtitles) each of the 31251 videos would take more than 24 hours with my PC set up
top50_videos <- video_info %>%
  group_by(channel_id) %>%
  top_n(n = 50, wt = vid_published_at)

#scraping video description
l_ds <- list()
row = 1

for (i in top50_videos$vid_id) {
  cat(row, "\n")
  ds_data <- get_video_details(video_id = i)
  ds_df <- data.frame(vid_id = top50_videos[row,1],
                      description = ds_data[["items"]][[1]][["snippet"]][["description"]],
                      published_at = top50_videos[row,2],
                      channel = top50_videos[row,3],
                      channel_id = top50_videos[row,4])
  l_ds[[i]] <- ds_df
  top_videos <- bind_rows(l_ds)
  
  row = row + 1
}

#Data cleansing, since some unicodes (emoji, special characters like ø) are not properly encoded
#letters, numbers, spaces, tabs, punctation, and URL pattern charcters like '//', '=', '&' remain
#also '%' remains, since it is strong related with coupon codes
top_videos$description <- gsub("[^[:alnum:][:blank:][:punct:][:graph:]?=&%/\\-]", "", top_videos$description)
#write.csv(top_videos, "./top_videos.csv", row.names = F)

#transcribe each video via its subtitles
#scraping command (get_caption) is used from the 'youtubecaption' package and scrapes the subtitles
#for-loop is used for the some reasons as above
#since not all videos have enabled subtitles, the 'get_caption' command would print and error message 
#and the for-loop would stop
#therefore, the 'tryCatch' function is used to catch such errors and the loop doesn't break
l_sub <- list()
row <- 1

for (i in top_videos$vid_id) {
  tryCatch({
    cat(row, "\n")
    
    url <- paste0("https://www.youtube.com/watch?v=",i)
    data <- get_caption(url)
    sub <- data.frame(text = data$text,
                      vid_id = data$vid,
                      description = top_videos[row,2],
                      duration = data$duration,
                      published_at = top_videos[row,3],
                      channel = top_videos[row,4],
                      channel_id = top_videos[row,5])
    
    l_sub[[i]] <- sub
    subtitles <- bind_rows(l_sub)
  }, 
  error = function(e) {
    cat("Error: ", conditionMessage(e), "\n")
  }
  )
  row <- row + 1
}

#write.csv(subtitles, "./subtitles.csv", row.names = F)


############################ Plotting some channel information for the presentation ####################################

#jpeg(file = "Channel_Dist.jpeg")
channel_info %>%
  arrange(desc(as.numeric(subscriberCount))) %>% # Number of subscriber is arranged descending, so plot displays subscriber count descending as well
  ggplot(aes(x = factor(title, level = c(as.character(title))),
                                y = subscriberCount)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) + #y-axis labels get displayed in numbers seperated by comma instead of 'e+/-123)
  labs(x = "YouTube Channel", y = "Amount of Subscribers", 
       title = "YouTube Channel Distribution \n Based On Subscriber") + #Titles of Axis and Plot
  theme_classic() + #application of a clean and plain theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), #x-axis labels get written vertical instead of horizontal due to better readibility
        plot.title = element_text(hjust = 0.5)) #Plot tile get centered
#dev.off()

#jpeg(file = "vidCount_Dist.jpeg")
channel_info %>%
  arrange(desc(as.numeric(videoCount))) %>%
  ggplot(aes(x = factor(title, level = c(as.character(title))),
             y = videoCount)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "YouTube Channel", y = "Amount of Videos per Channel",
       title = "YouTube Channel Distribution \n Based on Videos") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))
#dev.off()

#jpeg(file = "viewCount_Dist.jpeg")
channel_info %>%
  arrange(desc(as.numeric(viewCount))) %>%
  ggplot(aes(x = factor(title, level = c(as.character(title))),
             y = viewCount)) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "YouTube Channel", y = "Number of Times a Channel was Viewed",
       title = "YouTube Channel Distribution \n Based on Times a Channel was Viewed") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.title = element_text(hjust = 0.5))
#dev.off()

######################################## Data Cleansing/Preperation ###############################################

#The data set for every scraped video subtitles contains of one row for each sentence
#Therefore, the data set consist of roughly 2 Millionr rows
#Thus, subtitle sentences of each video get combined and stored in one row
comb_sub <- subtitles %>%
  select(text, vid_id, description, published_at, channel, channel_id) %>%
  group_by(vid_id) %>%
  mutate(subtitles = paste(text, collapse = " ")) %>%
  select(-text) %>%
  distinct(vid_id, .keep_all = T)

#Data cleansing, letters, numbers, and punctation remain
comb_sub$subtitles = str_replace_all(comb_sub$subtitles, "[^[:graph:]]", "")
comb_sub$description = str_replace_all(comb_sub$description, "[^[:graph:]]", "")
#Every letter character in subtitles and description get lowercased
comb_sub$subtitles <- tolower(comb_sub$subtitles)
comb_sub$description <- tolower(comb_sub$description)
top_videos$description <- tolower(top_videos$description)

#write.csv(comb_sub, "./YT_Data.csv", row.names = F)

#filtering the signed advertisments from the video description
#Swart et al. (2020) show that these 6 words are a clear indicator for disclosed advertisement
ads <- top_videos %>%
  filter(grepl('(^|\\s)ad($|\\s)|advertisment|sponsorship
               |sponsored|sponsoring|affiliate', 
               description))

colnames(ads)[3] <- "published_at" #change colname from 'vid_published_at' to 'published_at' to get cohesive column names through the datasets

#Dataset to manual filter ads/non ads to create training dataset
lookUp <- comb_sub %>%
  anti_join(ads, by = "vid_id")

ads1 <- lookUp[c("225","226","228","455","457","459","461","462","463","467","492","495",
                 "496","498","499","538","540","541","543","545","587","589","600","608",
                 "609","610","616","635","641","643","646","689","729","778","779","780"),]

no_ads <- lookUp[c("20","21","24","51","52","53","101","108","109","125","126","127","176",
                    "177","178","223","224","227","273","274","275","323","324","325","367",
                    "368","369","418","419","420","460","469","470","491","493","500","539",
                    "544","546","603","636","639","640","642","644","647","648","652","654",
                    "690","691","692","730","731","733"),]

no_ads$label = 0
no_ads = subset(no_ads, select = -c(6))

#Detect particular URL links with specific patterns which are related to social media marketing within the video descriptions
#see Swart et al. 2020 (or https://github.com/mike-swart/AdIntuition/blob/master/utm_link_patterns.txt) for the specific patterns
utm_pattern <- c("utm_medium=","utm_source=","utm_content")
utm_pattern <- str_c(utm_pattern, collapse = "|")

ads2 <- top_videos %>%
  filter(grepl(utm_pattern, description))
colnames(ads2)[3] <- "published_at"

#merge manual and automatized advertisement detection
ads <- bind_rows(ads, ads1, ads2)
ads <- ads %>%
  distinct(vid_id, .keep_all = T) #if video duplicates exist in this dataset, it keeps just one of them and all variables

colnames(ads)[6] <- "label"
ads$label = 1
ads <- subset(ads, select = -c(7))

#filter fpr just the hidden advertisments
ads_hidden <- ads %>%
  filter(!str_detect(description, "(^|\\s)ad($|\\s)|advertisment|sponsorship|sponsored|sponsoring"))

#training dataset
#label 0: no ad, 1: ad
training_hidden <- bind_rows(ads_hidden, no_ads) #consits of hidden advertisementand no advertisement
training <- bind_rows(ads, no_ads) #consits of disclosed, ubdisclosed, and no advertisements

#test data
test <- comb_sub %>%
  anti_join(training, by = "vid_id") #removes the videos of the training dataset

########################################## Analysis ##############################################################

####### Training sample
samp_train <- sample(1:nrow(training_hidden),
                     round(nrow(training_hidden)*.70), #70% of the training data is used to train the algorithm
                     replace = F)                      #30% is used for validation
tr <- training_hidden[samp_train, ]
te <- training_hidden[-samp_train, ]

training_hidden <- rbind(tr,te)

####### Train SVM
corpus <- Corpus(VectorSource(training_hidden$description))

#document matrix is needed for to compute the algorithm
#usage of a combinated weight approach for words: 'term frequency' and 'inverse document term frequency'
#so, the frequency of how rarely a word is used get adjusted which states the importance of a word
#commands are used from the 'RTextTools' package
dtm <- DocumentTermMatrix(corpus, 
                          control = list(weighting = 
                                           function(x)
                                             weightTfIdf(x, normalize = F))) 

train_codes = training_hidden$label

container <- create_container(dtm,
                              t(train_codes),
                              trainSize = 1:nrow(tr),
                              testSize = (nrow(tr)+1):nrow(training_hidden),
                              virgin = F)

#SVM model get computed and trained
models <- train_models(container, algorithms = "SVM")

results <- classify_models(container, models)

out <- data.frame(label_svm = results$SVM_LABEL,
                  prob_svm = results$SVM_PROB,
                  actual_label = training_hidden$label[(nrow(tr)+1):nrow(training_hidden)])

#getting the performance measurements
precision_train_1 <- as.data.frame(create_precisionRecallSummary(container, results))
row.names(precision_train) <- c("0, Bigram: No Ad", "1, Bigram: Ad")

####### N-Grams
#Creation of n-gram matrix, with n-gram = 2 
#Bigram works best for English (see https://thibaut-deveraux.medium.com/how-to-create-a-bag-of-words-embedding-in-r-e609095ebf53)
matrix_bi <- create_matrix(training_hidden$description, language = "English", 
                           minDocFreq = 1, minWordLength = 1, ngramLength = 2, toLower = T)

#SVM computing commands are the same as above
container_bi <- create_container(matrix_bi,
                                 t(train_codes),
                                 trainSize = 1:nrow(tr),
                                 testSize = (nrow(tr)+1):nrow(training_hidden),
                                 virgin = F)

models <- train_models(container_bi, algorithms = "SVM")

results <- classify_models(container_bi, models)

precision_bi <- as.data.frame(create_precisionRecallSummary(container, results))
row.names(precision_bi) <- c("0, Bigram: No Ad", "1, Bigram: Ad")


####### Test data (description)
#SVM is applied to test data, same commands as above
corpus <- Corpus(VectorSource(test$description))
dtm_test <- DocumentTermMatrix(corpus, control = list(weighting = 
                                                       function(x)
                                                         weightTfIdf(x, normalize = F)))

row.names(dtm_test) <- (nrow(dtm)+1):(nrow(dtm)+nrow(dtm_test))

dtm_p <- c(dtm, dtm_test)

train_codes_p <- train_codes

container_p <- create_container(dtm_p,
                              t(train_codes_p),
                              trainSize = 1:nrow(dtm),
                              testSize = (nrow(dtm)+1):(nrow(dtm)+nrow(dtm_test)),
                              virgin = T)

model_p <- train_models(container_p, algorithms = "SVM")

prediction <- classify_models(container_p, model_p)

out_p <- data.frame(model_label = prediction$SVM_LABEL,
                    model_prob = prediction$SVM_PROB,
                    actual = test)

#counting the number of videos with detected advertisement within the description
out_p %>%
  filter(model_label == 1) %>%
  count() #245


####### Test data (subtitles)

#test$subtitles = str_replace_all(test$subtitles, "[^[:graph:]]", " ")

#Applying SVM to the subtitles from the test data
corpus <- Corpus(VectorSource(test$subtitles))
dtm_test <- DocumentTermMatrix(corpus, control = list(weighting = 
                                                        function(x)
                                                          weightTfIdf(x, normalize = F)))

row.names(dtm_test) <- (nrow(dtm)+1):(nrow(dtm)+nrow(dtm_test))

dtm_sub <- c(dtm, dtm_test)

container_sub <- create_container(dtm_sub,
                                t(train_codes),
                                trainSize = 1:nrow(dtm),
                                testSize = (nrow(dtm)+1):(nrow(dtm)+nrow(dtm_test)),
                                virgin = T)

model_sub <- train_models(container_sub, algorithms = "SVM")

prediction_sub <- classify_models(container_sub, model_sub)

out_sub <- data.frame(model_label = prediction_sub$SVM_LABEL,
                    model_prob = prediction_sub$SVM_PROB,
                    actual = test)

#counting detected advertisement within the subtitles
out_sub %>%
  filter(model_label == 1) %>%
  count() #76


#counting number of videos with detected advertisement
#however, disclosed advertisement is removed from the counting
#thus, only undisclosed advertisement remains
out_p %>%
  bind_rows(out_sub) %>%
  filter(model_label == 1) %>%
  distinct(actual.vid_id, .keep_all = T) %>%
  filter(!str_detect(actual.description, "(^|\\s)ad($|\\s)|advertisment|sponsorship|sponsored|sponsoring|affiliate")) %>%
  count() #315

#store the videos with detected hidden advertisement as a dataframe
all <- out_p %>%
  bind_rows(out_sub) %>%
  filter(model_label == 1) %>%
  distinct(actual.vid_id, .keep_all = T) %>%
  filter(!str_detect(actual.description, "(^|\\s)ad($|\\s)|advertisment|sponsorship|sponsored|sponsoring|affiliate"))

#write.csv(comb_sub, "./undisclosed_ads_YT.csv", row.names = F)



