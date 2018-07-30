#The Discussion Code - Local
  #Social Listening Code
  library(dplyr)
  library(RCurl)
  library(httr)
  library(twitteR)
  library(stringr)
  library(sparklyr) 
  library(devtools)
  library(aws.s3)
  library(openxlsx)
  setwd("/Users/johntaylor/Box Sync/Analytics/The Conversation/Data")

  #Hard Coded Variables
  tweet_amount <- 200
  
  ###Trump Friends - Trump Friends
  trump_friends_user_details <- getUser("realdonaldtrump")
  trump_friends_user_details <- trump_friends_user_details$getFriends()
  trump_friends_user_details <- twListToDF(trump_friends_user_details)
  trump_friends_user_details$group <- 'Trump Friends'
  
  ###Trump
  trump <- c("realdonaldtrump")
  trump_name <- c('Trump')
  trump_user_details <- lookupUsers(trump[1])
  trump_user_details <- twListToDF(trump_user_details)
  trump_user_details$group <- 'Trump'

  ###Journalists
  journalists <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 3)
  journalists_user_details <- lookupUsers(journalists$handle)
  journalists_user_details <- twListToDF(journalists_user_details)
  journalists_user_details$group <- journalists[1,2]

  ###healthcare
  healthcare <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 4)
  healthcare_user_details <- lookupUsers(healthcare$handle)
  healthcare_user_details <- twListToDF(healthcare_user_details)
  healthcare_user_details$group <- healthcare[1,2]

  ###Opioids
  opioids <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 5)
  opioids_user_details <- lookupUsers(opioids$handle)
  opioids_user_details <- twListToDF(opioids_user_details)
  opioids_user_details$group <- opioids[1,2]
  
  ###Presidential Candidates 
  candidates <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 6)
  candidates_user_details <- lookupUsers(candidates$handle)
  candidates_user_details <- twListToDF(candidates_user_details)
  candidates_user_details$group <- candidates[1,2]
  
  #Congress
  congress <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 7)
  congress_user_details <- lookupUsers(congress$handle)
  congress_user_details <- twListToDF(congress_user_details)
  congress_user_details <- merge(congress_user_details, congress, by.x="screenName", by.y = "handle")
  
  #Attorneys General
  attorneys_general <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 8)
  attorneys_general_user_details <- lookupUsers(attorneys_general$handle)
  attorneys_general_user_details <- twListToDF(attorneys_general_user_details)
  attorneys_general_user_details <- merge(attorneys_general_user_details, attorneys_general, by.x="screenName", by.y = "handle")
  
  #Lt. Governors
  lt_governors <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 9)
  lt_governors_user_details <- lookupUsers(lt_governors$handle)
  lt_governors_user_details <- twListToDF(lt_governors_user_details)
  lt_governors_user_details <- merge(lt_governors_user_details, lt_governors, by.x="screenName", by.y = "handle")
  
  #St. Health Commisioners
  state_health_commisioners <- read.xlsx("/Users/johntaylor/Box Sync/John Taylor/Social Listening/Social Listening Lists.xlsx", sheet = 10)
  state_health_commisioners_user_details <- lookupUsers(state_health_commisioners$handle)
  state_health_commisioners_user_details <- twListToDF(state_health_commisioners_user_details)
  state_health_commisioners_user_details <- merge(state_health_commisioners_user_details, state_health_commisioners, by.x="screenName", by.y = "handle")
  
  #Full Bind  
  full_twitter_user_details <- rbind(congress_user_details, trump_user_details, trump_friends_user_details, candidates_user_details, journalists_user_details, healthcare_user_details, opioids_user_details)#, attorneys_general_user_details, lt_governors_user_details, state_health_commisioners_user_details)
  full_twitter_user_details <- full_twitter_user_details[!grepl("TRUE", full_twitter_user_details$protected),]
  write.csv(full_twitter_user_details, "full_twitter_user_details.csv")
  dc_twitterverse <- twListToDF(userTimeline(full_twitter_user_details$screenName[1], n=1))
  dc_twitterverse$group <- ""
  dc_twitterverse <- dc_twitterverse[0,]
  for (j in 1:length(full_twitter_user_details$screenName)) {
    tryCatch({
      dc_twitterverse_loop <- twListToDF(userTimeline(full_twitter_user_details$screenName[j], n=tweet_amount, includeRts = T, excludeReplies = F))
      dc_twitterverse_loop$group <- full_twitter_user_details$group[j]
      dc_twitterverse <- rbind(dc_twitterverse, dc_twitterverse_loop)}, error=function(e){})
  }
  dc_twitterverse$date <- substr(dc_twitterverse$created, 1, 10)
  dc_twitterverse$date <- as.Date(dc_twitterverse$date)
  
  
  #Only bring in the most recent day
  days_back <- 8
  dc_twitterverse <- dc_twitterverse %>%
    filter(date > Sys.Date()-days_back & date < Sys.Date())
  
  #solid day
  #dc_twitterverse <- dc_twitterverse %>%
  #  filter(date > as.Date("02/13/18", "%m/%d/%y") & date < as.Date("02/22/18", "%m/%d/%y"))
  
  #DC Twitterverse Cleaned
  dc_twitterverse_cleaned <- dc_twitterverse #Because we want to keep dc_twitterverse as is so tweets can be referenced in their entirity in the dashboard, we need a seperate dataset for those tweet that have been cleaned which can then be eventually parsed.
  for ( i in 1:length(dc_twitterverse_cleaned$text)) { #Going through each tweet where each tweet's text which is defined by [i] and performing the following functions:
    dc_twitterverse_cleaned$text[i] <- gsub('&', '',  dc_twitterverse_cleaned$text[i]) # Remove the ampersand and replace it with a blank for each tweet [i]
    dc_twitterverse_cleaned$text[i] <- gsub('http\\S+\\s*',"", dc_twitterverse_cleaned$text[i]) #Remove anything that looks like a url and anything connected to it for each tweet [i]
    dc_twitterverse_cleaned$text[i] <- str_replace( dc_twitterverse_cleaned$text[i],'RT|rt @[a-z,A-Z]*: ','') #Remove any text that looks like it is related to a Retweet (RT) for each tweet [i]
    dc_twitterverse_cleaned$text[i] <- gsub(("'[a-z]+"), "", dc_twitterverse_cleaned$text[i]) #Using Regex to remove any apostrophe as well as any subsequent text up until a space for each tweet [i]
    dc_twitterverse_cleaned$text[i] <- stringr::str_replace_all(dc_twitterverse_cleaned$text[i],'[^#^@^a-zA-Z\\s]', ' ') #Remove any punctuation for each tweet [i] with the exception of # and @ (for twitter)
    dc_twitterverse_cleaned$text[i] <- gsub('[[:digit:]]', '',  dc_twitterverse_cleaned$text[i]) #Removing any numeric characters for each tweet [i]
    dc_twitterverse_cleaned$text[i] <- sub('(\\w+)\\s+([A-Z][[:punct:]]\\s*){1,}', '\\1 ', dc_twitterverse_cleaned$text[i]) #Removing any single letter words for each tweet [i].  This is primarily because some of the previous cleaning leaves trailing or leading letters like 'i' or 's'
    dc_twitterverse_cleaned$text[i] <- stringr::str_replace_all(dc_twitterverse_cleaned$text[i], "[\\s]+", " ") #Removing any additional white space such that there is a maximum one space between each word for each tweet [i]
    dc_twitterverse_cleaned$text[i] <- tolower( dc_twitterverse_cleaned$text[i]) #Lastly - making every character lower case for each tweet [i]
  }
  dc_twitterverse_cleaned$text[dc_twitterverse_cleaned$text == ""] <- NA
  dc_twitterverse_cleaned <- dc_twitterverse_cleaned[!is.na(dc_twitterverse_cleaned$text),]

  #Creating the words dataset
  words_final <- data.frame(words = character(0), tweet_id = character(0))
  for (i in 1:length(dc_twitterverse_cleaned$text)) {
    words <- strsplit(dc_twitterverse_cleaned$text[i]," ")
    tweet_id <- dc_twitterverse_cleaned$id[i]
    words <- unlist(words)
    words <- as.data.frame(words)
    words$tweet_id <- tweet_id
    words_final <- rbind(words_final, words)
  }
  words_final$words[words_final$words==""] <- NA
  words_final <- na.omit(words_final)

  #Remove stop words: There are a few methods please see which one works the best
  stop_words <- c("a", "about", "above","the", "above","do", "across", "after","amp", "afterwards", "again", "against", "all", "almost", "alone", "along", "already", "also","although","always","am","among", "amongst", "amoungst", "amount",  "an", "and", "another", "any","anyhow","anyone","anything","anyway", "anywhere", "are", "around", "as",  "at", "back","be","became", "because","become","becomes", "becoming", "been", "before", "beforehand", "behind", "being", "below", "beside", "besides", "between", "beyond", "bill", "both", "bottom","but", "by", "call", "can", "cannot", "cant", "co", "con", "could", "couldnt", "cry", "de", "describe", "detail", "do", "done", "down", "due", "during", "each", "eg", "eight", "either", "eleven","else", "elsewhere", "empty", "enough", "etc", "even", "ever", "every", "everyone", "everything", "everywhere", "except", "few", "fifteen", "fify", "fill", "find", "fire", "first", "five", "for", "former", "formerly", "forty", "found", "four", "from", "front", "full", "further", "get", "give", "go", "had", "has", "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby", "herein", "hereupon", "hers", "herself", "him", "himself", "his", "how", "however", "hundred", "ie", "if", "in", "inc", "indeed", "interest", "into", "is", "it", "its", "itself", "keep", "last", "latter", "latterly", "least", "less", "ltd", "made", "many", "may", "me", "meanwhile", "might", "mill", "mine", "more", "moreover", "most", "mostly", "move", "much", "must", "my", "myself", "name", "namely", "neither", "never", "nevertheless", "next", "nine", "no", "nobody", "none", "noone", "nor", "not", "nothing", "now", "nowhere", "of", "off", "often", "on", "once", "one", "only", "onto", "or", "other", "others", "otherwise", "our", "ours", "ourselves", "out", "over", "own","part", "per", "perhaps", "please", "put", "rather", "re", "same", "see", "seem", "seemed", "seeming", "seems", "serious", "several", "she", "should", "show", "side", "since", "sincere", "six", "sixty", "so", "some", "somehow", "someone", "something", "sometime", "sometimes", "somewhere", "still", "such", "system", "take", "ten", "than", "that", "the", "their", "them", "themselves", "then", "thence", "there", "thereafter", "thereby", "therefore", "therein", "thereupon", "these", "they", "thickv", "thin", "third", "this", "those", "though", "three", "through", "throughout", "thru", "thus", "to", "together", "too", "top", "toward", "towards", "twelve", "twenty", "two", "un", "under", "until", "up", "upon", "us", "very", "via", "was", "we", "well", "were", "what", "whatever", "when", "whence", "whenever", "where", "whereafter", "whereas", "whereby", "wherein", "whereupon", "wherever", "whether", "which", "while", "whither", "who", "whoever", "whole", "whom", "whose", "why", "will", "with", "within", "without", "would", "yet", "you", "your", "yours", "yourself", "yourselves", "the")
  '%!in%' <- function(x,y)!('%in%'(x,y))
  words_final <- words_final %>%
    filter(words %!in% stop_words)
  words_final$words <- as.character(words_final$words)
  words_final <- words_final[nchar(words_final$words) > 1,]
  
  #Full Loop for extracting replies
  #for (i in 1:length(dc_twitterverse$id)) {
  #  if(!str_detect(dc_twitterverse$group, "Congress")) {next}
  #  url <- paste("http://twitter.com", dc_twitterverse$screenName[i], "status", dc_twitterverse$id[i], sep="/")
  #  webpage <- read_html(url)
  #  replies <- webpage %>%
  #    html_nodes("span.ProfileTweet-actionCount") %>%
  #    html_text()
  #  replies <- replies[1]
  #  replies <- as.numeric(gsub("[^0-9]", "", replies))
  #  dc_twitterverse$replies[i] <- replies
  #}

  #Range standardizing RTs Fav and Replies
  #range <- function(x){(x-min(x))/(max(x)-min(x))}
  #dc_twitterverse$st_retweets <- range(dc_twitterverse$retweetCount)
  #dc_twitterverse$st_favorites <- range(dc_twitterverse$favoriteCount)
  #dc_twitterverse$st_replies <- range(dc_twitterverse$retweetCount)
  
  #Extracting the original author of retweets
  dc_twitterverse$original_author <- ifelse(dc_twitterverse$isRetweet == T, 
                                            str_extract(dc_twitterverse$text, "@\\w+"),
                                            NA)
  
  #Changing to eastern time
  dc_twitterverse$created <- dc_twitterverse$created - (60*60 *4)
  
  #Writing out the datasets
  write.csv(dc_twitterverse, 'dc_twitterverse.csv')
  write.csv(words_final, 'words_final.csv')
  write.csv(full_twitter_user_details, 'full_twitter_user_details.csv')

  

   