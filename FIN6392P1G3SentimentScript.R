# Required Packages
  require(RedditExtractoR)
  require(SentimentAnalysis)
  require(plyr)
  require(dplyr)
  require(hablar)
  require(tidyr)

# Keywords used to find relevant subreddits
  listOfSubredditKeywords = c("stocks", "investing", "business")


# Subreddits pertaining to the above keywords will be stored in this variable
  listOfSubredditTitles = c()

# For loop that finds subreddits relevant to the keywords in "listOfSubredditKeywords"
  for(keyword in listOfSubredditKeywords) {
    listOfSubredditTitles <<- unique(append(listOfSubredditTitles, find_subreddits(keyword)$subreddit)) # Take the UNIQUE relevant subreddits from the temporary list, append, and update it onto the global listOfSubredditTitles object
  }


### AMZN SENTIMENT ANALYSIS ###
  
  # Search query for every subreddit in "listOfSubredditTitles"
  listOfThreadKeywords_AMZN = c("AMZN", "Amazon")
  
  # Within every relevant subreddit, all threads containing the above keywords will be stored in this variable
  threadData_AMZN = data.frame()
  
  # DO NOT RUN (unless you're looking to clear the variable's content)
  threadData_AMZN = threadData_AMZN[0,]
  
  # Nested For loop that searches for each thread keyword in each subreddit from "listOfSubredditTitles"
  for(title in listOfSubredditTitles) {
    for (threadKeyword in listOfThreadKeywords_AMZN) {
      tempThreadData_AMZN = find_thread_urls(
        keywords = threadKeyword,
        sort_by = "relevance",
        subreddit = title,
        period = "all"
      )
      
      # Condition that skips the following code and moves onto the next iteration if true
      if(is.na(tempThreadData_AMZN)) next
      
      # Trims local thread data to only include threads between given dates and those with comments greater than 0 and less than 25
      tempThreadData_AMZN = subset(
        tempThreadData_AMZN,
        comments > 0 &
          comments < 25 &
          as.Date(date_utc) > "2020-02-25" &
          as.Date(date_utc) < "2022-02-25"
      )
      
      # Appends trimmed local thread data from line 24 to global object in line 5
      threadData_AMZN <<- rbind(
        threadData_AMZN,
        tempThreadData_AMZN
      ) %>%
        arrange(desc(comments)) %>%
        group_by(date_utc) %>%
        slice(1)
    }
  }
  
  # Adds first thread link's comment data so the data frame contains the variables
  threadComments_AMZN = get_thread_content(threadData_AMZN$url[1])$comments
  # Empties data you just put in, but keeps the variables
  threadComments_AMZN = threadComments_AMZN[0,]
  
  # (Optional) Run to see how many comments you're about to parse
  sum(threadData_AMZN$comments)
  
  # Appends comment data to global variable from previous lines
  for(link in threadData_AMZN$url) {
    threadComments_AMZN <<- bind_rows(threadComments_AMZN %>% convert(chr(comment_id)), get_thread_content(link)$comments %>% convert(chr(comment_id)))
  }
  
  
  # Stores all sentiment data for each comment
  AMZNSentiment = analyzeSentiment(threadComments_AMZN$comment)
  # Updates variable to only contain the comments' dates, sentimentGI, and word count
  AMZNSentimentDataFinal = data.frame(threadComments_AMZN$date, AMZNSentiment$SentimentGI, AMZNSentiment$WordCount)
  # Keeps all elements, but sorts it by date (oldest to newest)
  AMZNSentimentDataFinal = AMZNSentimentDataFinal[order(as.Date(AMZNSentimentDataFinal$threadComments_AMZN.date), decreasing = FALSE),]
  
  
  
### TSLA SENTIMENT ANALYSIS ###
  
  # Search query for every subreddit in "listOfSubredditTitles"
  listOfThreadKeywords_TSLA = c("TSLA", "Tesla")
  
  # Within every relevant subreddit, all threads containing the above keywords will be stored in this variable
  threadData_TSLA = data.frame()
  
  # DO NOT RUN (unless you're looking to clear the variable's content)
  threadData_TSLA = threadData_TSLA[0,]
  
  # Nested For loop that searches for each thread keyword in each subreddit from "listOfSubredditTitles"
  for(title in listOfSubredditTitles) {
    for (threadKeyword in listOfThreadKeywords_TSLA) {
      tempThreadData_TSLA = find_thread_urls(
        keywords = threadKeyword,
        sort_by = "relevance",
        subreddit = title,
        period = "all"
      )
      
      # Condition that skips the following code and moves onto the next iteration if true
      if(is.na(tempThreadData_TSLA)) next
      
      # Trims local thread data to only include threads between given dates and those with comments greater than 0 and less than 25
      tempThreadData_TSLA = subset(
        tempThreadData_TSLA,
        comments > 0 &
          comments < 25 &
          as.Date(date_utc) > "2020-02-25" &
          as.Date(date_utc) < "2022-02-25"
      )
      
      # Appends trimmed local thread data from line 24 to global object in line 5
      threadData_TSLA <<- rbind(
        threadData_TSLA,
        tempThreadData_TSLA
      ) %>%
        arrange(desc(comments)) %>%
        group_by(date_utc) %>%
        slice(1)
    }
  }
  
  # Adds first thread link's comment data so the data frame contains the variables
  threadComments_TSLA = get_thread_content(threadData_TSLA$url[1])$comments
  # Empties data you just put in, but keeps the variables
  threadComments_TSLA = threadComments_TSLA[0,]
  
  # (Optional) Run to see how many comments you're about to parse
  sum(threadData_TSLA$comments)
  
  # Appends comment data to global variable from line 38 and 40
  for(link in threadData_TSLA$url) {
    threadComments_TSLA <<- bind_rows(threadComments_TSLA %>% convert(chr(comment_id)), get_thread_content(link)$comments %>% convert(chr(comment_id)))
  }
  
  
  # Stores all sentiment data for each comment
  TSLASentiment = analyzeSentiment(threadComments_TSLA$comment)
  # Updates variable to only contain the comments' dates, sentimentGI, and word count
  TSLASentimentDataFinal = data.frame(threadComments_TSLA$date, TSLASentiment$SentimentGI, TSLASentiment$WordCount)
  # Keeps all elements, but sorts it by date (oldest to newest)
  TSLASentimentDataFinal = TSLASentimentDataFinal[order(as.Date(TSLASentimentDataFinal$threadComments_TSLA.date), decreasing = FALSE),]
  
  
  
### SBUX SENTIMENT ANALYSIS ###
  
  # Search query for every subreddit in "listOfSubredditTitles"
  listOfThreadKeywords_SBUX = c("SBUX", "Starbucks")
  
  # Within every relevant subreddit, all threads containing the above keywords will be stored in this variable
  threadData_SBUX = data.frame()
  
  # DO NOT RUN (unless you're looking to clear the variable's content)
  threadData_SBUX = threadData_SBUX[0,]
  
  # Nested For loop that searches for each thread keyword in each subreddit from "listOfSubredditTitles"
  for(title in listOfSubredditTitles) {
    for (threadKeyword in listOfThreadKeywords_SBUX) {
      tempThreadData_SBUX = find_thread_urls(
        keywords = threadKeyword,
        sort_by = "relevance",
        subreddit = title,
        period = "all"
      )
      
      # Condition that skips the following code and moves onto the next iteration if true
      if(is.na(tempThreadData_SBUX)) next
      
      # Trims local thread data to only include threads between given dates and those with comments greater than 0 and less than 25
      tempThreadData_SBUX = subset(tempThreadData_SBUX, comments > 2 & comments < 25 & as.Date(date_utc) > "2020-02-25" & as.Date(date_utc) < "2022-02-25")
      
      # Appends trimmed local thread data from line 24 to global object in line 5
      threadData_SBUX <<- rbind(
        threadData_SBUX,
        tempThreadData_SBUX
      ) %>%
        arrange(desc(comments)) %>%
        group_by(date_utc) %>%
        slice(1)
    }
  }
  
  # Adds first thread link's comment data so the data frame contains the variables
  threadComments_SBUX = get_thread_content(threadData_SBUX$url[1])$comments
  # Empties data you just put in, but keeps the variables
  threadComments_SBUX = threadComments_SBUX[0,]
  
  # (Optional) Run to see how many comments you're about to parse
  sum(threadData_SBUX$comments)
  
  # Appends comment data to global variable from line 38 and 40
  for(link in threadData_SBUX$url) {
    threadComments_SBUX <<- bind_rows(threadComments_SBUX %>% convert(chr(comment_id)), get_thread_content(link)$comments %>% convert(chr(comment_id)))
  }
  
  
  # Stores all sentiment data for each comment
  SBUXSentimentDataFinal = analyzeSentiment(threadComments_SBUX$comment)
  # Updates variable to only contain the comments' dates, sentimentGI, and word count
  SBUXSentimentDataFinal = data.frame(threadComments_SBUX$date, SBUXSentimentDataFinal$SentimentGI, SBUXSentimentDataFinal$WordCount)
  # Keeps all elements, but sorts it by date (oldest to newest)
  SBUXSentimentDataFinal = SBUXSentimentDataFinal[order(as.Date(SBUXSentimentDataFinal$threadComments_SBUX.date), decreasing = FALSE),]