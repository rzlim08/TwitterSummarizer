parse_tweets<-function(tweets) {
  ## Function to parse tweets into a useful list
  tweet_info <<- list()
  total_favourites<-0
  total_retweets<-0
  num_retweets<- 0
  num_tweets<- 0
  num_replies<- 0 
  hashtag_count = list()
  mentions_count = list()
  source_count = list()
  url_count = list()
  tweet_text_list = list()
  
  for(tweet in tweets){
    source<-tweet$statusSource
    start <-str_locate(source, '>')[1]+1
    end <-tail(unlist(str_locate_all(source, '<')), n=1)-1
    source_name <-substring(source, start, end)
    if(is.null(source_count[[source_name]])){
      source_count[[source_name]]<-1
    }
    else{
      val = source_count[[source_name]]
      source_count[[source_name]]<-val+1
    }
    if(tweet$isRetweet){
      num_retweets = num_retweets+1
    }
    else{
      ## Only do most stats on non-retweets
      hashtags = str_extract_all(tweet$text, "#\\S+")[[1]]
      mentions = str_extract_all(tweet$text, "@\\S+")[[1]]
      
      if(identical(tweet$getReplyToSN(), character(0))){
        num_tweets<-num_tweets+1
        tweet_text_list[[num_tweets]]<- tweet$text
        total_favourites<-total_favourites+tweet$favoriteCount
        total_retweets<-total_retweets+tweet$retweetCount
      }
      else{
        num_replies<-num_replies+1
      }
      if (!identical(hashtags, character(0))){
        for(j in hashtags){
          if(is.null(hashtag_count[[j]])) {
            hashtag_count[[j]]<-1
          }
          else{
            val = hashtag_count[[j]]
            hashtag_count[[j]]<-val+1
          }
        }
      }
      if(!identical(mentions, character(0))) {
        for(j in mentions) {
          if(is.null(mentions_count[[j]])) {
            mentions_count[j]<-1
          }
          else{
            val = mentions_count[[j]]
            mentions_count[[j]]<-val+1
          }
        }
      }
      if(length(tweet$urls)>0){
        for(j in tweet$urls['expanded_url'][[1]]){
          url = sub('https://', '', j)
          url = sub('http://', '', url)
          url = strsplit(url, split = '/')[[1]][1]
          if(is.null(url_count[[url]])){
            url_count[[url]]<-1
          }
          else{
            val = url_count[[url]]
            url_count[[url]]<-val+1
          }
        }
      }
    }
  }
  top_hashtags <- get_top_five(hashtag_count)
  top_mentions <- get_top_five(mentions_count)
  top_urls <- get_top_five(url_count)
  
  tweet_info[['sources']]<<-source_count
  tweet_info[['top_mentions']]<<-top_mentions
  tweet_info[['top_hashtags']]<<-top_hashtags
  tweet_info[['number_retweets']]<<-num_retweets
  tweet_info[['top_urls']]<<-top_urls
  tweet_info[['number_replies']]<<-num_replies
  tweet_info[['number_tweets']]<<-num_tweets
  tweet_info[['total_favourites']]<<-total_favourites
  tweet_info[['total_retweets']]<<-total_retweets
  tweet_info[['last_tweets']]<<-tweet_text_list
}

get_top_five<- function(count_list){
  ## Gets the top 5 accounts associated with a list
  top_list = ''
  if(length(count_list)!=0){
    max_len <- min(length(count_list), 5)
    top_list = paste(
      names(count_list[order(unlist(count_list), decreasing=TRUE)][1:max_len]), collapse = ", "
    )
  }
  return(top_list)
}