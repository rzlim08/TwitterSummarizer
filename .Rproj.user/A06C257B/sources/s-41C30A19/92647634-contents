parse_tweets<-function(tweet) {
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
  for(i in tweet){
    source<-i$statusSource
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
    source_count[[source_name]]
    if(i$isRetweet){
      num_retweets = num_retweets+1
    }
    else{
      hashtags = str_extract_all(i$text, "#\\S+")[[1]]
      mentions = str_extract_all(i$text, "@\\S+")[[1]]
      if(identical(i$getReplyToSN(), character(0))){
        num_tweets<-num_tweets+1
        total_favourites<-total_favourites+i$favoriteCount
        total_retweets<-total_retweets+i$retweetCount
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
      if(length(i$urls)>0){
        for(j in i$urls['expanded_url'][[1]]){
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
  top_hashtags = ''
  if(length(hashtag_count)!= 0){
    top_hashtags = paste(
      names(hashtag_count[order(unlist(hashtag_count), decreasing=TRUE)][1:5]), collapse = ", "
    )
  }
  top_mentions = ''
  if(length(mentions_count)!=0){
    top_mentions = paste(
      names(mentions_count[order(unlist(mentions_count), decreasing=TRUE)][1:5]), collapse = ", "
    )
  }
  top_urls = ''
  if(length(url_count)!=0){
    top_urls = paste(
      names(url_count[order(unlist(url_count), decreasing=TRUE)][1:5]), collapse = ", "
    )
  }
  tweet_info[['sources']]<<-source_count
  tweet_info[['top_mentions']]<<-top_mentions
  tweet_info[['top_hashtags']]<<-top_hashtags
  tweet_info[['number_retweets']]<<-num_retweets
  tweet_info[['top_urls']]<<-top_urls
  tweet_info[['number_replies']]<<-num_replies
  tweet_info[['number_tweets']]<<-num_tweets
  tweet_info[['total_favourites']]<<-total_favourites
  tweet_info[['total_retweets']]<<-total_retweets
  return(tweet_info) 
}