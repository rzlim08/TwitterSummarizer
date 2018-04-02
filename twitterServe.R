server <- shinyServer(function(input, output) {
  source('tweetParse.R')
  user <- reactive({
    u <<- getUser(input$screen_name)
    tweets<<- userTimeline(user = input$screen_name, n=200, includeRts = TRUE)
  }) 
  get_tweets<-reactive({
    
  })
  
  ####################################### Panel 1 #####################################
  
  output$value <- renderText({ 
    ## Render the username 
    input$search
    u = isolate(user())
    u$name
  })
  output$profilePic<-renderText({
    input$search
    if(is.null(u$profileImageUrl)){
      return(NULL)
    }
    return({c('<img src="',u$profileImageUrl,'"style = "width:100px;height:100px;">')})
  })
  output$description <- renderText({
    input$search
    paste('Description:', u$description, sep = " ")
  })
  output$numStatusesStyle <- renderText({
    input$search
    paste('<div style = "font-family: Helvetica Neue">',
          '<div style = "font-size:75%;" >TWEETS</div>',
          '<div style = "font-size: 200%;font-weight:bold;text-align:center">', prettyNum(u$statusesCount, big.mark = ','), '</div>',
          '</div>', sep = ' ')
  })
  output$numFavouritesStyle<- renderText({
    input$search
    paste('<div style = "font-family: Helvetica Neue">',
          '<div style = "font-size:75%;">FAVOURITES</div>',
          '<div style = "font-size: 200%;font-weight:bold;text-align:center">', prettyNum(u$favoritesCount,big.mark = ','), '</div>',
          '</div>', sep = ' ')
  })
  
  output$numFollowing <- renderText({
    input$search
    paste('Following:', u$friendsCount, sep = " ")
  })
  output$numFollowingStyle<- renderText({
    input$search
    paste('<div style = "font-family: Helvetica Neue">',
          '<div style = "font-size:75%;">FOLLOWING</div>',
          '<div style = "font-size: 200%;font-weight:bold;text-align:center">', prettyNum(u$friendsCount, big.mark=','), '</div>',
          '</div>', sep = ' ')
  })
  output$numFollowersStyle<-renderText({
    input$search
    paste('<div style = "font-family: Helvetica Neue">',
          '<div style = "font-size:75%;">FOLLOWERS</div>',
          '<div style = "font-size: 200%;font-weight:bold;text-align:center">', prettyNum(u$followersCount,big.mark = ','), '</div>',
          '</div>', sep = ' ')
  })
  output$location <-renderText({
    input$search
    paste('Location:', u$location, sep = ' ')
  })
  ####################################### Panel 2 #####################################
  output$lastTweets <- renderText({
    input$search
    isolate(get_tweets())
    parse_tweets(tweets)
    paste("Last", length(tweets), "tweets", sep = ' ')
  })
  
  output$sourceAndPrevious <- renderText({
    paste("Source of tweets", sep = ' ')
  })
  
  output$favoritesTweet <- renderText({
    input$search
    val = tweet_info[['total_favourites']]/tweet_info[['number_tweets']]
    paste('<div style = "font-family:Helvetica Neue;color:white;padding:10px;">',
          '<div style = "font-size:75%"> FAVOURITES/TWEET</div>',
          '<div style = "font-size: 200%;font-weight:bold;">', round(val,digits=1), '</div>',
          '</div>'
    )
  })
  output$retweetsTweet<- renderText({
    input$search
    val = tweet_info[['total_retweets']]/tweet_info[['number_tweets']]
    paste('<div style = "font-family:Helvetica Neue;color:white;padding:10px;">',
          '<div style = "font-size:75%"> RETWEETS/TWEET</div>',
          '<div style = "font-size: 200%;font-weight:bold;">', round(val,digits=1), '</div>',
          '</div>'
    )
  })
  
  output$topHashtags<- renderText({
    input$search
    paste('<div style = "font-size:75%"> TOP HASHTAGS</div>',
          '<div style = "font-size: 90%;font-weight:bold;">', tweet_info[['top_hashtags']], '</div>')
  })
  output$numRetweets<-renderText({
    input$search
    paste("Number of Retweets:", tweet_info[['number_retweets']], sep = ' ')
  })
  output$topMentions<- renderText({
    input$search
    paste('<div style = "font-size:75%"> TOP MENTIONS</div>',
          '<div style = "font-size: 90%;font-weight:bold;">', tweet_info[['top_mentions']], '</div>')
  })
  output$topURLs<- renderText({
    input$search
    paste('<div style = "font-size:75%"> TOP URLS</div>',
          '<div style = "font-size: 90%;font-weight:bold;">', tweet_info[['top_urls']], '</div>')
  })
  output$pastWeek<- renderPlot({
    input$search
    created<-lapply(tweets, function(x) as.Date(x$created))
    df <- as.data.frame(do.call('c',created))
    colnames(df)<- 'date'
    ggplot(df, aes(x = date))+
      geom_histogram(binwidth = 1, colour = 'black', aes(fill = ..count..))+
      xlim(range(seq(from = Sys.Date()-8, to = Sys.Date(), by= 'days'))) +
      labs(title = "Tweets/Messages per day last 7 days", x = 'date', y = 'Number of Tweets')
  })
  
  ####################################### Panel 3 #####################################
  output$source<- renderPlot({
    input$search
    sources <- as.data.frame(unlist(tweet_info[['sources']]))
    colnames(sources)<-'source'
    ggplot(sources, aes(x = factor(rownames(sources)), y = source))+
      geom_bar(stat = 'identity', aes(fill = factor(rownames(sources))))+
      labs(title = "Source of Tweets", x = 'source', y = 'count')+
      theme(axis.text.x = element_blank())+ 
      guides(fill=guide_legend(title="Twitter Source Title"))
    
  })
})