# App to view statistics and tweets of twitter users
library(shiny)
library(shinyjs)
library(devtools)
library(twitteR)
library(stringr)
library(ggplot2)

## Access token and keys 
ACCESS_TOKEN = '773947253102968832-uWJpi3aBQIXrfFVIAXtmxomVtNkJlNK'
ACCESS_SECRET = 'Gm3htYaL07uvo6gk2lAAGUZPwKilMJWjXPEwoPoStV3nw'
CONSUMER_KEY = 'TG3d15B5zgWXvvMHJOBIzWsWp'
CONSUMER_SECRET = 'IDvM8ftxBFkD3Gfrm6AUfvt4gXnrY1Y9gDeYtudLWC2NeK5AgX'

## setup twitter authentication 
setup_twitter_oauth(CONSUMER_KEY, CONSUMER_SECRET, ACCESS_TOKEN, ACCESS_SECRET)

ui <- shinyUI(fluidPage(
  useShinyjs(),
   tags$body(tags$style("body{font-family:Georgia; color:#404040; background-color: #f2f2f2; padding:30px}")),
  
   # Application title
   mainPanel(
     div(style = 'color:#1DA1F2;',titlePanel("TwitterProfileViewer")),
     div(style="display:inline-block", textInput("screen_name", "Twitter Username", "hootsuite")),
     a(id = 'hide-stats', actionButton("search", "Search")),
     div(style= 'color:#262626;', h3(textOutput("value"))),
     htmlOutput("profilePic"),
     textOutput("description"),
     textOutput("location"),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numStatusesStyle")),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numFavouritesStyle")),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numFollowersStyle")),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numFollowingStyle")),
     textOutput("favouritesTweet"),
     a(id = 'toggle', actionButton("tweetsStats", "Get Tweet Data")),
     width = 4
   ), 
   mainPanel(
     div( id = 'statistics',
     h3(textOutput("lastTweets")),
     div(style="display:inline-block;background-color:#F5883D;width:49%",htmlOutput("retweetsTweet")),
     div(style="display:inline-block;background-color:#9341d2;width:49%",htmlOutput("favoritesTweet")),
     div(style="padding:8px;background-color:#f8f8f8", htmlOutput("topHashtags")), 
     div(style="padding:8px;background-color:#f8f8f8", htmlOutput("topMentions")), 
     div(style="padding:8px;background-color:#f8f8f8", htmlOutput("topURLs")),
     plotOutput("pastWeek")
     ),
     width = 4
   ), 
  mainPanel(
    div( id = 'graph_and_tweets',
         h3(textOutput("sourceAndPrevious")),
         plotOutput("source"),
         textOutput("pastTweets")
    ),
    width = 4
  )
  
))

server <- shinyServer(function(input, output) {
   user <- reactive({
     u <<- getUser(input$screen_name)
     }) 
   get_tweets<-reactive({
     tweets<<- userTimeline(user = input$screen_name, n=200, includeRts = TRUE)
   })
   onclick('toggle', shinyjs::show(id = 'statistics', anim = TRUE))
   onclick('hide-stats', hide(id='statistics', anim = TRUE))
   output$value <- renderText({ 
     input$search
     u = isolate(user())
     u$name
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
   output$lastTweets <- renderText({
     input$tweetsStats
     isolate(get_tweets())
     parse_tweets(tweets)
     paste("Last", length(tweets), "tweets", sep = ' ')
   })
   
   output$sourceAndPrevious <- renderText({
     paste("Source of tweets", sep = ' ')
   })
   
   output$favoritesTweet <- renderText({
     input$tweetsStats
     val = tweet_info[['total_favourites']]/tweet_info[['number_tweets']]
     paste('<div style = "font-family:Helvetica Neue;color:white;padding:10px;">',
           '<div style = "font-size:75%"> FAVOURITES/TWEET</div>',
           '<div style = "font-size: 200%;font-weight:bold;">', round(val,digits=1), '</div>',
           '</div>'
           )
   })
   output$retweetsTweet<- renderText({
     input$tweetsStats
     val = tweet_info[['total_retweets']]/tweet_info[['number_tweets']]
     paste('<div style = "font-family:Helvetica Neue;color:white;padding:10px;">',
           '<div style = "font-size:75%"> RETWEETS/TWEET</div>',
           '<div style = "font-size: 200%;font-weight:bold;">', round(val,digits=1), '</div>',
           '</div>'
     )
   })
   
   output$topHashtags<- renderText({
     input$tweetsStats
     paste('<div style = "font-size:75%"> TOP HASHTAGS</div>',
           '<div style = "font-size: 90%;font-weight:bold;">', tweet_info[['top_hashtags']], '</div>')
   })
   output$numRetweets<-renderText({
     input$tweetsStats
     paste("Number of Retweets:", tweet_info[['number_retweets']], sep = ' ')
   })
   output$topMentions<- renderText({
     input$tweetsStats
     paste('<div style = "font-size:75%"> TOP MENTIONS</div>',
           '<div style = "font-size: 90%;font-weight:bold;">', tweet_info[['top_mentions']], '</div>')
   })
   output$topURLs<- renderText({
     input$tweetsStats
     paste('<div style = "font-size:75%"> TOP URLS</div>',
           '<div style = "font-size: 90%;font-weight:bold;">', tweet_info[['top_urls']], '</div>')
   })
   output$pastWeek<- renderPlot({
    input$tweetsStats
    created<-lapply(tweets, function(x) as.Date(x$created))
    df <- as.data.frame(do.call('c',created))
    colnames(df)<- 'date'
    ggplot(df, aes(x = date))+
      geom_histogram(binwidth = 1, colour = 'black', aes(fill = ..count..))+
      xlim(range(seq(from = Sys.Date()-8, to = Sys.Date(), by= 'days'))) +
      labs(title = "Tweets/Messages per day last 7 days", x = 'date', y = 'Number of Tweets')
   })
   output$source<- renderPlot({
     input$tweetsStats
     sources <- as.data.frame(unlist(tweet_info[['sources']]))
     colnames(sources)<-'source'
     ggplot(sources, aes(x = factor(rownames(sources)), y = source))+
       geom_bar(stat = 'identity', aes(fill = factor(rownames(sources))))+
       labs(title = "Source of Tweets", x = 'source', y = 'count')+
       theme(axis.text.x = element_blank())+ 
       guides(fill=guide_legend(title="Twitter Source Title"))
     
     })
       
   output$profilePic<-renderText({
     input$search
     if(is.null(u$profileImageUrl)){
       return(NULL)
     }
     return({c('<img src="',u$profileImageUrl,'"style = "width:100px;height:100px;">')})
   })
   
})
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
  tweet_text_list = list()
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
      ## Only do most stats on non-retweets
      hashtags = str_extract_all(i$text, "#\\S+")[[1]]
      mentions = str_extract_all(i$text, "@\\S+")[[1]]
      
      if(identical(i$getReplyToSN(), character(0))){
        num_tweets<-num_tweets+1
        tweet_text_list[[num_tweets]]<- i$text
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
  tweet_info[['last_tweets']]<<-tweet_text_list
}
# Run the application 
shinyApp(ui = ui, server = server)

