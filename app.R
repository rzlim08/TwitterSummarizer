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


## UI Creation
ui <- shinyUI(fluidPage(
  useShinyjs(),
   tags$body(tags$style("body{font-family:Georgia; color:#404040; background-color: #f2f2f2; padding:30px}")),
  
   # Application title and user info
   mainPanel(
     div(style = 'color:#1DA1F2;',titlePanel("TwitterProfileViewer")),
     div(style="display:inline-block", textInput("screen_name", "Twitter Username", "hootsuite")),
     actionButton("search", "Search"),
     div(style= 'color:#262626;', h3(textOutput("value"))),
     htmlOutput("profilePic"),
     textOutput("description"),
     textOutput("location"),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numStatusesStyle")),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numFavouritesStyle")),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numFollowersStyle")),
     div(style = "display:inline-block;padding-right:10px;", htmlOutput("numFollowingStyle")),
     textOutput("favouritesTweet"),
     width = 4
   ), 
   ## Past tweet statistics
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
  
  ## Past tweets and source
  mainPanel(
    div( id = 'graph_and_tweets',
         h3(textOutput("sourceAndPrevious")),
         plotOutput("source"),
         textOutput("pastTweets")
    ),
    width = 4
  )
  
))
source('twitterServe.R')
# Run the application 
shinyApp(ui = ui, server = server)

