# 
library(shiny)
library(shinyjs)
library(devtools)
library(twitteR)
library(stringr)
library(ggplot2)
ACCESS_TOKEN = '773947253102968832-uWJpi3aBQIXrfFVIAXtmxomVtNkJlNK'
ACCESS_SECRET = 'Gm3htYaL07uvo6gk2lAAGUZPwKilMJWjXPEwoPoStV3nw'
CONSUMER_KEY = 'TG3d15B5zgWXvvMHJOBIzWsWp'
CONSUMER_SECRET = 'IDvM8ftxBFkD3Gfrm6AUfvt4gXnrY1Y9gDeYtudLWC2NeK5AgX'

setup_twitter_oauth(CONSUMER_KEY, CONSUMER_SECRET, ACCESS_TOKEN, ACCESS_SECRET)
tweets<<- userTimeline(user = 'hootsuite', n=200, includeRts = TRUE)
