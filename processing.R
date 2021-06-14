library(tidyverse)
library(lubridate)
library(highcharter)
# Install devtools in order to install circlepackR
## install.packages("devtools")
# For windows, also install Rtools: https://cran.r-project.org/bin/windows/Rtools/
## devtools::install_github("jeromefroe/circlepackeR")
library(circlepackeR)
library(data.tree)
library(treemap)
library(ggrepel)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)


##### Process string/numeric to date #####
stringToDate <- function(targetString) {
  if (class(targetString) != "character") {
    targetString <- toString(targetString)
  }
  
  newString = paste(
    substring(targetString, 1, 4), 
    substring(targetString, 5, 6), 
    substring(targetString, 7, 8), 
    sep = "-"
  )
  
  as.Date(newString)
}

str_replace()

# Read data
# replyTo = replies received
# replyToAbusive = abusive tweets received
originData <- read_csv("https://figshare.shef.ac.uk/ndownloader/files/22749029")

# Transform date
originData <- originData %>% 
  rowwise() %>% 
  mutate(startTime = stringToDate(startTime)) %>% 
  mutate(endTime = stringToDate(endTime)) %>%
  replace(is.na(.), "unknown") %>%
  replace("Sinn F\\u00e9in", "Sinn Féin")


# campaignPeriodData <- originData

### data for options ###
partyOptions <- pull(distinct(originData, party))
genderOptions <- pull(distinct(originData, gender))
 

# sort parties for each date
# campaignTimeSeries <- campaignTimeSeries[
#   order(
#     campaignTimeSeries$startTime, 
#     match(campaignTimeSeries$party, topParties$party)
#   ),
# ]

# campaignPeriodDataAnimation <- campaignPeriodData %>%
#   group_by(startTime, party) %>%
#   summarise(across(where(is.numeric), ~sum(.x))) %>% head(500)
# 
# ggplot(campaignPeriodDataAnimation, aes(x = retweetedAuthor, y = replyToAbusive, size = replyTo, color = party)) +
#   geom_point(alpha = 0.7, show.legend = FALSE) +
#   transition_time(startTime)
# 
# anim_save("animationTweets.gif")

