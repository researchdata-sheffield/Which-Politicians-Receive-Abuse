library(rsconnect)
library(tidyverse)
library(lubridate)

library(ggrepel)
library(leaflet)
library(htmlwidgets)
library(highcharter)

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
  replace("Sinn F\\u00e9in", "Sinn Féin") %>%
  mutate(
    fill = case_when(
      party == "Conservative Party" ~ "#0087DC",
      party == "Labour Party" ~ "#DC241f",
      party == "Liberal Democrats" ~ "#FDBB30",
      party == "Scottish National Party" ~ "#FFFF00",
      party == "Independent" ~ "#DDDDDD",
      party == "Democratic Unionist Party" ~ "#D46A4C",
      party == "The Brexit Party" ~ "#12B6CF",
      party == "Sinn F\\u00e9in" ~ "#326760",
      TRUE ~ "#cccccc"
    )
  )
  
campaignPeriodData <- originData

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

