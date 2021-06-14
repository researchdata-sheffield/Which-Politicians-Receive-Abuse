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



# Read data
# replyTo = replies received
# replyToAbusive = abusive tweets received
originData <- read_csv(
  "https://figshare.shef.ac.uk/ndownloader/files/22749029")

# Transform date
originData <- originData %>% 
  rowwise() %>% 
  mutate(startTime = stringToDate(startTime)) %>%
  mutate(endTime = stringToDate(endTime)) %>%
  replace(is.na(.), "unknown")

campaignPeriodData <- originData

### data for options ###
partyOptions <- pull(distinct(originData, party))
genderOptions <- pull(distinct(originData, gender))
 



campaignPeriodDataCircle <- campaignPeriodData %>% 
  group_by(name) %>% 
  summarise(startTime = first(startTime), 
            endTime = last(endTime), 
            across(where(is.character), ~first(.x)),
            across(where(is.numeric), ~sum(.x))
            ) %>% 
  mutate(abuseTotal = abuseSexist + abuseRacist + abusePolitical)

if(count(campaignPeriodDataCircle) > 1000) {
  campaignPeriodDataCircle <- campaignPeriodDataCircle %>% filter(replyToAbusive > 400)
}

# appened path
campaignPeriodDataCircle$pathString <- paste("MP", 
                                             campaignPeriodDataCircle$party,
                                             campaignPeriodDataCircle$name,
                                       sep = "/")

nodes <- as.Node(campaignPeriodDataCircle)

circlepackeR(nodes, size = "replyToAbusive")


######################################
########## circular barplot ########## 
######################################
# adapted from https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html


campaignPeriodDataCircular <- campaignPeriodData %>% 
  group_by(name) %>% 
  summarise(startTime = first(startTime), 
            endTime = last(endTime), 
            across(where(is.character), ~first(.x)),
            across(where(is.numeric), ~sum(.x))
  ) %>% 
  mutate(abuseTotal = abuseSexist + abuseRacist + abusePolitical)

mpNumber <- nrow(campaignPeriodDataCircular)

campaignPeriodDataCircular <- campaignPeriodDataCircular %>%
  arrange(desc(abuseTotal)) %>%
  head(ifelse(mpNumber >= 20, 20, mpNumber)) %>%
  rowwise() %>%
  mutate(abuseTotal = log(abuseTotal, 1.01)) %>%
  arrange(name)



campaignPeriodDataCircular$fill <- ifelse(
  campaignPeriodDataCircular$party == "Conservative Party", "#0087DC", 
    ifelse(campaignPeriodDataCircular$party == "Labour Party", "#DC241f", 
      ifelse(campaignPeriodDataCircular$party == "Liberal Democrats", "#FDBB30",
        ifelse(campaignPeriodDataCircular$party == "Scottish National Party", "#FFFF00",
          ifelse(campaignPeriodDataCircular$party == "Democratic Unionist Party", "#D46A4C",  
            "#cccccc"
          )
        )
      )
    )
)
  
# calculate the ANGLE of the labels
numberOfBar <- nrow(campaignPeriodDataCircular)
campaignPeriodDataCircular$id <- seq(1, numberOfBar)
angle <-  90 - 360 * (campaignPeriodDataCircular$id - 0.5) / numberOfBar

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
campaignPeriodDataCircular$hjust <- ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
campaignPeriodDataCircular$angle <- ifelse(angle < -90, angle + 180, angle)
# ----- ------------------------------------------- ---- #

yMax <- max(campaignPeriodDataCircular$abuseTotal) + 200
yMin <- -1 * yMax + 200

p <- ggplot(campaignPeriodDataCircular, aes(x = as.factor(id), y = abuseTotal)) +
  geom_bar(stat="identity", fill=campaignPeriodDataCircular$fill) +
  ylim(yMin,yMax) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0,-1,0,-1), "cm") ,
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
  ) +
  coord_polar(start = 0) +
  geom_text(data=campaignPeriodDataCircular, 
            aes(x = id, y= abuseTotal + 70, label = name, hjust = hjust), 
            color="black", 
            fontface="bold",
            alpha=0.6, 
            size=2.8, 
            angle = campaignPeriodDataCircular$angle, 
            inherit.aes = FALSE ) + 
  labs(
    title = paste0("Top ", numberOfBar, " MPs by total number of abuse reply received"),
    subtitle = "Sums were calculated from these categories: Sexist, Political, and Racist.")

p



##########################################
########## donut chart - gender ########## 
##########################################
# adapted from https://www.r-graph-gallery.com/128-ring-or-donut-plot.html
campaignPeriodDonut <- campaignPeriodData %>%
  select(name, gender) %>%
  replace(is.na(.), "unknown") %>%
  distinct() %>%
  group_by(gender) %>%
  summarise(count = n())

campaignPeriodDonut$fraction <- campaignPeriodDonut$count / sum(campaignPeriodDonut$count)

# Compute the cumulative percentages (top of each rectangle)
campaignPeriodDonut$ymax <- cumsum(campaignPeriodDonut$fraction)
campaignPeriodDonut$ymin <- c(0, head(campaignPeriodDonut$ymax, n=-1))

campaignPeriodDonut$labelPosition <- (campaignPeriodDonut$ymax + campaignPeriodDonut$ymin) / 2
campaignPeriodDonut$label <- paste0(campaignPeriodDonut$gender, ": ", campaignPeriodDonut$count)


ggplot(campaignPeriodDonut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) +
  geom_rect() +
  geom_label_repel( x=3.5, aes(y=labelPosition, label=label), size=5) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(0.5, 4)) +
  theme_void() +
  theme(legend.position = "none")


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

