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
library(plotly)


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
campaignPeriodData <- read_csv(
  "https://figshare.shef.ac.uk/ndownloader/files/22749029")

# Transform date
campaignPeriodData <- campaignPeriodData %>% 
  rowwise() %>% 
  mutate(startTime = stringToDate(startTime)) %>%
  mutate(endTime = stringToDate(endTime)) 

# %>% mutate(abuseTotal = abuseSexist + abuseRacist + abusePolitical)
  
campaignPeriodData

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
  
campaignPeriodDataCircular <- campaignPeriodDataCircular %>%
  arrange(desc(abuseTotal)) %>%
  head(30) %>%
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
campaignPeriodDonut$label <- paste0(campaignPeriodDonut$gender, "\n value: ", campaignPeriodDonut$count)


ggplot(campaignPeriodDonut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) +
  geom_rect() +
  geom_label_repel( x=3.5, aes(y=labelPosition, label=label), size=5) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(0.5, 4)) +
  theme_void() +
  theme(legend.position = "none")


#################################
########## time series ########## 
#################################

campaignTimeSeries <- campaignPeriodData %>% 
  group_by(startTime, party) %>%
  summarise(replyToAbusive = sum(replyToAbusive)) 

topParties <- campaignTimeSeries %>% 
  group_by(party) %>%
  summarise(count = sum(replyToAbusive)) %>%
  arrange(desc(count)) %>%
  head(5)

campaignTimeSeries <- campaignTimeSeries %>% 
  filter(party %in% topParties$party) %>%
  mutate(
    fill = case_when(
      party == "Conservative Party" ~ "#0087DC",
      party == "Labour Party" ~ "#DC241f",
      party == "Liberal Democrats" ~ "#FDBB30",
      party == "Scottish National Party" ~ "#FFFF00",
      party == "Independent" ~ "#DDDDDD",
      party == "Democratic Unionist Party" ~ "#D46A4C",
      party == "The Brexit Party" ~ "#12B6CF",
      TRUE ~ "#cccccc",
    )
  )

timeSeriesColours <- campaignTimeSeries %>% 
  group_by(party) %>%
  summarise(count = sum(replyToAbusive), fill = first(fill)) %>%
  arrange(desc(count))

# sort parties for each date
campaignTimeSeries <- campaignTimeSeries[
  order(
    campaignTimeSeries$startTime, 
    match(campaignTimeSeries$party, topParties$party)
  ),
]


# plotly
campaignTimeSeries %>% 
  ungroup() %>%
  plot_ly(x = ~ startTime) %>% 
  add_lines(
    y = ~ replyToAbusive, 
    color = ~ factor(party)
  ) %>%
  layout(
    xaxis = list(
      title = "Date"
    ), 
    yaxis = list(
      title = "Number of abusive tweets received"
    ))

# highcharter
hchartTS <- campaignTimeSeries %>%
  hchart(
    type = "line",
    hcaes(x = startTime, y = replyToAbusive, group = factor(party)),
    marker=list(symbol='circle', radius=2)
  ) %>%
  hc_xAxis(
    title = list(text = "Date")
  ) %>%
  hc_yAxis(
    title = list(text = "Number of abusive tweets received")
  )

for (row in 1:nrow(timeSeriesColours)) {
  hchartTS <- hchartTS %>% 
    hc_add_series(campaignTimeSeries, type="line", color=timeSeriesColours[row,]$fill, zIndex=0)
}





hchartTS <- highchart() %>%
  hc_xAxis(
    title = list(text = "Date"),
    dateTimeLabelFormats = list(day = '%d %b'), 
    type = "datetime",
    plotLines = list(
      list(
        label = list(text = "TV Event (ITV)"),
        color = "#E8E8E8",
        width = 1,
        value = datetime_to_timestamp(as.Date("2019-11-19"))
      ),
      list(
        label = list(text = "TV Event (BBC)"),
        color = "#E8E8E8",
        width = 1,
        value = datetime_to_timestamp(as.Date("2019-11-22"))
      ),
      list(
        label = list(text = "TV Event (Channel 4)"),
        color = "#E8E8E8",
        width = 1,
        value = datetime_to_timestamp(as.Date("2019-11-28"))
      ),
      list(
        label = list(text = "TV Event (BBC)"),
        color = "#E8E8E8",
        width = 1,
        value = datetime_to_timestamp(as.Date("2019-12-06"))
      ),
      list(
        label = list(text = "Election"),
        color = "#E8E8E8",
        width = 1.5,
        value = datetime_to_timestamp(as.Date("2019-12-12"))
      )
    )
  ) %>%
  hc_yAxis(
    title = list(text = "Number of abusive tweets received")
  )

for (row in 1:nrow(timeSeriesColours)) {
  hchartTS <- hchartTS %>% 
    hc_add_series(
      data = campaignTimeSeries %>% filter(party == timeSeriesColours[row,]$party), 
      type="line", 
      hcaes(x = startTime, y = replyToAbusive),
      marker = list(symbol='circle', radius=1),
      name = timeSeriesColours[row,]$party, 
      color = timeSeriesColours[row,]$fill 
    )
}

hchartTS


# campaignPeriodDataAnimation <- campaignPeriodData %>%
#   group_by(startTime, party) %>%
#   summarise(across(where(is.numeric), ~sum(.x))) %>% head(500)
# 
# ggplot(campaignPeriodDataAnimation, aes(x = retweetedAuthor, y = replyToAbusive, size = replyTo, color = party)) +
#   geom_point(alpha = 0.7, show.legend = FALSE) +
#   transition_time(startTime)
# 
# anim_save("animationTweets.gif")

