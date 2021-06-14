appServer <- function(input, output, session) {
  #######################################
  ########### Global Reactive ###########
  #######################################
  filteredData <- reactive({
    data <- originData %>%
      filter(
        between(
          startTime, 
          input$dateRange[1], 
          input$dateRange[2]
        )
      ) 
    
    if(!is.null(input$party)) {
      data <- data %>%
        filter(party %in% input$party)
    }
    
    if(!is.null(input$genders)) {
      data <- data %>%
        filter(gender %in% input$genders)
    }
    data
  })
  
  
  makeCircularChart() <- reactive({
    # 
    campaignPeriodCircular = filteredData() %>% 
      group_by(name) %>% 
      summarise(startTime = first(startTime), 
                endTime = last(endTime), 
                across(where(is.character), ~first(.x)),
                across(where(is.numeric), ~sum(.x))
      ) %>% 
      mutate(abuseTotal = abuseSexist + abuseRacist + abusePolitical)
    
    mpNumber = nrow(campaignPeriodCircular)
    
    campaignPeriodCircular = campaignPeriodCircular %>%
      arrange(desc(abuseTotal)) %>%
      head(ifelse(mpNumber >= 20, 20, mpNumber)) %>%
      rowwise() %>%
      mutate(abuseTotal = log(abuseTotal, 1.01)) %>%
      arrange(name)
    
    campaignPeriodCircular = campaignPeriodCircular %>%
      mutate(
        fill = case_when(
          party == "Conservative Party" ~ "#0087DC",
          party == "Labour Party" ~ "#DC241f",
          party == "Liberal Democrats" ~ "#FDBB30",
          party == "Scottish National Party" ~ "#FFFF00",
          party == "Independent" ~ "#DDDDDD",
          party == "Democratic Unionist Party" ~ "#D46A4C",
          party == "The Brexit Party" ~ "#12B6CF",
          TRUE ~ "#cccccc"
        )
      )
    
    # adapted from https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html
  })
  
  makeTimeSeries <- reactive({
    campaignTimeSeries = filteredData() %>%
      group_by(startTime, party) %>%
      summarise(replyToAbusive = sum(replyToAbusive), .groups = "keep") %>%
      mutate(
        fill = case_when(
          party == "Conservative Party" ~ "#0087DC",
          party == "Labour Party" ~ "#DC241f",
          party == "Liberal Democrats" ~ "#FDBB30",
          party == "Scottish National Party" ~ "#FFFF00",
          party == "Independent" ~ "#DDDDDD",
          party == "Democratic Unionist Party" ~ "#D46A4C",
          party == "The Brexit Party" ~ "#12B6CF",
          TRUE ~ "#cccccc"
        )
      )
    
    timeSeriesColours = campaignTimeSeries %>% 
      group_by(party) %>%
      summarise(count = sum(replyToAbusive), fill = first(fill)) %>%
      arrange(desc(count))
    
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
    
    # add line for each party
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
    
    return(hchartTS)
  })
  
  
  
  
  ########################################
  ########### Render Dashboard ###########
  ########################################
  
  # Value Boxes ------------------------------------
  output$numOfPoliticians <- renderValueBox({
    valueBox(
      length(pull(distinct(filteredData(), name))), 
      "Number of politicians", 
      icon = icon("user-friends"),
      color = "purple"
    )
  })
  output$numOfParties <- renderValueBox({
    valueBox(
      length(pull(distinct(filteredData(), party))), 
      "Number of parties", 
      icon = icon("vote-yea")
    )
  })
  output$totalAbusiveReplies <- renderValueBox({
    valueBox(
      sum(filteredData()$replyToAbusive), 
      "Total abusive replies", 
      icon = icon("angry"),
      color = "red"
    )
  })
  output$percentOfAbusiveReplies <- renderValueBox({
    valueBox(
      round(
        sum(filteredData()$replyToAbusive)/sum(filteredData()$replyTo)*100, 
        digits = 2
      ),
      "Percentage of abusive replies", 
      icon = icon("percentage"),
      color = "yellow"
    )
  })
  
  
  ####### Time Series #######
  output$timeSeries <- renderHighchart({
    makeTimeSeries()
  })
}
