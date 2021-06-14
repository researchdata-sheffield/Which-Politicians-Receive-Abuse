appServer <- function(input, output, session) {
  #######################################
  ########### Global Reactive ###########
  #######################################
  filterBeforeDebounce <- reactive({
    req(input$dateRange)
    
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
  
  filteredData <- debounce(filterBeforeDebounce, 2000)

  # Make circular chart -------------------------
  makeCircularChart <- reactive({
    # prepare data
    campaignPeriodCircular = filteredData() %>% 
      group_by(name) %>% 
      summarise(startTime = first(startTime), 
                endTime = last(endTime), 
                across(where(is.character), ~first(.x)),
                across(where(is.numeric), ~sum(.x))
      ) %>% 
      mutate(abuseTotal = abuseSexist + abuseRacist + abusePolitical)
    
    # get top 20 or less MP
    mpNumber = nrow(campaignPeriodCircular)
    mpNumber = ifelse(mpNumber >= 20, 20, mpNumber)
    
    campaignPeriodCircular = campaignPeriodCircular %>%
      arrange(desc(abuseTotal)) %>%
      head(mpNumber) %>%
      rowwise() %>%
      mutate(abuseTotal = log(abuseTotal, 1.01)) %>%
      arrange(name)
    
    # Apply party colours
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
          party == "Sinn F\\u00e9in" ~ "#326760",
          TRUE ~ "#cccccc"
        )
      )
    
    # adapted from https://www.r-graph-gallery.com/296-add-labels-to-circular-barplot.html
    
    campaignPeriodCircular$id = seq(1, mpNumber)
    angle =  90 - 360 * (campaignPeriodCircular$id - 0.5) / mpNumber
    
    # calculate the alignment of labels: right or left
    campaignPeriodCircular$hjust = ifelse( angle < -90, 1, 0)
    
    # flip angle BY to make them readable
    campaignPeriodCircular$angle = ifelse(angle < -90, angle + 180, angle)

    # set y limits
    yMax = max(campaignPeriodCircular$abuseTotal) + 200
    yMin = -1 * yMax + 200
    
    p = ggplot(campaignPeriodCircular, aes(x = as.factor(id), y = abuseTotal)) +
      geom_bar(stat="identity", fill=campaignPeriodCircular$fill) +
      ylim(yMin,yMax) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(1,-1,1,-1), "cm") ,
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
      ) +
      coord_polar(start = 0) +
      geom_text(data=campaignPeriodCircular, 
                aes(x = id, y= abuseTotal + 70, label = name, hjust = hjust), 
                color="black", 
                fontface="bold",
                alpha=0.6, 
                size=2.8, 
                angle = campaignPeriodCircular$angle, 
                inherit.aes = FALSE ) + 
      labs(
        title = paste0(
          "Top ", 
          mpNumber, 
          " MP by total number of abuse reply received (calculated from these categories: Sexist, Political, and Racist)"
        )
      )
    
    return(p)
  })
  
  
  # Make circle chart -----------------------------------
  makeCircleChart <- reactive({
    campaignPeriodCircle = filteredData() %>% 
      group_by(name) %>% 
      summarise(startTime = first(startTime), 
                endTime = last(endTime), 
                across(where(is.character), ~first(.x)),
                across(where(is.numeric), ~sum(.x))
      ) %>% 
      mutate(abuseTotal = abuseSexist + abuseRacist + abusePolitical)
    
    if(count(campaignPeriodCircle) > 1000) {
      campaignPeriodCircle = campaignPeriodCircle %>% filter(replyToAbusive > 400)
    }
    
    # appened path
    campaignPeriodCircle$pathString = paste("MP", 
                                            campaignPeriodCircle$party,
                                            campaignPeriodCircle$name,
                                            sep = "/")
    
    nodes = as.Node(campaignPeriodCircle)
    p = circlepackeR(nodes, size = "replyToAbusive", width = "500px", height = "500px")
    return(p)
  })
  
  
  # Make donut chart ------------------------------------
  makeDonutChart <- reactive({
    campaignPeriodDonut = filteredData() %>%
      select(name, gender) %>%
      replace(is.na(.), "unknown") %>%
      distinct() %>%
      group_by(gender) %>%
      summarise(count = n())
    
    campaignPeriodDonut$fraction = campaignPeriodDonut$count / sum(campaignPeriodDonut$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    campaignPeriodDonut$ymax <- cumsum(campaignPeriodDonut$fraction)
    campaignPeriodDonut$ymin <- c(0, head(campaignPeriodDonut$ymax, n=-1))
    
    campaignPeriodDonut$labelPosition <- (campaignPeriodDonut$ymax + campaignPeriodDonut$ymin) / 2
    campaignPeriodDonut$label <- paste0(campaignPeriodDonut$gender, ": ", campaignPeriodDonut$count)
    
    
    p = ggplot(campaignPeriodDonut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=gender)) +
      geom_rect() +
      geom_label_repel( x=3.5, aes(y=labelPosition, label=label), size=5) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(0.5, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
    return(p)
  })
  
  
  # Make time series ------------------------------------
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
          party == "Sinn F\\u00e9in" ~ "#326760",
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
  
  output$circularPlot <- renderPlot({
    makeCircularChart()
  })

  output$circlePlot <- renderCirclepackeR({
    makeCircleChart()
  })
  
  output$donutPlot <- renderPlot({
    makeDonutChart()
  })
}
