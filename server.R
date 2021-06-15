appServer <- function(input, output, session) {
  plotReady <- reactiveValues(ready = FALSE)
  filteredData <- reactiveValues()
  
  #######################################
  ########### Global Reactive ###########
  #######################################
  # filteredDataFunction <- eventReactive(input$applyFilter, {
  #   req(input$dateRange)
  #   
  #   data <- originData %>%
  #     filter(
  #       between(
  #         startTime, 
  #         input$dateRange[1], 
  #         input$dateRange[2]
  #       )
  #     ) 
  #   
  #   if(!is.null(input$party)) {
  #     data <- data %>%
  #       filter(party %in% input$party)
  #   }
  #   
  #   if(!is.null(input$genders)) {
  #     data <- data %>%
  #       filter(gender %in% input$genders)
  #   }
  #   data
  # })
  
  observeEvent(input$applyFilter, {
    req(input$dateRange)
    
    shinyjs::html("applyFilter", "Loading, please wait ...")
    shinyjs::disable("applyFilter")
    plotReady$ready <- FALSE
    
    # filter data
    filteredData$data <- originData[
      (originData$startTime >= input$dateRange[1] & 
         originData$startTime <= input$dateRange[2]
      ),
    ]

    
    if(!is.null(input$party)) {
      filteredData$data <- filteredData$data[filteredData$data$party %in% input$party,]
    }
    
    if(!is.null(input$genders)) {
      filteredData$data <- filteredData$data[filteredData$data$gender %in% input$genders,]
    }
  })

  
  #filteredData <- debounce(filterBeforeDebounce, 2000)

  # Make circular chart -------------------------
  makeCircularChart <- eventReactive(filteredData$data, {
    # prepare data
    campaignPeriodCircular = filteredData$data %>% 
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
                aes(x = id, y= abuseTotal + 80, label = name, hjust = hjust), 
                color="black", 
                fontface="bold",
                alpha=0.7, 
                size=4, 
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
  makeCircleChart <- eventReactive(filteredData$data, {
    campaignPeriodCircle = filteredData$data %>% 
      group_by(name) %>% 
      summarise(startTime = first(startTime), 
                endTime = last(endTime), 
                across(where(is.character), ~first(.x)),
                across(where(is.numeric), ~sum(.x))
      )
    
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
  makeDonutChart <- eventReactive(filteredData$data, {
    campaignPeriodDonut = filteredData$data %>%
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
      geom_label_repel(x=3.5, aes(y=labelPosition, label=label), size=5) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(0.5, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
    return(p)
  })
  
  
  # Make time series ------------------------------------
  makeTimeSeries <- eventReactive(filteredData$data, {
    campaignTimeSeries = filteredData$data %>%
      group_by(startTime, party) %>%
      summarise(replyToAbusive = sum(replyToAbusive), fill = fill, .groups = "keep")
    
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
  
  
  # Make treemap ---------------------------------------
  makeTreemap <- eventReactive(filteredData$data, {
    levelOptions <- list(
      list(
        level = 1,
        borderWidth = 0.5,
        borderColor = "transparent",
        dataLabels = list(
          enabled = TRUE,
          align = "left",
          verticalAlign = "top",
          style = list(fontSize = "12px", textOutline = FALSE, color = "white")
        )
      ),
      list(
        level = 2,
        borderWidth = 0.3,
        borderColor = "transparent",
        colorVariation = list(key = "brightness", to = 0.2),
        dataLabels = list(enabled = FALSE),
        style = list(fontSize = "8px", textOutline = FALSE, color = "white")
      )
    )
    
    campaignHierarchical = filteredData$data %>%
      select(name, party, replyToAbusive) %>%
      group_by(name, party) %>%
      summarise(replyToAbusive = sum(replyToAbusive), .groups = "keep")
    
    # colours
    cols = filteredData$data %>% 
      group_by(party) %>%
      summarise(count = sum(replyToAbusive), fill = fill, .groups = "keep") %>%
      unique() %>%
      arrange(desc(count))
    
    
    p = hchart(
      data_to_hierarchical(
        campaignHierarchical, 
        c(party, name), 
        replyToAbusive, 
        colors = cols$fill
      ),
      type = "treemap",
      levelIsConstant = FALSE,
      allowDrillToNode = TRUE,
      levels = levelOptions,
      tooltip = list(valueDecimals = FALSE)
    ) %>% 
      hc_chart(
        style = list()
      ) %>% 
      hc_size(height = 800)
    
    # Change loading button back to default
    plotReady$ready <- TRUE
    
    if (plotReady$ready) {
      shinyjs::html("applyFilter", "Apply Filter Options!")
      shinyjs::enable("applyFilter")
    }
    
    return(p)
  })
  
  calculateValues <- eventReactive(filteredData$data, {
    valueList = list()
    
    valueList[["politicians"]] = length(pull(distinct(filteredData$data, name)))
    valueList[["parties"]] = length(pull(distinct(filteredData$data, party)))
    valueList[["replies"]] = sum(filteredData$data$replyToAbusive)
    valueList[["percentage"]] = round(
      sum(filteredData$data$replyToAbusive)/sum(filteredData$data$replyTo)*100, 
      digits = 2
    )
    
    return(valueList)
  })
  
  
  ########################################
  ########### Render Dashboard ###########
  ########################################
  
  # Value Boxes ------------------------------------
  output$numOfPoliticians <- renderValueBox({
    valueBox(
      calculateValues()[["politicians"]], 
      "Number of politicians", 
      icon = icon("user-friends"),
      color = "purple"
    )
  })
  output$numOfParties <- renderValueBox({
    valueBox(
      calculateValues()[["parties"]], 
      "Number of parties", 
      icon = icon("vote-yea")
    )
  })
  output$totalAbusiveReplies <- renderValueBox({
    valueBox(
      calculateValues()[["replies"]], 
      "Total abusive replies", 
      icon = icon("angry"),
      color = "red"
    )
  })
  output$percentOfAbusiveReplies <- renderValueBox({
    valueBox(
      calculateValues()[["percentage"]],
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

  # output$circlePlot <- renderCirclepackeR({
  #   makeCircleChart()
  # })
  output$treemap <- renderHighchart({
    makeTreemap()
  })
  
  output$donutPlot <- renderPlot({
    makeDonutChart()
  })
}