# Read data -------------------------------------
## replyTo = replies received
## replyToAbusive = abusive tweets received
originData <- read_csv("https://figshare.shef.ac.uk/ndownloader/files/22749029")

# Preprocess ------------------------------------
originData <- originData %>%
  rowwise() %>%
  mutate(startTime = stringToDate(startTime)) %>%
  mutate(endTime = stringToDate(endTime)) %>%
  replace(is.na(.), "unknown") %>%
  mutate(party = ifelse(party == "Sinn F\\u00e9in", "Sinn Fein", party)) %>%
  mutate(
    fill = case_when(
      party == "Conservative Party" ~ "#0087DC",
      party == "Labour Party" ~ "#DC241f",
      party == "Liberal Democrats" ~ "#FDBB30",
      party == "Scottish National Party" ~ "#FFFF00",
      party == "Independent" ~ "#DDDDDD",
      party == "Democratic Unionist Party" ~ "#D46A4C",
      party == "The Brexit Party" ~ "#12B6CF",
      party == "Sinn Fein" ~ "#326760",
      TRUE ~ "#cccccc"
    )
  ) %>%
  mutate(name = iconv(name, 'utf-8', 'ascii', sub=''))

write.csv(originData, file = "data/processedData.csv", row.names = FALSE)