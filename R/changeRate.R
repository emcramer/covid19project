
libs <- c("tidyverse", "dplyr", "ggplot2", "lubridate", "glue", "cowplot", "here")

easypackages::libraries(libs)

# get the data
countyData <- read.csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")
countyMeasuresData <- read.csv(here::here("data/countyMeasures.csv"))
countyMeasuresData$Local.Measures.Date <- ymd(countyMeasuresData$Local.Measures.Date)

# add when local measures were taken
countyData <- left_join(countyData, countyMeasuresData)

# pivot the data to be longer along the date dimension, and remove appended "X"
convDate <- function(x) {
  print(colnames(x))
  tmp <- gsub("X", "", x$Date)
}
countyDateData <- pivot_longer(countyData
                               , cols = starts_with("X")
                               , names_to = "Date"
                               , values_to = "Count")
countyDateData$Date <- gsub("X", "", countyDateData$Date) %>%
  gsub("\\.", "-", .) %>%
  mdy()

# calculate the number of new cases each day
calcNewCases <- function(x) {
  tmp <- c(0, diff(x$Count, 1))
  return(tmp)
}
calcChangeRatio <- function(x){
  tmp <- x$Count/dplyr::lag(x$Count)
  tmp[is.nan(tmp)] <- 0
  tmp[is.na(tmp)] <- 0
  tmp[is.infinite(tmp)] <- 0
  return(tmp)
}
newCases <- countyDateData %>%
  group_by(County.Name) %>%
  add_column(newCases=calcNewCases(.)) %>%
  add_column(changeRatio=calcChangeRatio(.))

# plot the number of cases as a bar and the change ratio as a line
countyPlotter <- function(df, measuresDate = NA){
  cname <- df$County.Name[1]
  # scaleFactor <- 1*10^floor(max(log10(df$Count)))
  p1 <- ggplot(data=df) +
    geom_bar(aes(x=Date, y=Count), stat='identity', color='black', fill = "grey") +
    labs(y = "Cases") +
    theme_bw()
  p2 <- ggplot(data=df) +
    geom_line(aes(x=Date, y=changeRatio, color='red'), size=1.3) + 
    geom_smooth(aes(x=Date, y=changeRatio), method='loess') + 
    scale_color_manual(name = 'Change Ratio', values = 'red', labels = cname) +
    labs(y="")
  if (!is.na(measuresDate)) {
    p2 <- p2 + 
      geom_vline(aes(xintercept = measuresDate
                     , linetype = factor(format(measuresDate, "%B %d, %Y")))
                 , color = "green") +
      scale_linetype_manual(name = "Local Measures Taken", values = measuresDate)
  }
  p2 <- p2 + theme_bw() + theme(legend.position = "bottom")
  prow <- plot_grid(p1
                    , p2 + theme(legend.position = "none")
                    )
  title <- ggdraw() +
    draw_label(glue("COVID-19 Cases in {cname}")) +
    theme(plot.margin = margin(0, 0, 0, 7))
  legend <- get_legend(p2 + theme(legend.box.margin = margin(0, 0, 0, 12)))
  p <- plot_grid(title
                 , legend
                 , prow
                 , ncol=1
                 , rel_heights = c(0.1, 0.1, 1))
  return(p)
}
countyDF <- filter(newCases, State=="CA", County.Name=="Los Angeles County")
countyPlotter(countyDF, measuresDate = countyDF$Local.Measures.Date[1])
