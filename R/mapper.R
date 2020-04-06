
libs <- c("tidyverse", "dplyr", "ggplot2", "lubridate")
easypackages::libraries(libs)

# get the data
countyData <- read.csv("https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv")

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

# set the region/state w/name
nationalData <- countyDateData %>%
  add_column(region= setNames(tolower(state.name), state.abb)[toupper(countyDateData$State)]) %>%
  group_by(County.Name) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  group_by(region) %>%
  summarise(TotalCases = sum(Count))

# merge the data with geographic info
mergedStates <- inner_join(map_data("state"), nationalData, by="region")

# plot the cases
mapPlotter <- function(df) {
  p <- ggplot(data=df) +
    geom_polygon(aes(x=long
                     , y=lat
                     , group=group
                     , fill=log(TotalCases))
                 , color="white"
                 , size=0.2
    ) +
    labs(title="Contiguous USA COVID-19 Cases") + 
    scale_fill_continuous(name="Number of Cases"
                          , low = "lightblue"
                          , high = "darkblue"
                          , breaks=seq(1, ceiling(max(log(df$TotalCases))), 2)
                          , na.value = "grey50") +
    theme_void()
  p
}
mapPlotter(mergedStates)
