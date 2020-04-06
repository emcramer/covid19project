# data collection for when different counties implemented stay at home measures

# libraries
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

