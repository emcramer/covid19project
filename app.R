#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libs <- c("shiny", "shinythemes", "tidyverse", "dplyr", "ggplot2", "lubridate", 
#           "knitr", "glue", "plotly", "cowplot")
# easypackages::libraries(libs)

# library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(knitr)
library(glue)
library(plotly)
library(cowplot)
library(readr)

# handy functions
capitalize <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

# load the data
covidData <- read_file(
    "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv"
    ) %>%
    iconv("ASCII", "UTF-8") %>%
    read.table(text = ., sep = ",", header = TRUE, stringsAsFactors = FALSE)
# covidData <- read.csv(
#     "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv"
#     , header = FALSE
# )
# cnames <- as.character(covidData[1, ])
# Encoding(cnames) <- "UTF-8"
# colnames(covidData) <- gsub("[^[:alnum:]|/]", "", cnames)
# covidData <- covidData[-1, ] %>%
#     as_tibble() %>%
#     mutate_at(vars(-c("countyFIPS", "CountyName", "State")), as.numeric)

# preprocess the data
# add local ordinance data
countyMeasuresData <- read.csv(
    "./data/countyMeasures.csv")
countyMeasuresData$Local.Measures.Date <- ymd(countyMeasuresData$Local.Measures.Date)

# add when local measures were taken
covidData <- left_join(covidData, countyMeasuresData)

# pivot the data to be longer along the date dimension, and remove appended "X"
convDate <- function(x) {
    print(colnames(x))
    tmp <- gsub("X", "", x$Date)
}
countyDateData <- pivot_longer(covidData
                               , cols = starts_with("X")
                               , names_to = "Date"
                               , values_to = "Count")
countyDateData$Date <- gsub("X", "", countyDateData$Date) %>%
    gsub("\\.", "-", .) %>%
    mdy()

# calculate the number of new cases each day and change ratio
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
crData <- countyDateData %>%
    group_by(County.Name) %>%
    add_column(newCases=calcNewCases(.)) %>%
    add_column(changeRatio=calcChangeRatio(.))

# get the number of cases by state and county
# set the region/state w/name
nationalLevelData <- countyDateData %>%
    add_column(region= setNames(tolower(state.name), state.abb)[toupper(countyDateData$State)]) %>%
    group_by(County.Name) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    group_by(region) %>%
    summarise(TotalCases = sum(Count)) %>%
    mutate(logTotalCases = log10(TotalCases))

# merge the data with geographic info
stateShapeData <- urbnmapr::states %>%
    mutate(region = tolower(state_name))
mergedStates <- inner_join(stateShapeData, nationalLevelData, by="region")

# county data manipulation
countyShapeData <- urbnmapr::counties %>%
    mutate(region = tolower(state_name)
           , stateFIPS = as.numeric(state_fips)
           , County.Name = as.character(county_name))
countyLevelData <- countyDateData %>%
    add_column(region= setNames(tolower(state.name), state.abb)[toupper(countyDateData$State)]) %>%
    group_by(County.Name) %>%
    filter(row_number() == n()) %>% 
    left_join(countyShapeData, by="County.Name") %>%
    mutate(logCount = log10(Count))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    tabsetPanel(
        tabPanel("USA Mapped Cases",
                 # Title
                 titlePanel("Mapped Nationwide and County COVID-19 Cases"),
                 # layout
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons(inputId = "nationState"
                                      , label = "Select National or State Level Map"
                                      , choices = c("National", "State")
                                      , selected = "National")
                         , conditionalPanel(
                             condition = "input.nationState == 'State'"
                             , selectInput("stateState"
                                           , label = "State"
                                           , choices = unlist(lapply(nationalLevelData$region, capitalize))
                                           , selected = "California"
                             )
                         )
                     ),
                     mainPanel(
                         withTags(
                             div(
                                 p("Mouse over the map of the United States to see the number of 
                                   confirmed COVID-19 cases in each state. Select the 'State' 
                                   radio button to further interactively examine the geographic 
                                   distribution of COVID-19 by county.")
                             )
                         ),
                         plotlyOutput("map")
                     )
                 )
        ),
        tabPanel("Change Ratio",
                 # Application title
                 titlePanel("County Level COVID-19 Change Ratios"),
                 
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         # Select the state
                         selectInput(inputId = "stateSelection"
                                     , label = strong("State")
                                     , choices = unique(covidData$State)
                                     , selected = "CA")
                         # Select the county
                         , selectInput(inputId = "countySelection"
                                       , label = "County"
                                       , choices = unique(covidData$County.Name)
                                       , selected = "Santa Clara County")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         textOutput("countyDesc"),
                         plotOutput("barPlot"),
                         withTags({
                             div(
                                 h1("What is the 'Change Ratio'?"),
                                 p("The", strong("change ratio")
                                   ," is the relationship between the number of new cases on the "
                                   , em('nth')
                                   , "day to the number of cases on the "
                                   , em("n-1th")
                                   , "day. It provides an understanding past the raw counts of the
                                   number of cases on a daily basis, and  gives us an idea of
                                   whether the measures we are taking to prevent the spread of 
                                   SARS-CoV-2 are having an effect."),
                                 p("Above I have plotted the number of COVID-19 cases on each day in a 
                                 US municipality, and alongside that, its change-ratio. I fit a ",
                                   a(href="https://en.wikipedia.org/wiki/Local_regression", "loess"),
                                   "curve to the change ratio so we can see what the general trend at 
                                 any given moment may be - are we improving in preventing the spread 
                                 of COVID-19?" ),
                                 
                             )
                         })
                     )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        stateDF <- filter(covidData, State==input$stateSelection)
        updateSelectInput(session = session
                          , "countySelection"
                          , label = "County"
                          , choices = unique(stateDF$County.Name)
                          , selected = stateDF$County.Name[1])
    })
    
    # subset the data to match the county
    countyDF <- reactive({
        req(c(input$stateSelection, input$countySelection))
        filter(crData, State==input$stateSelection, County.Name==input$countySelection)
    })
    
    output$barPlot <- renderPlot({
        df <- countyDF()
        # plot the county data and change ratio
        countyPlotter <- function(df, measuresDate = NA){
            cname <- df$County.Name[1]
            # scaleFactor <- 1*10^floor(max(log10(df$Count)))
            p1 <- ggplot(data=df) +
                geom_bar(aes(x=Date, y=Count), stat='identity', color='black', fill = "grey") +
                labs(y = "Cases") +
                theme_bw()
            p2 <- ggplot(data=df) +
                geom_line(aes(x=Date, y=changeRatio, color='red'), size=1.3) + 
                scale_y_continuous(trans = 'log10') + 
                geom_smooth(aes(x=Date, y=changeRatio), method='loess') + 
                scale_color_manual(name = 'Change Ratio (CR)', values = 'red', labels = cname) +
                labs(y=expression(Log[10](CR)))
            if (!is.na(measuresDate)) {
                p2 <- p2 + 
                    geom_vline(aes(xintercept = measuresDate
                                   , linetype = factor(format(measuresDate, "%B %d, %Y")))
                               , color = "green") +
                    scale_linetype_manual(name = "Local Measures Taken", values = measuresDate)
                p1 <- p1 + 
                    geom_vline(aes(xintercept = measuresDate
                                   , linetype = factor(format(measuresDate, "%B %d, %Y")))
                               , color = "green")
            }
            p2 <- p2 + theme_bw() + theme(legend.position = "bottom")
            prow <- plot_grid(p1 + theme(legend.position = "none")
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
        countyPlotter(df, df$Local.Measures.Date[1])
    })
    
    # Add a description
    output$countyDesc <- renderText({
        glue("Showing data for {input$countySelection}, {input$stateSelection}.")
    })
    
    # add a state selection input for the national/state level case map
    output$stateSelector <- renderUI({
        if (input$nationalState == "State") {
            return(
                selectInput(inputId = "stateSelection"
                            , label = strong("State")
                            , choices = unlist(lapply(nationalLevelData$region, capitalize))
                            , selected = "California")
            )
        } else {
            return()
        }
    })
    
    
    # plot a national map or a state level map
    observe({
        nationalState <- input$nationState
        if(nationalState=="National") {
            output$map <- renderPlotly({
                # plot the cases
                mapPlotter <- function(df) {
                    p <- ggplot(data=df) +
                        geom_polygon(aes(x=long
                                         , y=lat
                                         , group=group
                                         , fill=logTotalCases
                                         , text = paste(paste("State:", state_name)
                                                        , paste("# Cases:", TotalCases)
                                                        , sep = '\n')
                        )
                        , color="white"
                        , size=0.2
                        ) +
                        labs(title="USA COVID-19 Cases"
                             , x = ""
                             , y = "") + 
                        scale_fill_continuous(name="Log10 of # of COVID-19 Cases"
                                              , low = "lightblue"
                                              , high = "darkblue"
                                              , breaks=1:5
                                              , labels=1:5
                                              , na.value = "grey50") +
                        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
                        theme_bw() +
                        theme(legend.position = 'bottom')
                    return(plotly::ggplotly(p, tooltip = c("label", "text")))
                }
                mapPlotter(mergedStates)
            })
        } else {
            # show the options for states
            # get which state is selected
            selectedState <- input$stateState
            # plot
            output$map <- renderPlotly({
                # ggplot() + 
                #     geom_text(aes(x=1, y=1, label="Hello World!"))
                mapPlotter <- function(df, stateName) {
                    plot_df <- df %>%
                        filter(state_name == stateName)
                    p <- ggplot(data=plot_df) +
                        geom_polygon(aes(x=long
                                         , y=lat
                                         , group=group
                                         , text=paste(County.Name, paste("# Cases:", Count)
                                                      , sep='\n')
                                         , fill=log10(Count)
                        )
                        , color="white"
                        , size=0.2
                        ) +
                        labs(title=glue::glue("Number of COVID-19 Cases by County in {stateName}")
                             , x=""
                             , y="") + 
                        scale_fill_continuous(name="Log10 of # of COVID-19 Cases"
                                              , low = "lightblue"
                                              , high = "darkblue"
                                              , breaks=seq(1, ceiling(max(log10(df$Count), na.rm = TRUE)), 2)
                                              , na.value = "grey50") +
                        coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
                        theme_bw()
                    return(plotly::ggplotly(p, tooltip = "text"))
                }
                mapPlotter(countyLevelData, selectedState)
            })
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
