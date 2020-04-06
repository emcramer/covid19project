# plot a state at county level
# plot the cases
mapPlotter <- function(df, stateName) {
  plot_df <- df %>%
    filter(state_name == stateName)
  p <- ggplot(data=plot_df) +
    geom_polygon(aes(x=long
                     , y=lat
                     , group=group
                     , fill=log10(Count))
                 , color="white"
                 , size=0.2
    ) +
    labs(title=glue::glue("Number of COVID-19 Cases by County in {stateName}")
         , fill = "Log10 of # of COVID-19 Cases") + 
    scale_fill_continuous(name="Number of Cases"
                          , low = "lightblue"
                          , high = "darkblue"
                          , breaks=seq(1, ceiling(max(log10(df$Count), na.rm = TRUE)), 2)
                          , na.value = "grey50") +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    theme_bw()
  p
}
mapPlotter(countyLevelData, "California")
