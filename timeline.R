library(timevis)
library(tidyverse)
library(RColorBrewer)
library(plotly)
library(timeline)

df_timeline <- read_csv("~/Documents/csv/timeline/test_timeline.csv")
df_events <- read_csv("~/Documents/csv/timeline/test_timeline_events.csv")

#df$title = df$content
#df$start <- parse_character(df$start)
#df$end <- parse_character(df$end)
#timevis(data = df, groups = data.frame(id=1:3, content=c("Scheduled Task", "Update", "Milestone")))

#p <- plot_ly() %>%  layout(xaxis = list(
#  range = 
#    c(as.numeric(as.POSIXct("2017-10-01", format="%Y-%m-%d"))*1000,
#      as.numeric(as.POSIXct("2018-09-30", format="%Y-%m-%d"))*1000),
#  type = "date"))
#t <- timelineS(df, scale='month', scale.format='%b', buffer.days = 10)
t <- timeline(as.data.frame(df_timeline), as.data.frame(df_events), event.group.col = 'groups', event.label = "Updates, Delays, Milestones", num.label.steps=10, event.label.method=1, event.spots=2)
