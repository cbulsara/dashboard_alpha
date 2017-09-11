#------------LOAD LIBS
library('tidyverse')
library('feedeR')

#------------DECLARE VARIABLES
rss_url <- 'https://www.us-cert.gov/ncas/alerts.xml'
keep_cols <- c('bulletin_number', 'bulletin_name', 'link')

#Columns to parse as datetimes.
cols_as_datetime <- c('date')

#------------READ FEED
cert <- feed.extract("https://www.us-cert.gov/ncas/alerts.xml")

#------------TIDY DATA
df <- data.frame(cert$items$title, cert$items$date, cert$items$link)
colnames(df) <- c('title', 'date', 'link')

df[cols_as_datetime] <-
  lapply(df[cols_as_datetime], function(x)
    parse_datetime(x, "%Y-%m-%d %H:%M:%S"))

#------------CREATE FIELDS
df <- mutate(df, bulletin_number = sapply(strsplit(as.character(df$title), split=':', fixed=TRUE), function(x) (x[1])))
df <- mutate(df, bulletin_name = sapply(strsplit(as.character(df$title), split=': ', fixed=TRUE), function(x) (x[2])))

#------------SELECT WANTED COLUMNS
df_CERT <- subset(df, select=keep_cols)

#------------CREATE DATA TABLE
dt_CERT <- datatable(head(df_CERT, 5), escape = FALSE, colnames=c('Bulletin Number', 'Bulletin Name', 'Link'), 
                     options = list(dom='t',
                     initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}")))
dt_CERT$Link <- paste0('<a href="', dt_CERT$Link, '">', dt_CERT$Link, '</a>')