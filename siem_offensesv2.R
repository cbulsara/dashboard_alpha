library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('sparkline')
library('formattable')
library('plotly')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/siem/offenses/'
csv_pattern <- '*.csv'

months <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')

cols_as_factor <- c(
  'localizedclosereason'
)

cols_as_datetime <- c(
  'formattedstarttime',
  'formattedendtime'
)

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = csv_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  as.tibble(read.table(x, sep=",", header=TRUE))))

#------------TIDY DATA
#replace periods in column headers with underscore
names(df) <- gsub('\\.', '_', names(df)) 
names(df) <- gsub('\\?', '_', names(df)) 

#column headers to lower case
names(df) <- tolower(names(df))


#set blank values to NA
df[df==''] <- NA

df[cols_as_factor] <-
  lapply(df[cols_as_factor], function(x)
    parse_factor(
      x,
      levels = NULL,
      ordered = TRUE,
      include_na = TRUE
    ))

df[cols_as_datetime] <-
  lapply(df[cols_as_datetime], function(x)
    parse_datetime(x, "%b %d, %Y, %I:%M:%S %p"))

#-------------------------Add fields
#df$formattedstarttime <- parse_date(df$formattedstarttime, "%b %d, %Y, %I:%M:%S %p")
df <- df %>% mutate(month = format(formattedstarttime, "%B"), 
                    year = format(formattedstarttime, "%y"))
df$month <- parse_factor(df$month, 
                         months,
                         ordered = TRUE,
                         include_na = TRUE)
df$year <- parse_factor(df$year, 
                         levels = NULL,
                         ordered = TRUE,
                         include_na = TRUE)

df$open <- ifelse(grepl("null", df$localizedclosereason), TRUE, FALSE)

df$positive <- ifelse(grepl("False_Positive", df$localizedclosereason, fixed=TRUE), TRUE, FALSE)

#-------------------------Group and Summarize
df_nbymonth <- df %>% 
                group_by(month) %>%
                summarize(n = n(),
                open = sum(open==TRUE), 
                closed = sum(open==FALSE), 
                n_pos = sum(positive==TRUE), 
                n_neg = sum(positive==FALSE),
                p = n_pos/closed)

#-------------------------Plot
p <- plot_ly(df_nbymonth, x=~month, y=~n, type = 'scatter', mode = 'lines', name = 'Total Incidents Opened') %>%
            add_trace(y = ~open, name = 'In Progress', type = 'bar', barmode = 'stack') %>%
            add_trace(y = ~closed, name = 'Closed', type = 'bar', barmode = 'stack') %>%
            add_trace(y = ~n_neg, name = 'False Positives')

