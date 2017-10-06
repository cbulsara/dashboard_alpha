#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/tasks/'
csv_pattern <- '.csv'

#Columns to parse as unordered factors
cols_as_factor <- c('assignment_group')

#Columns to parse as datetimes.
cols_as_datetime <- c('sys_created_on',
                      'closed_at')

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = csv_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  read_csv(x)))

#------------TIDY DATA

df[cols_as_datetime] <-
  lapply(df[cols_as_datetime], function(x)
    parse_datetime(x, "%Y-%m-%d %H:%M:%S", locale = locale(tz = "America/Los_Angeles")))
#df$sys_created_on <- parse_datetime(df$sys_created_on, "%Y-%m-%d %H:%M:%S", locale = locale(tz="America/Los_Angeles"))
#df$closed_at <- parse_datetime(df$closed_at, "%Y-%m-%d %H:%M:%S", locale = locale(tz="America/Los_Angeles"))

#Other unordered factors
df[cols_as_factor] <-
  lapply(df[cols_as_factor], function(x)
    parse_factor(
      x,
      levels = NULL,
      ordered = TRUE,
      include_na = TRUE
    ))

#------------CREATE FIELDS
df <-
  mutate(df, type = ifelse(
    grepl("Block Phishing", df$short_description),
    'phishing',
    'malware'
  ))

#------------CREATE SUMMARIES
tasks_Means  <- df %>%
  group_by(type) %>%
  summarize(n = n(), attch = round(mean(duration), 1))

#Assign colors based on response time
if (tasks_Means$attch <= 1) {
  tasks_Means <- mutate(tasks_Means, color = 'yellow')
}
if (tasks_Means$attch > 1) {
  tasks_Means <- mutate(tasks_Means, color = 'yellow')
}
if (tasks_Means$attch > 4) {
  tasks_Means <- mutate(tasks_Means, color = 'red')
}

#------------PLOT DATA
