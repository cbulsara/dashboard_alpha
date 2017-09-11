#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/siem/'
logsource_pattern <- 'logsources*'

#Columns to parse as dates.
cols_as_date <- c('Measure_Date')

cols_as_datetime <-
  c('Last_Event_Time', 'Creation_Date', 'Modification_Date')

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = logsource_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  read_csv(x)))


#------------TIDY DATA
#die whitespace die
names(df) <- gsub(' ', '_', names(df))

#just dates, not datetime
df[cols_as_date] <-
  lapply(df[cols_as_date], function(x)
    parse_date(x, "%m/%d/%Y"))

#datetime
df[cols_as_datetime] <-
  lapply(df[cols_as_datetime], function(x)
    parse_datetime(x, "%b %d, %Y, %I:%M %p", na = c('', 'NA')))

#------------CREATE SUMMARIES & KPIs

#Log source reporting in error
df_Error <-
  df %>% group_by(Measure_Date, Status) %>% summarize(n = n()) %>% mutate(p = n / sum(n))
kpi_LogSourceErrorPercent <-
  df_Error %>% filter(Measure_Date == toString(max(df_Error$Measure_Date))) %>% filter(Status ==
                                                                                         'Error') %>% .$p

if (kpi_LogSourceErrorPercent <= 0.1)  {
  kpi_LogSourceErrorPercentColor <-  'green'
}
if (kpi_LogSourceErrorPercent > 0.1)   {
  kpi_LogSourceErrorPercentColor <-  'yellow'
}
if (kpi_LogSourceErrorPercent > 0.15)  {
  kpi_LogSourceErrorPercentColor <-  'red'
}

#Log sources reporting to Console vs. EP vs. FP
df_EP <-
  df %>% filter(Status == 'Success') %>% group_by(Measure_Date, Target_Destination) %>% summarize(n = n(), sumeps = sum(Average_EPS)) %>% mutate(p = sumeps / sum(sumeps))
kpi_EPPercent <-
  df_EP %>% filter(Measure_Date == toString(max(df_EP$Measure_Date))) %>% filter(Target_Destination == 'eventcollector109 :: scrippsep') %>% .$p
if (kpi_EPPercent >= 0.9)  {
  kpi_EPPercentColor <-  'green'
}
if (kpi_EPPercent < 0.9)   {
  kpi_EPPercentColor <-  'yellow'
}
if (kpi_EPPercent < 0.85)  {
  kpi_EPPercentColor <-  'red'
}
