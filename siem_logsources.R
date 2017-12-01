#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')

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

#------------CREATE FIELDS


#------------CREATE SUMMARIES & KPIs

#Log source reporting in error
df_Error <-
  df %>% group_by(Measure_Date, Status) %>% summarize(n = n()) %>% mutate(p = n / sum(n))
kpi_LogSourceErrorPercent <-
  df_Error %>% filter(Measure_Date == toString(max(df_Error$Measure_Date))) %>% filter(Status == "Error") %>% .$p

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
df_EP <- mutate(df_EP, destination = ifelse(Target_Destination == 'eventcollector0 :: carnivore', 'Console', 'Event Processor'))
#------------PLOT DATA
gg_SIEMError <-
  ggplot(data = df_Error,
         mapping = aes(x = Measure_Date, y = n, fill = Status)) +
  scale_fill_brewer(palette = "Set1") +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(
    x = 'Observation Date',
    y = 'n',
    fill = 'Log Source Status',
    title = 'SIEM Log Source Status',
    subtitle = '"Success" = Reporting to SIEM'
  ) +
  geom_smooth(method = 'lm',
              position = 'fill',
              show.legend = FALSE)

gg_SIEMEP <-
  ggplot(data = df_EP,
         mapping = aes(x = Measure_Date, y = sumeps, fill = destination)) +
  scale_fill_brewer(palette = "Set1") +
  geom_bar(position = 'fill', stat = 'identity') +
  labs(
    x = 'Observation Date',
    y = 'EPS',
    fill = 'Target Destination',
    title = 'SIEM EPS Destination',
    subtitle = '"Event Processor" is the Desired Destination'
  ) +
  geom_smooth(method = 'lm',
              position = 'fill',
              show.legend = FALSE)

spkchr_SIEMError <-
  spk_chr(
    round(filter(df_Error, Status == 'Error') %>% .$p, 2),
    type = 'line',
    chartRangeMin = 0,
    chartRangeMax = 1,
    lineColor = 'lime',
    lineWidth = 3,
    fillColor = FALSE,
    height = 25,
    width = 100
  )
spkchr_SIEMEP <-
  spk_chr(
    round(filter(df_EP, destination == 'Console') %>% .$p, 2),
    type = 'line',
    chartRangeMin = 0,
    chartRangeMax = 1,
    lineColor = 'lime',
    lineWidth = 3,
    fillColor = FALSE,
    height = 25,
    width = 100
  )

