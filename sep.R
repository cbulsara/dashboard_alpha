#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('sparkline')
library('formattable')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/sep/'
csv_pattern <- '*.csv'
os_levels <- c(
  'UNKNOWN',
  'Windows XP Professional',
  'Windows 7 Professional Edition',
  'Windows 7 Ultimate Edition',
  'Windows 7 Enterprise Edition',
  'Windows 10 Enterprise Edition',
  'Windows Server 2003 family Standard Edition',
  'Windows Server 2003 family Enterprise Edition',
  'Windows Server 2008 Standard Edition',
  'Windows Server 2008 Standard Edition Without Hyper-V',
  'Windows Server 2008 Enterprise Edition',
  'Windows Server 2008 R2 Standard Edition',
  'Windows Server 2008 R2 Enterprise Edition',
  'Windows Server 2012 Datacenter Edition',
  'Windows Server 2012 Standard Edition',
  'Windows Server 2012 R2 Standard Edition',
  'Windows Server 2015 Standard Edition',
  'CentOS',
  'Red Hat Enterprise Linux Server'
)

#Columns to parse as factors. Excludes OS, for which we have a special ordered factor
#to highlight legacy OS
cols_as_factor <- c(
  'Client_Version',
  'Policy_Serial',
  'HI_Status',
  'Status',
  'Auto_Protect_On',
  'Antivirus_engine_On',
  'Download_Insight_On',
  'SONAR_On',
  'Tamper_Protection_On',
  'Intrusion_Prevention_On',
  'IE_Browser_Protection_On',
  'Firefox_Browser_Protection_On',
  'Early_Launch_Antimalware_On'
)

#Columns to parse as dates.
cols_as_date <- c('Measure_Date',
                  'Pattern_Date')

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = csv_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  read_csv(x)))

#------------TIDY DATA

#Set Operating_System NAs to 'UNKNOWN' and read as an ordered factor per ordering above
#MAINTENANCE: update os_levels :(
df$Operating_System[is.na(df$Operating_System)] <- 'UNKNOWN'
df$Operating_System <-
  parse_factor(df$Operating_System,
               os_levels,
               ordered = TRUE,
               include_na = TRUE)

#Other unordered factors
df[cols_as_factor] <-
  lapply(df[cols_as_factor], function(x)
    parse_factor(
      x,
      levels = NULL,
      ordered = TRUE,
      include_na = TRUE
    ))

#dates

#Ingesting files parsed with my python parser right now. parse_datetime would make it easy to ingest direct from SEP
df$Last_Scan_Time[df$Last_Scan_Time == 'Never'] <- '01/01/1970 00:00'
df$Last_Scan_Time <- parse_date(df$Last_Scan_Time, '%m/%d/%Y %H:%M')
df[cols_as_date] <-
  lapply(df[cols_as_date], function(x)
    parse_date(x, "%m/%d/%Y"))

#------------CREATE FIELDS

#Double_Version creates a number version of Client_Version that is easier to work with
df <-
  mutate(df, Double_Version = as.double(str_replace_all(Client_Version, '\\.', '')))

#Scan_Age in days = Measure_Date - Last_Scan_Time
df <- mutate(df, Scan_Age = Measure_Date - Last_Scan_Time)

#Pattern_Age in days = Measure_Date - Pattern_Date
df <- mutate(df, Pattern_Age = Measure_Date - Pattern_Date)

#In_CMDB returns True if the conditional field calculated by the python parse != left_only
#DEPRECATED
#df <-
#  mutate(df, In_CMDB = ifelse(found != 'left_only', TRUE, FALSE))

#KPI_Protected is True if desired protections are enabled and scan/pattern ages are < 6 days
df <- mutate(
  df,
  KPI_Protected = ifelse(
    Status == 'Enabled' &
      Auto_Protect_On == 'Enabled' &
      Antivirus_engine_On == 'Enabled' &
      Firefox_Browser_Protection_On == 'Enabled' &
      IE_Browser_Protection_On == 'Enabled' &
      Intrusion_Prevention_On == 'Enabled' &
      SONAR_On == 'Enabled' &
      Tamper_Protection_On == 'Enabled' &
      grepl('Enabled', Download_Insight_On) &
      Double_Version > 12170000000 &
      Pattern_Age < 7 &
      Scan_Age < 7,
    TRUE,
    FALSE
  )
)

#Scan and Pattern Ages are compliant (TRUE) if they are less than 6 days old
df <-
  mutate(df, Scan_Compliant = ifelse(as.numeric(Scan_Age, units = 'days') < 7, TRUE, FALSE))
df <-
  mutate(df, Pattern_Compliant = ifelse(as.numeric(Pattern_Age, units = 'days') < 6, TRUE, FALSE))

#------------CREATE SUMMARIES and KPIs

#These follow the same basic pattern:
# 1. Start with the base dataframe
# 2. Group it by Measure_Date and the metric you want a count/percent for
# 3. Summarize to get count
# 4. Mutate a percentage based on the count
# df_Pattern_Compliant has 1 or 2 NA's for some reason, so we add an additional line to make those FALSE

df_Protected <-
  df %>% group_by(Measure_Date, KPI_Protected) %>% summarize(n = n()) %>% mutate(p = n / sum(n))
df_Protected_True <- df_Protected %>% filter(KPI_Protected == TRUE)
df_Protected_False <-
  df_Protected %>% filter(KPI_Protected == FALSE)

df_OS <- df %>%
  #mutate(c = (grepl('XP', Operating_System, fixed = TRUE) | grepl('2003', Operating_System, fixed = TRUE))) %>%
  group_by(Measure_Date, Operating_System) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  mutate(c = (
    grepl('XP', Operating_System, fixed = TRUE) |
      grepl('2003', Operating_System, fixed = TRUE)
  ))
df_OS_Compliant <- df %>%
  mutate(c = (
    grepl('XP', Operating_System, fixed = TRUE) |
      grepl('2003', Operating_System, fixed = TRUE)
  )) %>%
  group_by(Measure_Date, c) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n))

df_OS_Compliant_False_N <- df_OS_Compliant %>% filter(c == TRUE) %>% .$n

df_Scan_Compliant <-
  df %>% group_by(Measure_Date, Scan_Compliant) %>% summarize(n = n()) %>% mutate(p = n / sum(n))

df_Pattern_Compliant <-
  df %>% group_by(Measure_Date, Pattern_Compliant) %>% summarize(n = n())
df_Pattern_Compliant$Pattern_Compliant[is.na(df_Pattern_Compliant$Pattern_Compliant)] <-
  FALSE

kpi_protected_percent <-
    df_Protected %>% filter(Measure_Date == toString(max(
      df_Protected$Measure_Date
    ))) %>% filter(KPI_Protected == TRUE) %>% .$p

kpi_protected_color <- 'black'
kpi_protected_color <- ifelse(kpi_protected_percent >= 0.95, 'green', kpi_protected_color)
kpi_protected_color <- ifelse(kpi_protected_percent < 0.95, 'yellow', kpi_protected_color)
kpi_protected_color <- ifelse(kpi_protected_percent < 0.85, 'red', kpi_protected_color)
kpi_protected_color <- ifelse(kpi_protected_percent < 0.5, 'black', kpi_protected_color)
 
kpi_os_compliant <- df_OS_Compliant %>% filter(c=='TRUE') %>% filter(Measure_Date == toString(max(
  df_Protected$Measure_Date))) %>% .$n

#------------PLOT DATA
spk_Protected <-
  spk_composite(
    sparkline(
      df_Protected_True$p,
      type = 'line',
      chartRangeMin = 0,
      chartRangeMax = 1,
      lineColor = 'blue'
    ),
    sparkline(
      df_Protected_False$p,
      type = 'line',
      chartRangeMin = 0,
      chartRangeMax = 1,
      lineColor = 'red'
    )
  )
spkchr_Protected <-
  spk_chr(
    round(df_Protected_True$p, 2),
    type = 'line',
    chartRangeMin = 0,
    chartRangeMax = 1,
    lineColor = 'lime',
    lineWidth = 3,
    fillColor = FALSE,
    height = 25,
    width = 100
  )
spkchr_OS <-
  spk_chr(
    df_OS_Compliant_False_N,
    type = 'line',
    chartRangeMin = 0,
    chartRangeMax = 1,
    lineColor = 'lime',
    lineWidth = 3,
    fillColor = FALSE,
    height = 25,
    width = 100
  )



gg_Protected <-
  ggplot(data = df_Protected,
         mapping = aes(x = Measure_Date, y = n, fill = KPI_Protected)) +
  geom_bar(stat = 'identity', position = 'fill') +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = 'Observation Date',
    y = 'n',
    title = 'Protection Status',
    subtitle = 'Measured Weekly',
    fill = 'Protected (True/False)',
    linetype = 'Protection Trend'
  ) 


gg_OS <-
  ggplot(data = df_OS,
         mapping = aes(x = Measure_Date, y = n, fill = Operating_System)) +
  geom_bar(stat = 'identity') +
  labs(
    x = 'Observation Date',
    y = 'n',
    fill = 'Operating System',
    title = 'Operating System Breakdown',
    subtitle = 'Measured Weekly'
  )

gg_Scan_Compliant <-
  ggplot(data = df_Scan_Compliant,
         mapping = aes(x = Measure_Date, y = n, fill = Scan_Compliant)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = 'Observation Date',
    y = 'n%',
    fill = 'Scan Age Compliant',
    title = 'Scan Age Compliance',
    subtitle = 'Compliant = Last Scanned Within 5 Days'
  ) 

gg_Pattern_Compliant <-
  ggplot(data = df_Pattern_Compliant,
         mapping = aes(x = Measure_Date, y = n, fill = Pattern_Compliant)) +
  geom_bar(position = 'fill', stat = 'identity') +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = 'Observation Date',
    y = 'n%',
    fill = 'Pattern Age Compliant',
    title = 'Pattern Age Compliance',
    subtitle = 'Compliant = Patterns Updated Within 5 Days'
  ) 
