#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('sparkline')
library('formattable')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/phishing/'
csv_pattern <- '.csv'

cols_as_factor <- c(
  'campaign_guid',
  'users_guid',
  'primary_email_opened',
  'primary_clicked',
  'campaign_title',
  'template_sophistication',
  'campaign_recipient_list',
  'teachable_moment_started',
  'acknowledgement_completed',
  'email_bounced',
  'passed_',
  'department',
  'location'
)

cols_as_datetime <- c(
  'date_email_opened',
  'date_clicked',
  'date_sent',
  'date_acknowledged',
  'date_reported'
)




#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = csv_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  as.tibble(read.table(x, sep=",", header=TRUE))))



#------------TIDY DATA

#set blank values to NA
df[df==''] <- NA

#replace periods in column headers with underscore
names(df) <- gsub('\\.', '_', names(df)) 

#column headers to lower case
names(df) <- tolower(names(df))

#Other unordered factors
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
    parse_datetime(x, "%m/%d/%Y %H:%M"))

#------------CREATE FIELDS, SUMMARIES, KPIs

#track whether a phishing attempt has been sent to the user with column attempted
df <- df %>%
          mutate(attempted = ifelse(is.na(date_sent), FALSE, TRUE))

df <- df %>%
          mutate(clicked = ifelse(is.na(date_clicked), FALSE, TRUE))

df <- df %>%
          mutate(acknowledged = ifelse(acknowledgement_completed == TRUE, TRUE, FALSE))

df <- df %>%
          mutate(compliant = ifelse(clicked == TRUE & acknowledged == TRUE, TRUE, FALSE))

#create a df for the compliance KPI
df_phishing_kpi_compliant <- df[df$attempted == TRUE,]
df_phishing_kpi_compliant <- df_phishing_kpi_compliant %>%
                group_by(compliant) %>%
                summarize(n = n(), p = n / nrow(df[df$attempted == TRUE,]))
#assign a color to the KPI within the df
df_phishing_kpi_compliant$kpi_color <- 'black'
df_phishing_kpi_compliant$kpi_color <- ifelse(df_phishing_kpi_compliant$p >= 0.95, 'green', df_phishing_kpi_compliant$kpi_color)
df_phishing_kpi_compliant$kpi_color <- ifelse(df_phishing_kpi_compliant$p < 0.95, 'yellow', df_phishing_kpi_compliant$kpi_color)
df_phishing_kpi_compliant$kpi_color <- ifelse(df_phishing_kpi_compliant$p < 0.85, 'red', df_phishing_kpi_compliant$kpi_color)
df_phishing_kpi_compliant$kpi_color <- ifelse(df_phishing_kpi_compliant$p < 0.5, 'black', df_phishing_kpi_compliant$kpi_color)

metric_attempted = nrow(df)
metric_clicked = nrow(df %>% filter(clicked == TRUE))
metric_acknowledged = nrow(df %>% filter(acknowledged == TRUE))
df_phishing_metrics <- data.frame(metric_name = factor(c('Sent', 'Clicked', 'Acknowledged', 'Reported'),
                                                       levels = c('Sent', 'Clicked', 'Acknowledged', 'Reported'),
                                                       order = TRUE),
                                                       metric_value = c(metric_attempted, metric_clicked, metric_acknowledged, 4))


#------------PLOT
gg_phishing_metrics <-
  ggplot(data = df_phishing_metrics, mapping = aes(x = metric_name, y = metric_value, fill = metric_name)) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = '',
    fill = '',
    y = '# of Users',
    title = 'Phishing Simulation',
    subtitle = 'Confidential - Do Not Distribute'
  ) 
