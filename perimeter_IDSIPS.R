#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/perimeter/'
csv_pattern <- 'soc*'

#Columns to parse as unordered factors
cols_as_factor <- c('site_name', 'application_types', 'device_health_status',
                    'status', 'service_type', 'managed_by', 'monitored_by',
                    'host_name', 'customer_host_name', 'service_name')



#Columns to parse as datetimes.
#cols_as_datetime <- c('')

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = csv_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  read_csv(x)))


#df$kpi_name <- mapvalues(df$kpi_name, from=c("Encinitas - Sante Fe Dr.", "La Jolla - 9888 Genesee Ave", 
#                                             "Corporate Office | San Diego", "La Jolla - 10666 N. Torrey Pines",
#                                             ), to=c("0101", "0102", "0103")

#------------TIDY DATA

#die whitespace die
names(df) <- gsub(' ', '_', names(df))
names(df) <- sapply(names(df), tolower)

#------------CREATE FIELDS, SUMMARIES, KPIs

#Generate the name that goes in the KPI ValueBox by taking customer_host_name,
#splitting it on ' - ', and retaining the latter half of the split.
df$kpi_name <- df$customer_host_name
df$kpi_name <- sapply(strsplit(df$kpi_name, split = ' - ', fixed = TRUE), function(x) (x[2]))

#Generate the kpi_value:
#+0.25 IDS Operational
#+0.25 IPS Operational
#+0.25 Devices in Support
#+0.25 Events Tuned
df$kpi_value <- as.integer('0')


#If the device is monitored by us, assume for now that it is not tuned and only providing IDS
df$kpi_value <- ifelse(df$monitored_by == 'CUSTOMER' && df$device_health_status == 'OK', df$kpi_value + 0.25, df$kpi_value)
df$kpi_value <- round(df$kpi_value,1)
df$kpi_color <- 'black'
df$kpi_color <- ifelse(df$kpi_value >= 0.95, 'green', df$kpi_color)
df$kpi_color <- ifelse(df$kpi_value < 0.95, 'yellow', df$kpi_color)
df$kpi_color <- ifelse(df$kpi_value < 0.85, 'red', df$kpi_color)
df$kpi_color <- ifelse(df$kpi_value < 0.5, 'black', df$kpi_color)


df_KPIIDSIPS <- df %>% 
                filter(grepl('IDS', application_types)) %>%
                group_by(site_name)
