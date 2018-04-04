library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('sparkline')
library('formattable')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/vms/'
csv_pattern <- '.csv'

cols_as_factor <- c(
  'plugin_name',
  'family',
  'severity',
  'rce',
  'authentication_bypass',
  'information_disclosure',
  'ip_address',
  'protocol',
  'port',
  'exploit_',
  'exploit_ease'
)

cols_as_datetime <- c(
  'first_discovered',
  'last_observed',
  'patch_publication_date'
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
names(df) <- gsub('\\?', '_', names(df)) 
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
    parse_datetime(x, "%b %d, %Y %H:%M:%S %Z"))

df <- mutate(df, exploitable = (grepl("Exploits are available", exploit_ease, fixed=TRUE) | grepl("No exploit is required", exploit_ease, fixed=TRUE)))
df <- mutate(df, config_issue = is.na(exploit_ease))

df_exec <- df %>% group_by(severity, exploitable) %>% summarize(n = n()) %>% mutate(p = n / sum(n))


