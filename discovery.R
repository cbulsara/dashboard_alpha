library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('sparkline')
library('formattable')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/vms/'
csv_pattern <- 'discovery_datacenter_all.csv'

cols_as_factor <- c(
  'os_cpe',
  'repository'
)

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = csv_pattern,
             full.name = TRUE)
df_disco <- do.call(rbind, lapply(csv_in, function(x)
  as.tibble(read.table(x, sep=",", header=TRUE))))

#------------TIDY DATA

#set blank values to NA
df_disco[df_disco==''] <- NA

#replace periods in column headers with underscore
names(df_disco) <- gsub('\\.', '_', names(df_disco)) 
names(df_disco) <- gsub('\\?', '_', names(df_disco))
names(df_disco) <- gsub(' ', '_', names(df_disco))
#column headers to lower case
names(df_disco) <- tolower(names(df_disco))

#Other unordered factors
df_disco[cols_as_factor] <-
  lapply(df_disco[cols_as_factor], function(x)
    parse_factor(
      x,
      levels = NULL,
      ordered = TRUE,
      include_na = TRUE
    ))

df_summary <- df_disco %>% group_by(repository) %>% dplyr::summarize(total = n())