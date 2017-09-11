#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/siem/'
offense_pattern <- 'offenses*'

#Columns to parse as unordered factors
cols_as_factor <- c('sirt')

#Columns to parse as dates.
cols_as_date <- c('Measure_Date')

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = offense_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  read_csv(x)))

#------------TIDY DATA
#Right now I'm only generating the %Offenses->SIRTs KPI, so not wasting a lot of cycles on tidy

df[cols_as_date] <-
  lapply(df[cols_as_date], function(x)
    parse_date(x, "%m/%d/%Y"))

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
#Not doing anything here yet

#------------CREATE SUMMARIES
df_SIEMOffenseToSirt <- df %>%
  group_by(Measure_Date, sirt) %>%
  summarize(n = n(), p = n / sum(n))



kpi_SIEMOffenseToSirt <-
  1 - (df_SIEMOffenseToSirt %>% filter(Measure_Date == toString(max(df_SIEMOffenseToSirt$Measure_Date))) %>% 
      filter(sirt == FALSE) %>% .$p
  )
if (df_SIEMOffenseToSirt$p <= 0.1) {
  kpi_SIEMOffenseToSirtColor <- 'green'
}
if (df_SIEMOffenseToSirt$p > 0.1) {
  kpi_SIEMOffenseToSirtColor <-  'yellow'
}
if (df_SIEMOffenseToSirt$p > 0.15) {
  kpi_SIEMOffenseToSirtColor <-  'red'
}