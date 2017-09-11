#------------LOAD LIBS
library('tidyverse')
library('stringr')
library('scales')

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/projects/'
csv_pattern <- '.csv'

#Columns to parse as dates.
cols_as_date <- c('date_start',
                  'date_finish')

#health columns to parse as factors
cols_as_health_factor <- c('status',
                           'health_schedule',
                           'health_budget',
                           'health_resources')

#levels for the health factors
health_levels <- c('Complete',
                   'Tracking',
                   'Attention',
                   'Blocked',
                   'Not Started',
                   'On Hold')

#levels for the type factor
type_levels <- c('Project',
                 'Risk Assessment')

#levels for the owner factor
owner_levels <- c('Cyrus Bulsara',
                  'Stacie Macfadyen')

#------------READ FILES
csv_in <-
  list.files(path = csv_path,
             pattern = csv_pattern,
             full.name = TRUE)
df <- do.call(rbind, lapply(csv_in, function(x)
  read_csv(x)))

#------------TIDY DATA
#parse all health columns as factors
df[cols_as_health_factor] <-
  lapply(df[cols_as_health_factor], function(x)
    parse_factor(
      x,
      levels = health_levels,
      ordered = FALSE,
      include_na = TRUE
    ))

#parse the type field as a factor
df$type <-
  parse_factor(df$type,
               type_levels,
               ordered = TRUE,
               include_na = TRUE)

#parse the owner field as a factor
df$owner <-
  parse_factor(df$owner,
               owner_levels,
               ordered = TRUE,
               include_na = TRUE)

#parse all of the date fields
df[cols_as_date] <-
  lapply(df[cols_as_date], function(x)
    parse_date(x, "%m/%d/%Y"))

#------------CREATE SUMMARIES
df_projects <- filter(df, df$type == 'Project')
df_ra <- filter(df, df$type == 'Risk Assessment')

#------------PLOT DATA
dt_projects <-
  datatable(df_projects) %>% formatStyle(cols_as_health_factor,
                                         backgroundColor = styleEqual(
                                           c(
                                             'Not Started',
                                             'Tracking',
                                             'Attention',
                                             'Blocked',
                                             'Complete',
                                             'On Hold'
                                           ),
                                           c('white', 'lime', 'yellow', 'red', 'aqua', 'orange')
                                         ))
dt_ra <- datatable(df_ra) %>% formatStyle(cols_as_health_factor,
                                          backgroundColor = styleEqual(
                                            c(
                                              'Not Started',
                                              'Tracking',
                                              'Attention',
                                              'Blocked',
                                              'Complete',
                                              'On Hold'
                                            ),
                                            c('white', 'lime', 'yellow', 'red', 'aqua', 'orange')
                                          ))
