library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('sparkline')
library('formattable')
library('plotly')

source("~/Documents/dashboard_alpha/discovery.R")

#------------DECLARE VARIABLES
csv_path <- '/home/cyrus/Documents/csv/vms/'
csv_pattern <- 'Vulnerability_All_CriticalHigh.csv'

cols_as_factor <- c(
  'plugin_name',
  'family',
  'severity',
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
             full.name = TRUE,)
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

#-------------------------Create Summaries
df_exec <- df %>% group_by(repository, exploitable) %>% dplyr::summarize(n_vulnerabilities = n()) %>% mutate
df_merge <- merge(df_exec, df_summary, by='repository', all.x=TRUE)
df_dedupe <- df[!duplicated(df$ip_address),]
df_dedupe_summary <- df_dedupe %>% group_by(repository, exploitable) %>% dplyr::summarize(unique_assets = n())
df_dedupe_summary <- merge(df_dedupe_summary, df_summary, by='repository', all.x=TRUE)
df_dedupe_summary <- df_dedupe_summary %>% mutate(ptotal = unique_assets / total)
df_join <- inner_join(df_dedupe_summary, df_merge)

#-------------------------Create plots

#Stacked bar, exploitable vs non by Repository
repos <- unique(df_join$repository)
non_exp <- df_join[df_join$exploitable=='FALSE',]$n_vulnerabilities
exp <- df_join[df_join$exploitable=='TRUE',]$n_vulnerabilities
df_vuln <- data.frame (repos, exp, non_exp)
p_VulnsbySite <-plot_ly(df_vuln, 
                  x=~repos, y=~exp, 
                  type = 'bar', 
                  text = ~exp, textposition = 'auto', 
                  name = 'Exploitable') %>% 
              add_trace(y = ~non_exp, text = ~non_exp, 
                  name = 'Non-Exploitable') %>% 
              layout (title = "Vulnerabilities By Site", 
                  xaxis = list(title = 'Site'), 
                  yaxis = list(title = '# of Vulnerabilities'), 
                  barmode = 'stack')

#Pie chart, assets affected by Critical and High vulnerabilities
#re-use repos from p_VulnsbySite
#repos <- unique(df_join$repository)
assets_n <- df_join[df_join$exploitable=='FALSE',]$unique_assets
assets_y <- df_join[df_join$exploitable=='TRUE',]$unique_assets
exp_assets <- assets_n + assets_y
sum_assets <- sum(exp_assets)
asset_totals <- df_join[df_join$exploitable=='TRUE',]$total
df_asset <- data.frame(repos,exp_assets,asset_totals)
df_asset <- df_asset %>% mutate(p = exp_assets/asset_totals)
piedata <- df_asset[,c('repos','exp_assets')]
pietitle <-paste("% Breakdown of", sum_assets, "Vulnerable Assets by Location", sep = " ")
p_VulnPercent <- plot_ly(piedata, 
                          labels = ~repos, 
                          values = ~exp_assets, 
                          type = 'pie', 
                          textposition='inside', 
                          textinfo='label+percent',
                          hoverinfo='text',
                          text=~paste(exp_assets, "assets")) %>%
                layout(title = pietitle)

  