library('tidyverse')
library('stringr')
library('scales')
library('RColorBrewer')
library('sparkline')
library('formattable')
library('plotly')
library('leaflet')
library('htmltools')

#Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiY2J1bHNhcmEiLCJhIjoiY2pmbXJ1emMxMGUxZDMzbXlkNnk0cHQzbSJ9.o3nHky3cH7VeadRQilaWVA')

source("~/dashboard_alpha/discovery.R")

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

#-------------------------Create Summaries
df_severity <- df %>% group_by(repository, severity) %>% summarize(n_vulnerabilities= n())
df_exec <- df %>% group_by(repository, exploitable) %>% dplyr::summarize(n_vulnerabilities = n()) 
df_merge <- merge(df_exec, df_summary, by='repository', all.x=TRUE)
df_dedupe <- df[!duplicated(df$ip_address),]
df_dedupe_summary <- df_dedupe %>% group_by(repository, exploitable) %>% dplyr::summarize(unique_assets = n())
df_dedupe_summary <- merge(df_dedupe_summary, df_summary, by='repository', all.x=TRUE)
df_dedupe_summary <- df_dedupe_summary %>% mutate(ptotal = unique_assets / total)
df_join <- inner_join(df_dedupe_summary, df_merge)

df_remediation <- df %>% group_by(plugin_name) %>% filter(exploit_ease == 'No exploit is required' | exploit_ease == 'Exploits are available' | is.na(exploit_ease))
df_remediation <- subset(df_remediation, select=c('plugin', 'plugin_name','synopsis', 'severity', 'cve', 'ip_address', 'dns_name', 'netbios_name'))

#-------------------------Generate Remediation CSVs
for (p in unique(df_remediation$plugin)) {
  df_rm <- df_remediation %>% filter(plugin == p)
  path <-'/home/cyrus/Documents/csv/vms/remediation/'
  filename <- paste(df_rm[1,]$plugin, "_", df_rm[1,]$severity, ".csv", sep = "")
  write.csv(df_rm, file = paste(path, filename, sep = ""))
}

#-------------------------Create plots

#Stacked bar, exploitable vs non by Repository
repos <- unique(df_join$repository)
non_exp <- df_join[df_join$exploitable=='FALSE',]$n_vulnerabilities
exp <- df_join[df_join$exploitable=='TRUE',]$n_vulnerabilities
sum_vulns <- exp + non_exp
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

#Stacked bar, Critical vs High by Repository
#re-use repos
#repos <- unique(df_join$repository)
crit <- df_severity[df_severity$severity=='Critical',]$n_vulnerabilities
high <- df_severity[df_severity$severity=='High',]$n_vulnerabilities
df_sev <- data.frame(repos, crit, high)
p_SevbySite <-plot_ly(df_severity, 
                        x=~repos, y=~high, 
                        type = 'bar', 
                        text = ~high, textposition = 'auto', 
                        name = 'High Risk') %>% 
              add_trace(y = ~crit, text = ~crit, 
                        name = 'Critical') %>% 
              layout (title = "Severity By Site", 
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

#Map of sites
lats <- c(32.887329, 33.455256, 32.810284)
lons <- c(-117.223650,-111.976237, -117.121274)
df_location <- data.frame(repos, exp_assets, lats, lons)
m_label <- paste("Site:", df_location$repos, "<br/>", "Vulnerable Assets:", df_location$exp_assets, sep = " ") %>% lapply(htmltools::HTML) 
m <- leaflet (data=df_location)
m <- m %>% addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% addCircleMarkers(~lons, ~lats, label = m_label, color = 'red', radius = ~exp_assets/10)


  