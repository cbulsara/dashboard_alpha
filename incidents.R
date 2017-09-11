#Load libraries
library('tidyverse')

#Read it
df <- read_csv("~/Documents/csv/incidents/incident_09012017.csv")

#Declare variables
months <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

#Parse it
df$subcategory = parse_factor(df$subcategory, levels = NULL)
df$assigned_to = parse_factor(df$assigned_to, levels = NULL)
df$current_impact[is.na(df$current_impact)] <- 2
df$future_impact[is.na(df$future_impact)] <- 1

df$opened_at <- parse_datetime(df$opened_at, "%Y-%m-%d %H:%M:%S", locale = locale(tz="America/Los_Angeles"))
df$resolved_at <- parse_datetime(df$resolved_at, "%Y-%m-%d %H:%M:%S", locale = locale(tz="America/Los_Angeles"))

#Create new fields
df <- mutate(df, hours = round(calendar_duration / 3600, 1), days = round(calendar_duration / 86400, 1))
df <- mutate(df, opened_month = format(opened_at, "%b"))
df <- mutate(df, resolved_month = format(resolved_at, "%b"))
df <- mutate(df, resolved_year = format(resolved_at, "%Y"))

#Create summaries
per_month_subs <- df %>% 
  group_by(resolved_month, subcategory) %>% 
  summarize(incidents = n(), attch = round(mean(hours), 1), attcd = round(mean(days),1))
per_month_subs$resolved_month <- parse_factor(per_month_subs$resolved_month, months, ordered = TRUE)

ytd_subs <- df %>% 
  group_by(resolved_year, subcategory) %>% 
  summarize(incidents = n(), attch = round(mean(hours), 1), attcd = round(mean(days),1))

tallyf = df %>% group_by(resolved_month) %>% tally
#Graph it

#basic stacked bar by month/category
gg_Summary = ggplot(data = per_month_subs) + 
  geom_bar(mapping = aes(x = per_month_subs$resolved_month, y = per_month_subs$incidents, fill = per_month_subs$subcategory), stat = "identity") +
  labs(x = 'Month', y = "# Incidents", title = 'Monthly Incident Totals by Category', fill = 'Incident Type')

gg_ytd_summary = ggplot(data = ytd_subs) +
  geom_bar(mapping = aes(x = reorder(ytd_subs$subcategory, ytd_subs$incidents), y = ytd_subs$incidents, fill = ytd_subs$subcategory), stat = "identity", show.legend = FALSE) +
  labs(x = '', y = '# Incidents', title = 'Incident Count YTD') +
  coord_flip()

gg_ytd_attc = ggplot(data = ytd_subs) +
  geom_bar(mapping = aes(x = reorder(ytd_subs$subcategory, ytd_subs$incidents), y = ytd_subs$attch, fill = ytd_subs$subcategory), stat = "identity", show.legend = FALSE) +
  labs(x = '', y = 'Average Time to Close (Hours)', title = 'Average Time to Close YTD') +
  coord_flip() +
  scale_y_reverse()

#stacked bar with trendlines for incident volume
#remove method = 'lm' for less smoothyness
#remove se = FALSE for error bar
#ggplot(data = per_month_subs) + 
#  geom_bar(mapping = aes(x = per_month_subs$resolved_month, y = per_month_subs$incidents, fill = per_month_subs$subcategory), stat = "identity") + 
#  geom_smooth(mapping = aes(x = resolved_month, y = incidencts, linetype = subcategory, group = subcategory), method = 'lm', se = FALSE)

#stacked bar with overall trendline
#remove method = 'lm' for less smoothyness
#remove se = FALSE for error bar
#ggplot(data = per_month_subs) + 
#  geom_bar(mapping = aes(x = per_month_subs$resolved_month, y = per_month_subs$incidents, fill = per_month_subs$subcategory), stat = "identity") + 
#  geom_smooth(data = tallyf, mapping = aes(x = tallyf$resolved_month, y = tallyf$n, group = 1), method = 'lm', se = FALSE)
 
