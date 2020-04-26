library(ggplot2)
library(jsonlite)
library(tidyverse)
library(ggrepel)
library(viridis)
library(hrbrthemes)

# Getting the data in the form of json from the api.
covid_data <- jsonlite::fromJSON("https://api.covid19india.org/data.json")

# Extarcting the day-wise cases data from the json.
time_series <- covid_data[["cases_time_series"]]

# Identifying the columns contaning numeric data as chr and converting
# the data using as.numeric() 
chr_cols <- c("dailyconfirmed","dailydeceased","dailyrecovered",
              "totalconfirmed","totaldeceased","totalrecovered") 
time_series[chr_cols] <- sapply(time_series[chr_cols], as.numeric)

# Extracting the data for past 30 days.
time_series <- time_series %>% arrange(desc(totalconfirmed)) %>% 
  top_n(30, totalconfirmed)

# Plot provides the trend of confirmed, recovered and deceased over 
# the past 30 days.
time_series %>% ggplot(aes(x = reorder(date, totalconfirmed))) + 
  geom_line(aes(y=dailyconfirmed, col="Daily confirmed"), group=1, size = 1) + 
  geom_line(aes(y=dailydeceased, col="Daily deceased"), group=1, size = 1) + 
  geom_line(aes(y=dailyrecovered, col="Daily recovered"), group=1, size = 1)  +
  xlab("Timeline") +
  ylab("Count") +
  ggtitle("COVID-19 growth in India") +
  scale_color_discrete(name = "Colour") +
  theme_pander() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

# Extracting the statewise data.
statewise <- covid_data[["statewise"]]
chr_col <- c("active","confirmed","deaths",
             "recovered") 
statewise[chr_col] <- sapply(statewise[chr_col], as.numeric)

# Removing the row containing the total data and states having 
# 0 confirmed cases.
statewise <- statewise[!(statewise$state == "Total") & 
                         statewise$confirmed>0,]

# Plot gives the statewise details of active, recovered and death cases.
statewise %>% select(c(state, active, recovered, deaths, confirmed)) %>%
  gather("Metric", "Value",-c(state, confirmed)) %>% 
  arrange(state) %>%
  ggplot(aes(Value, y = reorder(state,confirmed), fill = Metric)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(name = "Metric", labels = c("Active cases", "Deaths", "Recovered"),discrete = T) +
  theme_ipsum() +
  ggtitle("Statewise Details:") +
  xlab("Count") +
  ylab("States") +
  theme(axis.title.y=element_text( size = 12, face = "bold", hjust = 0.5), axis.title.x = element_text( size = 12, face = "bold.italic", hjust = 0.5), plot.title = element_text( size = 18, face = "bold", hjust = 0.5))

# Filtering the top 10 states in terms of confirmed cases.
statewise_top <- statewise  %>% arrange(desc(deaths)) %>% 
  top_n(10, confirmed)

# Plot provides the relation between the confirmed and recovered cases for the 
# top 10 cases. The deaths are provided by the size of the points.
statewise_top %>% ggplot(aes(confirmed, recovered, label = state)) + 
  geom_point(aes(size = deaths), col = "red") +
  geom_label_repel(size = 4, box.padding = 0.35, point.padding = 0.5,
                   segment.color = 'grey50') +
  ggtitle("Top 10 States") +
  xlab("Confirmed") +
  ylab("Recovered") +
  labs(size='Deaths') +
  theme_bw() +
  theme(axis.title.y=element_text( size = 12, hjust = 0.5), axis.title.x = element_text( size = 12, hjust = 0.5))
  