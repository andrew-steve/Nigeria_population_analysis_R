# Load the Libraries #####
library(tidyverse)
library(knitr)
library(plotly)

# Import the data #######
df <- read.csv('Data/data.csv')
head(df)

# Extract the row containing data about Nigeria
nigeria_df <- df[df$Country.Name == 'Nigeria',]
# confirm we have no missing values
missing_values <- colSums(is.na(nigeria_df))
missing_values
# Reshape the data to long format for easy plotting
nigeria_pop <- pivot_longer(nigeria_df, cols = starts_with("X"), names_to = "Year", values_to = "Population")
# Convert the year column to numeric and remove the 'X'
nigeria_pop$Year <- as.numeric(gsub("X", "", nigeria_pop$Year))

# Use ggplot2 to create a time-series plot
ggplot(nigeria_pop, aes(x = Year, y = Population)) +
  geom_line(col ='blue') +
  labs(title = "Population Growth of Nigeria (1960-2022)",
       x = "Year",
       y = "Population") +
  scale_y_continuous(labels = scales::number_format()) +
  theme_minimal()

# Calculate the population annual growth rate
nigeria_pop$Annual_Growth_Rate <- NA
for (i in 2:nrow(nigeria_pop)) {
  nigeria_pop$Annual_Growth_Rate[i] <- 
    (nigeria_pop$Population[i] - nigeria_pop$Population[i - 1]) / nigeria_pop$Population[i - 1] * 100
}
# visualize the growth rate over the years
fig <- plot_ly(nigeria_pop, x = ~Year, y = ~Annual_Growth_Rate, type = 'scatter', mode = 'lines+markers', 
               line = list(color = 'blue'), marker = list(color = 'blue'))
# Customize the layout
fig <- fig %>% 
  layout(title = 'Annual Growth Rate Over the Years',
         xaxis = list(title = 'Year', color = 'blue'),
         yaxis = list(title = 'Growth Rate (%)', color='blue'))
# Show the interactive plot
fig


# Choropleth Map
nigeria_map <- plot_ly(nigeria_pop, 
               type = 'choropleth', 
               locations = ~Country.Code, 
               z = ~Population, 
               text = ~Country.Name,
               frame = ~Year)


\# Customize the layout
nigeria_map <- nigeria_map %>% 
  layout(
    geo = list(
      scope = 'africa',  # Set the geographical scope to Africa
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'mercator')
    ),
    colorbar = list(title = 'Population', tickvals = seq(40000000, 300000000, by = 5000000), tickformat = ','),
    annotations = list(
      x = 1.05,
      y = 0.5,
      xref = 'paper',
      yref = 'paper',
      text = 'Population',
      showarrow = FALSE
    )
  )

# Show the interactive plot
nigeria_map

