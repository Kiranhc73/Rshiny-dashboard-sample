
library(tidyverse)
library(shiny)
library(plotly)
library(scales)
library(readxl)


"Sample - Superstore.xls" %>% 
  read_excel() %>% 
  mutate(Profit_ratio= Profit/Sales)-> store_data


store_data %>% colnames()

col_Sub_profit<- function(){
  store_data %>% 
    group_by(`Sub-Category`) %>% 
    summarise(Profit = sum(Profit), .groups = "drop") %>% 
    mutate(Profit = Profit %>% round(2) %>% sort(decreasing = TRUE)) %>% 
    ggplot() +
    geom_col(aes(x = `Sub-Category`, y = Profit, fill = `Sub-Category`), show.legend = FALSE) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, size = 15),
          axis.text.y = element_text(size = 15)) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3)) 
    # geom_text(aes(x = `Sub-Category`, y = Profit, label = scales::dollar(Profit, prefix = "", suffix = "k", scale = 1e-3)),
              # hjust=1, angle = 90, size = 4, vjust = -0.5)
}

col_sub_sales<-function(){
  store_data %>% 
    group_by(`Sub-Category`) %>% 
    summarise(Sales=sum(Sales), .groups = "drop" ) %>% 
    mutate(Sales=Sales %>% round(2) %>% sort(decreasing = T)) %>% 
    ggplot()+
    geom_col(aes(x=`Sub-Category`,y=Sales,fill=`Sub-Category`),show.legend = FALSE)+
    geom_hline(yintercept = 0, color = "red", linetype = "dashed")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45,size = 15),
          axis.text.y = element_text(size = 15)) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))
    # geom_text(aes(x=`Sub-Category`,y=Sales,label = scales::dollar(Sales, prefix = "",suffix = "k", scale = 1e-3)), vjust = -0.5,hjust=1, angle = 90,size=4)
 
}

col_region_sales<-function(){
  store_data %>% 
    group_by(Region) %>% 
    summarise(Sales=sum(Sales), .groups = "drop" ) %>% 
    mutate(Sales=Sales %>% round(2) %>% sort(decreasing = T)) %>% 
    ggplot()+
    geom_col(aes(x=Region,y=Sales,fill=Region),show.legend = FALSE)+
    geom_hline(yintercept = 0, color = "red", linetype = "dashed")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45,size = 15),
          axis.text.y = element_text(size = 15)) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))
    # geom_text(aes(x=Region,y=Sales,label = scales::dollar(Sales, prefix = "",suffix = "k", scale = 1e-3)), vjust = -0.5, hjust=1, angle = 90,size=4)

}


col_region_profit<- function(){
  store_data %>% 
    group_by(Region) %>% 
    summarise(Profit=sum(Profit), .groups = "drop" ) %>% 
    mutate(Profit=Profit %>% round(2) %>% sort(decreasing = T)) %>% 
    ggplot()+
    geom_col(aes(x=Region,y=Profit,fill=Region),show.legend = FALSE)+
    geom_hline(yintercept = 0, color = "red", linetype = "dashed")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45,size = 15),
          axis.text.y = element_text(size = 15))+
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))
    # geom_text(aes(x=Region,y=Profit,label = scales::dollar(Profit, prefix = "",suffix = "k", scale = 1e-3)), vjust = -0.5,hjust=1, angle = 90, size = 4)

}

store_data %>% 
  ggplot(aes(x = Sales, y = Profit, color = Discount, size = Profit_ratio)) +
  geom_point(alpha = 0.7) +  # Adding transparency for better visibility of overlapping points
  scale_color_gradient(low = "blue", high = "red") +  # Custom color scale
  scale_size_continuous(range = c(1, 6)) +  # Custom size range for points
  theme_minimal() +  # Aesthetically pleasing minimal theme
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    axis.title = element_text(size = 15),
    axis.text.x  = element_text(size = 15),
    axis.text.y   = element_text(size = 15),# Increase axis title font size
    legend.title = element_text(size = 15),  # Increase legend title font size
    legend.text = element_text(size = 15)  # Increase legend text font size
  )->Scatter_plot



# Group data by month and year, and summarize
store_data %>%
  mutate(year = year(`Order Date`), month = month(`Order Date`,label=T)) %>%
  group_by(year, month) %>%
  summarise(Sales = sum(Sales),.groups = "drop")->data_grouped

# Convert year and month to a single factor for labeling
data_grouped$year_month <- factor(paste(data_grouped$month, data_grouped$year, sep = "-"))

# Create a line graph with time-based x-axis using ggplot2
ggplot(data_grouped, aes(x = year_month, y = Sales, group=1)) +
  geom_line(color = "blue") +
  scale_x_discrete(labels = data_grouped$year_month) +
  labs(x = "Year-Month", y = "Sales", title = "Trend of Sales") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60 , hjust = 1,size=15),
        axis.text.y = element_text(size=15))+
scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))-> time_series_sales

store_data %>%
  mutate(year = year(`Order Date`), month = month(`Order Date`,label=T)) %>%
  group_by(year, month) %>%
  summarise(Profit = sum(Profit),.groups = "drop")->data_grouped

# Convert year and month to a single factor for labeling
data_grouped$year_month <- factor(paste(data_grouped$month, data_grouped$year, sep = "-"))

# Create a line graph with time-based x-axis using ggplot2
ggplot(data_grouped, aes(x = year_month, y = Profit, group=1)) +
  geom_line(color = "blue") +
  scale_x_discrete(labels = data_grouped$year_month) +
  labs(x = "Year-Month", y = "Profit", title = "Trend of Profit") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60 , hjust = 1,size=15),
        axis.text.y = element_text(size = 15))+
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))-> time_series_profit
