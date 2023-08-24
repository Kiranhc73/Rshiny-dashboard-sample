
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
    theme(axis.text.x = element_text(angle = 45)) +
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
    theme(axis.text.x = element_text(angle = 45),
          axis.text.y = element_text()) +
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
    theme(axis.text.x = element_text(angle = 45),
          axis.text.y = element_text()) +
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
    theme(axis.text.x = element_text(angle = 45))+
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))
    # geom_text(aes(x=Region,y=Profit,label = scales::dollar(Profit, prefix = "",suffix = "k", scale = 1e-3)), vjust = -0.5,hjust=1, angle = 90, size = 4)

}

store_data %>% 
  ggplot(aes(x = Sales, y = Profit, color = Discount, size = Profit_ratio)) +
  geom_point(alpha = 0.7) +  # Adding transparency for better visibility of overlapping points
  scale_color_gradient(low = "blue", high = "red") +  # Custom color scale
  scale_size_continuous(range = c(1, 4)) +  # Custom size range for points
  theme_minimal() +  # Aesthetically pleasing minimal theme
  theme(
    legend.position = "bottom",  # Move legend to the bottom
    plot.title = element_text(hjust = 0.5),  # Center the plot title
    axis.title = element_text(size = 12),  # Increase axis title font size
    legend.title = element_text(size = 10),  # Increase legend title font size
    legend.text = element_text(size = 9)  # Increase legend text font size
  )->Scatter_plot



