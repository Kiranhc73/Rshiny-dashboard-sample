
library(tidyverse)
library(shiny)
library(plotly)
library(readxl)


"Sample - Superstore.xls" %>% 
  read_excel() %>% 
  mutate(Profit_ratio= Profit/Sales)-> store_data


store_data %>% colnames()

col_Sub_profit<- function(){
  store_data %>% 
    group_by(`Sub-Category`) %>% 
    summarise(Profit=sum(Profit), .groups = "drop" ) %>% 
    mutate(Profit=Profit %>% round(2) %>% sort(decreasing = T)) %>% 
    ggplot()+
    geom_col(aes(x=`Sub-Category`,y=Profit,fill=`Sub-Category`),show.legend = FALSE)+
    geom_hline(yintercept = 0, color = "red", linetype = "dashed")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45))+
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))-> bar_chart
  ggplotly(bar_chart) %>% 
    layout(showlegend = FALSE)
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
    scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = "k", scale = 1e-3))-> bar_chart
  ggplotly(bar_chart) %>% 
    layout(showlegend = FALSE)
}


