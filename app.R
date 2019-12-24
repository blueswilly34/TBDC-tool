# Load libraries ---- 
library(gplots)
library(ggplot2)
library(ggsci)
library(magrittr)
library(ggpubr)
library(ggridges)
library(grid)
library(gridExtra)
library(reshape2)
library(scales)
library(ggthemes)
library(ggalluvial)
library(RColorBrewer)
library(directlabels)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(gganimate)
library(gifski)
library(av)
library(magick)
library(devtools)
library(extrafont)
library(cowplot)
library(ggrepel)
library(ggforce)
require("readxl")
require(sf)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)

# Load Data ----
load("Neo.RData")
geo <- readRDS("TBDC_border_sf.RDS")
bor_trn <- geo[[1]]
bor_nat <- geo[[2]]
bor_reg <- geo[[3]]
bor_cty <- geo[[4]]
bor_twn <- geo[[5]]


# UI forming ----
ui <- dashboardPage(
  dashboardHeader(title = "TBDC tools",
                  titleWidth = 200),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Trend", tabName = "trend", icon = icon("dashboard")),
      menuItem("Age density", tabName = "age_den", icon = icon("th")),
      menuItem("Ranking", tabName = "rank_bar", icon = icon("th")),
      menuItem("Geographic", tabName = "geo", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Trend (Long/Cross)
      tabItem(tabName = "trend",
              fluidPage(theme = shinytheme("united"),titlePanel(""),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("trend_cause", label = h3("Cause_name"),
                                        choices = levels(demo$cause_name),
                                        selected = "Colon and rectum cancer",multiple = TRUE),
                            selectInput("trend_ind", label = h3("Measure"),
                                        choices = c("Prevalence","Deaths","YLDs","YLLs","DALYs"),
                                        selected = 1),
                            selectInput("trend_units", label = h3("Units"),
                                        choices = c("Count","Rate","Percentage"),selected = 1),
                            selectInput("trend_stand", label = h3("Adjust"),
                                        choices = c("Crude","Age-standardized"),selected = 1),
                            width = 3
                            ),
                          
                          mainPanel(plotOutput(outputId = "linePlot", 
                                               width = "1080px", height = "720px"))))
      ),
      
      # Age-pattern
      tabItem(tabName = "age_den",
              fluidPage(theme = shinytheme("united"),titlePanel(""),
                        sidebarLayout(
                          sidebarPanel(
                            checkboxGroupInput("age_scause", label = h3("Cause_name"),
                                               choices = levels(demo$Cause_name),
                                               selected = c("Lip and oral cavity cancer")),
                            sliderInput("age_year","Year:",min = 2000,max = 2017,value = 2017),
                            sliderInput("age_scale","Scale:",min = 1,max = 10,value = 1)
                          ),
                          mainPanel(img(src='https://tinyurl.com/wtp3jvb'))))
      ),
      
      # Rank-bar
      tabItem(tabName = "rank_bar",
              fluidPage(theme = shinytheme("united"),titlePanel(""),
                        sidebarLayout(
                          sidebarPanel(
                            checkboxGroupInput("rank_cause", label = h3("Cause_name"),
                                               choices = levels(demo$Cause_name),
                                               selected = "Lip and oral cavity cancer"),
                            sliderInput("rank_year","Year:",min = 2000,max = 2017,value = 2017)
                          ),
                          mainPanel(img(src='https://tinyurl.com/wtp3jvb'))))
      ),
      # geo
      tabItem(tabName = "geo",
              fluidPage(theme = shinytheme("united"),titlePanel(""),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("geo_cause", label = h3("Cause_name"),
                                        choices = levels(demo$cause_name),
                                        selected = "Colon and rectum cancer"),
                            selectInput("geo_geo", label = h3("Level"),
                                        choices = c("City","Town"),
                                        selected = "Town"),
                            sliderInput("geo_year","Year:",min = 2000,max = 2016,value = 2016),
                            selectInput("geo_ind", label = h3("Measure"),
                                        choices = c("Prevalence","Deaths","YLDs","YLLs","DALYs"),
                                        selected = 1),
                            selectInput("geo_stand", label = h3("Adjust"),
                                        choices = c("Crude","Age-standardized"),selected = 1),
                            width = 3
                          ),
                          mainPanel(plotOutput(outputId = "geoPlot", 
                                               width = "1080px", height = "720px")))))))
  )

# Server forming ----
server <- function(input, output, session) {
  #Trend
  output$linePlot <- renderPlot({
    #####First step. cause and index#####
    tmp <- subset(demo, demo$cause_name%in%input$trend_cause&demo$Year!=2017)
    tmp$index <- tmp[,input$trend_ind]
    tmp <- tmp[,-7:-11]
    
    #####Second step. crude/st#####
    f_count_crude <- function(dataset){
      t2 <- aggregate(index~Year+cause_name, dataset, sum)
      colnames(t2) <- c("Year", "cause_name", "index")
      return(t2)
    }
    f_rate_crude <- function(dataset){
      t2 <- aggregate((index*100000/population)~Year+cause_name, dataset, sum)
      colnames(t2) <- c("Year", "cause_name","index")
      return(t2)
    }
    f_rate_st <- function(dataset){
      t2 <- aggregate((index*100000*stand_percent/population)~Year+cause_name, dataset, sum)
      colnames(t2) <- c("Year", "cause_name", "index")
      return(t2)
    }
    f_percent_crude <- function(dataset){
      t2 <- aggregate((index/population)~Year+cause_name, dataset, sum)
      colnames(t2) <- c("Year", "cause_name", "index")
      return(t2)
    }
    f_percent_st <- function(dataset){
      t2 <- aggregate((index*stand_percent/population)~Year+cause_name, dataset, sum)
      colnames(t2) <- c("Year", "cause_name", "index")
      return(t2)
    }
    
    f_crude <- function(dataset){
      t1 <- aggregate(cbind(index, population)~Year+cause_name, dataset, sum)
      ft <- switch(input$trend_units, "Count" = f_count_crude, "Rate" = f_rate_crude, "Percentage" = f_percent_crude)
      t3 <- ft(t1)
      return(t3)
    }
    f_st <- function(dataset){
      t1 <- aggregate(cbind(index, population)~Year+Age_group+stand_percent+cause_name, dataset, sum)
      ft <- switch(input$trend_units, "Rate" = f_rate_st, "Percentage" = f_percent_st)
      t3 <- ft(t1)
      return(t3)
    }
    

    fs <- switch(input$trend_stand, "Crude" = f_crude, "Age-standardized" = f_st)
    
    tmp <-fs(tmp)
    
    
    ggplot(data=tmp, aes(x=Year, y=index, colour = cause_name)) +
      geom_line(size = 1) + geom_point(size = 3) + 
      scale_colour_brewer(palette='Dark2')+
      labs(title=input$trend_cause, subtitle=input$trend_ind, x="Year", y=input$trend_units, 
           caption=input$trend_stand) + expand_limits(y = 0) +
      theme(
        text = element_text(),
        plot.title = element_text(face = "bold",
                                  size = rel(1.5), hjust = 0.45, vjust = 0.5),
        plot.subtitle = element_text(size = rel(1.25), hjust = 0.45, vjust = 0.5),
        plot.background = element_rect(fill = '#f2f2f2',colour = NA ),
        
        panel.background = element_rect(fill = '#f2f2f2',colour = NA ),
        #  panel.border = element_rect(colour = NA),
        panel.grid.major = element_line(colour="DarkGray",linetype = "dotted",size = 0.5),#背景格線
        panel.grid.major.x = element_line(colour="DarkGray",linetype = "dotted",size = 0.5),
        panel.grid.minor = element_blank(),
        
        axis.title = element_text(face = "bold",size = rel(0.9)),
        axis.title.x = element_text(vjust = 0, size = 13),
        axis.title.y = element_text(hjust = 0.5, vjust = 2.5,angle = 90, size = 13),
        axis.text.x = element_text(hjust = 0.5, angle = 0, size = 10), 
        axis.line = element_line(colour="black"),#坐標軸顏色
        axis.ticks = element_line(),
        
        legend.position = "bottom",
        legend.key.size= unit(0.2, "cm"),
        legend.text = element_text(size=rel(1.25)),
        
        plot.margin=unit(c(10,5,5,5),"mm"),
        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
        strip.text = element_text(face="bold")
      )
 
    
  })
  
  output$geoPlot <- renderPlot({
    
    tmp <- subset(demo, demo$town%in%bor_trn$TownshipMOH_ID&
                    demo$cause_name%in%input$geo_cause&demo$Year==input$geo_year)
    tmp$index <- tmp[,input$geo_ind] #index
    tmp <- tmp[,-7:-11]
    tmp <- aggregate(cbind(index, population)~cause_name+Year+town+Age_group+stand_percent, tmp, sum)
    names(tmp)[names(tmp) == "town"] <- "TownshipMOH_ID"
    tmp <- inner_join(geo[[1]],tmp)
    #####Second step. city/town, crude/st#####
    f_geo_city_crude <- function(data_geo){
      t1 <- aggregate(cbind(index,population)~cause_name+Year+CountyMOI_ID, data_geo, sum)
      t1$rate <- t1$index*100000/t1$population
      return(t1)
    }
    f_geo_town_crude <- function(data_geo){
      t1 <- aggregate(cbind(index,population)~cause_name+Year+TownshipMOI_ID, data_geo, sum)
      t1$rate <- t1$index*100000/t1$population
      return(t1)
    }
    f_geo_city_st <- function(data_geo){
      t1 <- aggregate(cbind(index,population)~cause_name+Year+CountyMOI_ID+Age_group+stand_percent, 
                      data_geo, sum)
      t1$rate <- t1$index*100000*t1$stand_percent/t1$population
      t2 <- aggregate(rate~cause_name+Year+CountyMOI_ID, 
                      t1, sum)
      return(t2)
    }
    f_geo_town_st <- function(data_geo){
      t1 <- aggregate(cbind(index,population)~cause_name+Year+TownshipMOI_ID+Age_group+stand_percent, 
                      data_geo, sum)
      t1$rate <- t1$index*100000*t1$stand_percent/t1$population
      t2 <- aggregate(rate~cause_name+Year+TownshipMOI_ID, 
                      t1, sum)
      return(t2)
    }
    
    f_geo_crude <- function(data_geo){
      ft <- switch(input$geo_geo, "Town" = f_geo_town_crude, "City" = f_geo_city_crude)
      t1<-ft(data_geo)
      return(t1)
    }
    f_geo_st <- function(data_geo){
      ft <- switch(input$geo_geo, "Town" = f_geo_town_st, "City" = f_geo_city_st)
      t1<-ft(data_geo)
      return(t1)
    }
    
    f_geo_s <- switch(input$geo_stand, "Crude" = f_geo_crude, "Age-standardized" = f_geo_st)
    
    tmp <- f_geo_s(tmp)
    
    quan <- quantile(tmp$rate, probs = seq(0,1,0.15))
    tmp$rank <- "0"
    for (i in 1:nrow(tmp)){
      x <- tmp$rate[i]
      if (x>=quan[1]&x<quan[2]){
        tmp$rank[i] <- "1"
      }
      else if (x>=quan[2]&x<quan[3]){
        tmp$rank[i] <- "2"
      }
      else if (x>=quan[3]&x<quan[4]){
        tmp$rank[i] <- "3"
      }
      else if (x>=quan[4]&x<quan[5]){
        tmp$rank[i] <- "4"
      }
      else if (x>=quan[5]&x<quan[6]){
        tmp$rank[i] <- "5"
      }
      else if (x>=quan[6]&x<quan[7]){
        tmp$rank[i] <- "6"
      }
      else if (x>=quan[7]){
        tmp$rank[i] <- "7"
      }
    }
    
    f_graph_city <- function(data_t1){
      t1 <- data_t1[,c("CountyMOI_ID", "cause_name", "rank")]
      colnames(t1) <- c("area","cause_name","rank")
      t2 <- left_join(bor_cty,t1)
      return(t2)
    }
    f_graph_town <- function(data_t1){
      t1 <- data_t1[,c("TownshipMOI_ID", "cause_name", "rank")]
      colnames(t1) <- c("area","cause_name","rank")
      t2 <- left_join(bor_twn,t1)
      return(t2)
    }
    f_graph <- switch (input$geo_geo,"Town" = f_graph_town, "City" = f_graph_city)
    
    tmp <- f_graph(tmp)
    quan_names <-c()
    for (i in 1:6) {
      quan_names[i] <- c(paste(round(quan[i],digits = 2),round(quan[i+1],digits = 2),sep = " - "))
    }
      quan_names[7] <- c(paste(round(quan[7],digits = 2),"Inf",sep = " - "))
    
    ggplot()+
      geom_sf(data = tmp, aes(fill=rank))+
      labs(title=input$geo_cause, subtitle = paste(input$geo_ind,input$geo_year))+
      scale_fill_manual(values = c("#ffff99","#ffd982","#ffbf73","#ff995c","#ff7345", "#d11a0f", "#9e0d08"),
                        labels = quan_names,
                        paste(input$geo_stand,"Rate"))+
    theme(plot.title = element_text(face = "bold",size = rel(2), hjust = 0.45, vjust = 0.5),
          plot.subtitle = element_text(size = rel(1.5), hjust = 0.45, vjust = 0.5),
          legend.title = element_text(face="bold",size = rel(1.5))
          )
    
  })
 
}

shinyApp(ui, server)
