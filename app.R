# Author:       Mathias Schindler
# Date:         29-09-2020
# Title:        Make shiny dashboard for portfolio
# Description:  Constructs shiny dashboard to illustrate marriage patterns for 
#                natives and non-Western immigrants, respectively.
#
#*****************************************************************************

# Preamble
{
  library(shiny)   
  library(tidyverse)
  library(readxl)
  library(scales) #<- for nice y- and x-axis scales
  #setwd("/Users/mathiasschindler/Library/Mobile Documents/com~apple~CloudDocs/Arbejde/_RFF/R_scripts_RFF/figur1_ny_eng") #<- comment out b4 upload to shinyapps.io
}

#Clear global environment
rm(list = ls())


# Load data ----
{
#18-23 y.o.  
  #immigrants
  data_fig1_1823_indv <- read_excel("data/Figur 1_1823_2.xlsx", sheet = 1)
  data_fig1_1823_indv <- select(data_fig1_1823_indv, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_1823_indv)
  
  #Danes
  data_fig1_1823_dk <- read_excel("data/Figur 1_1823_2.xlsx", sheet = 2)
  data_fig1_1823_dk <- select(data_fig1_1823_dk, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_1823_dk)
  
#24-28 y.o.  
  #immigrants
  data_fig1_2428_indv <- read_excel("data/Figur 1_2428.xlsx", sheet = 1)
  data_fig1_2428_indv <- select(data_fig1_2428_indv, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_2428_indv)
  
  #Danes
  data_fig1_2428_dk <- read_excel("data/Figur 1_2428.xlsx", sheet = 2)
  data_fig1_2428_dk <- select(data_fig1_2428_dk, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_2428_dk)  
  
#29-32 y.o.  
  #immigrants
  data_fig1_2932_indv <- read_excel("data/Figur 1_2932.xlsx", sheet = 1)
  data_fig1_2932_indv <- select(data_fig1_2932_indv, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_2932_indv)
  
  #Danes
  data_fig1_2932_dk <- read_excel("data/Figur 1_2932.xlsx", sheet = 2)
  data_fig1_2932_dk <- select(data_fig1_2932_dk, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_2932_dk)
  
#18-32 y.o.
  #immigrants
  data_fig1_1832_indv <- read_excel("data/Figur 1_1832.xlsx", sheet = 1)
  data_fig1_1832_indv <- select(data_fig1_1832_indv, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_1832_indv)
  
  #Danes
  data_fig1_1832_dk <- read_excel("data/Figur 1_1832.xlsx", sheet = 2)
  data_fig1_1832_dk <- select(data_fig1_1832_dk, c("year", "giftkvinde", "giftmand", "giftalle"))
  head(data_fig1_1832_dk)
}


# Create ui ----
{
  ui <- fluidPage(
    titlePanel("Marraige Patterns for Non-Western Immigrants and Natives in Denmark, 1994-2018")
    ,
    sidebarLayout(
      sidebarPanel(
        helpText("Please choose ethnicity, sex, age group and time period to be illustrated in figure on the right")
        ,
        radioButtons(inputId = "etno", label = "Ethnicity", choices = c("Non-Western Migrants (1. and 2. Generation)" = 1, "Natives" = 2))
        ,
        checkboxGroupInput(inputId = "gender", label="Gender",
                           choices = c("Both Sexes" = "giftalle", "Men" = "giftmand", "Women" = "giftkvinde"), selected = "giftalle")
        ,
        selectInput(inputId = "age", label = "Age Group", choices = c("18-32" = 4, "18-23" = 1, "24-28" = 2, "29-32" = 3), selected = 4 )
        ,
        sliderInput("year", label = "Time Period", min = 1994, max = 2018, value = c(1994, 2018), sep = "", step = 1)
      )
      ,
      mainPanel(
        textOutput("text")
        ,
        plotOutput("plot")
        #,
        #tableOutput("test_head"), tableOutput("test_tail"), 
        , 
        uiOutput("text_under")
      )
    )
  )
}  


#Vectors with names for choice variables
  gender_vec <- c("Both Sexes" = "giftalle", "Men" = "giftmand", "Women" = "giftkvinde")
  gender_vec_leg <- c("Both Sexes" = "giftalle", "Men" = "giftmand", "Women" = "giftkvinde")
  age_vec <- c("18-32" = 4, "18-23" = 1, "24-28" = 2, "29-32" = 3)
  etno_vec <- c("Non-Western Immigrants" = 1, "Natives" = 2)

  
# Create server function ----
{
  server <- function(input, output){
    
    #1) Create text to be displayed above graph
    output$text <- renderText({
      
      if(length(input$gender) == 1){
        paste("You have chosen to illustrate share of", names(age_vec)[age_vec == input$age], "year-old married", names(etno_vec)[etno_vec == input$etno], 
              "(", names(gender_vec)[gender_vec == input$gender], ") in the time period", input$year[1], "-", input$year[2], "in Denmark:")
      } else if(length(input$gender) == 2){
        paste("You have chosen to illustrate share of", names(age_vec)[age_vec == input$age], "year-old married", names(etno_vec)[etno_vec == input$etno], 
              "(", names(gender_vec)[gender_vec == input$gender[1]], "and", names(gender_vec)[gender_vec == input$gender[2]], ")", "in the time period", 
              input$year[1], "-", input$year[2], "in Denmark:")
      } else if(length(input$gender) == 3){  
        paste("You have chosen to illustrate share of", names(age_vec)[age_vec == input$age], "year-old married", names(etno_vec)[etno_vec == input$etno], 
              "(", names(gender_vec)[gender_vec == input$gender[1]], "and", names(gender_vec)[gender_vec == input$gender[2]], "and", 
              names(gender_vec)[gender_vec == input$gender[3]], ")", "in the time period", input$year[1], "-", input$year[2], "in Denmark:")
      } else {
        paste("You have chosen not to visualize any data.")
      }
    })
    
    #2) Create plot to displayed underneath text
    output$plot <- renderPlot({
      
      #New dataset with restrictions specified in UI  
      if(input$age == 1 & input$etno == 1){
        data_t <- data_fig1_1823_indv[data_fig1_1823_indv[,"year"] >= input$year[1] & data_fig1_1823_indv[,"year"] <= input$year[2], c("year",input$gender)]
      } else if (input$age == 1 & input$etno == 2){
        data_t <- data_fig1_1823_dk[data_fig1_1823_dk[,"year"] >= input$year[1] & data_fig1_1823_dk[,"year"] <= input$year[2], c("year", input$gender)]
      } else if (input$age == 2 & input$etno == 1){
        data_t <- data_fig1_2428_indv[data_fig1_2428_indv[,"year"] >= input$year[1] & data_fig1_2428_indv[,"year"] <= input$year[2], c("year", input$gender)]
      } else if (input$age == 2 & input$etno == 2){
        data_t <- data_fig1_2428_dk[data_fig1_2428_dk[,"year"] >= input$year[1] & data_fig1_2428_dk[,"year"] <= input$year[2], c("year", input$gender)]
      } else if (input$age == 3 & input$etno == 1){
        data_t <- data_fig1_2932_indv[data_fig1_2932_indv[,"year"] >= input$year[1] & data_fig1_2932_indv[,"year"] <= input$year[2], c("year", input$gender)]
      } else if (input$age == 3 & input$etno == 2){
        data_t <- data_fig1_2932_dk[data_fig1_2932_dk[,"year"] >= input$year[1] & data_fig1_2932_dk[,"year"] <= input$year[2], c("year", input$gender)]
      } else if (input$age == 4 & input$etno == 1){
        data_t <- data_fig1_1832_indv[data_fig1_2932_indv[,"year"] >= input$year[1] & data_fig1_1832_indv[,"year"] <= input$year[2], c("year", input$gender)]
      } else if (input$age == 4 & input$etno == 2){
        data_t <- data_fig1_1832_dk[data_fig1_2932_dk[,"year"] >= input$year[1] & data_fig1_1832_dk[,"year"] <= input$year[2], c("year", input$gender)]
      } else {
      }
      
      #Plot the data:
      if(length(input$gender) == 1){
        ggplot(data = data_t) + 
          geom_line(aes_string(x = "year", y = input$gender[1]), color = "darkorange", size = 1.25) +
          scale_y_continuous(name = "Share Married", breaks = pretty_breaks(), labels= scales::percent_format(accuracy = 1)) +
          scale_x_continuous(name = "Year", breaks = seq(from = input$year[1], to = input$year[2], by = 2)) +
          theme_bw(base_size = 16)
      } else if(length(input$gender) == 2){
        ggplot(data = data_t) + 
          geom_line(aes_string(x = "year", y = input$gender[1], color = '"darkorange"'), size = 1.25) + 
          geom_line(aes_string(x = "year", y = input$gender[2], color = '"darkorange2"'), size = 1.25) +
          scale_y_continuous(name = "Share Married", breaks = pretty_breaks(), labels= scales::percent_format(accuracy = 1)) +
          scale_x_continuous(name = "Year", breaks = seq(from = input$year[1], to = input$year[2], by = 2)) +
          scale_colour_manual(name = 'Gender', values =c('darkorange'='darkorange','darkorange2'='darkorange2'), 
                              labels = c(names(gender_vec_leg)[gender_vec_leg == input$gender[1]],
                                         names(gender_vec_leg)[gender_vec_leg == input$gender[2]])) + 
          theme_bw(base_size = 16) + 
          theme(axis.text.x = element_text(angle = 45, vjust  =0.75, hjust = 0.75))
      } else if(length(input$gender) == 3){
        ggplot(data = data_t) + 
          geom_line(aes_string(x = "year", y = input$gender[1], color = '"darkorange"'), size = 1.25) + 
          geom_line(aes_string(x = "year", y = input$gender[2], color = '"darkorange2"'), size = 1.25) + 
          geom_line(aes_string(x = "year", y = input$gender[3], color = '"darkorange4"'), size = 1.25) +
          scale_y_continuous(name = "Share Married", breaks = pretty_breaks(), labels= scales::percent_format(accuracy = 1)) +
          scale_x_continuous(name = "Year", breaks = seq(from = input$year[1], to = input$year[2], by = 2)) +
          scale_colour_manual(name = 'Gender', values =c('darkorange'='darkorange','darkorange2'='darkorange2', 'darkorange4' = 'darkorange4'), 
                              labels = c(names(gender_vec_leg)[gender_vec_leg == input$gender[1]],
                                         names(gender_vec_leg)[gender_vec_leg == input$gender[2]], 
                                         names(gender_vec_leg)[gender_vec_leg == input$gender[3]])) +
          theme_bw(base_size = 16) + 
          theme(axis.text.x = element_text(angle = 45, vjust  =0.75, hjust = 0.75))
      } else {
      }
    })
    
    #3) Display link to script under plot
    git_url <- a("GitHub repo", href="https://github.com/MathiasSchindlerCPH/shiny-fig-rff/blob/main/app.R")
    output$text_under <- renderUI({tagList("To inspect script used to construct this dashboard please refer to the designated ", git_url,".")})
  }
}


#Run the app ----
shinyApp(ui = ui, server = server)
