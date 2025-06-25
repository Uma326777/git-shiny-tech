library(shiny)
library(formatters)
library(gt)
library(tidyverse)
library(rlistings)
library(shinythemes)
library(DT)
library(dplyr)
library(stringr)
library(plotly)


ui <-navbarPage(theme= shinytheme("united"), "MS123 Analysis",

               tabPanel("data",
                        mainPanel(
                          tabsetPanel(
                            tabPanel("subject data",
                                     mainPanel(
                                       dataTableOutput("sl")
                                     )
                              
                            ),
                         
                          tabPanel("Adverse Events",
                                   mainPanel(
                                     dataTableOutput("ae")
                                   ))
                          
                        ))),
                tabPanel("Tables",
                        mainPanel(
                        tabsetPanel(
                        tabPanel("Lab table",
                         mainPanel(
                      verbatimTextOutput("Table1"),
                       width=12
                      )) ,
     
                   tabPanel("plot",
                    mainPanel(
                    plotlyOutput("plot1"),
                    width=12
                    ))
      
                  ))
)
)

server <- function(input, output, session){
  
  ex_adsl_pc <- ex_adsl %>%
    rename_with(~ str_to_title(.x), .cols = everything())
  
  output$sl <- renderDT(ex_adsl_pc,
                        options= list(pageLength = 5))
  output$ae <- renderDT(ex_adae, 
                        options= list(pageLength = 5))
  
  data <- reactive({ex_adlb %>%
      group_by(USUBJID,PARAMCD) %>%
      arrange(USUBJID,PARAMCD, AVAL)%>%
      mutate(
        #first.
        MIN = if_else(row_number(AVAL) == 1, "Y", ""),
        #last.
        MAX = if_else(row_number(AVAL) == n(), "Y", "")
      )%>% filter(SUBJID =="id-105")%>% select(USUBJID,PARAMCD,AVAL,AVISIT, MIN, MAX)})
  output$Table1 <- renderPrint({
    lsting <- as_listing(
      df = data(),
      disp_cols = c( "PARAMCD","AVAL", "MIN", "MAX"),
      key_cols = c("USUBJID", "AVISIT"),
      main_title = "Lab listing",
      subtitles = c("Other sub titles1", "Other sub titles2"),
      main_footer = c("Footnote1", "Footnote2"),
      prov_footer = "Source:ADLB, data:"
    )
    lsting
  })
  output$plot1<-  renderPlotly({
    plot_ly(data = ex_adlb, x = "ADY", y = "AVAL", color = "PARAMCD", type = "scatter", mode = "markers")%>%
      layout(
        title = "Customized Scatter Plot",
        xaxis = list(title = input$x),
        yaxis = list(title = input$y)
      )
  })
}
shinyApp(ui, server)
