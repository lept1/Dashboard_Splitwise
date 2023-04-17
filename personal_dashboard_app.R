library(shiny)
library(arrow)
library(magrittr)
library(tidyverse)
library(ggplot2)


ui <- fluidPage(theme = shinythemes::shinytheme("united"),
                titlePanel("Splitwise Dashboard"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput("myfileinput", "Please choose a csv File", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    selectInput('myselectinput','Scegli categoria di spesa', ""),
                    selectInput('selectperiod','Calcola a partire da', ""),
                    textOutput("total")
                  ),
                  mainPanel(tabsetPanel(
                    tabPanel('Risultati',
                             textOutput("total_categorical"),
                             plotOutput("plot")
                             ),
                    tabPanel('Sede Roma',
                             textOutput("total_roma"),
                             plotOutput("plot_roma")
                    ),
                    tabPanel('Tabella',DT::dataTableOutput("mytable"))
                    )
                  )
                )
)


server <- function(input, output, session) {
  
  #Reactive to store loaded data
  reactives <- reactiveValues(
    
    mydata = NULL
    
  )
  
  #Observe file being selected
  observeEvent(input$myfileinput, {
    
    #Store loaded data in reactive
    reactives$mydata <- read.csv(file = input$myfileinput$datapath)%>%
      mutate(across(everything(),tolower)) %>%
      select(Data,Descrizione,Categorie,Costo,Valuta
      )%>%
      mutate(Costo=as.numeric(Costo)
      )%>%
      mutate(Data=lubridate::as_date(Data)
      )%>%
      filter(Categorie!='pagamento' & Categorie!='affitto')%>%
      mutate(Descrizione=
               case_when(
                 Descrizione!='sede roma' & Categorie=='autobus/treno' ~ 'biglietti gite',
                 TRUE ~ Descrizione)
      )%>%
      mutate(Descrizione=
               case_when(
                 Categorie=='parcheggio' & Descrizione!='sede roma' ~ 'parcheggio gite',
                 TRUE ~ Descrizione)
      )%>% 
      mutate(Mese=format(as.Date(Data), "%Y-%m"))
    
    #Update select input
    updateSelectInput(session, inputId = 'myselectinput', label = 'Scegli categoria di spesa', choices  = unique(reactives$mydata$Categorie))
    updateSelectInput(session, inputId = 'selectperiod', label = 'Calcola a partire da', choices  = unique(reactives$mydata$Mese))
  })
  
  #Data table
  output$mytable <- DT::renderDataTable({
    validate(
      need(input$myfileinput != "", "Caricare lista dati")
    )
    reactives$mydata
  })
  
  output$total <- renderText({
    validate(
      need(input$myfileinput != "", "Caricare lista dati")
    )
    tot<-reactives$mydata%>%
      dplyr::summarise(Totale=sum(Costo))%>%
      pull(Totale)
    
    string<- glue::glue('Totale spese: {tot} €')
    string
  })
  
  output$total_categorical <- renderText({
    validate(
      need(input$myfileinput != "", "Caricare lista dati")
    )
    tot<-reactives$mydata%>%
      filter(Categorie==input$myselectinput)%>%
      summarise(Totale=sum(Costo))%>%
      pull(Totale)
    
    string<- glue::glue('Totale spese per {input$myselectinput}: {tot} €')
    string
  })
  
  output$plot<-renderPlot({
    validate(
      need(input$myfileinput != "", "Caricare lista dati")
    )
    pp<-reactives$mydata%>%
      group_by(Mese) %>%
      filter(Categorie==input$myselectinput)%>%
      summarise(Totale=sum(Costo))%>%
      ggplot(aes(y = Mese, x = Totale)) +
      geom_bar(stat = 'identity',show.legend = FALSE,fill='steelblue')+
      ylab('Mese')+
      xlab('Totale (eur)')+
      theme_minimal()+
      theme(axis.text = element_text(colour = 'steelblue',
                                     size=10),
            axis.title=element_text(colour = 'steelblue',
                                    size=18))
    pp
  })
  output$total_roma <- renderText({
    validate(
      need(input$myfileinput != "", "Caricare lista dati")
    )
    tot<-reactives$mydata%>%
      filter(Descrizione=='sede roma')%>%
      summarise(Totale=sum(Costo))%>%
      pull(Totale)
    
    string<- glue::glue('Totale spese per {input$myselectinput}: {tot} €')
    string
  })
  
  output$plot_roma<-renderPlot({
    validate(
      need(input$myfileinput != "", "Caricare lista dati")
    )
    pp<-reactives$mydata%>%
      group_by(Mese) %>%
      filter(Descrizione=='sede roma')%>%
      summarise(Totale=sum(Costo))%>%
      ggplot(aes(y = Mese, x = Totale)) +
      geom_bar(stat = 'identity',show.legend = FALSE,fill='steelblue')+
      ylab('Mese')+
      xlab('Totale (eur)')+
      theme_minimal()+
      theme(axis.text = element_text(colour = 'steelblue',
                                     size=10),
            axis.title=element_text(colour = 'steelblue',
                                    size=18))
    pp
  })
  
}

shinyApp(ui = ui, server = server)