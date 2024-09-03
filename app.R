# Buscador nheengatu
# André Amorim & Antônio Lessa, 2023

# Bibliotecas
library(rsconnect)
library(readr)
library(shiny)
library(ggplot2)
library(tuneR)
library(shinyjs)
library(audio)
#library(seewave)
library(DT)
library(glue)
library(tidyverse)

# Importar planilha

Nheen <- read_delim("Nheen.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)

Nheen$StartTime <- as.numeric(Nheen$StartTime)
Nheen$EndTime <- as.numeric(Nheen$EndTime)

# Definir índice da ocorrência

ocorrencia <- 1

##################

ui <- fluidPage(
  h3("Buscador"),
  br(),
  h3("\n"),
  textInput("caption", "Digite um termo em nheengatu", ""),
  verbatimTextOutput("value"),
  uiOutput('my_audio'),
  useShinyjs(),
  br(),
  uiOutput('ui'),
  uiOutput('audiotabela'),
  br(),
  br(),
  div(style = "position: relative; bottom: 0;", 
      uiOutput("fonte")),
  div(style = "position: relative; bottom: 0;",  br(),br(),br(),
      actionButton("sobreb", "Sobre o aplicativo")),
  hidden(div(style = "position: relative; bottom: 0;", id='text_div',
      verbatimTextOutput("sobre"))),
  hidden(div(style = "position: relative; bottom: 0;", id='text_div2',
      uiOutput("lic"))),
  br()
)



server <- function(input, output) {
  
  output$value <- renderPrint({
    
    req(input$caption)
    
    termo_b <- paste0("\\b", input$caption, "\\b")
    
    i.word <- if(length(grep(gsub(" ", "", termo_b), Nheen$Label, ignore.case=TRUE, value=F))!=0){
      
      grep(gsub(" ", "", termo_b), Nheen$Label, ignore.case=TRUE, value=F)[ocorrencia]
      
    } else {
      grep(gsub(" ", "", input$caption), Nheen$Label, ignore.case=TRUE, value=F)[ocorrencia]
      
      
    }
    
    
    
    cat("Termo","\n", Nheen[i.word,7][[1]])
    cat("\n", "\n")
    
    cat("Glosa", "\n", Nheen[i.word,9][[1]])
    cat("\n", "\n")
    
    cat("Contexto","\n", Nheen[i.word,11][[1]])
    cat("\n", "\n")
    
    cat("Segmentação do contexto","\n", Nheen[i.word,12][[1]])
    cat("\n", "\n")
    
    cat("Glosa do contexto","\n", Nheen[i.word,13][[1]])
    cat("\n", "\n")
    
    cat("Tradução do contexto","\n", Nheen[i.word,15][[1]])
    cat("\n", "\n")
    
    cat("Arquivo","\n", Nheen[i.word,21][[1]])
    cat("\n", "\n")
    
  })
  
  
  
  # audio player
  output$my_audio <- renderUI({
    
    req(input$caption)
    
    termo_b <- paste0("\\b", input$caption, "\\b")
    
    
    i.word <- if(length(grep(gsub(" ", "", termo_b), Nheen$Label, ignore.case=TRUE, value=F))!=0){
      
      grep(gsub(" ", "", termo_b), Nheen$Label, ignore.case=TRUE, value=F)[ocorrencia]
      
    } else {
      grep(gsub(" ", "", input$caption), Nheen$Label, ignore.case=TRUE, value=F)[ocorrencia]
      
      
    }
    
    

    wvname <- gsub(".TextGrid", ".WAV", Nheen$Arquivo[i.word])
    
    
    
    writeWave(
      readWave(paste0("wav/",wvname),
               from = Nheen$StartTime[i.word],
               to = Nheen$EndTime[i.word], 
               units="seconds"), paste0("wav/","1",wvname))
    
    
    
    tags$audio(id='my_audio_player',
               controls = "controls",
               tags$source(
                 src = markdown:::.b64EncodeFile(paste0("wav/","1",wvname)),
                 type='audio/ogg; codecs=vorbis'))
    
    
    
  })
  
  
  output$ui <- renderUI({
    tags$div(
      dataTableOutput('df'),
      tags$br(),
      verbatimTextOutput('text')
    )
  })
  
  
  
  output$df <- renderDataTable({
    
    req(input$caption)
    
    termo_b <- paste0("\\b", input$caption, "\\b")
    
    i.word<-    if(length(grep(gsub(" ", "", termo_b), Nheen$Label, ignore.case=TRUE, value=F))!=0){
      
      grep(gsub(" ", "", termo_b), Nheen$Label, ignore.case=TRUE, value=F)
      
    } else {
      grep(gsub(" ", "", input$caption), Nheen$Label, ignore.case=TRUE, value=F)
      
      
    }
    
    url <- a("Romania Amerindia", href="https://www.geisteswissenschaften.fu-berlin.de/pt/we05/institut/mitarbeiter/reich/forschung/DFG-projekt-zweisprachige-Prosodie/index.html")
    output$fonte <- renderUI({
      tagList("Fonte dos dados:", url)
      
      
    })
    
    
    Nheen <- Nheen %>%
      mutate(
        serial_no = row_number(), 
        " " = glue('<button id="custom_btn" onclick="Shiny.onInputChange(\'button_id\', \'{serial_no}\')">Ouvir</button>')
      )
    
    
names(Nheen)[c(7,9, 11, 15, 12,13, 21)] <- c("Termo",	"Glosa",	"Contexto", "Tradução do contexto", "Morfemas",	"Glosa do contexto", "Arquivo")
    DT::datatable(
      Nheen[i.word,c(23,7,9, 11, 15, 12, 13, 21)],
      caption = htmltools::tags$caption("Outras ocorrências", style="font-size:20px; color:black"),  
      escape = FALSE,
      selection = 'none',
      options = list(
        searching = FALSE, 
        dom = 'tpi',
        pageLength = 3,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.6/i18n/pt-BR.json')
        
      )
    )
  })
  
  

  
  
  
  # audio player da planilha
  
  output$audiotabela <- renderUI({
    
    req(input$button_id)
    
    i.botao <-  as.numeric(glue('{input$button_id}'))
    
    
    wvname <- gsub(".TextGrid", ".WAV", Nheen$Arquivo[i.botao])
    
    
    
    writeWave(
      readWave(paste0("wav/",wvname),
               from = Nheen$StartTime[i.botao],
               to = Nheen$EndTime[i.botao], 
               units="seconds"), paste0("wav/","1",wvname))
    
    
    
    tags$audio(id='my_audio_player',
               controls = "controls",
               autoplay = TRUE,
               tags$source(
                 src = markdown:::.b64EncodeFile(paste0("wav/","1",wvname)),
                 type='audio/ogg; codecs=vorbis'))
    
    
    
  })
  
  
  observeEvent(input$sobreb, {
    toggle('text_div')
    toggle('text_div2')
 
    output$sobre <-   renderText({
"Descrição do aplicativo:
Este aplicativo é um buscador do nheengatu que apresenta o termo buscado, 
segmentação, glosa, tradução, contexto e áudio do contexto com a palavra buscada. 
Ele realiza buscas em dados do nheengatu que já estão publicados online e apresenta sua fonte, 
localizada na planilha e na parte inferior da página. 
Para mais informações sobre os dados, consulte o link indicado na fonte. 

Estamos aperfeiçoando o aplicativo!
Por questões de glosas ausentes e do processo automático de tokenização, segmentação e alinhamento, 
algumas ocorrências podem não estar devidamente alinhadas e podem divergir em relação aos originais. 
O processo manual de alinhamento dos termos e de preenchimento das glosas ausentes conforme a fonte está em andamento.
Com exceção desses casos de acréscimo de glosas e segmentação das palavras, 
buscamos usar os dados como publicados originalmente, ou seja, 
sem qualquer intervenção de caráter normativo. 

Entre em contato!
Este buscador é resultado de uma série de ferramentas em desenvolvimento para a promoção do estudo, ensino, 
pesquisa e documentação da língua nheengatu. Dados de outras fontes e variedades serão acrescentados no futuro. 
Ficaríamos felizes ao responder dúvidas e receber sugestões, comentários e colaborações. 
Qualquer contribuição será muito bem-vinda!

Autoria e concepção do aplicativo:
André Amorim e Antônio Lessa. 

Contato: 
yrl_apps@outlook.com
"

 
      
    })
    

    
    url_licença <- a("CC BY-NC-SA 4.0.", 
                     href="https://creativecommons.org/licenses/by-nc-sa/4.0/deed.pt_BR")
    output$lic <- renderUI({
      tagList("Licença:", url_licença)
      
      
    })
      
      
    })
    
    

  
 
}

# See above for the definitions of ui and server


shinyApp(ui = ui, server = server)







