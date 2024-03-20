library("shiny")
library("move2")
library("sf")
library("dplyr")

## TODO: make collapse, separator, "\n" work...

shinyModuleUserInterface <- function(id, label) {
  ns <- NS(id)
  tagList(
    titlePanel(h1("Define the track ID",h5("The track ID is what defines a track. If you have downloaded the data from Movebank with the Movebank App, than by default each track will correspond to a deployment. One animal can have several deployments, and mostly it makes sense to separate them into different tracks, as there might be large gaps between these deployments. With this App you can (1) join several tracks into one, e.g. by choosing the individual_local_identifier, which will join all tracks of the same individual into one (keep in mind the possible gaps); or you can (2) split one track into multiple tracks by using an attribute (also those created in another App) which has assigned to each location a season, behavioural state, etc (be aware that this could create an extreme large number of tracks)."))),
    fluidRow(
      column(4,uiOutput(ns('uiAttributeLcurr'))),
      column(4,uiOutput(ns('uiAttributeLnew'))),
      column(3,actionButton(ns("gobttn"),"Apply selected attribute"))
    ),
    fluidRow(
      column(3,textOutput(ns("currID"))),
      column(3,offset=1,textOutput(ns("selecID"))),
      column(3,textOutput(ns("selCol")))
    )
  )
}


shinyModule <- function(input, output, session, data) {
  ns <- session$ns
  data_out <- reactiveVal(data)
  
  output$uiAttributeLcurr <- renderUI({
    trkid <- mt_track_id_column(data)
    selectInput(ns("attributeLcurr"), "Attribute currently defining the tracks", selected=trkid,choices=trkid)
  })
  
  output$uiAttributeLnew <- renderUI({
    trkattrb <- names(mt_track_data(data))
    evntattrb <- names(data)
    attrbs <- c(trkattrb,evntattrb)
    attrbs <- c("",attrbs[order(attrbs)])
    selectInput(ns("attributeLnew"), "Select attribute that will define the track Id", choices=attrbs,selected = "")
  })
  
  output$currID <- renderPrint({cat(paste("CURRENT track IDs (n=",length(unique(mt_track_id(data))),"): ",paste(unique(mt_track_id(data)),collapse=", "), sep="\n"))})
  
  observeEvent(input$attributeLnew,{
    if(input$attributeLnew%in%names(mt_track_data(data))){
      selID <- unique(mt_track_data(data) %>% pull(input$attributeLnew))
      if(is.factor(selID)){selID <- as.character(selID)}
    } 
    if(input$attributeLnew%in%names(data)){
      selID <- unique(data %>% pull(input$attributeLnew))
      if(is.factor(selID)){selID <- as.character(selID)}
    }
    if(input$attributeLnew==""){selID <- NA}
    
    output$selecID <- renderPrint({cat(paste("SELECTED track IDs (n=",length(selID),"): ",paste(selID,collapse = ", "),sep="\n")) })
  })
  
  observeEvent(input$gobttn,{
    if(input$attributeLnew%in%names(mt_track_data(data))){
      dataN <- mt_as_event_attribute(data_out(), input$attributeLnew)
      output$selCol <- renderPrint({
        tryCatch(
          mt_track_id(dataN) <- input$attributeLnew,
          error = function(c){
            c$message <- paste0('"',input$attributeLnew,'"', " cannot be used to define tracks. The attribute defining the tracks needs to be a character or numeric (without units)")
            stop(c)
          })
      })
      data_out(dataN)
    } 
    if(input$attributeLnew%in%names(data)){
      dataN <- data_out()
      output$selCol <- renderPrint({
        tryCatch(
          mt_track_id(dataN) <- input$attributeLnew,
          error = function(c){
            c$message <- paste0('"',input$attributeLnew,'"', " cannot be used to define tracks. The attribute defining the tracks needs to be a character or numeric (without units)")
            stop(c)
          })
      })
      data_out(dataN)
    }
  })
  return(data_out)
}
