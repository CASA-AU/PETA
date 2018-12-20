#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
suppressPackageStartupMessages(library(dplyr))
library(stringr)

### Constants

up_arrow <- "\u2191"
dn_arrow <- "\u2193"
up_parrow <- "\u21C8"
dn_parrow <- "\u21CA"
ne_arrow <- "\u2197"
se_arrow <- "\u2198"
up_circle <- "\u25E0"
dn_circle <- "\u25E1"

### Read data

projDir <- "/home/henry_m/ShinyApps/PETA"

ui <- fluidPage(
   
   # Application title
   titlePanel("Trend Monitoring Report"),
   
   selectInput(
     'iteration',
     label = "Select PETA run:",
     choices = c("", basename(dirname(list.files(path = "outputs", pattern = "Output.csv", full.names = TRUE, recursive = TRUE))))
   ),
   
   tabsetPanel(
     tabPanel(
       "PETA",
       dataTableOutput('peta')
     ),
     tabPanel(
       "Charts",
       imageOutput('chart')
     ),
     tabPanel(
       "Related trends",
       dataTableOutput('related')
     )
   )
)

server <- function(input, output) {

   output$peta <- renderDataTable({
     
     data <- formatted()
     
     if (is.null(data)) {
       shiny::showNotification(
         "Please select an option from the drop-down menu.",
         closeButton = FALSE,
         type = "message"
       )
       return(NULL)
     }
     
     datatable(
       data,
       options = list(serverSide = TRUE, pageLength = 100),
       rownames = FALSE,
       filter = "top",
       selection = "single"
      ) %>%
       formatRound('Average Count') %>% 
       formatStyle(
         columns = which(colnames(data) == 'Last Special'),
         backgroundColor = styleEqual(
           levels = c(up_parrow, up_arrow, dn_arrow, dn_parrow),
           values = c('#E26B0A','#FFFF00','#538DD5','#B1A0C7')
         )
       ) %>% 
       formatStyle(
         columns = which(
           colnames(data) %in% c(
             'Trend',
             'Last Special',
             'Second Last Special',
             'Over-dispersion',
             'Excess Zeros',
             'Change in Level',
             'Nonlinearity',
             'Seasonality'
           )
         ),
         textAlign = 'center'
       )
   })
   
   output$chart <- renderImage({
     selectedRow <- input$peta_rows_selected
     
     if (length(selectedRow)) {
       print(paste("Selected row number:", selectedRow))
     } else {
       print("No rows Selected")
       return(
         list(
           src = 'Maldives.jpg',
           width = 800,
           height = 532,
           alt = 'Maldives')
       )
     }
     
     selected <- formatted()[selectedRow, ]
     
     print(paste("Selected row variable & value:", paste0(selected$`Analysis Variable`, '-', selected$`Specific Variable`)))
     
     return(
       list(
         src = normalizePath(path = paste0(projDir, '/outputs/', input$iteration, '/', selected$`Analysis Variable`, '-', selected$`Specific Variable`, '.svg')),
         contentType = 'image/svg+xml',
         width = 1200,
         height = 500,
         alt = paste(selected$`Analysis Variable`, '-', selected$`Specific Variable`)
       )
     )
   }, deleteFile = FALSE)
   
   output$related <- renderDataTable({
     
     selectedRow <- input$peta_rows_selected
     
     if (length(selectedRow)) {
       print(paste("Selected row number:", selectedRow))
     } else {
       print("No rows Selected")
     }
     
     selected <- formatted()[selectedRow, ]
     
     fanalysisvar <- str_split(selected$`Analysis Variable`, pattern = "_by_")[[1]]
     svar <- str_split(selected$`Specific Variable`, pattern = "_._")[[1]]

     print(paste("Selected row variable:", paste(fanalysisvar, collapse = ", ")))
     print(paste("Selected row value:", paste(svar, collapse = ", ")))
     
     if (length(fanalysisvar) == 2) {
       # The individual variables that make up the combination
       # Include the combination
       relatedSelection <- formatted()[
         (rownames(formatted()) == selectedRow) |
         (formatted()$`Analysis Variable` == fanalysisvar[1] & formatted()$`Specific Variable` == svar[1]) |
           (formatted()$`Analysis Variable` == fanalysisvar[2] & formatted()$`Specific Variable` == svar[2]), ]
     } else {
       # Any combination of variables that includes the selected variable
       relatedSelection <- formatted()[grepl(fanalysisvar, formatted()$`Analysis Variable`, fixed = TRUE) & grepl(svar, formatted()$`Specific Variable`, fixed = TRUE), ]
     }
     
     datatable(
       relatedSelection,
       options = list(serverSide = TRUE, pageLength = 100),
       rownames = FALSE,
       filter = "top",
       selection = "none"
     ) %>%
       formatRound('Average Count') %>% 
       formatStyle(
         columns = which(colnames(formatted()) == 'Last Special'),
         backgroundColor = styleEqual(
           levels = c(up_parrow, up_arrow, dn_arrow, dn_parrow),
           values = c('#E26B0A','#FFFF00','#538DD5','#B1A0C7')
         )
       ) %>% 
       formatStyle(
         columns = which(
           colnames(formatted()) %in% c(
             'Trend',
             'Last Special',
             'Second Last Special',
             'Over-dispersion',
             'Excess Zeros',
             'Change in Level',
             'Nonlinearity',
             'Seasonality'
           )
         ),
         textAlign = 'center'
       )
   })
   
   filename <- reactive({
     paste0(projDir, '/outputs/', input$iteration, "/Output.csv")
   })
   
   formatted <- reactive({
     if (!file.exists(filename())) {
       return(NULL)
     }
     
     output <- read.csv(filename(), stringsAsFactors = FALSE)
     
     return(
       mutate(
         output,
         `Model Fitted` = case_when(
           ModelFitted == 1 ~ "No, All Zero",
           ModelFitted == 2 ~ "No, Counts Below Threshold and Too Many Zeros",
           ModelFitted == 3 ~ "No, Counts Below Threshold",
           ModelFitted == 4 ~ "No, Too Many Zeros",
           ModelFitted == 5 ~ "Yes",
           TRUE ~ ""
         ),
         Exposure = if_else(
           is.na(Exposure) | nchar(Exposure) == 0 | Exposure == 0,
           "",
           "Yes"
         ),
         Trend = case_when(
           Trend == "up" ~ ne_arrow,
           Trend == "down" ~ se_arrow,
           TRUE ~ ""
         ),
         `Last Special` = case_when(
           LastSpecial == 1 ~ up_parrow,
           LastSpecial == 2 ~ up_arrow,
           LastSpecial == 3 ~ "",
           LastSpecial == 4 ~ dn_arrow,
           LastSpecial == 5 ~ dn_parrow,
           TRUE ~ ""
         ),
         `Second Last Special` = case_when(
           SecondLastSpecial == 1 ~ up_parrow,
           SecondLastSpecial == 2 ~ up_arrow,
           SecondLastSpecial == 3 ~ "",
           SecondLastSpecial == 4 ~ dn_arrow,
           SecondLastSpecial == 5 ~ dn_parrow,
           TRUE ~ ""
         ),
         `Last Quarter Changes Previous Model` = case_when(
           LastQuarterChangesPattern %in% 1:4 ~ "Had Trend + LS",
           LastQuarterChangesPattern %in% 5:8 ~ "Had LS",
           LastQuarterChangesPattern %in% c(10,12) ~ "Had Trend",
           LastQuarterChangesPattern %in% c(13,15) ~ "Was Constant",
           TRUE ~ ""
         ),
         `Under-dispersion` = if_else(
           is.na(Underdispersion) | nchar(Underdispersion) == 0 | Underdispersion == 0,
           "",
           "^^^"
         ),
         `Over-dispersion` = if_else(
           is.na(Overdispersion) | nchar(Overdispersion) == 0 | Overdispersion == 0,
           "",
           "/\\/\\/\\"
         ),
         `Excess Zeros` = if_else(
           is.na(ExcessZeros) | nchar(ExcessZeros) == 0 | ExcessZeros == 0,
           "",
           "000"
         ),
         Outliers = if_else(
           is.na(Outliers) | nchar(Outliers) == 0 | Outliers == 0,
           0L,
           Outliers
         ),
         `Change in Level` = case_when(
           ChangeInLevel == 1 ~ "_—",
           ChangeInLevel == 3 ~ "—_",
           TRUE ~ ""
         ),
         Nonlinearity = case_when(
           Nonlinearity == "concave" ~ up_circle,
           Nonlinearity == "convex" ~ dn_circle,
           TRUE ~ ""
         ),
         Seasonality = if_else(
           is.na(Seasonality) | nchar(Seasonality) == 0 | Seasonality == 0,
           "",
           "~~"
         ),
         `Over-dispersion Removed` = case_when(
           OverdispersionRemoved == 1 ~ "C",
           OverdispersionRemoved == 2 ~ "N",
           OverdispersionRemoved == 3 ~ "S",
           OverdispersionRemoved == 12 ~ "C N",
           OverdispersionRemoved == 13 ~ "C S",
           OverdispersionRemoved == 23 ~ "N C",
           OverdispersionRemoved == 123 ~ "C N S",
           TRUE ~ ""
         )
       ) %>%
       select(
         AnalysisVariable,
         SpecificVariable,
         AverageCount,
         LastCount,
         `Model Fitted`,
         Exposure,
         Trend,
         `Last Special`,
         `Second Last Special`,
         `Last Quarter Changes Previous Model`,
         `Under-dispersion`,
         `Over-dispersion`,
         `Excess Zeros`,
         Outliers,
         `Change in Level`,
         Nonlinearity,
         Seasonality,
         `Over-dispersion Removed`
       ) %>% 
       rename(
         `Analysis Variable` = AnalysisVariable,
         `Specific Variable` = SpecificVariable,
         `Average Count` = AverageCount,
         `Last Count` = LastCount
       ) %>% 
       mutate(
         `Model Fitted` = factor(`Model Fitted`),
         Trend = factor(Trend),
         `Last Special` = factor(`Last Special`),
         `Second Last Special` = factor(`Second Last Special`),
         `Last Quarter Changes Previous Model` = factor(`Last Quarter Changes Previous Model`),
         `Under-dispersion` = factor(`Under-dispersion`),
         `Over-dispersion` = factor(`Over-dispersion`),
         `Excess Zeros` = factor(`Excess Zeros`),
         `Change in Level` = factor(`Change in Level`),
         Nonlinearity = factor(Nonlinearity),
         Seasonality = factor(Seasonality),
         `Over-dispersion Removed` = factor(`Over-dispersion Removed`)
       )
     )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

