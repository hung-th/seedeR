library(shiny)
library(shinydashboard)

library(raster)
#library(rgdal)
library(RStoolbox)
library(ggplot2)
#library(RColorBrewer)
library(ggspatial)
library(ggnewscale)

ssp.list 	  <- c(126, 245, 370, 585)
time.list   <- c("2041-2060", "2061-2080", "2081-2100")
meta        <- read.table("./data/meta.tsv", header = T, sep = "\t")
sp.list     <- unique(meta$Species)
source("./functions/match.R")

ui <- dashboardPage(
  dashboardHeader(title = "seedeR",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Henry (Chief Developer)",
                                 message = "This app is launched for alpha test.",
                                 icon = icon("warning"),
                                 time = "2021-10-30"
                               )),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "Server is running normally",
                                 icon = icon("check-double"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "App is not ready to use",
                                 icon = icon("exclamation-triangle"),
                                 status = "danger"
                               )
                               )
                  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Predict", tabName = "predict", icon = icon("search-location")),
      menuItem("Announcement", tabName = "announcement", icon = icon("bullhorn")),
      menuItem("About", tabName = "about", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              
              fluidRow(
                box(
                  title = "Embargo!",
                  status = "primary",
                  solidHeader = T,
                  width = 12,
                  h2("This is still under alpha development. Not for public use."),
                  br()
                )
              ),
              
              fluidRow(
                box(
                  title = "Welcome!",
                  status = "primary",
                  solidHeader = T,
                  width = 12,
                  "This is what I called 'Fantastic seeds and where to plant them'. Rosewoods, in particularly Dalbergia cochinchinensis and Dalbergia oliveri, are threatened from illegal logging and deforestation. There has been emerging efforts to conserve and restore them. One key component of succesful restoration is the germplasm. Genetically and adaptively diverse germplasm help safeguard their survival. This App can be used to predict the match between future restoration site and the current genetic adaptation.",
                  br()
                )
              ),
              
              
              fluidRow(
                infoBox("Version", "Alpha.2", icon = icon("code-branch")),
                infoBox("Species", length(meta$Species), icon = icon("tree"), color = "green"),
                infoBox("Progress", "Alpha Test in progress", icon = icon("tasks"), color = "purple"),
                infoBox("Last updated", "2021-11-11", icon = icon("calendar-day"), color = "yellow")
              )
        
      ),
      
      tabItem(tabName = "predict",
              
              fluidRow(   # Row 1
                infoBoxOutput("speciesBox"),
                infoBoxOutput("SNP_allBox"),
                infoBoxOutput("SNP_adaptiveBox"),
                infoBoxOutput("samplesBox")
              ),       # Row 1 closed 
              
              fluidRow(   # Row2
              box(
                title = "Inputs",
                status = "primary",
                collapsible = T,
                solidHeader = T,
                selectInput(inputId = "sp", label = "Choose a species", choices = sp.list),
                selectInput(inputId = "ssp", label = "Choose a Shared Socioenomic Pathway (SSP)", choices = ssp.list),
                selectInput(inputId = "time", label = "Choose a Time Period", choices = time.list),
                textInput(inputId = "site.x", label = "Input the Longitude"),
                textInput(inputId = "site.y", label = "Input the Latitude"),
                actionButton(inputId = "go", label = "Run")
              ),
              
              box(
                title = "Output",
                status = "success",
                collapsible = T,
                solidHeader = T,
                p("Right click to save the image."),
                br(),
                plotOutput("p")
              )
              )   # Row 2 closed
              
              ),  # Home tab closed
      
      tabItem(tabName = "about",
              box(
                title = "About the App",
                status = "primary",
                solidHeader = T,
                
                p("This App is part of the Darwin Initiative-funded Conserving Dalbergia project. Based on a population-wide genomic study, the adaptive turnover functions are used to predict the genetic landscape across the entire species range in both present and future climate scenarios. Genetic offset is calculated as the averaged Euclidean distance of allelic turnover between a future spatial point (hypothetical restoration site) and the current landsacpe, using five climate scenarios: BCC-CSM2-MR, CNRM-ESM2-1, IPSL-CM6A-LR, MIROC6, MRI-ESM2-0. The genetic offset is scaled and reversed to predict the adaptive genetic similarity between 0 (most dissimilar) and 1 (most similar). This App is under Alpha test (as of 31 October 2021): the functionality of the App is valid, but the server and its delivery are still under maintenance. Spatial experts and population geneticists will also be consulted before releasing this App to the public Beta test. The App Developers hold no liability in using this App.")
                
              ),
              box(
                title = "About the Developers",
                status = "primary",
                solidHeader = T,
                
                p("The Chief Developer is ", a("Henry Hung", href = "http://hung.studio"), ". Henry is a forest scientist at the Department of Plant Sciences, University of Oxford. His main research concerns the adaptation and genomics of forest trees. He started his research in tropical forests in Southeast Asia. Now he is shifting his focus to temperate forests in Europe and North America. He also conducts research in Wytham and Blenheim in Oxford. He is appointed to the Events Committee of the British Ecological Society and the Forest Ecosystems Specialist Group of the International Union for Conservation of Nature."),
                img(src = "henry_hung.jpg", height = "200px"),
                br(),
                br(),
                br(),
                p("There is no other developer at the moment.")
              )
      ),    # Developer tab closed
      
      tabItem(tabName = "announcement",
              box(
                title = "App Updates",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                icon("calendar"),
                "2021-10-30",
                br(),
                strong("Alpha verison is launched"),
                br(),
                "Major updates: - Predict function is launched. - One new species (Dalbergia cochinchinensis) is added.",
                br(),
                "To be updated in next versions: - Map and threat functions."
              ),
              box(
                title = "News",
                status = "primary",
                solidHeader = T,
                collapsible = T,
                icon("calendar"),
                "2021-10-30",
                br(),
                "Fantastic seeds and where to plant them?"
              )
              
              
              
              ) # Announcement tab closed
      
    )   # Tabs closed
    
  )   # Dashboard body closed 
)  
  

server <- function(input, output){
  
  sp <- reactive(input$sp) 
  
  output$speciesBox <- renderInfoBox({
    infoBox(
      "Species", meta[meta$Species == sp(), "Name"], icon = icon("tree")
    )
  })
  output$SNP_allBox <- renderInfoBox({
    infoBox(
      "Number of all SNPs", meta[meta$Species == sp(), "SNP_all"], icon = icon("wine-glass")
    )
  })
  output$SNP_adaptiveBox <- renderInfoBox({
    infoBox(
      "Number of adaptive SNPs", meta[meta$Species == sp(), "SNP_adaptive"], icon = icon("wine-glass-alt")
    )
  })
  output$samplesBox <- renderInfoBox({
    infoBox(
      "Number of samples", meta[meta$Species == sp(), "Samples"], icon = icon("list-ol")
    )
  })
  
  observeEvent(input$go,{
    
      sp <- input$sp
      ssp <- input$ssp
      time <- input$time
      site.x <- input$site.x
      site.y <- input$site.y
  
      p <- run(sp, ssp, time, site.x, site.y)
      
      if (is.na(p) == F){
        output$p <- renderPlot(p)
        
        observeEvent(p,{
          showModal(modalDialog(
            title = "Finished!",
            "You can now close this dialogue box and save the image."
          ))  
        })
      }
      
  })
}

shinyApp(ui = ui, server = server)
