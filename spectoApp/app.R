#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tuneR)
library(seewave)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Spectogram Generation and Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput("audio", "Upload WAV File",
                accept = ".wav"),
      numericInput("wl", "Window Length (FFT)",
                   value = 512,
                   min = 128,
                   max = 4096, step = 128),
      numericInput("f", "Maximum Frequency",
                   value = 5,
                   min = 0.25,
                   max = 20, step = 0.25),
      uiOutput("timeInput")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("How it works"),
      p("This app will visualize and analyze a wav audio file. User can browse for and
             upload a desired wav file. After it is uploaded, the app will generate a spectogram and a
             data table with some basic metrics of the audio. The user can adjust the maximum frequency being displayed along
             with the time stamps of specific audio elements by adjusting the min and max time options to narrow down the recording.
             Users can also change the windown length to change the frequency and time resolution of the spectogram."),
      plotOutput("spectrogram"),
      dataTableOutput("metrics")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$timeInput <- renderUI({
    req(input$audio)
    wav_file <- readWave(input$audio$datapath)
    duration <- length(wav_file@left)/wav_file@samp.rate
    tagList(
      numericInput("Mint", "Minimum Time",
                   value = 0, min = 0, max = duration - 0.25, step = 0.25),
      numericInput("Maxt", "Maximum Time",
                   value = duration, min = 0.25, max = duration, step = 0.25)
    )
  })
  
  output$spectrogram <- renderPlot({
    req(input$audio, input$Maxt, input$Mint)
    wav_file <- readWave(input$audio$datapath)
    spectro(wav_file,
            wl = input$wl,
            flim = c(0, input$f),
            tlim = c(input$Mint, input$Maxt)
    )
    
    
  })
  
  output$metrics <- renderDataTable({
    req(input$audio)
    wav_file <- readWave(input$audio$datapath)
    dur <- duration(wav_file)
    
    f0 <- fund(wav_file,f = wav_file@samp.rate, plot = FALSE)
    meanF0 <- mean(f0, na.rm = TRUE)
    
    aci <- ACI(wav_file)
    
    en <- env(wav_file, f = wav_file@samp.rate, plot = FALSE)
    db <- 20*log10(en + 1e-6)
    
    entropy <- H(wav_file)
    
    df <- data.frame(DurationSec = round(dur, 2),
                     MeanFundFreq_Hz = round(meanF0, 2),
                     ACI = aci,
                     Max.db = max(db),
                     Min.db = min(db),
                     Mean.db = mean(db),
                     Entropy = entropy
    )
    df
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
