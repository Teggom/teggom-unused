library(shiny)
ui <- fluidPage(
  titlePanel("Gather Data on input .Osu File"),
  fileInput("osu_file", "Choose a .osu file",
            multiple = F,
            accept = c(".osu")),
  verbatimTextOutput("out"),
  actionButton(inputId = "newpage", label = "Run")
)
server <- function(input, output){
  runready <- reactiveValues(ok = F)
  observeEvent(input$newpage, {
    if(is.null(input$osu_file)){
      # do nothing
    } else {
      shinyjs::disable("newpage")
      runready$ok <- T
    }
  })
  analysis <- reactive({
    if(!runready$ok){
      return(NULL)
    } else {
      DF <- input$osu_file
      Text_File <- as.vector(read.table(DF$datapath, sep = "\n", quote = "", fill=F)$V1)
      Text_File=Text_File[!grepl("\\/\\/", Text_File)]
      
      # Get where Hit Objects start
      Hit_Object_Line <- match(T, grepl("\\[HitObjects\\]", Text_File))
      Version_Line <- match(T, grepl("Version\\:", Text_File))
      Version_C <- gsub("Version\\:\\s*", "", Text_File[Version_Line])
      #Text_File[Version_Line] <- paste("Version:", Version)
      # Split into Notes and HeadingData 
      Heading_Data <- Text_File[1:(Hit_Object_Line-1)]
      Notes <- Text_File[(Hit_Object_Line+1):length(Text_File)]
      
      Timing_Line <- match(T, grepl("\\[TimingPoints\\]", Heading_Data))
      # Get the raw time line
      Time_Data <- Heading_Data[Timing_Line+1]
      # Milliseconds per beat
      MSPB <- as.numeric(gsub("\\,.*$", "", gsub("^\\-?\\d*\\.?\\d*\\,", "", Time_Data)))
      
      # 1MSPB => 44.1 Ints 
      # seconds per beat
      SPB = MSPB/1000 # seconds per beat
      # Hz per beat 
      # this is how many raw integers in the song pass each beat
      Hz_per_Beat = SPB*44100 #sec/beat*ints/sec = ints/beat
      #Beats Per Minute/Second
      BPM = 60/SPB
      BPS = 1/SPB
      res <- c("File read successfully\n",
               paste("BPM:", BPM, "\n"),
               paste("Total Notes:", length(Notes), "\n"),
               paste("Version:", Version_C, "\n"))
      res
      
    }
  })
  
  output$out <- renderText({
    analysis()
  })
  
}
shinyApp(ui=ui, server = server)