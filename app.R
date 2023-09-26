library(shiny)
library(av)


# Define UI for application that creates and displays a video
ui <- fluidPage(
  tags$style(
    HTML(
      "
      body, html {
        height: 100%;
        margin: 0;
      }
      #matrix-iframe-container {
        position: absolute;
        width: 100%;
        height: 70%; /* Adjust the height as needed */
        top: 0;
        left: 0;
        background-color: white; /* Set background color for upper half */
        z-index: 2;
      }
      #matrix-iframe {
        position: absolute;
        width: 100%;
        height: 70%; /* Adjust the height as needed */
        bottom: 0;
        left: 0;
        border: none;
        z-index: ;
      }
      "
    )
  ),
  tags$iframe(id = "matrix-iframe", src = "matrix_background.html", frameborder = "0"),
  # Application title
  titlePanel("Video Creator App"),
  
  # Set the background color
  style = "background-color: #f5f5f5;",
  
  # Control layout
  fluidRow(
    column(4, offset = 1,
           # Radio buttons for ordering options
           radioButtons("order", "Order by:", 
                        choices = c("Created", "Modified", "Size", "Name"),
                        inline = TRUE)
    ),
    
    column(4,
           # Select folder button
           actionButton('folder', 'Select a Folder')
    )
  ),
  
  fluidRow(
    column(4, offset = 1,
           # Number of frames per second slider
           sliderInput("frames",
                       "Number of Frames per Second:",
                       min = 1,
                       max = 50,
                       value = 10)
    ),
    
    column(4,
           # Create Video button
           shiny::actionButton("go", "Create Video", class = "btn-primary")
    )
  )
)

# Define server logic
server <- function(input, output,session) {
  session$onSessionEnded({
    #print("Stop!")
    stopApp
  })
  
  observeEvent(input$folder,{
    setwd(choose.dir("","Select folder with images"))
  })
  
  image_files <- reactive({
    if (input$folder != 0) {
      details = file.info(list.files(pattern="*.png"))
      
      # Sort the files
      if(input$order == "Modified"){details <- details[with(details, order(as.POSIXct(mtime))), ]}
      if(input$order == "Created"){details <- details[with(details, order(as.POSIXct(ctime))), ]}
      if(input$order == "Size"){details <- details[with(details, order(size)), ]}
      if(input$order == "Name"){details <- details %>% arrange(row.names(details))}
      files = rownames(details)
      return(files)
    }
  })
  
  observeEvent(input$go,{
    if (!is.null(image_files())) {
      # If video is being created, show a progress bar; otherwise, show the video
      if (input$go != 0) {
        shiny::withProgress(
          message = 'Creating video...',
          detail = 'Please wait...',
          value = 0,
          expr = {
            path <- paste0(choose.dir("", "Save Output"), "/output.mp4")
            # Video creation code here
            av::av_encode_video(image_files(), path, framerate = input$frames)
            # Report Succes of Video Creation
            showModal(modalDialog(
              title = span(h3(strong("Your output has been saved!"), style = 'font-size:16px;color:#6cbabf;'))
            ))
            utils::browseURL(path)
          }
        )
        return(NULL)
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
