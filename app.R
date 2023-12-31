library(shiny)
library(shinyWidgets)
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
           #radioButtons("order", "Order by:", 
          #              choices = c("Created", "Modified", "Size", "Name"),
          #              inline = TRUE)
           radioGroupButtons(
             inputId = "order",
             label = "Order by:",
             choices = c("Created", "Modified", "Size", "Name"),
             status = "primary",
             checkIcon = list(
               yes = icon("ok", 
                          lib = "glyphicon"),
               no = icon("remove",
                         lib = "glyphicon"))
           )
    ),
    
    column(4,
           # Select folder button
           #actionButton('folder', 'Select a Folder'),
           actionBttn("folder", "Select a Folder", style = "bordered", color = "success", icon = icon("file")),
           radioGroupButtons("data_format", "File format", choices = c("jpg", "jpeg", "png"), 
                             status = "primary",
                             checkIcon = list(
                               yes = icon("ok", 
                                          lib = "glyphicon"),
                               no = icon("remove",
                                         lib = "glyphicon")))
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
           shiny::actionButton("go", "Create Video", class = "btn-danger")
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
      details = file.info(list.files(pattern=paste0("*.", input$data_format)))
      
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
            ##av::av_encode_video(image_files(), path, framerate = input$frames)
            video_function <- function(images, path, frames){
              tryCatch(
                #try to do this
                {
                  av::av_encode_video(images, path, framerate = frames)
                  showModal(modalDialog(
                    title = span(h3(strong("Your output has been saved!"), style = 'font-size:16px;color:#6cbabf;'))))
                },
                #if an error occurs, tell me the error
                error=function(e) {
                  showModal(modalDialog(
                    title = span(h3(strong("An Error Occurred!"), 
                                    style = 'font-size:16px;color:#6cbabf;')),paste(e)))
                  #message('An Error Occurred')
                  
                },
                #if a warning occurs, tell me the warning
                warning=function(w) {
                  message('A Warning Occurred')
                  print(w)
                  return(NA)
                }
              )
            }
            video_function(image_files(), path, input$frames)
            # Report Succes of Video Creation
            try(utils::browseURL(path))
          }
        )
        return(NULL)
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
