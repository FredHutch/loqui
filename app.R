library(shiny)
library(ari)

ui <- fluidPage(
  titlePanel("Loqui: Make videos"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("url", 
                label = "Google Slides URL",
                value = "",
                placeholder = "URL"),
      actionButton("go", "Generate")
    ),
    mainPanel(h4("Rendered Video (mp4)"),
              uiOutput("video"),
              uiOutput("download"))
  )
)

server <- function(input, output, session) {
  
  res <- eventReactive(input$go, {
    # text
    pptx_path <- download_gs_file(input$url, out_type = "pptx")
    pptx_notes_vector <- pptx_notes(pptx_path)
    
    # images
    pdf_path <- download_gs_file(input$url, out_type = "pdf")
    image_path <- pdf_to_pngs(pdf_path)
    
    # create video
    ari_spin(images = image_path, 
             paragraphs = pptx_notes_vector,
             service = "coqui",
             output = "www/ari-video.mp4")
  })
  
  output$video <- renderUI({
    video_path <- attr(res(), "outfile")
    tags$video(src = basename(video_path), 
               type = "video/mp4",
               height ="400px", 
               width="400px",
               autoplay = TRUE, 
               controls = TRUE)
  })
  
  observeEvent(input$go, {
    output$download <- renderUI({
      downloadButton("download_button")
    })
  })
  # https://stackoverflow.com/questions/33416557/r-shiny-download-existing-file
  output$download_button <- downloadHandler(
    filename = "video.mp4",
    content = function(file) {
      file.copy("www/ari-video.mp4", file)
    },
    contentType = "video/mp4"
  )
  
  
}

shinyApp(ui, server)