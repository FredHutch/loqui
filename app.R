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
              uiOutput("video"))
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$go, {
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
    
    output$video <- renderUI({
      tags$video(src = "ari-video.mp4", 
                 type = "video/mp4",
                 height ="400px", 
                 width="400px",
                 autoplay = TRUE, 
                 controls = TRUE)
    })
    
    
  })
}

shinyApp(ui, server)