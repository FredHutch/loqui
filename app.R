library(shiny)
library(ari)

## Proof of concept script

# gs_url <- "https://docs.google.com/presentation/d/1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk/edit#slide=id.p"
# 
# # Script (text)
# pptx_path <- download_gs_file(gs_url, out_type = "pptx")
# pptx_notes_vector <- pptx_notes(pptx_path)
# 
# # Images
# pdf_path <- download_gs_file(gs_url, out_type = "pdf")
# filenames <- pdf_to_pngs(pdf_path)
# 
# # Create a video from images and text
# ari_spin(images = filenames, paragraphs = pptx_notes_vector, service = "coqui")


ui <- fluidPage(
  titlePanel("Loqui: Make videos"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("gs_url", 
                label = "Google Slides URL")	
    ),
    mainPanel(h4("Rendered Video (mp4)"),
              uiOutput("video"))
  )
)

server <- function(input, output, session) {
  output$video <- renderUI({
    tags$video(src = "sample.mp4", 
               type = "video/mp4",
               height ="400px", 
               width="400px",
               autoplay = TRUE, 
               controls = TRUE)
  })
}

shinyApp(ui, server)