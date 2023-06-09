library(shiny)
library(ari)
library(magrittr)

# images for pickerInput stored in www/img/ from the root app directory
imgs <- c("img/coqui.png", "img/aws.jpeg", "img/google.png", "img/ms.jpeg")
img_name <- c("Coqui TTS", "Amazon Polly", "Google Cloud Text-to-Speech", "Microsoft Cognitive Services Text-to-Speech")

select_choice_img <- function(img, text) {
  shiny::HTML(paste(
    tags$img(src=img, width=30, height=22),
    text
  ))
}

ui <- fluidPage(
  titlePanel("Loqui: Generate videos using ari"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("url", 
                label = "Google Slides URL",
                value = "",
                placeholder = "Paste a URL"),
      shinyWidgets::pickerInput("service",
                                label = "Text-to-Speech Service", 
                                choices = c("Coqui TTS" = "coqui",
                                            "Amazon Polly" = "amazon",
                                            "Google Cloud Text-to-Speech" = "google",
                                            "Microsoft Cognitive Services Text-to-Speech" = "microsoft"),
                                choicesOpt = list(content = purrr::map2(imgs, img_name, select_choice_img))),
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
             service = input$service,
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