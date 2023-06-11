library(shiny)
library(ari)
library(dplyr)
library(readr)

voices_coqui <- read_csv("data/voices-coqui.csv")
voices_amazon <- read_csv("data/voices-amazon.csv")
voices_google <- read_csv("data/voices-google.csv") %>% 
  filter(!is.na(language))
voices_ms <- read_csv("data/voices-ms.csv")
names(voices_ms) <- tolower(names(voices_ms))

# Dummy Google Slides URL: 
# https://docs.google.com/presentation/d/1Dw_rBb1hySN_76xh9-x5J2dWF_das9BAUjQigf2fN-E/edit#slide=id.p

# images for pickerInput stored in www/img/ from the root app directory
imgs <- c("img/coqui.png", "img/aws.jpeg", "img/google.png", "img/ms.jpeg")
img_name <- c("Coqui TTS", "Amazon Polly", 
              "Google Cloud Text-to-Speech", "Microsoft Cognitive Services Text-to-Speech")

select_choice_img <- function(img, text) {
  shiny::HTML(paste(
    tags$img(src=img, width=25, height=22),
    text
  ))
}

ui <- fluidPage(
  titlePanel("Loqui: Generate videos using ari"),
  hr(),
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
      uiOutput("voice_options"),
      actionButton("go", "Generate"),
      br(),
      br(),
      tags$img(src = "img/logo.png", width = "500px")
    ),
    mainPanel(h4("Rendered Video (mp4)"),
              uiOutput("video"),
              uiOutput("download"))
  )
)

server <- function(input, output, session) {
  
  output$voice_options <- renderUI({
    if (input$service == "coqui") {
      tagList(
        selectInput("coqui_lang", "Select Language (TTS)", 
                    choices = unique(voices_coqui$language)),
        selectInput("coqui_dataset", "Select Dataset (TTS)", choices = NULL),
        selectInput("coqui_model_name", "Select Model Name (TTS)", choices = NULL)
      )
      
    } else if (input$service == "amazon") {
      tagList(
        selectInput("amazon_lang", "Select Language", 
                    choices = unique(voices_amazon$language)),
        selectInput("amazon_gender", "Select Gender", choices = NULL),
        selectInput("amazon_voice", "Select Voice", choices = NULL)
      )
    } else if (input$service == "google") {
      tagList(
        selectInput("google_lang", "Select Language", 
                    choices = unique(voices_google$language)),
        selectInput("google_gender", "Select Gender", choices = NULL),
        selectInput("google_voice", "Select Voice", choices = NULL)
      )
    } else {
      tagList(
        selectInput("ms_locale", "Select Locale",
                    choices = unique(voices_ms$locale)),
        selectInput("ms_gender", "Select Gender", choices = NULL),
        selectInput("ms_voice", "Select Voice", choices = NULL)
      )
    }
  })
  
  # Coqui
  voices_coqui_reactive <- reactive({
    filter(voices_coqui, language == input$coqui_lang)
  })
  observeEvent(input$coqui_lang, {
    choices <- unique(voices_coqui_reactive()$dataset)
    updateSelectInput(inputId = "coqui_dataset", choices = choices) 
  })
  voices_coqui_dataset_reactive <- reactive({
    req(input$coqui_dataset)
    filter(voices_coqui_reactive(), dataset == input$coqui_dataset)
  })
  observeEvent(input$coqui_dataset, {
    choices <- unique(voices_coqui_dataset_reactive()$model_name)
    updateSelectInput(inputId = "coqui_model_name", choices = choices) 
  })
  
  # Amazon
  voices_amazon_reactive <- reactive({
    filter(voices_amazon, language == input$amazon_lang)
  })
  observeEvent(input$amazon_lang, {
    choices <- unique(voices_amazon_reactive()$gender)
    updateSelectInput(inputId = "amazon_gender", choices = choices) 
  })
  voices_amazon_gender_reactive <- reactive({
    req(input$amazon_gender)
    filter(voices_amazon_reactive(), gender == input$amazon_gender)
  })
  observeEvent(input$amazon_gender, {
    choices <- unique(voices_amazon_gender_reactive()$voice)
    updateSelectInput(inputId = "amazon_voice", choices = choices) 
  })
  
  # Google
  voices_google_reactive <- reactive({
    filter(voices_google, language == input$google_lang)
  })
  observeEvent(input$google_lang, {
    choices <- unique(voices_google_reactive()$gender)
    updateSelectInput(inputId = "google_gender", choices = choices) 
  })
  voices_google_gender_reactive <- reactive({
    req(input$google_gender)
    filter(voices_google_reactive(), gender == input$google_gender)
  })
  observeEvent(input$google_gender, {
    choices <- unique(voices_google_gender_reactive()$voice)
    updateSelectInput(inputId = "google_voice", choices = choices) 
  })
  
  # Microsoft
  voices_ms_reactive <- reactive({
    filter(voices_ms, locale == input$ms_locale)
  })
  observeEvent(input$ms_locale, {
    choices <- unique(voices_ms_reactive()$gender)
    updateSelectInput(inputId = "ms_gender", choices = choices) 
  })
  voices_ms_gender_reactive <- reactive({
    req(input$ms_gender)
    filter(voices_ms_reactive(), gender == input$ms_gender)
  })
  observeEvent(input$ms_gender, {
    choices <- unique(voices_ms_gender_reactive()$name)
    updateSelectInput(inputId = "ms_voice", choices = choices) 
  })
  
  # MP4 Video
  res <- eventReactive(input$go, {
    # text
    pptx_path <- download_gs_file(input$url, out_type = "pptx")
    pptx_notes_vector <- pptx_notes(pptx_path)
    
    # images
    pdf_path <- download_gs_file(input$url, out_type = "pdf")
    image_path <- pdf_to_pngs(pdf_path)
    
    # create video
    switch(input$service,
           coqui     = ari_spin(images = image_path, 
                                paragraphs = pptx_notes_vector,
                                service = input$service,
                                # voice = voices_coqui_reactive()$model_name,
                                output = "www/ari-video.mp4"),
           amazon    = ari_spin(images = image_path, 
                                paragraphs = pptx_notes_vector,
                                service = input$service,
                                output = "www/ari-video.mp4"),
           google    = ari_spin(images = image_path, 
                                paragraphs = pptx_notes_vector,
                                service = input$service,
                                output = "www/ari-video.mp4"),
           microsoft = ari_spin(images = image_path, 
                                paragraphs = pptx_notes_vector,
                                service = input$service,
                                output = "www/ari-video.mp4"))
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