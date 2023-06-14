library(shiny)
library(ari)
library(dplyr)
library(readr)

# Voice Data
voices_coqui <- read_csv("data/voices-coqui.csv") %>% 
  # Remove after testing
  filter(language == "en", dataset == "ljspeech", model_name == "tacotron2-DDC_ph")
voices_amazon <- read_csv("data/voices-amazon.csv")
voices_google <- read_csv("data/voices-google.csv") %>% 
  filter(!is.na(language))
voices_ms <- read_csv("data/voices-ms.csv")
names(voices_ms) <- tolower(names(voices_ms))

# images for pickerInput stored in www/i/ from the root app directory
imgs <- c("i/img/coqui.png", "i/img/aws.jpeg", "i/img/google.png", "i/img/ms.jpeg")
img_name <- c("Coqui TTS", "Amazon Polly", 
              "Google Cloud Text-to-Speech", "Microsoft Cognitive Services Text-to-Speech")

select_choice_img <- function(img, text) {
  shiny::HTML(paste(
    tags$img(src=img, width=25, height=22),
    text
  ))
}

ui <- fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="i/favicon.ico")),
  titlePanel(tagList(
    "Loqui, Generate Videos using ari",
    span(
      actionButton("Help", 
                   label = "Help",
                   icon = icon("question"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/loqui/issues`, '_blank')",
                   style="color: #fff; background-color: #FF0000"),
      actionButton("github",
                   label = "Code",
                   icon = icon("github"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/loqui`, '_blank')",
                   style="color: #fff; background-color: #767676; border-color: #767676"),
      style = "position:absolute;right:2em;"
    )
  ),
  windowTitle = "Loqui"),
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
                                            "Microsoft Cognitive Services Text-to-Speech" = "ms"),
                                choicesOpt = list(content = purrr::map2(imgs, img_name, select_choice_img))),
      uiOutput("voice_options"),
      actionButton("go", "Generate"),
      br(),
      br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "i/posit.jpeg", height = "30px")
      ),
      tags$img(src = "i/img/logo.png", width = "90%"),
    ),
    mainPanel(h3("Rendered Video (mp4)"),
              uiOutput("video"),
              uiOutput("download"))
  )
)

server <- function(input, output, session) {
  
  output$voice_options <- renderUI({
    if (input$service == "coqui") {
      tagList(
        selectInput("coqui_lang", "Select Language", 
                    choices = unique(voices_coqui$language)),
        selectInput("coqui_dataset", "Select Dataset", choices = NULL),
        selectInput("coqui_model_name", "Select Model Name", choices = NULL),
        selectInput("coqui_vocoder_name", "Select Vocoder Name",
                    choices = "ljspeech/univnet")
        # Remove after testing
        # c("libri-tts/wavegrad",
        #   "libri-tts/fullband-melgan",
        #   "ek1/wavegrad",
        #   "ljspeech/multiband-melgan",
        #   "ljspeech/hifigan_v2",
        #   "ljspeech/univnet",
        #   "blizzard2013/hifigan_v2",
        #   "vctk/hifigan_v2",
        #   "sam/hifigan_v2")
      )
      # Remove after testing
    } else if (input$service == "amazon") {
      # tagList(
      #   selectInput("amazon_lang", "Select Language", 
      #               choices = unique(voices_amazon$language)),
      #   selectInput("amazon_gender", "Select Gender", choices = NULL),
      #   selectInput("amazon_voice", "Select Voice", choices = NULL)
      # )
    } else if (input$service == "google") {
      # tagList(
      #   selectInput("google_lang", "Select Language", 
      #               choices = unique(voices_google$language)),
      #   selectInput("google_gender", "Select Gender", choices = NULL),
      #   selectInput("google_voice", "Select Voice", choices = NULL)
      # )
    } else {
      # tagList(
      #   selectInput("ms_locale", "Select Language",
      #               choices = unique(voices_ms$locale)),
      #   selectInput("ms_gender", "Select Gender", choices = NULL),
      #   selectInput("ms_voice", "Select Voice", choices = NULL)
      # )
    }
  })
  
  # Coqui
  voices_coqui_reactive <- reactive({
    filter(voices_coqui, language == input$coqui_lang)
  })
  observeEvent(input$coqui_lang, {
    freezeReactiveValue(input, "coqui_dataset")
    choices <- unique(voices_coqui_reactive()$dataset)
    updateSelectInput(inputId = "coqui_dataset", choices = choices) 
  })
  voices_coqui_dataset_reactive <- reactive({
    req(input$coqui_dataset)
    filter(voices_coqui_reactive(), dataset == input$coqui_dataset)
  })
  observeEvent(input$coqui_dataset, {
    freezeReactiveValue(input, "coqui_model_name")
    choices <- unique(voices_coqui_dataset_reactive()$model_name)
    updateSelectInput(inputId = "coqui_model_name", choices = choices) 
  })
  
  # Amazon
  voices_amazon_reactive <- reactive({
    filter(voices_amazon, language == input$amazon_lang)
  })
  observeEvent(input$amazon_lang, {
    freezeReactiveValue(input, "amazon_gender")
    choices <- unique(voices_amazon_reactive()$gender)
    updateSelectInput(inputId = "amazon_gender", choices = choices) 
  })
  voices_amazon_gender_reactive <- reactive({
    req(input$amazon_gender)
    filter(voices_amazon_reactive(), gender == input$amazon_gender)
  })
  observeEvent(input$amazon_gender, {
    freezeReactiveValue(input, "amazon_voice")
    choices <- unique(voices_amazon_gender_reactive()$voice)
    updateSelectInput(inputId = "amazon_voice", choices = choices) 
  })
  
  # Google
  voices_google_reactive <- reactive({
    filter(voices_google, language == input$google_lang)
  })
  observeEvent(input$google_lang, {
    freezeReactiveValue(input, "google_gender")
    choices <- unique(voices_google_reactive()$gender)
    updateSelectInput(inputId = "google_gender", choices = choices) 
  })
  voices_google_gender_reactive <- reactive({
    req(input$google_gender)
    filter(voices_google_reactive(), gender == input$google_gender)
  })
  observeEvent(input$google_gender, {
    freezeReactiveValue(input, "google_voice")
    choices <- unique(voices_google_gender_reactive()$voice)
    updateSelectInput(inputId = "google_voice", choices = choices) 
  })
  
  # Microsoft
  voices_ms_reactive <- reactive({
    filter(voices_ms, locale == input$ms_locale)
  })
  observeEvent(input$ms_locale, {
    freezeReactiveValue(input, "ms_gender")
    choices <- unique(voices_ms_reactive()$gender)
    updateSelectInput(inputId = "ms_gender", choices = choices) 
  })
  voices_ms_gender_reactive <- reactive({
    req(input$ms_gender)
    filter(voices_ms_reactive(), gender == input$ms_gender)
  })
  observeEvent(input$ms_gender, {
    freezeReactiveValue(input, "ms_voice")
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
           coqui = ari_spin(images = image_path, 
                            paragraphs = pptx_notes_vector,
                            service = "coqui",
                            model_name = input$coqui_model_name,
                            vocoder_name = input$coqui_vocoder_name,
                            output = "www/ari-video.mp4"),
           amazon = ari_spin(images = image_path, 
                             paragraphs = pptx_notes_vector,
                             service = "amazon",
                             voice = input$amazon_voice,
                             output = "www/ari-video.mp4"),
           google = ari_spin(images = image_path, 
                             paragraphs = pptx_notes_vector,
                             service = "google",
                             voice = input$google_voice,
                             output = "www/ari-video.mp4"),
           ms = ari_spin(images = image_path, 
                         paragraphs = pptx_notes_vector,
                         service = "microsoft",
                         voice = input$ms_voice,
                         output = "www/ari-video.mp4"))
  })
  
  output$video <- renderUI({
    video_path <- attr(res(), "outfile")
    tags$video(src = "i/ari-video.mp4", 
               type = "video/mp4",
               height ="450px", 
               width="800px",
               autoplay = TRUE, 
               controls = TRUE)
  })
  
  observeEvent(input$go, {
    output$download <- renderUI({
      downloadButton("download_button")
    })
  })
  # Source: https://stackoverflow.com/questions/33416557/r-shiny-download-existing-file
  output$download_button <- downloadHandler(
    filename = "video.mp4",
    content = function(file) {
      file.copy("www/ari-video.mp4", file)
    },
    contentType = "video/mp4"
  )
}

addResourcePath("/i", file.path(getwd(), "www"))
options <- list()
if (!interactive()) {
  options$port = 3838
  options$launch.browser = FALSE
  options$host = "0.0.0.0"
  
}
shinyApp(ui, server, options=options)