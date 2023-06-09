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
      checkboxInput("select_voice", "Customize Voice", value = FALSE),
      # Only show if checkbox is checked
      conditionalPanel(
        condition = "input.select_voice & input.service == 'coqui'",
        uiOutput("select_coqui_lang"),
        uiOutput("select_coqui_dataset"),
        uiOutput("select_coqui_model_name")
      ),
      conditionalPanel(
        condition = "input.select_voice & input.service == 'amazon'",
        uiOutput("select_amazon_lang"),
        uiOutput("select_amazon_gender"),
        uiOutput("select_amazon_voice")
      ),
      conditionalPanel(
        condition = "input.select_voice & input.service == 'google'",
        uiOutput("select_google_lang"),
        uiOutput("select_google_gender"),
        uiOutput("select_google_voice")
      ),
      conditionalPanel(
        condition = "input.select_voice & input.service == 'microsoft'",
        uiOutput("select_ms_locale"),
        uiOutput("select_ms_gender"),
        uiOutput("select_ms_voice")
      ),
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
  # Voices (Coqui)
  voices_coqui_reactive <- reactive({
    voices_coqui %>% 
      filter(language == input$coqui_lang) %>% 
      filter(dataset == input$coqui_dataset) %>% 
      filter(model_name == input$coqui_model_name)
  })
  
  output$select_coqui_lang <- renderUI({
    selectizeInput("coqui_lang",
                   "Select Language", 
                   choices = unique(voices_coqui$language))
  })
  
  output$select_coqui_dataset <- renderUI({
    choice_dataset <- reactive({
      req(input$coqui_lang)
      
      voices_coqui %>% 
        filter(language == input$coqui_lang) %>% 
        pull(dataset) %>% 
        unique()
    })
    
    selectizeInput("coqui_dataset", "Select Dataset",
                   choices = choice_dataset())
  })
  
  output$select_coqui_model_name <- renderUI({
    choice_model_name <- reactive({
      req(input$coqui_lang, 
          input$coqui_dataset)
      
      voices_coqui %>% 
        filter(language == input$coqui_lang) %>% 
        filter(dataset == input$coqui_dataset) %>% 
        dplyr::pull(model_name) %>% 
        unique()
    })
    
    selectizeInput("coqui_model_name", "Select Model Name",
                   choices = choice_model_name())
  })
  # Voices (Coqui) END
  
  # Voices (Amazon Polly)
  voices_amazon_reactive <- reactive({
    voices_amazon %>% 
      filter(language == input$amazon_lang,
             gender == input$amazon_gender,
             voice == input$amazon_voice)
  })
  
  output$select_amazon_lang <- renderUI({
    selectizeInput("amazon_lang",
                   "Select Language", 
                   choices = unique(voices_amazon$language))
  })
  
  output$select_amazon_gender <- renderUI({
    choice_gender <- reactive({
      req(input$amazon_lang)
      
      voices_amazon %>% 
        filter(language == input$amazon_lang) %>% 
        dplyr::pull(gender) %>% 
        unique()
    })
    
    selectizeInput("amazon_gender", "Select Gender",
                   choices = choice_gender())
  })
  
  output$select_amazon_voice <- renderUI({
    choice_voice <- reactive({
      req(input$amazon_lang,
          input$amazon_gender)
      
      voices_amazon %>% 
        filter(language == input$amazon_lang) %>% 
        filter(gender == input$amazon_gender) %>% 
        dplyr::pull(voice) %>% 
        unique()
    })
    
    selectizeInput("amazon_voice", "Select Voice",
                   choices = choice_voice())
  })
  # Voices (Amazon Polly) END
  
  # Voices (Google)
  voices_google_reactive <- reactive({
    voices_google %>% 
      filter(language == input$google_lang,
             gender == input$google_gender,
             voice == input$google_voice)
  })
  
  output$select_google_lang <- renderUI({
    selectizeInput("google_lang",
                   "Select Language", 
                   choices = unique(voices_google$language))
  })
  
  output$select_google_gender <- renderUI({
    choice_gender <- reactive({
      req(input$google_lang)
      
      voices_google %>% 
        filter(language == input$google_lang) %>% 
        dplyr::pull(gender) %>% 
        unique()
    })
    
    selectizeInput("google_gender", "Select Gender",
                   choices = choice_gender())
  })
  
  output$select_google_voice <- renderUI({
    choice_voice <- reactive({
      req(input$google_lang,
          input$google_gender)
      
      voices_google %>% 
        filter(language == input$google_lang) %>% 
        filter(gender == input$google_gender) %>% 
        dplyr::pull(voice) %>% 
        unique()
    })
    
    selectizeInput("google_voice", "Select Voice",
                   choices = choice_voice())
  })
  # Voices (Google) END
  
  # Voices (Microsoft)
  voices_ms_reactive <- reactive({
    voices_ms %>% 
      filter(locale == input$ms_locale,
             gender == input$ms_gender,
             name == input$ms_voice)
  })
  
  output$select_ms_locale <- renderUI({
    selectizeInput("ms_locale",
                   "Select Locale", 
                   choices = unique(voices_ms$locale))
  })
  
  output$select_ms_gender <- renderUI({
    choice_gender <- reactive({
      req(input$ms_locale)
      
      voices_ms %>% 
        filter(locale == input$ms_locale) %>% 
        dplyr::pull(gender) %>% 
        unique()
    })
    
    selectizeInput("ms_gender", "Select Gender",
                   choices = choice_gender())
  })
  
  output$select_ms_voice <- renderUI({
    choice_voice <- reactive({
      req(input$ms_locale,
          input$ms_gender)
      
      voices_ms %>% 
        filter(locale == input$ms_locale) %>% 
        filter(gender == input$ms_gender) %>% 
        dplyr::pull(name) %>% 
        unique()
    })
    
    selectizeInput("ms_voice", "Select Voice",
                   choices = choice_voice())
  })
  # Voices (Microsoft) END

  # MP4 Video
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