library(shiny)
library(shinyjs)
library(ari)
library(dplyr)
library(readr)
library(pdftools)
library(blastula)

# Voice Data
voices_coqui <- read_csv("data/voices-coqui.csv", show_col_types = FALSE) %>% 
  # Remove after testing
  filter(language == "en", dataset == "ljspeech", model_name == "tacotron2-DDC_ph")
voices_amazon <- read_csv("data/voices-amazon.csv", show_col_types = FALSE)
voices_google <- read_csv("data/voices-google.csv", show_col_types = FALSE) %>% 
  filter(!is.na(language))
voices_ms <- read_csv("data/voices-ms.csv", show_col_types = FALSE)
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

# Check if email is valid 
is_valid_email <- function(x) {
  grepl("([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))", x)
}

# Start of Shiny app
ui <- fluidPage(
  useShinyjs(),
  # css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "i/hutch_theme.css")
  ),
  # favicon
  tags$head(tags$link(rel="shortcut icon", href="i/img/favicon.ico")),
  # css to center the progress bar
  # https://stackoverflow.com/a/52585505/14804653
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);
           left: calc(50% - 400px);
           font-size: 250%;
           font-family: Times;
           color: #1c3b61;
           text-align: center;
           }
           "
      )
    )
  ),
  titlePanel(tagList(
    "Loqui",
    span(
      actionButton("demo", 
                   label = "Demo",
                   icon = icon("youtube"),
                   onclick ="window.open(`https://github.com/FredHutch/loqui#getting-help`, '_blank')"),
      actionButton("help", 
                   label = "Help",
                   icon = icon("circle-exclamation"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/loqui#getting-help`, '_blank')"),
      actionButton("github",
                   label = "Code",
                   icon = icon("github"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/loqui`, '_blank')"),
      style = "position:absolute;right:2em;"
    )
  ),
  windowTitle = "Loqui"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      textInput("gs_url", 
                label = "Google Slides URL",
                value = "",
                placeholder = "Paste a URL"),
      shinyWidgets::pickerInput("service",
                                label = "Text-to-Speech Service", 
                                choices = c("Coqui TTS" = "coqui"),
                                choicesOpt = list(content = purrr::map2(imgs, img_name, select_choice_img)[[1]])),
      # "Amazon Polly" = "amazon",
      # "Google Cloud Text-to-Speech" = "google",
      # "Microsoft Cognitive Services Text-to-Speech" = "ms"),
      # choicesOpt = list(content = purrr::map2(imgs, img_name, select_choice_img))),
      uiOutput("voice_options"),
      actionButton("generate", "Generate", icon = icon("person-running")),
      br(),
      br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "i/img/posit.jpeg", height = "30px")
      ),
      tags$img(src = "i/img/logo.png", width = "90%"),
      h6("This initiative is funded by the following grant: National Cancer Institute (NCI) UE5 CA254170")
    ),
    mainPanel(
      tabsetPanel(id = "inTabset",
                  tabPanel(
                    title = div("About",
                                style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                    value = "about",
                    h3("Introducing Loqui: A Shiny app for Creating Automated Courses with ari"),
                    div( 
                      p("Loqui is an open source web application that enables the creation of automated courses using ari,
                        an R package for generating videos from text and images. Loqui takes as input a Google Slides URL,
                        extracts the speaker notes from the slides, and converts them into an audio file. 
                        Then, it converts the Google Slides to images and ultimately,
                        generates an mp4 video file where each image is presented with its corresponding audio."),
                      
                      p("The functionality of Loqui relies on two R packages, namely ari and text2speech, which run in the background.
                        Although  it is certainly possible to go directly to these packages and run their functions for course generation,
                        we realize that not everyone feels comfortable programming in R. This web application offers an intuitive and user-friendly
                        interface allowing individuals to effortlessly create automated courses without the need for programming skills."),
                      h4("Prerequisites"),
                      tags$ul(
                        tags$li("Start from a Google Slides that you wish to generate automated courses from. 
                           Make sure all slides contain speaker notes. A slide without a speaker note will generate a scratchy radio sound."),
                        tags$li("Turn link sharing on. Make sure \"General access\" is set to \"Anyone with the link 
                          (Anyone on the internet with the link can edit)\" 
                           and Role can be set to either Editor, Viewer, or Commenter."),
                        tags$li("For more information, read Section 2 \"How to share a Google Slides presentation via a link\" of this",
                                a(href = "https://www.brightcarbon.com/blog/how-to-share-google-slides-presentation", "blogpost."))
                      ),
                      
                      h4("Instructions"),
                      tags$ul(
                        tags$li("In the provided box below, please provide a valid email address as this app requires it to function properly."),
                        tags$li("Copy and Paste the Google Slides URL into the text box labeled \"Google Slides URL\"."),
                        tags$li("Choose the Text-to-Speech Service. Please note that as of mid-2023, only the Coqui TTS engine is available 
                          as a free option.
                        However, paid services like Amazon Polly, Google Cloud Text-to-Speech, and Microsoft Azure Text-to-Speech will be introduced in the future."),
                        tags$li("Select the desired voice options for the text-to-speech engine. We have already pre-selected the voice options that sound the most human-like.
                        However, if you prefer alternative voice options, kindly inform us, and we will accommodate your request. 
                        If there is sufficient interest, we may consider expanding the list of voice options to provide more choices."),
                        tags$li("Click the \"Generate\" button to initiate the course generation process.")
                      ),
                      em("Privacy Policy: The data we collect is limited to the date and time of usage, duration of the generated video, and the provided email address.
                         We don't collect any other information."),
                      style = "font-family: Arial; color: #1c3b61"),
                    br(),
                    # textOutput("duration_text"),
                    textInput("email", "Email Address"),
                    actionButton("get_started", "Get Started", icon = icon("rocket"))
                  ),
                  tabPanel(
                    title = div("Rendered Video", 
                                style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                    value = "rendered_video",
                    br(),
                    uiOutput("video"),
                    br(),
                    fluidRow(column(11, htmlOutput("video_info"))),
                    fluidRow(uiOutput("video_btn"))
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  # Disable buttons when email is not provided
  observe({
    toggleState("generate",
                !is.null(input$email) && input$email != "" && is_valid_email(input$email))
    toggleState("download_btn",
                !is.null(input$email) && input$email != "" && is_valid_email(input$email))
    toggleState("send_email",
                !is.null(input$email) && input$email != "" && is_valid_email(input$email))
    toggleState("get_started",
                !is.null(input$email) && input$email != "" && is_valid_email(input$email))
  })
  # Switch tabs when "Get Started" is clicked
  observeEvent(input$get_started, {
    updateTabsetPanel(session, "inTabset", selected = "rendered_video")
  })
  # Voice Options
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
      # Remove when paid services are good2go
    } else if (input$service == "amazon") {
      # tagList(
      #   selectInput("amazon_lang", "Select Language", 
      #               choices = unique(voices_amazon$language)),
      #   selectInput("amazon_gender", "Select Gender", choices = NULL),
      #   selectInput("amazon_voice", "Select Voice", choices = NULL)
      # )
    } else if (input$service == "google") {
      # Remove when paid services are good2go
      # tagList(
      #   selectInput("google_lang", "Select Language", 
      #               choices = unique(voices_google$language)),
      #   selectInput("google_gender", "Select Gender", choices = NULL),
      #   selectInput("google_voice", "Select Voice", choices = NULL)
      # )
    } else {
      # Remove when paid services are good2go
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
  res <- eventReactive(input$generate, {
    withProgress(message = 'Converting slides to PPTX...', 
                 value = 0, 
                 detail="0%", {
                   # text
                   Sys.sleep(0.5)
                   # download as pptx
                   pptx_path <- download_gs_file(input$gs_url, out_type = "pptx")
                   incProgress(1/5, 
                               message = "Converting slides to PPTX...Done!", 
                               detail = "20%")
                   Sys.sleep(0.5)
                   incProgress(0, 
                               message = "Extracting speaker notes...", 
                               detail = "20%")
                   # extract speaker notes
                   pptx_notes_vector <- pptx_notes(pptx_path)
                   incProgress(1/5, 
                               message = "Extracting speaker notes...Done!", 
                               detail = "40%")
                   Sys.sleep(0.5)
                   incProgress(0, 
                               message = "Converting slides to PDF...", 
                               detail = "40%")
                   # download as pdf
                   pdf_path <- download_gs_file(input$gs_url, out_type = "pdf")
                   incProgress(1/5, 
                               message = "Converting slides to PDF...Done!", 
                               detail = "60%")
                   Sys.sleep(0.5)
                   incProgress(0, 
                               message = "Converting PDF to PNG...", 
                               detail = "60%")
                   # convert to png
                   image_path <- pdf_to_pngs(pdf_path)
                   incProgress(1/5, 
                               message = "Converting PDF to PNG...Done!", 
                               detail = "80%")
                   Sys.sleep(0.5)
                   incProgress(0, 
                               message = "Rendering video...", 
                               detail = "80%")
                   Sys.sleep(1)
                   incProgress(0, 
                               message = "Rendering takes a few minutes!", 
                               detail = "80%")
                   Sys.sleep(3)
                   incProgress(0, 
                               message = "Rendering video...", 
                               detail = "80%")
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
                   incProgress(1/5, 
                               message = "Rendered video as mp4", 
                               detail = "100%")
                   Sys.sleep(1.5)
                 })
  })
  # Show video when "Generate" is clicked
  output$video <- renderUI({
    # Hack: Wait until stuff inside res() is processed
    video_path <- attr(res(), "outfile")
    tags$video(src = "i/ari-video.mp4", 
               type = "video/mp4",
               height ="480px", 
               width="854px",
               autoplay = TRUE,
               controls = TRUE
               )
  })
  # Show video title and buttons when "Generate" is clicked
  observeEvent(input$generate, {
    pdf_path <- download_gs_file(input$gs_url, "pdf")
    video_info_reactive <- pdf_info(pdf = pdf_path)
    
    output$video_info <- renderUI({
      span(textOutput("video_title"), 
           style = "font-weight: bold; 
                    font-family: Arial; 
                    font-size: 25px; 
                    color: #1c3b61")
      
      output$video_title <- renderText({
        video_info_reactive$keys$Title
      })
    })
    output$video_btn <- renderUI({
      column(12,
             downloadButton("download_btn"),
             actionButton("send_email", "Email", icon = icon("inbox")),
             align = "left"
      )
    })
  })
  
  # Download rendered video
  # Source: https://stackoverflow.com/questions/33416557/r-shiny-download-existing-file
  output$download_btn <- downloadHandler(
    filename = "loqui_video.mp4",
    content = function(file) {
      file.copy("www/ari-video.mp4", file)
    },
    contentType = "video/mp4"
  )
  
  # Send email
  observeEvent(input$send_email, {
    # Date/Time
    date_time <- add_readable_time()
    # Compose Email
    email <- compose_email(
      body = md(glue::glue(
        "Hello there! Attached is the video generated by Loqui.")),
      footer = md(glue::glue("Email sent on {date_time}."))
    )
    # Send email
    email %>% 
      add_attachment(file = "www/ari-video.mp4") %>%
      smtp_send(
        to = input$email,
        from = "howardbaek@fredhutch.org",
        subject = "Loqui Video",
        credentials = creds_anonymous(host = "mx.fhcrc.org", port = 25)
      )
  })
  
  # res_duration <- eventReactive(res(), {
  #   pattern <- "Duration: ([0-9:.]+)"
  #   duration_raw <- system2("ffmpeg", "-i i/ari-video.mp4 2>&1 | grep \"Duration\"", 
  #                           stdout = TRUE)
  #   duration_raw <- regmatches(txt,regexpr("Duration: (\\d{2}:\\d{2}:\\d{2}\\.\\d{2})", duration_raw))
  #   sub(pattern, "\\1", duration_raw)
  # })
  # Duration
  output$duration_text <- renderText({ 
    # Hack: Wait until stuff inside res() is processed
    video_path <- attr(res(), "outfile")
    
    pattern <- "Duration: ([0-9:.]+)"
    duration_raw <- system2("ffmpeg", "-i www/ari-video.mp4 2>&1 | grep \"Duration\"", 
                            stdout = TRUE)
    duration_raw <- regmatches(duration_raw, regexpr("Duration: (\\d{2}:\\d{2}:\\d{2}\\.\\d{2})", duration_raw))
    sub(pattern, "\\1", duration_raw)
  })
 
  
}
# Code for Deployment to Hutch servers
addResourcePath("/i", file.path(getwd(), "www"))
options <- list()
if (!interactive()) {
  options$port = 3838
  options$launch.browser = FALSE
  options$host = "0.0.0.0"
  
}
shinyApp(ui, server, options=options)