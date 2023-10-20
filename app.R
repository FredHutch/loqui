# Packages ----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(ari)
library(dplyr)
library(readr)
library(pdftools)
library(blastula)
library(googlesheets4)
library(promises)
library(future)
library(ipc)
library(gsplyr)
library(ptplyr)
# Options ----
options("future.rng.onMisuse" = "ignore")
plan(multisession, workers = 25)

# Voice Data ----
voices_coqui <- read_csv("data/voices-coqui.csv", show_col_types = FALSE) %>% 
  # Remove after testing
  filter(language == "en", 
         dataset %in% c("ljspeech", "jenny"),
         model_name %in% c("tacotron2-DDC_ph", "jenny"))
# Don't need Paid Voices for now
# voices_amazon <- read_csv("data/voices-amazon.csv", show_col_types = FALSE)
# voices_google <- read_csv("data/voices-google.csv", show_col_types = FALSE) %>% 
#   filter(!is.na(language))
# voices_ms <- read_csv("data/voices-ms.csv", show_col_types = FALSE)
# names(voices_ms) <- tolower(names(voices_ms))

# images for pickerInput stored in www/i/ from the root app directory
imgs <- c("i/img/coqui.png", "i/img/aws.jpeg", "i/img/google.png", "i/img/ms.jpeg")
img_name <- c("Coqui TTS", "Amazon Polly", 
              "Google Cloud Text-to-Speech", "Microsoft Cognitive Services Text-to-Speech")

# Select image ----
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

# Start of Shiny app ----
ui <- fluidPage(
  useShinyjs(),
  useShinyFeedback(),
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
                   onclick ="window.open(`https://youtu.be/G7JyvCAxg40`, '_blank')"),
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
      textInput("email", "Email Address (where video should be sent)"),
      div(
        shinyWidgets::prettySwitch("auto_email", "Once video finishes rendering, send email automatically",
                                   value = TRUE, status = "success", fill = TRUE),
        style = "color: #1c3b61;"
      ),
      radioButtons("presentation_tool", "Presentation Tool",
                   c("Google Slides" = "google_slides",
                     "Powerpoint" = "powerpoint")
      ),
      div(
        prettySwitch("burn_subtitle", "Add Subtitles",
                     status = "success", fill = TRUE),
        style = "color: #1c3b61;"
      ),
      uiOutput("user_input"),
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
      tags$img(src = "i/img/logo.png", width = "90%")
    ),
    mainPanel(
      tabsetPanel(id = "inTabset",
                  tabPanel(
                    title = div("About",
                                style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                    value = "about",
                    h3("Introducing Loqui: A Shiny app for Creating Automated Videos"),
                    div( 
                      p("Loqui is an open source web application that enables the creation of automated videos using ari,
                        an R package for generating videos from text and images. Loqui takes as input either a Google Slides URL
                        or a Microsoft PowerPoint file, extracts the speaker notes from the slides, and converts them into an audio file. 
                        Then, it converts the Google Slides to images and ultimately,
                        generates an mp4 video file where each image is presented with its corresponding audio."),
                      
                      p("The functionality of Loqui relies on two R packages, namely ari and text2speech, which run in the background.
                        Although  it is certainly possible to go directly to these packages and run their functions for course generation,
                        we realize that not everyone feels comfortable programming in R. This web application offers an intuitive and user-friendly
                        interface allowing individuals to effortlessly create automated videos without the need for programming skills."),
                      uiOutput("loqui_demo"),
                      em("Privacy Policy: We only collect the date and time of usage, duration of the generated video, and the provided email address."),
                      h5("This initiative is funded by the following grant: National Cancer Institute (NCI) UE5 CA254170"),
                      style = "font-family: Arial; color: #1c3b61"),
                    actionButton("show_example", "Show Example", icon = icon("magnifying-glass"),
                                 style = "font-family: Arial; font-weight: bold; color: #F4F4F4; background-color: #0A799A; border-radius: 12px;")
                  ),
                  tabPanel(
                    title = div("Examples", 
                                style = "font-family: Arial; color: #1c3b61; font-weight: bold"), 
                    value = "loqui_example",
                    br(),
                    h3("DeepPhe: Natural Language Processing Tool"),
                    uiOutput("loqui_example_ui_1"),
                    br(),
                    h3("EMERSE: Electronic Medical Record Search Engine"),
                    uiOutput("loqui_example_ui_2"),
                    br(),
                    h3("pVACtools: Cancer Immunotherapy Suite"),
                    uiOutput("loqui_example_ui_3")
                  ),
                  tabPanel(
                    title = div("Rendered Video", 
                                style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                    value = "rendered_video",
                    br(),
                    uiOutput("video_ui"),
                    br(),
                    fluidRow(column(11, htmlOutput("video_info"))),
                    fluidRow(uiOutput("video_btn"))
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  # Disable buttons when email is not provided and Google Slides URL (if provided) is not accessible
  observe({
    shinyjs::toggleState("generate",
                         !is.null(input$email) && input$email != "" && is_valid_email(input$email) && 
                           !inherits(try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE), "try-error"))
    shinyjs::toggleState("download_btn",
                         !is.null(input$email) && input$email != "" && is_valid_email(input$email) &&
                           !inherits(try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE), "try-error"))
    shinyjs::toggleState("send_email",
                         !is.null(input$email) && input$email != "" && is_valid_email(input$email) &&
                           !inherits(try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE), "try-error"))
  })
  
  # Display feedback message when email address is not valid
  observeEvent(input$email, {
    if (input$email != "" & !is_valid_email(input$email)) {
      shinyFeedback::showFeedbackWarning(
        inputId = "email",
        text = "Invalid email. Please try again."
      )  
    } else {
      shinyFeedback::hideFeedback("email")
    }
  })
  
  # Display feedback message when Google Slides link isn't accessible
  observeEvent(input$gs_url, {
    res <- try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE)
    if(input$gs_url != "" & inherits(res, "try-error")) {
      shinyFeedback::showFeedbackWarning(
        inputId = "gs_url",
        text = "Please set General access of the slides to 'Anyone with the link'."
      )  
    } else {
      shinyFeedback::hideFeedback("gs_url")
    }
  })
  
  # Select Google Slides or PowerPoint
  output$user_input <- renderUI({
    if (input$presentation_tool == "google_slides") {
      textInput("gs_url", 
                label = "Google Slides URL (Enable Link Sharing)",
                placeholder = "Paste a Google Slides URL")
    } else { 
      fileInput("pptx_file", NULL, accept = ".pptx",
                buttonLabel = "Upload .pptx")
    }
  })
  
  # Switch tabs when "Get Started" is clicked
  observeEvent(input$generate, {
    updateTabsetPanel(session, "inTabset", selected = "rendered_video")
  })
  
  # Switch tabs when "Show Example" is clicked
  observeEvent(input$show_example, {
    updateTabsetPanel(session, "inTabset", selected = "loqui_example")
  })
  
  # Create unique name for video file
  video_name <- eventReactive(input$generate, {
    current_time <- Sys.time()
    current_time <- format(current_time, "%Y-%m-%d-%H-%M-%S")
    unique_file_name <- paste0("www/ari-video-", current_time, ".mp4")
    
    unique_file_name
  })
  
  # Video with subtitles
  video_name_subtitle <- eventReactive(input$burn_subtitle, {
    # create unique name for video file
    current_time <- Sys.time()
    current_time <- format(current_time, "%Y-%m-%d-%H-%M-%S")
    unique_file_name <- paste0("www/subtitled-ari-video-", current_time, ".mp4")
    
    unique_file_name
  })
  
  # Show videos when "Show Examples" is clicked
  output$loqui_demo <- renderUI({
    tags$video(src = "i/video/loqui.mp4", 
               type = "video/mp4",
               height ="480px", 
               width="790px",
               controls = TRUE)
  })
  
  # Show videos when "Show Examples" is clicked
  output$loqui_example_ui_1 <- renderUI({
    tags$video(src = "i/video/deepphe.mp4", 
               type = "video/mp4",
               height ="480px", 
               width="854px",
               controls = TRUE)
  })
  output$loqui_example_ui_2 <- renderUI({
    tags$video(src = "i/video/emerse.mp4", 
               type = "video/mp4",
               height ="480px", 
               width="854px",
               controls = TRUE)
  })
  output$loqui_example_ui_3 <- renderUI({
    tags$video(src = "i/video/pVACtools.mp4", 
               type = "video/mp4",
               height ="480px", 
               width="854px",
               controls = TRUE)
  })
  
  # Voice Options
  output$voice_options <- renderUI({
    if (input$service == "coqui") {
      tagList(
        selectInput("coqui_model_name", "Select Model Name (Voice)", 
                    choices = unique(voices_coqui$model_name),
                    selected = "jenny")
        # TODO: Refactor after ITCR PI Meeting 08/04/2023
        # selectInput("coqui_lang", "Select Language", choices = unique(voices_coqui$language)),
        # selectInput("coqui_dataset", "Select Dataset", choices = NULL),
        # selectInput("coqui_model_name", "Select Model Name", choices = NULL),
        # selectInput("coqui_vocoder_name", "Select Vocoder Name",
        #             choices = NULL)
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
  
  # TODO: Refactor after ITCR PI Meeting 08/04/2023
  # Coqui 
  # voices_coqui_reactive <- reactive({
  #   filter(voices_coqui, language == input$coqui_lang)
  # })
  # observeEvent(input$coqui_lang, {
  #   freezeReactiveValue(input, "coqui_dataset")
  #   choices <- unique(voices_coqui_reactive()$dataset)
  #   updateSelectInput(inputId = "coqui_dataset", choices = choices)
  # })
  # voices_coqui_dataset_reactive <- reactive({
  #   req(input$coqui_dataset)
  #   filter(voices_coqui_reactive(), dataset == input$coqui_dataset)
  # })
  # observeEvent(input$coqui_dataset, {
  #   freezeReactiveValue(input, "coqui_model_name")
  #   choices <- unique(voices_coqui_dataset_reactive()$model_name)
  #   updateSelectInput(inputId = "coqui_model_name", choices = choices)
  # })
  # voices_coqui_model_reactive <- reactive({
  #   req(input$coqui_model_name)
  #   filter(voices_coqui_dataset_reactive(), dataset == input$coqui_model_name)
  # })
  # observeEvent(input$coqui_model_name, {
  #   freezeReactiveValue(input, "coqui_vocoder_name")
  #   choices <- ifelse(input$coqui_model_name == "jenny", "jenny", "ljspeech/univnet")
  #   updateSelectInput(inputId = "coqui_vocoder_name", choices = choices)
  # })
  
  # Remove after testing
  # # Amazon
  # voices_amazon_reactive <- reactive({
  #   filter(voices_amazon, language == input$amazon_lang)
  # })
  # observeEvent(input$amazon_lang, {
  #   freezeReactiveValue(input, "amazon_gender")
  #   choices <- unique(voices_amazon_reactive()$gender)
  #   updateSelectInput(inputId = "amazon_gender", choices = choices) 
  # })
  # voices_amazon_gender_reactive <- reactive({
  #   req(input$amazon_gender)
  #   filter(voices_amazon_reactive(), gender == input$amazon_gender)
  # })
  # observeEvent(input$amazon_gender, {
  #   freezeReactiveValue(input, "amazon_voice")
  #   choices <- unique(voices_amazon_gender_reactive()$voice)
  #   updateSelectInput(inputId = "amazon_voice", choices = choices) 
  # })
  # 
  # # Google
  # voices_google_reactive <- reactive({
  #   filter(voices_google, language == input$google_lang)
  # })
  # observeEvent(input$google_lang, {
  #   freezeReactiveValue(input, "google_gender")
  #   choices <- unique(voices_google_reactive()$gender)
  #   updateSelectInput(inputId = "google_gender", choices = choices) 
  # })
  # voices_google_gender_reactive <- reactive({
  #   req(input$google_gender)
  #   filter(voices_google_reactive(), gender == input$google_gender)
  # })
  # observeEvent(input$google_gender, {
  #   freezeReactiveValue(input, "google_voice")
  #   choices <- unique(voices_google_gender_reactive()$voice)
  #   updateSelectInput(inputId = "google_voice", choices = choices) 
  # })
  # 
  # # Microsoft
  # voices_ms_reactive <- reactive({
  #   filter(voices_ms, locale == input$ms_locale)
  # })
  # observeEvent(input$ms_locale, {
  #   freezeReactiveValue(input, "ms_gender")
  #   choices <- unique(voices_ms_reactive()$gender)
  #   updateSelectInput(inputId = "ms_gender", choices = choices) 
  # })
  # voices_ms_gender_reactive <- reactive({
  #   req(input$ms_gender)
  #   filter(voices_ms_reactive(), gender == input$ms_gender)
  # })
  # observeEvent(input$ms_gender, {
  #   freezeReactiveValue(input, "ms_voice")
  #   choices <- unique(voices_ms_gender_reactive()$name)
  #   updateSelectInput(inputId = "ms_voice", choices = choices) 
  # })
  # 
  
  
  # Generate video ----
  observeEvent(input$generate, {
    # Create a progress bar
    progress <- AsyncProgress$new(message = "Processing...")
    # Inputs used inside future_promise()
    service <- input$service
    coqui_model_name <- input$coqui_model_name
    coqui_vocoder_name <- ifelse(coqui_model_name == "jenny", 
                                 "jenny", 
                                 "ljspeech/univnet")
    which_tool <- input$presentation_tool
    burn_subtitle <- input$burn_subtitle
    gs_url <- input$gs_url
    pptx_upload_datapath <- input$pptx_file$datapath
    user_email <- input$email
    auto_email <- input$auto_email
    video_name <- video_name()
    video_name_subtitle <- video_name_subtitle()
    app_url <- "https://loqui.fredhutch.org"
    
    res <- reactiveVal()
    future_promise({
      # extract speaker notes
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      # download google slides as pptx
      if(which_tool == "google_slides") {
        pptx_path <- gsplyr::download(gs_url, type = "pptx")
      } else {
        # or fetch path to pptx on server
        pptx_path <- pptx_upload_datapath
      }
      progress$inc(amount = 1/5, message = "Processing...")
      pptx_notes_vector <- ptplyr::extract_notes(pptx_path)
      progress$inc(amount = 1/5, message = "Processing...")
      
      # download as pdf
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      # download google slides as pdf
      if (which_tool == "google_slides") {
        pdf_path <- gsplyr::download(gs_url, type = "pdf")
      } else {
        # convert pptx slides to pdf
        if (Sys.info()['sysname'] == "Linux") {
          Sys.setenv(LD_LIBRARY_PATH="")
        }
        pdf_path <- ptplyr::convert_pptx_pdf(pptx_upload_datapath)
      }
      progress$inc(amount = 1/5, message = "Processing...")
      
      # convert to png
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      image_path <- ptplyr::convert_pdf_png(pdf_path)
      progress$inc(amount = 1/5, message = "Processing...")
      progress$inc(amount = 0, message = "This step requires a few minutes...")
      Sys.sleep(2)
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      
      # combine images and notes with ari_spin()
      switch(service,
             coqui = ari::ari_spin(images = image_path, 
                                   paragraphs = pptx_notes_vector,
                                   output = video_name,
                                   tts_engine_args = coqui_args(coqui_model_name,
                                                                coqui_vocoder_name),
                                   subtitles = burn_subtitle),
             amazon = ari::ari_spin(images = image_path, 
                                    paragraphs = pptx_notes_vector,
                                    service = "amazon",
                                    voice = input$amazon_voice,
                                    output = video_name),
             google = ari::ari_spin(images = image_path, 
                                    paragraphs = pptx_notes_vector,
                                    service = "google",
                                    voice = input$google_voice,
                                    output = video_name),
             ms = ari::ari_spin(images = image_path, 
                                paragraphs = pptx_notes_vector,
                                service = "microsoft",
                                voice = input$ms_voice,
                                output = video_name)
      )
      
      # Burn subtitles
      if (burn_subtitle) {
        srt_file <- paste0(tools::file_path_sans_ext(video_name), ".srt")
        ari_burn_subtitles(video_name, srt_file, video_name_subtitle)
      }
      
      progress$inc(amount = 1/5, message = "Processing...Done!", detail = "100%")
      Sys.sleep(3)
      progress$close()
      
      # Email
      if (auto_email) {
        # Video Link
        if (burn_subtitle) {
          video_name_processed <- gsub("www/", "", video_name_subtitle)
        } else {
          video_name_processed <- gsub("www/", "", video_name)
        }
        video_link <- paste0(app_url, "/", "i", "/", video_name_processed)
        # Date/Time
        date_time <- add_readable_time()
        # Compose Email
        email <- compose_email(
          body = md(glue::glue(
            "Dear Loqui User,
            
To access the video, simply click on the following link: [{video_link}]({video_link}). To download the video, click the three 
vertical dots and select 'Download'.
            
We also invite you to visit our website at [https://hutchdatascience.org](https://hutchdatascience.org)
to explore a wealth of valuable resources and stay updated on the latest news from
the Fred Hutch Data Science Lab (DaSL). 

Feel free to reach out to us with any questions at howardbaek@fredhutch.org or by filing a [GitHub issue](https://github.com/FredHutch/loqui/issues).
We will respond to your inquiries as soon as possible.
            
Howard Baek
")),
          footer = md(glue::glue("Email automatically sent on {date_time}."))
        )
        # Send email
        email %>% 
          smtp_send(
            from = "loqui-noreply@fredhutch.org",
            to = user_email,
            subject = "Video Generated by Loqui from Fred Hutch Data Science Lab (DaSL)",
            credentials = creds_anonymous(host = "mx.fhcrc.org", port = 25)
          )
      }
      
      # Google Sheets
      if (burn_subtitle) {
        ffmpeg_cmd <- paste0("-i", " ", video_name_subtitle, " ", "2>&1 | grep \"Duration\"")
      } else {
        ffmpeg_cmd <- paste0("-i", " ", video_name, " ", "2>&1 | grep \"Duration\"")
      }
      duration_raw <- system2("ffmpeg", ffmpeg_cmd, stdout = TRUE)
      duration_raw <- regmatches(duration_raw, regexpr("Duration: (\\d{2}:\\d{2}:\\d{2}\\.\\d{2})", duration_raw))
      video_duration <- sub("Duration: ([0-9:.]+)", "\\1", duration_raw)
      date_time <- add_readable_time()
      # Authorize
      gs4_auth(cache=".secrets", email="howardbaek.fh@gmail.com")
      # Append
      gs_url <- "https://docs.google.com/spreadsheets/d/1G_HTU-bv2k5txExP8EH3ScUfGqtW1P3syThD84Z-g9k/edit?usp=sharing"
      sheet_append(gs_url,
                   data.frame(date_time = date_time,
                              video_duration = video_duration, 
                              email = user_email))
      
      # Final output
      # Replace "www" with "i"
      if (burn_subtitle) {
        gsub("www", "i", video_name_subtitle)
      } else {
        gsub("www", "i", video_name)
      }
    }) %...>% res
    
    # Show video when "Generate" is clicked
    output$video_ui <- renderUI({
      res <- res()
      tags$video(src = res, 
                 type = "video/mp4",
                 height ="480px", 
                 width="854px",
                 autoplay = TRUE,
                 controls = TRUE)
    })
    
    # Extract video info
    if (which_tool == "google_slides") {
      pdf_path <- gsplyr::download(gs_url, "pdf")
    } else {
      # convert pptx slides to pdf
      if (Sys.info()['sysname'] == "Linux") {
        Sys.setenv(LD_LIBRARY_PATH="")
      }
      pdf_path <- ari::pptx_to_pdf(pptx_upload_datapath)
    }
    pdf_info <- pdftools::pdf_info(pdf = pdf_path)
    
    # Show video title
    output$video_info <- renderUI({
      span(textOutput("video_title"),
           style = "font-weight: bold;
                    font-family: Arial;
                    font-size: 25px;
                    color: #1c3b61")
      output$video_title <- renderText({
        pdf_info$keys$Title
      })
    })
    # Show video buttons (download/send email)
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
      if (input$burn_subtitle) {
        file.copy(video_name_subtitle(), file)
      } else {
        file.copy(video_name(), file)
      }
    },
    contentType = "video/mp4"
  )
  
  # Send email
  observeEvent(input$send_email, {
    # Dialog Box
    showModal(modalDialog(
      title = span(h4("Success message:"), style = "color: #1c3b61;font-family:Times;font-weight: bold;"),
      span(paste0("Email with the video file has been sent to ", input$email, "."), style = "color: #1c3b61;font-family:Arial")
    ))
    
    # Video Link
    video_name_processed <- gsub("www/", "", video_name())
    # Video Link
    app_url <- "https://loqui.fredhutch.org"
    video_link <- paste0(app_url, "/", "i", "/", video_name_processed)
    # Date/Time
    date_time <- add_readable_time()
    # Compose Email
    email <- compose_email(
      body = md(glue::glue(
        "Dear Loqui User,
            
To access the video, simply click on the following link: [{video_link}]({video_link}). To download the video, click the three 
vertical dots and select 'Download'.
            
We also invite you to visit our website at [https://hutchdatascience.org](https://hutchdatascience.org)
to explore a wealth of valuable resources and stay updated on the latest news from
the Fred Hutch Data Science Lab (DaSL). 

Feel free to reach out to us with any questions at howardbaek@fredhutch.org or by filing a [GitHub issue](https://github.com/FredHutch/loqui/issues).
We will respond to your inquiries as soon as possible.
            
Howard Baek
")),
      footer = md(glue::glue("Email sent on {date_time}."))
    )
    # Send email
    email %>% 
      smtp_send(
        from = "loqui-noreply@fredhutch.org",
        to = input$email,
        subject = "Video Generated by Loqui from Fred Hutch Data Science Lab (DaSL)",
        credentials = creds_anonymous(host = "mx.fhcrc.org", port = 25)
      )
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