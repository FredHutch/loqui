# Packages ----

## Shiny + ari
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(ari)

## Data manipulation
library(dplyr)
library(readr)
library(pdftools)

## Google Sheets+Email
library(blastula)
library(googlesheets4)

# The promises library is necessary for the %...>% operator.
library(promises) 

# The future library is needed for the future() function call used inside future_promise()
# This is how you will launch asynchronous tasks.
library(future) 

# Launches up to 25 background R processes on the same machine
plan(multisession, workers = 25)
library(ipc)

## PPTX/Google Slide manipulation
library(gsplyr)
library(ptplyr)

# Options ----
options("future.rng.onMisuse" = "ignore",
        shiny.maxRequestSize = 10 * 1024^2) # Maximum file upload size: 10 MB

# Images for pickerInput stored in i/ from the root app directory
imgs <- c("i/img/coqui.png", "i/img/aws.jpeg", "i/img/google.png", "i/img/ms.jpeg")
img_name <- c("Coqui TTS", "Amazon Polly", 
              "Google Cloud Text-to-Speech", "Microsoft Cognitive Services Text-to-Speech")

# Select image
select_choice_img <- function(img, text) {
  shiny::HTML(paste(
    tags$img(src=img, width=25, height=22),
    text
  ))
}

# Function to check if email is valid 
is_valid_email <- function(x) {
  grepl("([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))", x)
}


# UI ----
ui <- fluidPage(
  # Setup to use shinyjs
  shinyjs::useShinyjs(),
  # and shinyFeedback
  shinyFeedback::useShinyFeedback(),
  
  # Hutch theme CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "i/hutch_theme.css")
  ),
  # Favicon
  tags$head(tags$link(rel="shortcut icon", href="i/img/favicon.ico")),
  # CSS to center the progress bar
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
    img(src = "i/img/logo-loqui.jpeg", height = "45px"),
    "Loqui: A Shiny app for Creating Automated Videos",
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
      div(textInput("email", "Email Address (where video should be sent)"), style = "font-size: 18px"),
      div(
        shinyWidgets::prettySwitch("auto_email", "Once video finishes rendering, send email automatically",
                                   value = TRUE, status = "success", fill = TRUE),
        style = "color: #1c3b61;font-size:16px"
      ),
      div(
        h5("NOTE: For Automatic Emails, Keep Browser Open"),
        style = "color: #1c3b61;font-size:16px"
      ),
      div(
        radioButtons("presentation_tool", "Presentation Tool",
                     c("Google Slides" = "google_slides",
                       "Powerpoint" = "powerpoint")),
        style = "font-size:18px"
      ),
      div(
        prettySwitch("burn_subtitle", "Embed Subtitles",
                     value = TRUE, status = "success", fill = TRUE),
        style = "color: #1c3b61;font-size:16px"
      ),
      uiOutput("user_input"),
      div(
        shinyWidgets::pickerInput("service",
                                  label = "Text-to-Speech Service", 
                                  choices = c("Coqui TTS" = "coqui"),
                                  choicesOpt = list(content = purrr::map2(imgs, img_name, select_choice_img)[[1]])),
        style = "font-size:18px"
      ),
      uiOutput("voice_options"),
      actionButton("generate", "Generate", icon = icon("person-running")),
      br(),
      br(),
      tags$img(src = "i/img/logo.png", width = "90%"),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "i/img/posit.jpeg", height = "30px")
      )
    ),
    mainPanel(
      tabsetPanel(id = "inTabset",
                  tabPanel(
                    title = div("About",
                                style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                    value = "about",
                    div( 
                      includeHTML("include-about.html"),
                      uiOutput("loqui_demo"),
                      h5("Privacy Policy: We only collect the date and time of usage, duration of the generated video, and the provided email address."),
                      h5("This initiative is funded by the following grant: National Cancer Institute (NCI) UE5 CA254170"),
                      style = "font-family: Arial; color: #1c3b61; font-size: 1.65rem")
                  ),
                  tabPanel(
                    title = div("Tips",
                                style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                    value = "tips",
                    div( 
                      includeHTML("include-tips.html"),
                      style = "font-family: Arial; color: #1c3b61; font-size: 1.65rem")
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

# Server ----
server <- function(input, output, session) {
  # Disable buttons when email is not provided and Google Slides URL (if provided) is not accessible
  observe({
    shinyjs::toggleState("generate",
                         !is.null(input$email) && input$email != "" && is_valid_email(input$email) && 
                           (!inherits(try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE), "try-error") ||
                              is.data.frame(input$pptx_file)))
    shinyjs::toggleState("download_btn",
                         !is.null(input$email) && input$email != "" && is_valid_email(input$email) &&
                           (!inherits(try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE), "try-error") ||
                              is.data.frame(input$pptx_file)))
    shinyjs::toggleState("send_email",
                         !is.null(input$email) && input$email != "" && is_valid_email(input$email) &&
                           (!inherits(try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE), "try-error") ||
                              is.data.frame(input$pptx_file)))
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
  
  # Show different inputs depending on Google Slides or PowerPoint
  output$user_input <- renderUI({
    if (input$presentation_tool == "google_slides") {
      div(
        textInput("gs_url", 
                  label = "Google Slides URL (Enable Link Sharing)",
                  placeholder = "Paste a Google Slides URL"),
        style = "font-size:18px"
      )
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
  
  # Demo of Loqui
  output$loqui_demo <- renderUI({
    tags$video(src = "i/video/loqui.mp4", 
               type = "video/mp4",
               height ="480px", 
               width="790px",
               controls = TRUE)
  })
  
  # Voice Options
  output$voice_options <- renderUI({
    if (input$service == "coqui") {
      div(
      selectInput("coqui_model_name", "Select Model Name (Voice)", 
                  choices = c("tacotron2-DDC_ph", "jenny", "fast_pitch"),
                  selected = "jenny"),
      style = "font-size:18px"
      )
    } 
  })
  
  
  # Create single reactive value
  res <- reactiveVal()
  
  # Start: Generate video ----
  observeEvent(input$generate, {
    # Create a progress bar
    progress <- AsyncProgress$new(message = "Processing...")
    
    # Read inputs to be used inside future_promise()
    service <- input$service
    coqui_model_name <- input$coqui_model_name
    coqui_vocoder_name <- switch(coqui_model_name,
                                 "jenny" = "jenny",
                                 "tacotron2-DDC_ph" = "ljspeech/univnet",
                                 "fast_pitch" = "ljspeech/hifigan_v2",
                                 stop("Invalid model name"))
    
    which_tool <- input$presentation_tool
    burn_subtitle <- input$burn_subtitle
    gs_url <- input$gs_url
    pptx_upload_datapath <- input$pptx_file$datapath
    user_email <- input$email
    auto_email <- input$auto_email
    video_name <- video_name()
    video_name_subtitle <- video_name_subtitle()
    app_url <- "https://loqui.fredhutch.org"
    
    # To learn more about {promises}, start here:
    # <https://rstudio.github.io/promises/articles/promises_01_motivation.html>
    # Read this to learn about using promises with Shiny:
    # <https://rstudio.github.io/promises/articles/promises_06_shiny.html>
    # See below for using promises inside observers:
    # <https://rstudio.github.io/promises/articles/promises_06_shiny.html#observers>
    
    # future_promise(): Creates a promise() that will execute the expr using future::future()
    # For information on `future_promise()`,
    # see <https://rstudio.github.io/promises/articles/promises_05_future_promise.html> 
    
    promises::future_promise({
      # Progress message
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      # Download google slides as pptx
      if(which_tool == "google_slides") {
        pptx_path <- gsplyr::download(gs_url, type = "pptx")
      } else {
        # or fetch path to pptx on server
        pptx_path <- pptx_upload_datapath
      }
      # Progress message
      progress$inc(amount = 1/5, message = "Processing...")
      # Extract speaker notes
      pptx_notes_vector <- ptplyr::extract_notes(pptx_path)
      # Progress message
      progress$inc(amount = 1/5, message = "Processing...")
      
      # Progress message
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      # Download google slides as pdf
      if (which_tool == "google_slides") {
        pdf_path <- gsplyr::download(gs_url, type = "pdf")
      } else {
        # Convert pptx slides to pdf
        if (Sys.info()['sysname'] == "Linux") {
          Sys.setenv(LD_LIBRARY_PATH="")
        }
        pdf_path <- ptplyr::convert_pptx_pdf(pptx_upload_datapath)
      }
      
      # Extract video title from pdf file
      pdf_info <- pdftools::pdf_info(pdf = pdf_path)
      video_title <- pdf_info$keys$Title
      
      # Progress message
      progress$inc(amount = 1/5, message = "Processing...")
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      # Convert to png
      image_path <- ptplyr::convert_pdf_png(pdf_path)
      # Progress message
      progress$inc(amount = 1/5, message = "Processing...")
      progress$inc(amount = 0, message = "This step requires a few minutes...")
      Sys.sleep(2)
      progress$inc(amount = 0, message = "Processing takes a few minutes...")
      
      # Generate videos from images (image_path) and text (pptx_notes_vector)
      ari::ari_spin(images = image_path, 
                    paragraphs = pptx_notes_vector,
                    output = video_name,
                    tts_engine_args = ari::coqui_args(coqui_model_name,
                                                      coqui_vocoder_name),
                    subtitles = TRUE)
      
      # Burn subtitles
      if (burn_subtitle) {
        srt_file <- paste0(tools::file_path_sans_ext(video_name), ".srt")
        ari::ari_burn_subtitles(video_name, srt_file, video_name_subtitle)
      }
      # Progress message
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
        date_time <- blastula::add_readable_time()
        # Compose Email
        email <- blastula::compose_email(
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
          blastula::smtp_send(
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
      date_time <- blastula::add_readable_time()
      # Authorize
      googlesheets4::gs4_auth(cache=".secrets", email="howardbaek.fh@gmail.com")
      # Append
      gs_url <- "https://docs.google.com/spreadsheets/d/1G_HTU-bv2k5txExP8EH3ScUfGqtW1P3syThD84Z-g9k/edit?usp=sharing"
      googlesheets4::sheet_append(gs_url,
                                  data.frame(date_time = date_time,
                                             video_duration = video_duration, 
                                             email = user_email))
      
      # Final output
      # Replace "www" with "i"
      if (burn_subtitle) {
        rendered_video_path <- gsub("www", "i", video_name_subtitle)
      } else {
        rendered_video_path <- gsub("www", "i", video_name)
      }
      
      final_res <- c(rendered_video_path, video_title)
      final_res
    }) %...>% res
    # '%...>%' is the promise pipe. 
    # See <https://rstudio.github.io/promises/articles/promises_02_intro.html> for 
    # more details on the promise pipe.
    
    # Show video when "Generate" is clicked
    output$video_ui <- renderUI({
      res <- res()[1]
      tags$video(src = res, 
                 type = "video/mp4",
                 height ="480px", 
                 width="854px",
                 autoplay = TRUE,
                 controls = TRUE)
    })
    
    # Show video title
    output$video_info <- renderUI({
      span(textOutput("video_title"),
           style = "font-weight: bold;
                    font-family: Arial;
                    font-size: 25px;
                    color: #1c3b61")
      output$video_title <- renderText({
        res()[2]
      })
    })
    
    # Show video buttons (download/send email)
    output$video_btn <- renderUI({
      column(12,
             downloadButton("download_btn", label = "Download Video"),
             downloadButton("download_subtitle_btn", label = "Download Subtitles"),
             actionButton("send_email", "Email", icon = icon("inbox")),
             align = "left"
      )
    })
  })
  # End: Generate video ----
  
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
  
  # Download subtitles (if exists)
  output$download_subtitle_btn <- downloadHandler(
    filename = "loqui_video_subtitle.srt",
    content = function(file) {
      srt_file <- paste0(tools::file_path_sans_ext(video_name()), ".srt")
      file.copy(srt_file, file)
    },
  )
  
  # Send email
  observeEvent(input$send_email, {
    # Dialog Box
    showModal(modalDialog(
      title = span(h4("Success message:"), style = "color: #1c3b61;font-family:Times;font-weight: bold;"),
      span(paste0("Email with the video file has been sent to ", input$email, "."), style = "color: #1c3b61;font-family:Arial")
    ))
    
    # Video Link
    if (input$burn_subtitle) {
      video_name_processed <- gsub("www/", "", video_name_subtitle())
    } else {
      video_name_processed <- gsub("www/", "", video_name())
    }
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
