## Proof of concept script

library(ariExtra)
library(googledrive)
library(pdftools)

# Google Slides ID
id <- "1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk"

# Google Drive Authorization
drive_auth()

###### Get the PNGs
# Save ppt file as pdf locally
pdf_file <- drive_download("https://docs.google.com/presentation/d/1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk/edit#slide=id.p", 
                           type = "pdf",
                           overwrite = TRUE)

# TODO: have this save the pngs to separate folder so it is not so sloppy
# pdf to pngs conversion
pdf_convert(pdf_file$local_path, 
            format = "png",
            dpi = 600)

#### Get the script
pptx_file <- drive_download("https://docs.google.com/presentation/d/1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk/edit#slide=id.p", 
                            type = "pptx",
                            overwrite = TRUE)
pptx_notes_vector <- pptx_notes("Mario video test slides.pptx")



### Create the audio files

# slide_text - text that is derived from the notes from downloaded googleslides. Is supplied to --text in TTS
# output_file - user specified path argument default would be current working directory. Is supplied to --out_path in TTS
# model_name - default model is tts_models/en/ljspeech/tacotron2-DDC No other models available at this time. Supplied to --model_name in TTS
create_single_audio <- function(slide_text) {
  
  tts_command <- paste0("tts --text", " \"", slide_text, "\" ",
                        "--model_name tts_models/en/ljspeech/tacotron2-DDC") # TODO: output_file
  withr::with_path("/opt/homebrew/Caskroom/miniforge/base/bin",
                   system(tts_command))
  
  # message(paste("Output file stored in ", output_file))
  # return(output_file)
}


### Use purrr instead of this and supply the slide text and the name of the output_file (name it something that makes sense)
all_audio_files <- map(pptx_notes_vector[1], create_single_audio)
