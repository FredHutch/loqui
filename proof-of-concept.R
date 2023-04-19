## Proof of concept script

library(ariExtra)
library(googledrive)
library(pdftools)
library(purrr)

# Google Drive Authorization
drive_auth()

gs_url <- "https://docs.google.com/presentation/d/1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk/edit#slide=id.g230e082e9c5_1_14"

###### Get the PNGs
# Save ppt file as pdf locally
pdf_file <- drive_download(gs_url, 
                           type = "pdf",
                           path = "loqui/test-slides.pdf",
                           overwrite = TRUE)

# pdf to png conversion
pdf_convert(pdf_file$local_path, 
            format = "png",
            # need to create png_files folder before running
            filenames = c("loqui/png_files/slide1.png",
                          "loqui/png_files/slide2.png",
                          "loqui/png_files/slide3.png",
                          "loqui/png_files/slide4.png",
                          "loqui/png_files/slide5.png",
                          "loqui/png_files/slide6.png"),
            dpi = 600)

#### Get the script
pptx_file <- drive_download(gs_url, 
                            type = "pptx",
                            path = "loqui/test-slides.pptx",
                            overwrite = TRUE)
pptx_notes_vector <- pptx_notes("loqui/test-slides.pptx")
names(pptx_notes_vector) <- NULL

### Create the audio files

# slide_text - text that is derived from the notes from downloaded googleslides. Is supplied to --text in TTS
# output_file - user specified path argument default would be current working directory. Is supplied to --out_path in TTS
# model_name - default model is tts_models/en/ljspeech/tacotron2-DDC No other models available at this time. Supplied to --model_name in TTS
create_single_audio <- function(slide_text) {
  
  tts_command <- paste0("tts --text", " \"", slide_text, "\" ",
                        "--model_name tts_models/en/ljspeech/tacotron2-DDC_ph --vocoder_name vocoder_models/en/ljspeech/univnet")
  withr::with_path("/opt/homebrew/Caskroom/miniforge/base/bin",
                   system(tts_command))
  
  # message(paste("Output file stored in ", output_file))
  # return(output_file)
}

create_single_audio(pptx_notes_vector[1])
create_single_audio(pptx_notes_vector[2])
create_single_audio(pptx_notes_vector[3])
create_single_audio(pptx_notes_vector[4])
create_single_audio(pptx_notes_vector[5])
create_single_audio(pptx_notes_vector[6])

# Iterate through ppt_notes_vector and output wav file
# map() would do the same thing as walk(), but return an empty list,
# whereas walk() returns .x
# walk(.x = pptx_notes_vector, .f = create_single_audio)
# output: loqui/tts_output1.wav, loqui/tts_output2.wav, loqui/tts_output3.wav

# TODO: Make a nice data.frame that links the audios and pngs together: ari_files_df

source("loqui/utils.R")
source("loqui/set_encoders.R")
source("loqui/ffmpeg_codecs.R")

## Weave audio and images together

# ari_stitch() arguments
images = c("loqui/png_files/slide1.png",
           "loqui/png_files/slide2.png",
           "loqui/png_files/slide3.png",
           "loqui/png_files/slide4.png",
           "loqui/png_files/slide5.png",
           "loqui/png_files/slide6.png")
audio = c("loqui/audio_files/tts_output1.wav",
          "loqui/audio_files/tts_output2.wav",
          "loqui/audio_files/tts_output3.wav",
          "loqui/audio_files/tts_output4.wav",
          "loqui/audio_files/tts_output5.wav",
          "loqui/audio_files/tts_output6.wav")

output = tempfile(fileext = ".mp4")
verbose = FALSE
cleanup = TRUE
ffmpeg_opts = ""
divisible_height = TRUE
audio_codec = get_audio_codec()
video_codec = get_video_codec()
video_sync_method = "2"
audio_bitrate = NULL
video_bitrate = NULL
pixel_format = "yuv420p"
fast_start = FALSE
deinterlace = FALSE
stereo_audio = TRUE
duration = NULL
video_filters = NULL
frames_per_second = NULL
check_inputs = TRUE


# Function body

# Stop if there are no images
stopifnot(length(images) > 0)
# Normalize paths of images and output (returen absolute path)
images <- normalizePath(images)
output_dir <- normalizePath(dirname(output))
output <- file.path(output_dir, basename(output))
# Stop if there is no audio 
stopifnot(
  length(audio) > 0,
  dir.exists(output_dir)
)
# Stop if images and audio are the same length
if (check_inputs) {
  stopifnot(
    identical(length(images), length(audio)),
    all(file.exists(images))
  )
}
# Read in wav file using tuneR::readWave()
audio <- map(audio, tuneR::readWave)
# pad wav file
audio <- pad_wav(audio, duration = duration)

if (verbose > 0) {
  message("Writing out Wav for audio")
}
if (verbose > 1) {
  print(audio)
}
audio <- match_sample_rate(audio, verbose = verbose)
# reduce audio (list) to single value
wav <- purrr::reduce(audio, tuneR::bind)
# create path to store wave file
wav_path <- file.path(output_dir, paste0("ari_audio_", get_random_string(), ".wav"))
# write wave file
tuneR::writeWave(wav, filename = wav_path)
# output: wav file that contains a voiceover of the entire script

if (cleanup) {
  on.exit(unlink(wav_path, force = TRUE), add = TRUE)
}

# converting all images to gif (if there any gif images)
img_ext <- tolower(tools::file_ext(images))
any_gif <- any(img_ext %in% "gif")
if (any_gif & !all(img_ext %in% "gif")) {
  if (verbose > 0) {
    message("Converting All files to gif!")
  }
  for (i in seq_along(images)) {
    iext <- img_ext[i]
    if (iext != "gif") {
      tfile <- tempfile(fileext = ".gif")
      ffmpeg_convert(images[i], outfile = tfile)
      images[i] <- tfile
    }
  }
}

# create txt path
input_txt_path <- file.path(output_dir, 
                            paste0("ari_input_", get_random_string(), ".txt"))

## on windows ffmpeg cancats names adding the working directory, so if
## complete url is provided it adds it twice.
if (.Platform$OS.type == "windows") {
  new_image_names <- file.path(output_dir, basename(images))
  if (!any(file.exists(new_image_names))) {
    file.copy(images, to = new_image_names)
  } else {
    warning("On windows must make basename(images) for ffmpeg to work")
  }
  images <- basename(images)
}

# adds "file 'IMAGE_PATH'" and duration 
# in a .txt file located at input_txt_path
for (ii in seq_along(images)) {
  cat(paste0("file ", "'", images[ii], "'", "\n"),
      file = input_txt_path, 
      append = TRUE)
  cat(paste0("duration ", wav_length(audio[[ii]]), "\n"),
      file = input_txt_path, 
      append = TRUE)
}
# duplicate last entry
cat(paste0("file ", "'", images[length(images)], "'", "\n"),
    file = input_txt_path, 
    append = TRUE)


# winslash: the separator to be used on Windows
input_txt_path <- normalizePath(input_txt_path, winslash = "/")

# needed for users as per
# https://superuser.com/questions/718027/
# ffmpeg-concat-doesnt-work-with-absolute-path
# input_txt_path = normalizePath(input_txt_path, winslash = "\\")

# find path to ffmpeg exectuable
ffmpeg <- ari::ffmpeg_exec(quote = TRUE)

# set video filters
if (!is.null(frames_per_second)) {
  video_filters <- c(video_filters, paste0("fps=", frames_per_second))
} else {
  video_filters <- c(video_filters, "fps=5")
}
if (divisible_height) {
  video_filters <- c(video_filters, '"scale=trunc(iw/2)*2:trunc(ih/2)*2"')
}


# workaround for older ffmpeg
# https://stackoverflow.com/questions/32931685/
# the-encoder-aac-is-experimental-but-experimental-codecs-are-not-enabled
experimental <- FALSE
if (!is.null(audio_codec)) {
  if (audio_codec == "aac") {
    experimental <- TRUE
  }
}
if (deinterlace) {
  video_filters <- c(video_filters, "yadif")
}
video_filters <- paste(video_filters, collapse = ",")
video_filters <- paste0("-vf ", video_filters)

if (any(grepl("-vf", ffmpeg_opts))) {
  warning("Found video filters in ffmpeg_opts, may not be used correctly!")
}
ffmpeg_opts <- c(video_filters, ffmpeg_opts)
ffmpeg_opts <- paste(ffmpeg_opts, collapse = " ")
# output: options to input into ffmpeg

# shQuote should seankross/ari#5
command <- paste(
  ffmpeg, "-y",
  "-f concat -safe 0 -i", shQuote(input_txt_path),
  "-i", shQuote(wav_path),
  ifelse(!is.null(video_codec), paste("-c:v", video_codec),
         ""
  ),
  ifelse(!is.null(audio_codec), paste("-c:a", audio_codec),
         ""
  ),
  ifelse(stereo_audio, "-ac 2", ""),
  ifelse(!is.null(audio_bitrate), paste("-b:a", audio_bitrate),
         ""
  ),
  ifelse(!is.null(video_bitrate), paste("-b:v", video_bitrate),
         ""
  ),
  # ifelse(deinterlace, "-vf yadif", ""),
  ifelse(!is.null(video_sync_method), paste("-fps_mode", "auto"),
         ""
  ),
  ifelse(!is.null(pixel_format), paste("-pix_fmt", pixel_format),
         ""
  ),
  ifelse(fast_start, "-movflags +faststart", ""),
  ffmpeg_opts,
  ifelse(!is.null(frames_per_second), paste0("-r ", frames_per_second), ""),
  ifelse(experimental, "-strict experimental", ""),
  "-max_muxing_queue_size 9999",
  "-threads 2",
  shQuote(output)
)
if (verbose > 0) {
  message(command)
}
if (verbose > 1) {
  message("Input text path is:")
  cat(readLines(input_txt_path), sep = "\n")
}

# IMPORTANT: run command in system
res <- system(command)


if (res != 0) {
  warning("Result was non-zero for ffmpeg")
}

if (cleanup) {
  on.exit(unlink(input_txt_path, force = TRUE), add = TRUE)
}
res <- file.exists(output) && file.size(output) > 0
if (!cleanup) {
  attr(res, "txt_path") <- input_txt_path
  attr(res, "wav_path") <- wav_path
  attr(res, "cmd") <- command
}
attr(res, "outfile") <- output
attr(res, "images") <- images

# return a (temporarily) invisible copy of res
invisible(res)
}

