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



# make a nice data.frame that links the audios and pngs together: ari_files_df

pad_wav <- function(wav, duration = NULL) {
  # See if wav inherits from "Wave" class
  is_Wave <- inherits(wav, "Wave")
  if (is_Wave) {
    wav <- list(wav)
  }
  if (is.null(duration)) {
    duration <- rep(NA, length(wav))
  }
  stopifnot(length(duration) == length(wav))
  # Iterate over wav and find "ideal duration"
  duration <- map2_int(.x = wav, .y = duration,
                       .f = function(wav, dur) {
                         ideal_duration <- ceiling(length(wav@left) / wav@samp.rate)
                         if (!is.na(dur)) {
                           ideal_duration <- max(ideal_duration, dur)
                         }
                         ideal_duration
                       })
  # Iterate over wav and create end_wav that binds to existing wav
  out_wav <- map2(.x = wav,
                  .y = duration,
                  .f = function(wav, ideal_duration) {
                    left <- rep(0, wav@samp.rate * ideal_duration - length(wav@left))
                    right <- numeric(0)
                    if (wav@stereo) {
                      right <- left
                    }
                    end_wav <- tuneR::Wave(
                      left = left,
                      right = right,
                      bit = wav@bit,
                      samp.rate = wav@samp.rate, 
                      pcm = wav@pcm
                    )
                    wav <- tuneR::bind(wav, end_wav)
                    wav
                  })
  
  if (is_Wave) {
    out_wav <- out_wav[[1]]
  }
  
  return(out_wav)
}


match_sample_rate <- function(audio, verbose = TRUE) {
  if (inherits(audio, "Wave")) {
    return(audio)
  }
  
  sample_rate <- sapply(audio, function(r) r@samp.rate)
  if (!all(sample_rate == sample_rate[[1]]) && verbose) {
    message("enforcing same sample rate, using minimum")
  }
  sample_rate <- min(sample_rate, na.rm = TRUE)
  if (verbose) {
    message(paste0("Sample rate downsampled to ", sample_rate))
  }
  audio <- lapply(audio, function(x) {
    if (x@samp.rate == sample_rate) {
      return(x)
    }
    tuneR::downsample(x, samp.rate = sample_rate)
  })
  sample_rate <- sapply(audio, function(r) r@samp.rate)
  stopifnot(all(sample_rate == sample_rate[[1]]))
  return(audio)
}


## Weave audio and images together
ari_stitch <- function(images, 
                       audio,
                       output = tempfile(fileext = ".mp4"),
                       verbose = FALSE,
                       cleanup = TRUE,
                       ffmpeg_opts = "",
                       divisible_height = TRUE,
                       audio_codec = get_audio_codec(),
                       video_codec = get_video_codec(),
                       video_sync_method = "2",
                       audio_bitrate = NULL,
                       video_bitrate = NULL,
                       pixel_format = "yuv420p",
                       fast_start = FALSE,
                       deinterlace = FALSE,
                       stereo_audio = TRUE,
                       duration = NULL,
                       video_filters = NULL,
                       frames_per_second = NULL,
                       check_inputs = TRUE) {
  # Stop if there are no images
  stopifnot(length(images) > 0)
  # Normalize paths of images and output
  images <- normalizePath(images)
  output_dir <- normalizePath(dirname(output))
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
  audio_wav <- lapply(audio, tuneR::readWave)
  
  audio_pad <- pad_wav(audio_wav, duration = duration)
  
  
  # Make a hard path
  output <- file.path(output_dir, basename(output))
  
  if (verbose > 0) {
    message("Writing out Wav for audio")
  }
  if (verbose > 1) {
    print(audio)
  }
  audio <- make_same_sample_rate(audio, verbose = verbose)
  wav <- purrr::reduce(audio, bind)
  wav_path <- file.path(output_dir, paste0("ari_audio_", grs(), ".wav"))
  writeWave(wav, filename = wav_path)
  if (cleanup) {
    on.exit(unlink(wav_path, force = TRUE), add = TRUE)
  }
  
  
  # converting all to gif
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
  
  input_txt_path <- file.path(
    output_dir,
    paste0(
      "ari_input_",
      grs(),
      ".txt"
    )
  )
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
  for (i in seq_along(images)) {
    cat(paste0("file ", "'", images[i], "'", "\n"),
        file = input_txt_path, append = TRUE
    )
    cat(paste0("duration ", duration(audio[[i]]), "\n"),
        file = input_txt_path, append = TRUE
    )
  }
  cat(paste0("file ", "'", images[i], "'", "\n"),
      file = input_txt_path, append = TRUE
  )
  input_txt_path <- normalizePath(input_txt_path, winslash = "/")
  
  # needed for users as per
  # https://superuser.com/questions/718027/
  # ffmpeg-concat-doesnt-work-with-absolute-path
  # input_txt_path = normalizePath(input_txt_path, winslash = "\\")
  
  ffmpeg <- ffmpeg_exec(quote = TRUE)
  
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
    " -shortest",
    # ifelse(deinterlace, "-vf yadif", ""),
    ifelse(!is.null(video_sync_method), paste("-vsync", video_sync_method),
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
  invisible(res)
}


