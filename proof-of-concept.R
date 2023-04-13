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
pdf_file <- drive_download("https://docs.google.com/presentation/d/1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk/edit#slide=id.p", type = "pdf", overwrite = TRUE)

# TODO: have this save the pngs to separate folder so it is not so sloppy
# pdf to pngs conversion
pdf_convert(pdf_file$local_path, format = "png", dpi = 1000)

pdf_file <- drive_download("https://docs.google.com/presentation/d/1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk/edit#slide=id.p", type = "pdf", overwrite = TRUE)

#### Get the text

pptx_file <- drive_download("https://docs.google.com/presentation/d/1sFsRXfK7LKxFm-ydib5dxyQU9fYujb95katPRu0WVZk/edit#slide=id.p", type = "pptx", overwrite = TRUE)
pptx_notes_vector <- pptx_notes("Mario video test slides.pptx")


### Create the audio files
create_single_audio <- function(slide_text, output_file) {
  
  # slide_text - text that is derived from the notes from downloaded googleslides. Is supplied to --text in TTS
  # output_file - user specified path argument default would be current working directory. Is supplied to --out_path in TTS
  # model_name - default model is tts_models/en/ljspeech/tacotron2-DDC No other models available at this time. Supplied to --model_name in TTS
  
  tts_command <- paste("tts --text", slide_text,
                       "--model_name tts_models/en/ljspeech/tacotron2-DDC",
                       "--out_path", output_file)
  
  source(tts_command)
  
  message(paste("Output file stored in ", output_file))
  
  return(output_file)
}


### Use purrr instead of this and supply the slide text and the name of the output_file (name it something that makes sense)
all_audio_files <- lapply(pptx_notes_vector, create_single_audio)

# make a nice data.frame that links the audios and pngs together ari_files_df

## Weave audio and text together

## ari_stitch(ari_files_df$pngs, ari_files_df$audio_files)

# https://github.com/jhudsl/ari/blob/main/R/ari_stitch.R

pptx_notes = function(file, ...) {
  
  df = pptx_slide_note_df(file, ...)
  if (is.null(df)) {
    return(NULL)
  }
  # need factor because they can be dumb with characters
  # and numerics and the file naming of PPTX files
  fac = basename(df$file)
  fac = factor(fac, levels = unique(fac))
  ss = split(df, fac)
  res = sapply(ss, function(x) {
    paste(x$text, collapse = " ")
  })
  if (any(trimws(res) %in% "")) {
    warning("Slides with no notes exists")
  }
  res[ res == ""] = ";"
  return(res)
}

#' @export
#' @rdname pptx_notes
pptx_slide_text_df = function(file, ...) {
  
  L = unzip_pptx(file)
  slides = L$slides
  
  if (length(slides) > 0) {
    # in case empty notes
    res = lapply(slides, function(x) {
      xx = xml_notes(x, collapse_text = FALSE, ...)
      if (length(xx) == 0) {
        return(NULL)
      }
      snum = sub("[.]xml", "", sub("slide", "", basename(x)))
      snum = as.numeric(snum)
      data.frame(
        file = x,
        slide = snum,
        text = xx,
        index = 1:length(xx),
        stringsAsFactors = FALSE)
    })
    res = do.call(rbind, res)
    return(res)
  } else {
    return(NULL)
  }
}

#' @export
#' @rdname pptx_notes
pptx_slide_note_df = function(file, ...) {
  
  L = unzip_pptx(file)
  notes = L$notes
  slides = L$slides
  note_dir = L$note_dir
  
  if (length(notes) > 0) {
    # in case empty notes
    assoc_notes = sub("slide", "", basename(slides))
    assoc_notes = paste0("notesSlide", assoc_notes)
    assoc_notes = file.path(note_dir, assoc_notes)
    no_fe = !file.exists(assoc_notes)
    if (any(no_fe)) {
      file.create(assoc_notes[no_fe])
      notes = assoc_notes
    }
    res = lapply(notes, function(x) {
      if (file.size(x) == 0) {
        xx = ""
      } else {
        xx = xml_notes(x, collapse_text = FALSE, ...)
      }
      if (length(xx) == 0) {
        xx = ""
      }
      snum = sub("[.]xml", "", sub("notesSlide", "", basename(x)))
      snum = as.numeric(snum)
      data.frame(
        file = x,
        slide = snum,
        text = xx,
        index = 1:length(xx),
        stringsAsFactors = FALSE)
    })
    res = do.call(rbind, res)
    return(res)
  } else {
    return(NULL)
  }
}

pptx_reorder_xml = function(files) {
  if (length(files) == 0) {
    return(files)
  }
  nums = basename(files)
  # nums = gsub(pattern = paste0(pattern, "(\\d*)[.]xml"),
  #             replacement = "\\1", nums)
  nums = sub("[[:alpha:]]*(\\d.*)[.].*", "\\1", nums)
  nums = as.numeric(nums)
  if (any(is.na(nums))) {
    warning(paste0("Trying to parse set of files (example: ", files[1],
                   ") from PPTX, failed"))
    return(files)
  }
  files = files[order(nums)]
}

#' @export
#' @rdname pptx_notes
unzip_pptx = function(file) {
  tdir = tempfile()
  dir.create(tdir)
  res = unzip(file, exdir = tdir)
  rm(res)
  slide_dir = file.path(tdir, "ppt", "slides")
  slides = list.files(path = slide_dir, pattern = "[.]xml$",
                      full.names = TRUE)
  slides = pptx_reorder_xml(slides)
  
  note_dir = file.path(tdir, "ppt", "notesSlides")
  notes = list.files(path = note_dir, pattern = "[.]xml$",
                     full.names = TRUE)
  notes = pptx_reorder_xml(notes)
  
  tdir = normalizePath(tdir)
  props_dir = file.path(tdir, "docProps")
  props_file = file.path(props_dir, "core.xml")
  ari_core_file = system.file("extdata", "docProps",
                              "core.xml", package = "ariExtra")
  if (!dir.exists(props_file)) {
    dir.create(props_dir, recursive = TRUE)
    file.copy(ari_core_file, props_file,
              overwrite = TRUE)
  }
  
  L = list(slides = slides,
           notes = notes,
           slide_dir = slide_dir,
           note_dir = note_dir,
           props_dir = props_dir,
           props_file = props_file,
           root_dir = tdir)
  return(L)
}

ex_file = system.file("extdata", "example.pptx", package = "ariExtra")
