# Create dataset of voices list (07/07/2023)
# Update: Include jenny model from Coqui TTS
library(dplyr)
library(readr)
library(text2speech)
library(jsonlite)

# Coqui TTS
voices_coqui_raw <- tts_voices(service = "coqui")
# language -> dataset -> model_name
voices_coqui_raw %>% write_csv("data/voices-coqui.csv")

# Google Cloud
voices_google_raw <- tts_voices(service = "google") %>% 
  as_tibble()
# language -> gender -> voice
voices_google_raw %>% write_csv("data/voices-google.csv")

# Amazon Polly
voices_amazon_raw <- tts_voices(service = "amazon") %>% 
  as_tibble()
# language -> gender -> voice
voices_amazon_raw %>% write_csv("data/voices-amazon.csv")

# Microsoft
voices_ms_raw <- fromJSON("data/voices-ms.json") %>% 
  as_tibble()
# locale -> gender -> name
voices_ms_raw %>% write_csv("data/voices-ms.csv")
