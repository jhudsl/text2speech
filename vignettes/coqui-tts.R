## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(text2speech)

## ---- eval = FALSE------------------------------------------------------------
#  set_coqui_path("your/path/to/tts")

## ---- eval = FALSE------------------------------------------------------------
#  tts_voices(service = "coqui")

## ---- eval = FALSE------------------------------------------------------------
#  tts(text = "Hello world!", service = "coqui")

## ---- eval = FALSE------------------------------------------------------------
#  tts(text = "Hello world, using a different voice!",
#      service = "coqui",
#      model_name = "fast_pitch",
#      vocoder_name = "ljspeech/hifigan_v2")

## ---- eval = FALSE------------------------------------------------------------
#  tts(text = "Hello world! I am saving the audio output in a local folder",
#      service = "coqui",
#      save_local = TRUE,
#      save_local_dest = "/full/path/to/local/folder")

## ---- echo = FALSE------------------------------------------------------------
sessionInfo()

