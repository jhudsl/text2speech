#' Convert Text to Speech using Google Cloud Text-to-Speech API
#' @export
#' @rdname tts
#' @param voice A full voice name that can be passed to the
#' service, such as the
#' argument `voice` for `get_synthesis`` from \code{aws.polly}, or
#' or [mscstts::ms_synthesize()] or the
#' `name` argument for [googleLanguageR::gl_talk()]
#' @examples
#' tts_default_voice("amazon")
#' tts_default_voice("google")
#' tts_default_voice("microsoft")
tts_google = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "en-US-Standard-C",
    bind_audio = TRUE,
    ...) {

  limit = 5000
  output_format = match.arg(output_format)
  audio_type = output_format

  output_format = switch(
    output_format,
    "mp3" = "MP3",
    "wav" = "LINEAR16")

  res = lapply(text, function(string) {
    strings = tts_split_text(string, limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)
      out = googleLanguageR::gl_talk(
        tt,
        output = output,
        audioEncoding = output_format,
        name = voice,
        ...)
    }, FUN.VALUE = character(1L))
    names(res) = NULL
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    # out = do.call(tuneR::bind, out)
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)
  res$audio_type = audio_type

  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }

  return(res)
}

#' Convert Text to Speech using Amazon Polly
#' @export
#' @rdname tts
#' @examples \dontrun{
#' text='<speak>
#'   He was caught up in the game.<break time="1s"/> In the middle of the
#'   10/3/2014 <sub alias="World Wide Web Consortium">W3C</sub> meeting,
#'   he shouted, "Nice job!" quite loudly. When his boss stared at him, he repeated
#'   <amazon:effect name="whispered">"Nice job,"</amazon:effect> in a
#'   whisper.
#' </speak>'
#' }
tts_amazon = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "Joanna",
    bind_audio = TRUE,
    ...) {
  if (!requireNamespace("aws.polly", quietly = TRUE)) {
    stop(paste0(
      "This function requires aws.polly to operate",
      " please use\n",
      "install.packages('aws.polly')\n",
      "to use these functions"))
  }
  limit = 1500
  output_format = match.arg(output_format)
  audio_type = output_format

  sample_rate = switch(
    output_format,
    "mp3" = 24000,
    "wav" = 16000)
  output_format = switch(
    output_format,
    "mp3" = "mp3",
    "wav" = "pcm")

  args = list(...)
  if (is.null(args$rate)) {
    args$rate = sample_rate
  }
  if (!is.null(args$format)) {
    warning(
      paste0(
        "format was specified in ... for tts_amazon",
        ", this should be specified in output_format argument, format",
        " is overridden")
    )
  }
  res = lapply(text, function(string) {
    strings = tts_split_text(string,
                             limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)

      args$text = tt
      args$voice = voice
      args$format = output_format

      out = do.call(
        aws.polly::get_synthesis,
        args = args)

      writeBin(out, con = output)
      if (audio_type == "wav") {
        output = pcm_to_wav(input = output, sample_rate = args$rate)
      }
      output
    }, FUN.VALUE = character(1L))
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    df
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)
  res$audio_type = audio_type
  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }

  return(res)

}


#' Convert Text to Speech using Microsoft Cognitive Services API
#' @export
#' @rdname tts
tts_microsoft = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "Microsoft Server Speech Text to Speech Voice (en-US, ZiraRUS)",
    bind_audio = TRUE,
    ...) {

  limit = 800
  output_format = match.arg(output_format)
  audio_type = output_format

  output_format = switch(
    output_format,
    "mp3" = "audio-24khz-160kbitrate-mono-mp3",
    "wav" = "riff-24khz-16bit-mono-pcm")


  res = lapply(text, function(string) {
    strings = tts_split_text(string,
                             limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)
      out = mscstts::ms_synthesize(
        tt,
        output_format = output_format,
        voice = voice,
        ...)
      writeBin(out$content, con = output)
      output
    }, FUN.VALUE = character(1L))
    names(res) = NULL
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    # out = do.call(tuneR::bind, out)
    df
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)

  res$audio_type = audio_type
  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }

  return(res)
}


# TODO: Add https://huggingface.co/spaces/coqui/CoquiTTS to roxygen comments
# Best tts and vocoder models: https://github.com/coqui-ai/TTS/discussions/1891
tts_coqui <- function(
    text,
    # path to tts
    exec_path,
    output_format = c("wav", "mp3"),
    model_name = "tacotron2-DDC_ph",
    vocoder_name = "univnet",
    bind_audio = TRUE,
    other_model = NULL,
    ...) {
  # Is there a max number of limits that coqui TTS takes? (https://github.com/coqui-ai/TTS/discussions/917)
  limit <- 2500
  output_format = match.arg(output_format)
  audio_type = output_format

  # TODO: Give user option to save output in temporary file / local folder

  # TODO: Include argument checks (model_name, vocoder_name)

  # English models names
  model_name <- switch(
    model_name,
    "tacotron2-DDC_ph" = "tts_models/en/ljspeech/tacotron2-DDC_ph",
    "vits"             = "tts_models/en/ljspeech/vits",
    "glow-tts"         = "tts_models/en/ljspeech/glow-tts",
    "speedy-speech"    = "tts_models/en/ljspeech/speedy-speech",
    "tacotron2-DCA"    = "tts_models/en/ljspeech/tacotron2-DCA",
    "tacotron2-DDC"    = "tts_models/en/ljspeech/tacotron2-DDC",
    "vits--neon"       = "tts_models/en/ljspeech/vits--neon",
    "fast_pitch"       = "tts_models/en/ljspeech/fast_pitch",
    "overflow"         = "tts_models/en/ljspeech/overflow",
    "neural_hmm"       = "tts_models/en/ljspeech/neural_hmm",
    "tacotron-DDC"     = "tts_models/en/sam/tacotron-DDC",
    "capacitron-t2-c50"= "tts_models/en/blizzard2013/capacitron-t2-c50",
    "capacitron-t2-c150_v2" = "tts_models/en/blizzard2013/capacitron-t2-c150_v2"
  )
  # Universal/English vocoder dataset/model
  vocoder_name <- switch(
    vocoder_name,
    "libri-tts/wavegrad"        =  "vocoder_models/universal/libri-tts/wavegrad",
    "libri-tts/fullband-melgan" =  "vocoder_models/universal/libri-tts/fullband-melgan",
    "ek1/wavegrad"              =  "vocoder_models/en/ek1/wavegrad",
    "ljspeech/multiband-melgan" =  "vocoder_models/en/ljspeech/multiband-melgan",
    "ljspeech/hifigan_v2"       =  "vocoder_models/en/ljspeech/hifigan_v2",
    "ljspeech/univnet"          =  "vocoder_models/en/ljspeech/univnet",
    "blizzard2013/hifigan_v2"   =  "vocoder_models/en/blizzard2013/hifigan_v2",
    "vctk/hifigan_v2"           =  "vocoder_models/en/vctk/hifigan_v2",
    "sam/hifigan_v2"            = "vocoder_models/en/sam/hifigan_v2"
  )

  # Iterate coqui tts over text
  res = lapply(text, function(string) {
    string_processed = tts_split_text(string, limit = limit)

    res = vapply(string_processed, function(tt) {
      output_path = tts_temp_audio(audio_type)
      tts_args <- paste0("--text", " ", shQuote(tt), " ",
                         "--model_name", " ", model_name, " ",
                         "--vocoder_name", " ", vocoder_name,
                         " ", "--out_path /private", output_path)
      # # Run command with temporary system search path
      res <- withr::with_path(exec_path,
                              system2("tts", tts_args))
      # Output file path
      output_path
    }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = string_processed,
                       wav = out, file = res)
  })
  # Post-processing
  names(res) = seq_along(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)
  res$audio_type = audio_type

  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }
  res
}

#' @rdname tts
#' @export
tts_default_voice = function(
    service = c("amazon", "google", "microsoft", "coqui")
) {
  voice = switch(
    service,
    google = "en-US-Standard-C",
    microsoft = "Microsoft Server Speech Text to Speech Voice (en-US, ZiraRUS)",
    amazon = "Joanna",
    coqui = "tacotron2-DDC")

  voice
}
