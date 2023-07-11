#' Text-to-Speech (Speech Synthesis)
#'
#' @description Convert text-to-speech using various engines, including Amazon
#' Polly, Coqui TTS, Google Cloud Text-to-Speech API, and Microsoft Cognitive
#' Services Text to Speech REST API.
#'
#' With the exception of Coqui TTS, all these engines are accessible as R
#' packages:
#' * [aws.polly](https://github.com/cloudyr/aws.polly) is a client for Amazon Polly.
#' * [googleLanguageR](https://github.com/ropensci/googleLanguageR) is a client to the Google Cloud Text-to-Speech API.
#' * [conrad](https://github.com/fhdsl/conrad) is a client to the Microsoft Cognitive Services Text to Speech REST API
#'
#' @param text A character vector of text to be spoken
#' @param exec_path System path to Coqui TTS executable
#' @param output_format Format of output files: "mp3" or "wav"
#' @param voice Full voice name
#' @param model_name (Coqui TTS only) Deep Learning model for Text-to-Speech
#'   Conversion
#' @param vocoder_name (Coqui TTS only) Voice coder used for speech coding and
#'   transmission
#' @param bind_audio Should the [text2speech::tts_bind_wav()] be run on after
#'   the audio has been created, to ensure that the length of text and the
#'   number of rows is consistent?
#' @param service Service to use (Amazon, Google, Microsoft, or Coqui)
#' @param save_local Should the audio file be saved locally?
#' @param save_local_dest If to be saved locally, destination where output file
#'   will be saved
#' @param bind_audio Should the [text2speech::tts_bind_wav()] be run on after
#'   the audio has been created, to ensure that the length of text and the
#'   number of rows is consistent?
#' @param ... Additional arguments
#'
#' @return A standardized `tibble` featuring the following columns:
#' * `index` : Sequential identifier number
#' * `original_text` : The text input provided by the user
#' * `text` : In case original_text exceeds the character limit, text represents the outcome of splitting original_text. Otherwise, text remains the same as original_text.
#' * `wav` : Wave object (S4 class)
#' * `file` : File path to the audio file
#' * `audio_type` : The audio format, either mp3 or wav
#' * `duration` : The duration of the audio file
#' * `service` : The text-to-speech engine used
#'
#' @export
#' @examples
#' \dontrun{
#' # Amazon Polly
#' tts("Hello world! This is Amazon Polly", service = "amazon")
#'
# Coqui TTS
#' tts("Hello world! This is Coqui TTS", service = "coqui")
#'
# Google Cloud Text-to-Speech API
#' tts("Hello world! This is Google Cloud", service = "google")
#'
# Microsoft Cognitive Services Text to Speech REST API
#' tts("Hello world! This is Microsoft", service = "microsoft")
#' }
tts = function(
    text,
    output_format = c("mp3", "wav"),
    service = c("amazon", "google", "microsoft", "coqui"),
    bind_audio = TRUE,
    ...) {

  service = match.arg(service)
  if (!tts_auth(service = service)) {
    warning(paste0("Service ", service, " not authorized/unavailable"))
  }

  output_format = match.arg(output_format)
  if (service == "google") {
    res = tts_google(
      text = text,
      output_format = output_format,
      bind_audio = bind_audio,
      ...)
  }
  if (service == "amazon") {
    res = tts_amazon(
      text = text,
      output_format = output_format,
      bind_audio = bind_audio,
      ...)
  }
  if (service == "microsoft") {
    res = tts_microsoft(
      text = text,
      output_format = output_format,
      bind_audio = bind_audio,
      ...)
  }
  if (service == "coqui") {
    cli::cli_alert_info("Coqui TTS does not support MP3 format; will produce a WAV audio output.")
    use_coqui()
    coqui_path <- getOption("path_to_coqui")

    res <- tts_coqui(
      text = text,
      exec_path = coqui_path,
      output_format = "wav",
      bind_audio = bind_audio,
      ...)
  }
  res$service = service
  return(res)
}

#' @export
#' @rdname tts
tts_amazon = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "Joanna",
    bind_audio = TRUE,
    save_local = FALSE,
    save_local_dest = NULL,
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
  # Copy and paste audio file into local destination
  if (save_local) {
    if (!is.null(save_local_dest)) {
      file.copy(normalizePath(res$file), save_local_dest)
    } else {
      cli::cli_alert_danger("Provide local destination where audio file will be saved")
    }
  }

  return(res)

}


#' @export
#' @rdname tts
tts_google = function(
    text,
    output_format = c("mp3", "wav"),
    voice = "en-US-Standard-C",
    bind_audio = TRUE,
    save_local = FALSE,
    save_local_dest = NULL,
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
  # Copy and paste audio file into local destination
  if (save_local) {
    if (!is.null(save_local_dest)) {
      file.copy(normalizePath(res$file), save_local_dest)
    } else {
      cli::cli_alert_danger("Provide local destination where audio file will be saved")
    }
  }

  return(res)
}


#' @export
#' @rdname tts
tts_microsoft = function(
    text,
    output_format = c("mp3", "wav"),
    voice = NULL,
    bind_audio = TRUE,
    save_local = FALSE,
    save_local_dest = NULL,
    ...) {
  # Set character limit
  limit = 800

  res = lapply(text, function(string) {
    strings = tts_split_text(string,
                             limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(output_format)
      out = conrad::ms_synthesize(
        tt,
        voice = voice,
        ...)
      writeBin(out, con = output)
      output
    }, FUN.VALUE = character(1L))
    names(res) = NULL
    out = lapply(res, tts_audio_read,
                 output_format = output_format)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    # out = do.call(tuneR::bind, out)
    df
  })
  names(res) = length(text)
  res = dplyr::bind_rows(res, .id = "index")
  res$index = as.numeric(res$index)

  res$audio_type = output_format
  if (bind_audio) {
    res = tts_bind_wav(res)
  }
  if ("wav" %in% colnames(res)) {
    res$duration = vapply(res$wav, wav_duration, FUN.VALUE = numeric(1))
  }
  # Copy and paste audio file into local destination
  if (save_local) {
    if (!is.null(save_local_dest)) {
      file.copy(normalizePath(res$file), save_local_dest)
    } else {
      cli::cli_alert_danger("Provide local destination where audio file will be saved")
    }
  }

  return(res)
}

#' @export
#' @rdname tts
tts_coqui <- function(
    text,
    exec_path,
    output_format = c("wav", "mp3"),
    model_name = "tacotron2-DDC_ph",
    vocoder_name = "ljspeech/univnet",
    bind_audio = TRUE,
    save_local = FALSE,
    save_local_dest = NULL,
    ...) {
  # Is there a max number of limits that coqui TTS takes? (https://github.com/coqui-ai/TTS/discussions/917)
  limit <- 2500
  output_format = match.arg(output_format)
  audio_type = output_format

  stopifnot(is.character(model_name), is.character(vocoder_name))
  # English models names
  model_name <- switch(
    model_name,
    "jenny"            = "tts_models/en/jenny/jenny",
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
    "jenny"                     =   NULL,
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

  # Vocoder is provided
  if(!is.null(vocoder_name)) {
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
        res <- withr::with_path(process_coqui_path(exec_path),
                                system2("tts", tts_args))
        # Output file path
        output_path
      }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
      out = lapply(res, tts_audio_read,
                   output_format = audio_type)
      df = dplyr::tibble(original_text = string,
                         text = string_processed,
                         wav = out, file = normalizePath(res))
    })
    # Vocoder not provided (in case of Jenny)
  } else {
    # Iterate coqui tts over text
    res = lapply(text, function(string) {
      string_processed = tts_split_text(string, limit = limit)

      res = vapply(string_processed, function(tt) {
        output_path = tts_temp_audio(audio_type)
        tts_args <- paste0("--text", " ", shQuote(tt), " ",
                           "--model_name", " ", model_name, " ",
                           " ", "--out_path /private", output_path)
        # # Run command with temporary system search path
        res <- withr::with_path(process_coqui_path(exec_path),
                                system2("tts", tts_args))
        # Output file path
        output_path
      }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
      out = lapply(res, tts_audio_read,
                   output_format = audio_type)
      df = dplyr::tibble(original_text = string,
                         text = string_processed,
                         wav = out, file = normalizePath(res))
    })
  }

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
  # Copy and paste audio file into local folder
  if (save_local) {
    if (!is.null(save_local_dest)) {
      file.copy(normalizePath(res$file), save_local_dest)
    } else {
      cli::cli_alert_danger("Provide local destination where audio file will be saved")
    }
  }
  res
}
