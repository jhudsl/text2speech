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

#' @rdname tts
#' @export
tts_default_voice = function(
  service = c("amazon", "google", "microsoft")
) {
  voice = switch(
    service,
    google = "en-US-Standard-C",
    microsoft = "Microsoft Server Speech Text to Speech Voice (en-US, ZiraRUS)",
    amazon = "Joanna")
  return(voice)
}
