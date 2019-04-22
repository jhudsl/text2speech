#' @export
#' @rdname tts
tts_google = function(
  text,
  output_format = c("mp3", "wav"),
  voice = "en-US-Standard-C",
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

  res = dplyr::bind_rows(res)
  return(res)
}

#' @export
#' @rdname tts
tts_amazon = function(
  text,
  output_format = c("mp3", "wav"),
  voice = "Joanna",
  ...) {

  limit = 1500
  output_format = match.arg(output_format)
  audio_type = output_format

  output_format = switch(
    output_format,
    "mp3" = "mp3",
    "wav" = "pcm")

  res = lapply(text, function(string) {
    strings = tts_split_text(string,
                             limit = limit)

    res = vapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)

      out = aws.polly::get_synthesis(
        tt,
        voice = voice,
        format = output_format,
        ...)
      writeBin(out, con = output)
      output
    }, FUN.VALUE = character(1L))
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    df = dplyr::tibble(original_text = string,
                       text = strings,
                       wav = out, file = res)
    df
  })
  res = dplyr::bind_rows(res)

  return(res)

}

#' @export
#' @rdname tts
tts_microsoft = function(
  text,
  output_format = c("mp3", "wav"),
  voice = "Microsoft Server Speech Text to Speech Voice (en-US, ZiraRUS)",
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
  res = dplyr::bind_rows(res)

  return(res)
}
