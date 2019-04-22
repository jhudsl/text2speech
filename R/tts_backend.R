tts_google = function(
  text,
  output_format = c("mp3", "wav"),
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

    res = lapply(strings, function(tt) {
      output = tts_temp_audio(audio_type)
      out = googleLanguageR::gl_talk(
        tt,
        output = output,
        audioEncoding = output_format,
        ...)
    })
    out = lapply(res, tts_audio_read,
                 output_format = audio_type)
    out = tuneR::bind(out)
  })

  return(res)
}

tts_amazon = function(
  text,
  output_format = c("mp3", "wav"),
  ...) {

  limit = 1500
  text = tts_split_text(text, limit = limit)

  output_format = match.arg(output_format)
  audio_type = output_format

  output_format = switch(
    output_format,
    "mp3" = "mp3",
    "wav" = "pcm")


  result = lapply(
    text,
    aws.polly::get_synthesis,
    format = output_format,
    ...)

  wav <- do.call(tuneR::bind, result)

  tmp <- tempfile(fileext = ext)
  writeBin(res, con = tmp)
  return(res)

}


tts_microsoft = function(
  text,
  output_format = c("mp3", "wav"),
  ...) {

  limit = 800
  text = tts_split_text(text, limit = limit)

  output_format = match.arg(output_format)
  audio_type = output_format

  output_format = switch(
    output_format,
    "mp3" = "audio-24khz-160kbitrate-mono-mp3",
    "wav" = "riff-24khz-16bit-mono-pcm")

  res = mscstts::ms_synthesize(
    text,
    output_format = output_format,
    ...)
  tmp <- tempfile(fileext = ext)
  writeBin(res$content, con = tmp)
  return(res)
}
