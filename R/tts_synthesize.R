tts_synthesize = function(
  text,
  output_format = c("mp3", "wav"),
  ...,
  type = c("amazon", "google", "microsoft")) {

  type = match.arg(type)
  output_format = match.arg(output_format)
  ext = paste0(".", output_format)
  if (type == "google") {
    res = tts_google(text = text, output_format = output_format,
               ...)
  }
  if (type == "amazon") {
    res = tts_amazon(text = text, output_format = output_format,
               ...)
  }
  if (type == "microsoft") {
    res = tts_microsoft(text = text, output_format = output_format,
                     ...)
  }

}
