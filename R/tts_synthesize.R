#' Text to Speech
#'
#' @param text A character vector of text to speak
#' @param output_format Format of output files
#' @param ... Additional arguments to
#' [text2speech::tts_google()],
#' [text2speech::tts_amazon()], or
#' [text2speech::tts_microsoft()]
#' @param service service to use
#' @param language An ISO 3166 country identification tag. If
#' NULL, then a data.frame of the results are returned.
#'
#' @return A `data.frame` of text and wav files
#' @export
tts = function(
  text,
  voice,
  output_format = c("mp3", "wav"),
  ...,
  service = c("amazon", "google", "microsoft")) {

  service = match.arg(service)
  output_format = match.arg(output_format)
  if (service == "google") {
    res = tts_google(text = text,
                     output_format = output_format,
                     voice = voice,
               ...)
  }
  if (service == "amazon") {
    res = tts_amazon(text = text, output_format = output_format,
               ...)
  }
  if (service == "microsoft") {
    res = tts_microsoft(text = text, output_format = output_format,
                     ...)
  }

}
