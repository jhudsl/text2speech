
#' Knitr Speak Enginge
#'
#' @param options A list of chunk options. Usually this is just the
#' object options passed to the engine function; see
#'  \code{\link{knit_engines}}
#'
#' @return A character string generated from the source code and
#' output using the appropriate output hooks.
#' @export
#'
#' @examples
#' knitr::knit_engines$set(speak = tts_speak_engine)
tts_speak_engine = function(options) {
  # print(options)
  if (!options$eval) {
    return(knitr::engine_output(options, options$code, ""))
  }
  output_format = options$output_format
  service = options$service
  voice = options$voice
  if (is.null(service)) {
    service = "google"
  }
  if (is.null(voice)) {
    voice = tts_default_voice(service = service)
  }
  if (is.null(output_format)) {
    output_format = "mp3"
  }
  out_path =  dirname(options$fig.path)
  dir.create(out_path, showWarnings = FALSE, recursive = TRUE)
  output = file.path(
    out_path,
    paste0(options$label, ".", output_format))
  # need to remake
  if (!file.exists(output)) {
    options$cache = FALSE
  }

  key_or_json_file = options$key_or_json_file
  if (!is.null(key_or_json_file)) {
    text2speech::tts_auth(service = service,
                          key_or_json_file = key_or_json_file)
  }
  text = paste0(options$code, collapse = " ")
  result = text2speech::tts(
    text = text,
    output_format = output_format,
    service = service,
    voice = voice
  )
  file.copy(result$file, output, overwrite = TRUE)
  out = utils::capture.output(output)
  if (knitr::is_html_output()) {
    if (options$results != "hide") {
      options$results = "asis"
      out = c("<audio controls>",
              paste0('<source src="', output, '">'),
              "</audio>",
              "")
    } else {
      out = ""
    }
  }
  print(out)
  knitr::engine_output(options, options$code, out)
}
