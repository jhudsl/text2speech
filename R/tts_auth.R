#' Authorize Text-to-Speech Engine
#'
#' @param service Type of text-to-speech engine
#' @param key_or_json_file Either an API key (for Microsoft)
#' or JSON file (for Google)
#' @param ... Additional arguments
#' @return A logical indicator of authorization
#' @export
#' @examples
#' # Amazon Polly
#' tts_auth("amazon")
#'
#' # Coqui TTS
#' tts_auth("coqui")
#'
#' # Google Cloud Text-to-Speech API
#' tts_auth("google")
#'
#' # Microsoft Cognitive Services Text to Speech REST API
#' tts_auth("microsoft")
tts_auth = function(service = c("amazon", "google", "microsoft", "coqui"),
                    key_or_json_file = NULL,
                    ...) {
  service = match.arg(service)
  if (service == "google") {
    res = tts_google_auth(key_or_json_file, ...)
  }
  if (service == "amazon") {
    res = tts_amazon_auth(key_or_json_file, ...)
  }
  if (service == "microsoft") {
    res = tts_microsoft_auth(key_or_json_file, ...)
  }
  if (service == "coqui") {
    res = tts_coqui_installed()
  }
  return(res)
}

#' Check Google Cloud Text-to-Speech API Authentication Status
#' @rdname tts_auth
#' @export
tts_google_authenticated = function() {
  res = try({
    suppressMessages({
      googleLanguageR::gl_talk_languages()
    })
  }, silent = TRUE)
  !inherits(res, "try-error")
}

#' Check Amazon Polly Text-to-Speech API Authentication Status
#' @rdname tts_auth
#' @export
tts_amazon_authenticated = function() {
  if (!requireNamespace("aws.polly", quietly = TRUE)) {
    stop(paste0(
      "This function requires aws.polly to operate",
      " please use\n",
      "install.packages('aws.polly')\n",
      "to use these functions"))
  }
  L = try({
    aws.polly::list_voices()
  })
  if (inherits(L, "try-error")) {
    return(FALSE)
  }
  if (length(L) == 0 || NROW(L) == 0) {
    return(FALSE)
  }
  return(TRUE)
}


#' Check Microsoft Cognitive Services Text to Speech REST API Authentication Status
#' @rdname tts_auth
#' @export
tts_microsoft_authenticated = function(...) {
  res = try({
    mscstts2::ms_get_token(...)
  })
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  res_status_code = res$response$status_code
  res_status_code < 400
}
