#' Authorize Text-to-Speech Engine
#'
#' @param service type of synthesis engine
#' @param key_or_json_file Either an API key (for Microsoft)
#' or JSON file (for Google)
#' @param ... Additional arguments to pass to
#' \code{\link{use_credentials}} or \code{\link{ms_get_tts_token}}
#'
#' @return A logical indicator of authorization
#' @export
#'
#' @examples
#' tts_auth("google")
#' tts_auth("microsoft")
#'
#' tts_google_authenticated()
#' tts_microsoft_authenticated()
#'
#' tts_google_auth()
#' tts_microsoft_auth()
#' if (requireNamespace("aws.polly", quietly = TRUE)) {
#' tts_auth("amazon")
#' tts_amazon_authenticated()
#' tts_amazon_auth()
#' }
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
    mscstts::ms_get_tts_token(...)
  })
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  res = res$request
  httr::status_code(res) < 400
}
