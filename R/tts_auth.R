#' Authorize Text-to-Speech Engine
#'
#' @param service type of synthesis engine
#' @param key_or_json_file Either an API key (for Microsoft)
#' or JSON file (for Google)
#' @param ... Additional arguments to pass to
#' [aws.signature::use_credentials()] or [mscstts::ms_get_tts_token()]
#'
#' @return A logical indicator of authorization
#' @export
#'
#' @examples
#' tts_auth("google")
#' tts_auth("amazon")
#' tts_auth("microsoft")
tts_auth = function(service = c("amazon", "google", "microsoft"),
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
  return(res)
}


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


#' @rdname tts_auth
#' @export
tts_amazon_authenticated = function() {
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

#' @rdname tts_auth
#' @export
tts_microsoft_authenticated = function(...) {
  res = mscstts::ms_get_tts_token(...)
  res = res$request
  httr::status_code(res) < 400
}
