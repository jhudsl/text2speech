#' Authenticate the user's Google Cloud credentials for using the Google Cloud Text-to-Speech (TTS) API

#' @rdname tts_auth
#' @export
tts_google_auth = function(key_or_json_file = NULL, ...) {
  if (!is.null(key_or_json_file)) {
    suppressMessages({
      auth = googleLanguageR::gl_auth(json_file = key_or_json_file)
    })
  }
  needed <- c("https://www.googleapis.com/auth/cloud-language",
              "https://www.googleapis.com/auth/cloud-platform")
  if (!tts_google_authenticated()) {
    suppressMessages({
      auth = googleAuthR::gar_attach_auto_auth(
        needed,
        environment_var = "GL_AUTH")
    })
  }
  if (!tts_google_authenticated()) {
    suppressMessages({
      auth = googleAuthR::gar_attach_auto_auth(
        needed,
        environment_var = "GL_AUTH_FILE")
    })
  }
  return(tts_google_authenticated())
}
