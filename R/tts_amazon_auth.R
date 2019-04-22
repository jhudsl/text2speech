#' @rdname tts_auth
#' @export
tts_amazon_auth = function(key_or_json_file = NULL, ...) {
  if (tts_amazon_authenticated()) {
    return(TRUE)
  }
  credential_file = aws.signature::default_credentials_file()
  if (file.exists(credential_file)) {
    aws.signature::use_credentials(...)
  }
  return(tts_amazon_authenticated())
}

