#' Authenticate the user's credentials for using the Microsoft Cognitive Services Text to Speech REST API,
#' @rdname tts_auth
#' @export
tts_microsoft_auth = function(key_or_json_file = NULL, ...) {
  if (!mscstts::ms_have_tts_key()) {
    mscstts::ms_set_tts_key(api_key = key_or_json_file)
    res = mscstts::ms_have_tts_key()
  }
  res = tts_microsoft_authenticated(...)
}
