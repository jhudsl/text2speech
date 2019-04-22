google_authenticated = function() {
  res = try({
    suppressMessages({
      googleLanguageR::gl_talk_languages()
    })
  }, silent = TRUE)
  !inherits(res, "try-error")
}


amazon_authenticated = function() {
  L = aws.polly::list_voices()
  if (length(L) == 0 || NROW(L) == 0) {
    return(FALSE)
  }
  return(TRUE)
}


microsoft_authenticated = function(...) {
  res = mscstts::ms_get_tts_token(...)
  res = res$request
  httr::status_code(res) < 400
}



tts_audio_read = function(
  file,
  output_format = c("mp3", "wav") ) {
  output_format = match.arg(output_format)
  out = switch(
    output_format,
    wav = tuneR::readWave(file),
    mp3 = tuneR::readMP3(file),
  )
  return(out)
}
tts_temp_audio = function(output_format = c("mp3", "wav") ) {
  output_format = match.arg(output_format)
  ext = paste0(".", output_format)
  tempfile(fileext = ext)
}
# text = stri_rand_lipsum(5)
# text = paste(text[1:5], collapse = " ")

tts_split_text = function(text, limit = 5000) {
  stopifnot(is.character(text) & length(text) == 1)
  nc = nchar(text)
  if (any(nc > limit)) {
    pieces <- ceiling(nchar(text)/limit)
    words <- strsplit(text, " ")[[1]]
    indices = ceiling(seq_along(words)/(length(words)/pieces))
    chunks <- split(words, indices)
    text = vapply(chunks, paste, collapse = " ",
                  FUN.VALUE = character(1))
  }
  return(text)
}
