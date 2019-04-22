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
