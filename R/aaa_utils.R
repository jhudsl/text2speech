# Read WAV or MP3 files in and store as a Wave object
tts_audio_read <- function(file,
                          output_format = c("wav", "mp3")) {
  output_format = match.arg(output_format)
  out = switch(
    output_format,
    wav = tuneR::readWave(file),
    mp3 = tuneR::readMP3(file)
  )
  out
}

# Create temporary audio file (mp3 or wav)
tts_temp_audio = function(output_format = c("mp3", "wav") ) {
  output_format = match.arg(output_format)
  ext = paste0(".", output_format)
  tempfile(fileext = ext)
}

# Split text into groups if nchar(text) exceeds limit
tts_split_text = function(text, limit = 50) {
  stopifnot(is.character(text) & length(text) == 1)
  num_char = nchar(text)

  # If number of characters exceeds the limit,
  # divide text into groups
  if (any(num_char > limit)) {
    # Number of pieces
    pieces <- ceiling(nchar(text) / limit)
    # Split text into words
    words <- strsplit(text, " ")[[1]]

    indices = ceiling(seq_along(words) / (length(words) / pieces))
    # Divide words into groups defined by indices
    chunks <- split(words, indices)
    # Concatenate chunks into a string, separated by a space
    text = vapply(chunks, paste, collapse = " ",
                  FUN.VALUE = character(1))
  }
  return(text)
}

# Calculate WAV audio duration
wav_duration = function(object) {
  if (inherits(object, "Wave")) {
    l <- length(object@left)
    return(round(l / object@samp.rate, 2))
  } else {
    return(NA_real_)
  }
}
