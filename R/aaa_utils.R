#' Play audio in a browser
#'
#' This uses HTML5 audio tags to play audio in your browser.
#'
#' Borrowed from \code{googleLanguageR::gl_talk_player()}
#'
#' @param audio The file location of the audio file.  Must be supported by HTML5.
#' @param html The html file location that will be created to host the audio file.
#'
#' @export
#' @examples
#' \dontrun{
#'   play_audio(audio = "audio.wav", html = "player.html")
#' }
play_audio <- function(audio = "output.wav",
                       html = "player.html"){
  # Write html code to a html file
  writeLines(sprintf('<html><body>
    <audio controls autoplay>
                     <source src="%s">
                     </audio>
                     </body></html>',
                     audio),
             html)
  # Load URL into browser
  utils::browseURL(html)
}

# Read WAV or MP3 file as a Wave object
tts_audio_read <- function(file, output_format = c("wav", "mp3")) {
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

# Split text into groups if number of characters exceeds limit
tts_split_text = function(text, limit = 50) {
  stopifnot(
    is.character(text) && length(text) == 1,
    is.numeric(limit) && length(limit) == 1 && is.finite(limit) &&
      limit >= 1 && limit == floor(limit)
  )
  if (nchar(text, type = "chars") <= limit) {
    return(text)
  }

  chunks <- character()
  remaining <- text
  while (nchar(remaining, type = "chars") > limit) {
    candidate <- substr(remaining, 1L, limit)
    next_char <- substr(remaining, limit + 1L, limit + 1L)
    last_char <- substr(candidate, limit, limit)

    if (grepl("[[:space:]]", last_char) ||
        grepl("[[:space:]]", next_char)) {
      split_at <- limit
    } else {
      spaces <- gregexpr("[[:space:]]", candidate, perl = TRUE)[[1]]
      spaces <- spaces[spaces > 0L]
      split_at <- if (length(spaces)) max(spaces) else limit
    }

    chunks <- c(chunks, substr(remaining, 1L, split_at))
    remaining <- substr(remaining, split_at + 1L, nchar(remaining, type = "chars"))
  }
  c(chunks, remaining)
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

#' Point to local coqui tts Executable File
#'
#' Function to set an option that points to the local coqui tts Executable File
#' \code{tts}.
#'
#' @param path path to the local coqui tts Executable File
#'
#' @details List of possible file path locations for the local coqui tts
#'   Executable File
#' \describe{
#'    \item{Linux}{/usr/bin/tts, /usr/local/bin/tts}
#'    \item{Mac}{/opt/homebrew/Caskroom/miniforge/base/bin/tts}
#'    \item{Windows}{C:\\Program Files\\tts}
#' }
#'
#' @return Returns nothing, function sets the option variable
#'   \code{path_to_coqui}.
#' @export
#'
#' @examplesIf interactive()
#' set_coqui_path("~/path/to/tts")
set_coqui_path <- function(path) {
  stopifnot(is.character(path))

  if (!file.exists(path)) stop(paste0("Cannot find ", "\"", path,"\""), call. = FALSE)
  options("path_to_coqui" = path)
}



# Prepare to use coqui "tts" by checking if it exists locally.
# Check option "path_to_coqui". If it's NULL, call find_coqui(), which
# will try to determine the local path to file "tts". If
# find_coqui() is successful, the path to "tts" will be assigned to option
# "path_to_coqui", otherwise an error is thrown.
use_coqui <- function() {
  coqui_path <- getOption("path_to_coqui")

  if (is.null(coqui_path)) {
    coqui_path <- find_coqui()
    set_coqui_path(coqui_path)
  }
}

# Returns the local path to "tts". Search is performed by looking in the known
# file locations for the current OS. If OS is not Linux, OSX, or Windows, an
# error is thrown. If path to "tts" is not found, an error is thrown.
find_coqui <- function() {
  user_os <- Sys.info()["sysname"]
  if (!user_os %in% names(coqui_paths_to_check)) {
    stop(coqui_path_missing, call. = FALSE)
  }

  coqui_path <- NULL
  for (path in coqui_paths_to_check[[user_os]]) {
    if (file.exists(path)) {
      coqui_path <- path
      break
    }
  }

  if (is.null(coqui_path)) {
    stop(coqui_path_missing, call. = FALSE)
  }

  coqui_path
}

# Returns the path to the directory that contains coqui "tts"
process_coqui_path <- function(coqui_path) {
  gsub("/tts$", "", coqui_path)
}


# List obj containing known locations of coqui "tts"
coqui_paths_to_check <- list(
  "Linux" = c("/usr/bin/tts",
              "/usr/local/bin/tts"),
  "Darwin" = c("/opt/homebrew/Caskroom/miniforge/base/bin/tts"),
  "Windows" = c("C:\\Program Files\\tts")
)

# Error message thrown if coqui "tts" cannot be found
coqui_path_missing <- paste(
  "Coqui TTS software required for advanced Text-to-Speech generation.",
  "Cannot determine file path to coqui TTS",
  "To download coqui TTS, visit: https://github.com/coqui-ai/TTS#install-tts \n",
  "If you've already downloaded the software, use function",
  "'set_coqui_path(path = \"path/to/coqui/tts\")' to point R to your local coqui tts Executable File"
)
