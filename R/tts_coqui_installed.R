#' Is coqui TTS installed on local system?
#'
#' @return `TRUE` or `FALSE`
#' @export
tts_coqui_installed <- function() {
  use_coqui()
  coqui_path <- getOption("path_to_coqui")

  res <- suppressWarnings(withr::with_path(process_coqui_path(coqui_path),
                          system("which tts", intern = TRUE)))

  if (!is.null(attr(res, "status"))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
