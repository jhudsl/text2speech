tts_coqui_installed <- function() {
  coqui_assert()
  coqui_path <- getOption("path_to_coqui")

  res <- suppressWarnings(withr::with_path(process_coqui_path(coqui_path),
                          system("which tts", intern = TRUE)))

  if (!is.null(attr(res, "status"))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
