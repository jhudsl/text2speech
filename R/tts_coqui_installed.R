tts_coqui_installed <- function() {
  coqui_assert()
  coqui_path <- getOption("path_to_coqui")
  coqui_path_processed <- gsub("/tts$", "", coqui_path)

  res <- withr::with_path(coqui_path_processed,
                          system("which tts", intern = TRUE))

  if (!is.null(attr(res, "status")) && attr(res, "status") == 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
