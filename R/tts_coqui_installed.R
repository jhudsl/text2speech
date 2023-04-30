tts_coqui_installed <- function() {
  coqui_assert()
  coqui_path <- getOption("path_to_coqui")

  res <- withr::with_path("/opt/homebrew/Caskroom/miniforge/base/bin",
                   system("which tts"))
  if (res == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
