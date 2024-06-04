isRstudio <- function(){
  Sys.getenv("RSTUDIO") == 1 || .Platform$GUI == "RStudio" || 
    if (suppressWarnings(requireNamespace("rstudioapi", quietly = TRUE))) {
      rstudioapi::isAvailable()
    }
  else {
    FALSE
  }
}