runJob <- function(exec, arguments = c()){
  stopifnot(file.exists(exec))
  options(warn = -1)
  mywd <- setwd(dir = dirname(exec))
  arguments = paste(arguments, collapse = " ")
  system(paste0("Rscript main.R ", arguments), wait = TRUE)
  setwd(mywd)
  options(warn = 0)
}
