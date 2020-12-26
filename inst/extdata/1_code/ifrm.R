ifrm <- function(objVec, env = globalenv()) {
  for (obj in objVec) {
    if ( exists(obj, envir = env)) {
      eval(parse(text = paste0("rm(", obj, ", envir = env) ")))
    }
  }
}