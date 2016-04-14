.onLoad <- function(libname, pkgname){
  if (!('package:AlteryxRDataX' %in% search())){
    #packageStartupMessage('Setting AlteryxFullUpdate to FALSE')
    AlteryxFullUpdate <<- FALSE
    options(alteryx.path = "C:/Program Files/Alteryx")
  } else {
    # options(error = dump_and_quit)
  }
}
