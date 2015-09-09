.onLoad <- function(libname, pkgname){
  if (!('package:AlteryxRDataX' %in% search())){
    message('Setting AlteryxFullUpdate to FALSE')
    AlteryxFullUpdate <<- FALSE
    options(alteryx.path = "C:/Program Files/Alteryx")
  } else {
    # options(error = dump_and_quit)
  }
}
