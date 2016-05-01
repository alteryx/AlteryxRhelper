.onLoad <- function(libname, pkgname){
  if (!('package:AlteryxRDataX' %in% search())){
    #packageStartupMessage('Setting AlteryxFullUpdate to FALSE')
    AlteryxFullUpdate <<- FALSE
    if (.Platform$OS.type == "windows"){
      alteryx.path = "C:/Program Files/Alteryx"
    } else {
      alteryx.path = "/Volumes/C/Program Files/Alteryx"
    }
    options(alteryx.path = alteryx.path)
  } else {
    # options(error = dump_and_quit)
  }
}
