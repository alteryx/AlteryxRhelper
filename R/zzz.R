.onLoad <- function(libname, pkgname){
  if (!('package:AlteryxRDataX' %in% search())){
    #packageStartupMessage('Setting AlteryxFullUpdate to FALSE')
    AlteryxFullUpdate <<- FALSE
    if (.Platform$OS.type == "windows"){
      alteryx.path = "C:/Program Files/Alteryx"
      options(alteryx.svnRdir = "C:/Users/ramnath/Desktop/DesignerTeam_Development/R")
    } else {
      alteryx.path = "/Volumes/C/Program Files/Alteryx"
    }
    options(alteryx.path = alteryx.path)
  } else {
    # options(error = dump_and_quit)
  }
}
