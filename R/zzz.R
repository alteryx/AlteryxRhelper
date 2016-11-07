.onLoad <- function(libname, pkgname){
  if (!('package:AlteryxRDataX' %in% search())){
    # packageStartupMessage('Setting AlteryxFullUpdate to FALSE')
    # AlteryxFullUpdate <<- FALSE
    if (.Platform$OS.type == "windows"){
      alteryx.path = "C:/Program Files/Alteryx"
      dev.dir = "Z:/SNIPPETS"
      alteryx.svndir = 'C:/Users/ramnath/Desktop/SVN_Full_Repos/Predictive_Development'
    } else {
      alteryx.path = "/Volumes/C/Program Files/Alteryx"
      alteryx.svndir = "/Volumes/C/Users/ramnath/Desktop/SVN_Full_Repos/Predictive_Development"
      dev.dir = "~/Desktop/SNIPPETS"
    }
    options(alteryx.svndir = alteryx.svndir)
    options(alteryx.path = alteryx.path)
    options(dev.dir = dev.dir)
  } else {
    # options(error = dump_and_quit)
  }
}
