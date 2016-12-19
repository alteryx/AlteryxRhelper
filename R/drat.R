#' Add package to drat repo
#' 
#' @param pkg name of the package
#' @param cleanup boolean indicating if built files should be cleaned up
#' @param ... additional parameters to be passed to \code{\link{insertPackage}}
#'   for binaries
#' @export
addToDrat <- function(pkg, cleanup = TRUE, ...){
  devDir <- file.path(getOption('dev.dir'), 'dev')
  ayxPkgDir <- file.path(devDir, 'AlteryxRPackage')
  dratPath <- file.path(ayxPkgDir, 'drat')
  with_dir_(ayxPkgDir, {
    if (.Platform$OS.type == 'unix'){
      srcPkg <- devtools::build(pkg)
      drat:::insertPackage(srcPkg, dratPath, ...)
      
      macBinary <- devtools::build(pkg, binary = TRUE)
      drat:::insertPackage(macBinary,  dratPath, ...)
      if (cleanup) on.exit(unlink(c(srcPkg, macBinary)))
    } else {
      winBinary <- devtools::build(pkg, binary = TRUE)
      drat:::insertPackage(winBinary,  dratPath, ...)
      if (cleanup) on.exit(unlink(winBinary))
    }   
  })
}