#' Install packages to R library in SVN
#' 
#' 
#' @param srcPkg path to source package
#' @param svnLibDir svn library directory
#' @export
installToSvn <- function(srcPkg, svnLibDir, install = TRUE){
  d <- read.dcf(file.path(srcPkg, "DESCRIPTION"))
  imports <- gsub("\\n", "", strsplit(d[,'Imports'], ",")[[1]])
  deps <- miniCRAN::pkgDep(imports, suggests = FALSE)
  installed <- rownames(installed.packages(svnLibDir))
  depsToInstall <- setdiff(deps, installed)
  library(devtools)
  if (install){
    withr::with_libpaths(svnLibDir, {
      message(
        "Installing following dependencies from CRAN\n  ",
        paste(depsToInstall, collapse = '\n  ')
      )
      install.packages(depsToInstall, svnLibDir)
      message(
        "Installing from source: ", basename(normalizePath(srcPkg))
      )
      install(".", lib = svnLibDir)
    }, action = 'prefix')
  }
  list(dependencies = deps, toInstall = depsToInstall)
}


#' Save manifest to SVN
#' 
#' @export
saveManifest <- function(svnLibDir = NULL){
  svnLibDir = file.path(getOption('alteryx.svnRdir'), 
    "R_Installed_Files/R-3.2.3/library"
  )
  d3 <- summary(packageStatus(svnLibDir))
  d4 <- d3$inst[,c('Package', 'Version', 'Status', 'Priority', 'Built')]
  rownames(d4) <- NULL
  message("Updating package manifest on SVN...")
  write.csv(d4, 
    file = file.path(
      getOption('alteryx.svnRdir'), "Installer", "packages.csv"
    ), 
    row.names = F
  )
  message("Updating package readme on SVN...")
  saveReadme()
}

#' Save readme
#' 
#' @param save whether or not to save the readme
#' @export
saveReadme <- function(save = TRUE){
  readmeFile = file.path(getOption('alteryx.svnRdir'), 
    "Installer/Readme.txt"
  )
  svnLibDir = file.path(getOption('alteryx.svnRdir'), 
    "R_Installed_Files/R-3.2.3/library"
  )
  pkgs <- summary(packageStatus(svnLibDir))$inst
  pkgs <- pkgs[is.na(pkgs[,"Priority"]),]
  pkgs <- pkgs[pkgs$Package != "translations",]
  ayxPackages <- c("AlteryxRDataX", "AlteryxRviz", "htmlwidgets")
  allPkgs <- unname(sort(c(pkgs$Package, ayxPackages)))
  if (save){
    writeLines(allPkgs, readmeFile)
  } else {
    return (allPkgs)
  }
}