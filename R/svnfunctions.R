#' Install packages to R library in SVN
#' 
#' 
#' @param srcPkg path to source package
#' @param svnLibDir svn library directory
#' @export
installToSvn <- function(srcPkg, install = TRUE){
  srcPkg <- normalizePath(srcPkg)
  rdirs <- getAyxSvnRDirs()
  svnLibDir <- rdirs$lib
  d <- read.dcf(file.path(srcPkg, "DESCRIPTION"))
  imports <- gsub("\\n", "", strsplit(d[,'Imports'], ",")[[1]])
  deps <- miniCRAN::pkgDep(imports, suggests = FALSE)
  installed <- rownames(installed.packages(svnLibDir))
  depsToInstall <- setdiff(deps, installed)
  library(devtools)
  if (install){
    withr::with_libpaths(svnLibDir, {
      if (length(depsToInstall) > 0){
        message(
          "Installing following dependencies from CRAN\n  ",
          paste(depsToInstall, collapse = '\n  ')
        )
        install.packages(depsToInstall, svnLibDir)
      }
      message(
        "Installing from source: ", basename(normalizePath(srcPkg))
      )
      install(srcPkg, lib = svnLibDir, dependencies = FALSE)
    }, action = 'prefix')
  }
  list(dependencies = deps, toInstall = depsToInstall)
}


#' Save manifest to SVN
#' 
#' @export
saveManifest <- function(){
  rdirs <- getAyxSvnRDirs()
  svnLibDir = rdirs$lib
  d3 <- summary(packageStatus(svnLibDir))
  d4 <- d3$inst[,c('Package', 'Version', 'Status', 'Priority', 'Built')]
  rownames(d4) <- NULL
  message("Updating package manifest on SVN...")
  write.csv(
    d4, 
    file = file.path(rdirs$installer, "packages.csv"), 
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
  rdirs <- getAyxSvnRDirs()
  readmeFile = file.path(rdirs$installer, "Readme.txt")
  svnLibDir = rdirs$lib
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

#' @export
getAyxSvnDirs <- function(svnDir = getOption("alteryx.svndir")){
  svnDir <- 'C:/Users/ramnath/Desktop/Alteryx10.6_Predictive_Development'
  list(
    htmlplugin = file.path(svnDir, 'HtmlPlugins'),
    macro = file.path(svnDir, 'RuntimeData', 'Macros', 'Supporting_Macros')
  )
}

#' @export
getAyxSvnRDirs <- function(svnDir = getOption("alteryx.svndir")){
  list(
    lib = file.path(svnDir, 'R', 'R_Installed_Files', 'R-3.2.3', 'library'),
    installer = file.path(svnDir, 'R', 'Installer')
  )
}