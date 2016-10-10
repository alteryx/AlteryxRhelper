#' Install packages to R library in SVN
#' 
#' 
#' @param srcPkg path to source package
#' @param install whether or not to actually install dependencies
#' @export
installToSvn <- function(srcPkg, to_install = TRUE){
  srcPkg <- normalizePath(srcPkg)
  rdirs <- getAyxSvnRDirs()
  svnLibDir <- rdirs$lib
  d <- read.dcf(file.path(srcPkg, "DESCRIPTION"))
  imports <- gsub("\\n", "", strsplit(d[,'Imports'], ",")[[1]])
  ayxPackages <- c("AlteryxPrescriptive", "AlteryxRDataX", "AlteryxRviz")
  imports <- setdiff(imports, ayxPackages)
  deps <- miniCRAN::pkgDep(imports, suggests = FALSE)
  installed <- rownames(installed.packages(svnLibDir))
  depsToInstall <- setdiff(deps, installed)
  install <- devtools::install
  if (to_install){
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
    file = file.path(rdirs$installer, "../Scripts", "packages.csv"), 
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
  ayxPackages <- c("AlteryxPrescriptive", "AlteryxRDataX", "AlteryxRviz")
  allPkgs <- unique(unname(sort(c(pkgs$Package, ayxPackages))))
  if (save){
    writeLines(allPkgs, readmeFile)
  } else {
    return (allPkgs)
  }
}

#' Get Alteryx SVN Directories
#' 
#' 
#' @param svnDir svn directory
#' @export
getAyxSvnDirs <- function(svnDir = getOption("alteryx.svndir")){
  svnDir <- 'C:\\Users\\ramnath\\Desktop\\SVN_Full_Repos\\Predictive_Development'
  list(
    htmlplugin = file.path(svnDir, "Alteryx", "Plugins", "AlteryxRPlugin", 
                           "HtmlPlugins"),
    macro = NULL
  )
}

#' @export
copyHtmlPluginToSvn <- function(pluginDir = "."){
  copyHtmlPlugin(ayxDir = getAyxSvnDirs())
}

#' Get Alteryx R SVN Directories
#' 
#' 
#' @param svnDir svn directory
#' @export
getAyxSvnRDirs <- function(svnDir = getOption("alteryx.svndir")){
  svnDir <- 'C:\\Users\\ramnath\\Desktop\\SVN_Full_Repos\\Predictive_Development'
  rPath <- file.path(svnDir, "3rdParty", "R")
  list(
    lib = file.path(rPath, 'R_Installed_Files', 'R-3.2.3', 'library'),
    installer = file.path(rPath, 'Installer')
  )
}