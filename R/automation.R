#' Copy predictive macros, samples and plugins from SVN
#' 
#' @param to directory to copy files to
#' @export
copyAlteryxRPlugin <- function(to = NULL){
  if (is.null(to)){
    to <- file.path(getOption('dev.dir'), 'dev', 'AlteryxRPlugin')
  }
  pluginDir <- file.path(getOption('alteryx.svndir'), 'Alteryx', 'Plugins',
    'AlteryxRPlugin'
  )
  files_to_copy <- list.files(pluginDir, full.names = T, 
      pattern = 'Macros|Samples|HtmlPlugins')
  if (!(file.exists(to))) {
    message("Creating directory ", to, "...")
    dir.create(to, recursive = TRUE)
  }
  message("Copying files to ", to, "...")
  file.copy(files_to_copy, to, recursive = TRUE)
}

runFromWindows <- function(){
  if(.Platform$OS.type != "windows"){
    stop("Please run this function from Windows", call. = FALSE)
  }
}


#' Download Installers from the Build Repo
#' 
#' @param buildRepo path to build repo.
#' @param to directory to download installers to.
#' @param buildDir build directory. 
#' @param branch string indicating branch. defaults to Predictive_Dev.
#' @param type string indicating installer type. one of 'Server', 'Gallery',
#'   'NonAdmin' or ''.
#' @export
downloadInstallers <- function(
   buildRepo = "\\\\DEN-IT-FILE-07\\BuildRepo", 
   to = "C:/Users/ramnath/Downloads",
   buildDir = NULL, branch = "Predictive_Dev", type = 'Server'){
  runFromWindows()
  to <- normalizePath(to)
  if (is.null(buildDir)){
    message("No buildDir specified. So defaulting to latest.")
    builds <-  dir(buildRepo, pattern = branch, full = TRUE)
    buildDir <- tail(builds, 1)
  }
  message("Build Directory is ", buildDir)
  ayxInstaller <- list.files(
    file.path(buildDir, 'Alteryx'), pattern = type, full.names = TRUE
  )
  message("Downloading ", ayxInstaller)
  file.copy(ayxInstaller, to)
  rInstaller <- list.files(
    file.path(buildDir, 'R'), pattern = 'RInstaller', full.names = TRUE
  )
  message("Downloading ", rInstaller)
  file.copy(rInstaller, to)
  list(ayxInstaller = ayxInstaller, rInstaller = rInstaller)
}

#' Install Alteryx
#' 
#' @param installers list of named installers.
#' @param from directory to install from.
#' @export
installAlteryx <- function(installers, from = 'C:/Users/ramnath/Downloads'){
  withr::with_dir(from, {
    r <- plyr::llply(installers, function(installer){
      message("Installing ", basename(installer))
      install_cmd <- paste(basename(installer), '/s')
      message('Running ', install_cmd)
      system(install_cmd)
    })
  })
}

# List all installed packages
listInstalledPackages <- function(){
  rdirs <- getAyxSvnRDirs()
  readmeFile = file.path(rdirs$installer, "Readme.txt")
  pkgs <- readLines(readmeFile)
  ayxPkgs <- grep("^Alteryx", pkgs, value = TRUE)
  list(
    cran = setdiff(pkgs, ayxPkgs),
    alteryx = ayxPkgs
  )
}

# Write RPluginIni
writeRPluginIni <- function(revo = "FALSE", replace = FALSE){
  RpluginIni <- file.path(getOption('alteryx.path'), 'Settings',
   'RPluginSettings.ini'
  )
  l <- c(
    RVersion = paste(R.version$major, R.version$minor, sep = "."),
    RExePath = normalizePath(R.home()),
    RevolutionRinstalled = "FALSE"
  )
  contents <- c(
    '[Settings]',
    paste(names(l), l, sep = "=")
  )
  if (replace){
    message('Writing new RpluginSettings.ini')
    writeLines(contents, con = RpluginIni)
  } else {
    return(contents)
  }
}

#' Install all needed packages that are missing
#'
#' @export
installAllPackages <- function(){
  runFromWindows()
  cranPkgs <- listInstalledPackages()$cran
  existing_packages <- row.names(installed.packages())
  needed_packages <- cranPkgs[!(cranPkgs %in% existing_packages)]
  if (length(.libPaths()) == 1) {
    lib <- .libPaths()
  } else {
    lib <- .libPaths()[2]
  }
  if (length(needed_packages) > 0){
    message("Installing packages ")
    message(paste(needed_packages, collapse = "\n"))
    install.packages(needed_packages)
  }
  ayxPackages <- c("AlteryxSim", "AlteryxPredictive",
   "AlteryxPrescriptive", "AlteryxRDataX",  "AlteryxRviz")
  ayxPackages <- file.path(getOption('dev.dir'), 'dev',
    'AlteryxRPackage', ayxPackages)
  library(devtools)
  withr::with_libpaths(lib, {
    lapply(ayxPackages, install)
  })
}

#' Update R installation
#' 
#' @export
updateRInstallation <- function(){
  message('Installing missing R packages...')
  installAllPackages()
  message("Updating RPluginSettings.ini...")
  writeRPluginIni(replace = TRUE)
}

