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
   buildDir = NULL, branch = "Predictive_Dev", type = 'Server',
   rInstaller = 'RInstaller'){
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
    file.path(buildDir, 'R'), pattern = rInstaller, full.names = TRUE
  )
  message("Downloading ", rInstaller)
  file.copy(rInstaller, to)
  list(
    ayxInstaller = file.path(to, basename(ayxInstaller)), 
    rInstaller = file.path(to, basename(rInstaller))
  )
}

#' Install Alteryx
#' 
#' @param installers list of paths to named installers.
#' @export
installAlteryx <- function(installers){
  withr::with_dir(dirname(installers), {
    r <- plyr::llply(basename(installers), function(installer){
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
installAllPackages <- function(dev = TRUE){
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

#' Install all packages
#' 
#' 
#' @export
installAllPackages2 <- function(
    cranRepo = 'https://mran.revolutionanalytics.com/snapshot/2016-01-01/',
    ayxRepo = 'https://alteryx.github.io/drat',
    buildRepo = "\\\\DEN-IT-FILE-07\\BuildRepo",
    branch = 'Predictive_Development'){
  runFromWindows()
  requiredPkgs <- unlist(listInstalledPackages(), use.names = F)
  requiredPkgs <- requiredPkgs[requiredPkgs != 'AlteryxRDataX']
  existing_packages <- row.names(installed.packages())
  needed_packages <- requiredPkgs[!(requiredPkgs %in% existing_packages)]
  if (length(.libPaths()) == 1) {
    lib <- .libPaths()
  } else {
    lib <- .libPaths()[2]
  }
  message("Installing AlteryxRDataX...")
  if (is.null(buildDir)){
    builds <-  dir(buildRepo, pattern = branch, full = TRUE)
    buildDir <- tail(builds, 1)
  }
  RDataX <- list.files(file.path(buildDir, 'R'), pattern = 'AlteryxRDataX_', 
    full.names = TRUE)
  install.packages(RDataX, repos = NULL)
  if (length(needed_packages) > 0){
    repos <- c(CRAN = cranRepo, Alteryx = ayxRepo)
    message("Installing missing packages from CRAN...")
    message(paste(needed_packages, collapse = "\n"))
    install.packages(needed_packages, repos = repos)
    message("Updating CRAN packages ...")
    update.packages()
  }
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

