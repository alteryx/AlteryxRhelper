getPluginFiles <- function(pluginDir = "."){
  dirs <- dirNames()
  pluginName = basename(normalizePath(pluginDir, mustWork = TRUE))
  #files = list.files(pluginDir, pattern = '^[^App]', full.names = F)
  #files =  files[!grepl("^(Supporting_Macros|Gui|LICENSE|README|Supporting_Files)", files)]
  files <- list.files(include.dirs = FALSE)
  files <- files[!grepl("^(Extras|Supporting_Macros|App|Gui)", files)]
  files = files[!grepl("^.*\\.Rproj", files)]
  supporting_macro <- file.path(
    dirs$macros, 
    paste0(pluginName, ".yxmc")
  )
  helper_macros <- list.files(
    file.path(dirs$macros, 'Supporting_Macros'), 
    pattern = "yxmc",
    full.names = TRUE
  )
  list(htmlplugin = files, macro = supporting_macro, helpers = helper_macros)
}

getAyxDirs <- function(alteryxDir = getOption('alteryx.path')){
  if (!dir.exists(alteryxDir)){
    stop("The directory to copy the plugin to ", alteryxDir, " does not exist")
  }
  ayxPluginDir <- file.path(alteryxDir, 'bin', 'HtmlPlugins')
  ayxMacroDir <- file.path(alteryxDir, 'bin', 
    'RuntimeData', 'Macros', 'Supporting_Macros'
  )
  list(htmlplugin = ayxPluginDir, macro = ayxMacroDir)
}


#' Build Plugin
#' 
#' 
#' @export
#' @param pluginDir directory containing the plugin
#' @param build whether or not to run npm build
buildPlugin2 <- function(pluginDir = ".", build = FALSE){
  yxmc <- list.files(
    file.path(pluginDir, "Supporting_Macros"), pattern = ".yxmc$", 
    full.names = T
  )
  pluginName = tools::file_path_sans_ext(basename(yxmc))
  guiFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  configFile <- file.path(pluginDir, sprintf("%sConfig.xml", pluginName))
  rFile <- list.files(
    file.path(pluginDir, "Supporting_Macros"), pattern = ".R$", full.names = T
  )
  updated <- FALSE
  if (dir.exists('Gui')){
    to_update <- isOlder2(guiFile, 'Gui/layout.html', 'Gui/overrides.yaml', yxmc)
  } else {
    to_update <-  isOlder2(guiFile, yxmc)
  }
  if (to_update){
    updated <- TRUE
    message("Updating Gui.html and Config.xml")
    createPluginFromMacro(pluginDir)
  }
  if (isOlder2(yxmc, rFile)){
    updated <- TRUE
    insertRcode(yxmc, rFile)
  }
  if (build){
    l <- as.list(append('app.min.js', list.files("App/src", recursive = TRUE)))
    if (do.call('isOlder2', l)){
      withr::with_dir("App", system('npm run build-umd'))
      file.copy('App/dist/src.js', 'app.min.js', overwrite = TRUE)
      updated = TRUE
    }
  }
  if (updated == FALSE){
    message("Nothing to update...")
  } else {
    copyHtmlPlugin(pluginDir)
  }
}
