getPluginFiles <- function(pluginDir = "."){
  pluginName = basename(normalizePath(pluginDir, mustWork = TRUE))
  files = list.files(pluginDir, pattern = '^[^App]', full.names = F)
  files =  files[!grepl("^(Supporting_Macros|Gui|LICENSE|README)", files)]
  files = files[!grepl("^.*\\.Rproj", files)]
  supporting_macro <- file.path(
    "Supporting_Macros", 
    paste0(pluginName, ".yxmc")
  )
  list(htmlplugin = files, macro = supporting_macro)
}

getAyxDirs <- function(alteryxDir = getOption('alteryx.path')){
  if (!dir.exists(alteryxDir)){
    stop("The directory to copy the plugin to ", alteryxDir, " does not exist")
  }
  ayxPluginDir <- file.path(alteryxDir, 'bin', 'HtmlPlugins')
  if (!(file.exists(ayxPluginDir))) {
    message("Creating ", pluginName, " directory")
    dir.create(to, recursive = TRUE)
  }
  ayxMacroDir <- file.path(alteryxDir, 'bin', 
    'RuntimeData', 'Macros', 'Supporting_Macros'
  )
  list(htmlplugin = ayxPluginDir, macro = ayxMacroDir)
}

#' Update Html Plugin
#'
#'
#' @param pluginDir plugin directory to copy from
#' @param ayxDir alteryx directories to copy to
#' @export
copyHtmlPlugin <- function(pluginDir = ".", ayxDir = getAyxDirs()){
  pluginName <- basename(normalizePath(pluginDir))
  ayxPluginDir <- file.path(ayxDir$htmlplugin, pluginName)
  if (!dir.exists(ayxPluginDir)) dir.create(ayxPluginDir)
  ayxMacroDir <- ayxDir$macro
  
  files <- getPluginFiles(pluginDir)
  
  pluginFiles <- file.path(pluginDir, files$htmlplugin)
  ayxPluginFiles <- dir(ayxPluginDir, full.names = TRUE)
  if (length(ayxPluginFiles) > 0){
    pluginFilesToUpdate <- pluginFiles[
      file.mtime(pluginFiles) > file.mtime(ayxPluginFiles)
    ]
  } else {
    pluginFilesToUpdate <- pluginFiles
  }
  if (length(pluginFilesToUpdate) > 0){
    message('Copying\n  ',  paste(pluginFilesToUpdate, collapse = "\n  "), 
      '\nto HtmlPlugins'
    )
    file.copy(pluginFilesToUpdate, ayxPluginDir, recursive = TRUE)
  } else {
    message(file.path('HtmlPlugins', basename(ayxPluginDir)), ' is up to date.')
  }
  
  macro <- file.path(pluginDir, files$macro)
  ayxMacro <- file.path(ayxMacroDir, basename(files$macro))
  if (file.exists(macro) && 
    !file.exists(ayxMacro) || file.mtime(macro) > file.mtime(ayxMacro)){
    message('Copying supporting macro ', basename(macro), ' to ', ayxMacroDir)
    file.copy(macro, ayxMacro, overwrite = TRUE)
  } else {
    message(file.path('SupportingMacros', basename(ayxMacro)), ' is up to date')
  }
}

#' Create YXI file
#' 
#' 
#' @param pluginDir plugin directory
#' @param toDir to directory
#' @export
createYXI2 <- function(pluginDir = ".", toDir = "."){
  pluginName = basename(normalizePath(pluginDir, mustWork = TRUE))
  pluginFiles <- getPluginFiles(pluginDir)
  toDir <- normalizePath(toDir, mustWork = TRUE)
  dirName = dirname(normalizePath(pluginDir))
  cwd = getwd(); setwd(dirName); on.exit(setwd(cwd));
  filesToCopy <- file.path(
    pluginName, 
    unlist(pluginFiles, use.names = F)
  )
  zip(
    file.path(toDir, paste0(pluginName, '.yxi')),
    filesToCopy,
    flags = ""
  )
}


#' Build Plugin
#' 
#' 
#' @export
#' @param pluginDir directory containing the plugin
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
