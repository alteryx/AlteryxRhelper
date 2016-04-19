isOlder2 <- function(target, ...){
  #if (!file.exists(target)){ return(TRUE) }
  any(file.mtime(target) < file.mtime(...))
}

#' Build Plugin
#' 
#' 
#' @export
#' @param pluginDir directory containing the plugin
buildPlugin <- function(pluginDir = "."){
  yxmc <- list.files(file.path(pluginDir, "Supporting_Macros"), pattern = ".yxmc$", full.names = T)
  pluginName = tools::file_path_sans_ext(basename(yxmc))
  guiFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  configFile <- file.path(pluginDir, sprintf("%sConfig.xml", pluginName))
  rFile <- list.files(file.path(pluginDir, "Supporting_Macros"), pattern = ".R$", full.names = T)
  updated <- FALSE
  if (dir.exists('Gui')){
    if (isOlder2(guiFile, 'Gui/layout.html', 'Gui/overrides.yaml', yxmc)){
      updated <- TRUE
      message("Updating Gui.html and Config.xml")
      createPluginFromMacro(pluginDir)
    }
  }
  if (isOlder2(yxmc, rFile)){
    updated <- TRUE
    insertRcode(yxmc, rFile)
  }
  if (updated == FALSE){
    message("Nothing to update...")
  } else {
    updateHtmlPlugin(pluginDir)
  }
}