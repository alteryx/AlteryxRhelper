isOlder2 <- function(target, ...){
  if (!file.exists(target)){ return(TRUE) }
  any(Filter(Negate(is.na), file.mtime(target) < file.mtime(...)))
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
  if (updated == FALSE){
    message("Nothing to update...")
  } else {
    updateHtmlPlugin(pluginDir)
  }
}

#' Generate table of configuration items
#' 
#' 
#' @export
#' @param yxmcFile path to macro
generateConfigurationTable <- function(yxmcFile){
  d <- yxmc2yaml(yxmcFile)
  d2 <- plyr::ldply(names(d), function(x){
    d_ = d[[x]]
    foo = function(x){x[is.null(x)] <- ""; x}
    c(
      dataName = x, 
      label = ifelse(is.null(d_$label), "", d_$label),
      default = ifelse(is.null(d_$default), "", d_$default),
      values = ifelse(is.null(d_$values), "", 
        jsonlite::toJSON(names(d_$values), auto_unbox = TRUE)
      )
    )
  })
}