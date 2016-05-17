isOlder2 <- function(target, ...){
  if (!file.exists(target)){ return(TRUE) }
  any(Filter(Negate(is.na), file.mtime(target) < file.mtime(...)))
}

#' Build Plugin
#' 
#' 
#' @export
#' @param pluginDir directory containing the plugin
buildPlugin <- function(pluginDir = ".", build = FALSE){
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
      default = ifelse(is.null(d_$default), 
        ifelse(is.null(d_$placeholder), "",  d_$placeholder),
        d_$default
      ),
      values = ifelse(is.null(d_$values), "", 
        gsub(",", ", ", jsonlite::toJSON(names(d_$values), auto_unbox = TRUE))
      )
    )
  })
}

#' Run build process to update app.min.js and app.css
#' 
#' 
#' @export
npmBuild <- function(pluginDir = ".", type = 'nwb'){
  with_dir_(pluginDir, {
    l <- as.list(append('app.min.js', list.files("App/src", recursive = TRUE, full.names = TRUE)))
    updated <- do.call(isOlder2, l)
    if (updated){
      if (type == 'nwb'){
        message("Running nwb build-umd")
        with_dir_("App", system('nwb build-umd'))
        message("Copying built files to app.min.js and app.css")
        jsFile = list.files("App/umd", pattern = "^.*\\.min.js$", full.names = TRUE)
        file.copy(jsFile, "app.min.js", overwrite = TRUE)
        cssFile = list.files("App/umd", pattern = "^.*\\.css$", full.names = TRUE)
        file.copy(cssFile, "app.css", overwrite = TRUE)
      }
      else {
        message("Running npm run build-umd")
        with_dir_("App", system('npm run build-umd'))
        message("Copying built file app.min.js")
        file.copy('App/dist/src.js', 'app.min.js', overwrite = TRUE)
        message("Copying built file app.css")
        file.copy('App/dist/style.css', 'app.css', overwrite = TRUE)
      }
    } else {
      message("Nothing to update")
    } 
  })
  updated
}