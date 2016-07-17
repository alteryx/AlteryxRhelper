#' Create Plugin from Macro
#' 
#' 
#' @export
#' @param pluginDir plugin directory
#' @param overrides should an override file be used
#' @param layout should a layout file be used
#' @param render_readme whether or not to render READMEs
#' @param ... arguments to pass on to makeIcon
#' @import yaml htmltools
createPluginFromMacro <- function(pluginDir = ".", overrides = NULL, 
    layout = NULL, render_readme = TRUE, ...){
  dirs <- dirNames()
  pluginDir = normalizePath(pluginDir)
  pluginName <- basename(pluginDir)
  yxmcFile <- file.path(
    pluginDir, dirs$macros, sprintf("%s.yxmc", pluginName)
  )
  if (is.null(layout)){
    if (file.exists(
      l <- file.path(pluginDir, dirs$extras, 'Gui', 'layout.html')
    )){
      layout = TRUE
    }
  }
  if (is.null(layout)){
    writeGuiHtml(pluginDir, overrides = overrides)
  } else {
    writeGuiHtmlFromLayout(pluginDir, overrides = overrides) 
  }
  yxmc2PluginConfig(yxmcFile, saveToFile = paste0(pluginName, "Config.xml"))
  if (!file.exists(icon <- paste0(pluginName, "Icon.png"))){
    makeIcon(icon, ...)
  }
  if (render_readme){
    renderReadmes(pluginDir, quiet = TRUE)
  }
}

#' Run build process to update app.min.js and app.css
#' 
#' 
#' @export
#' @param pluginDir plugin directory
#' @param type nwb or mobx
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

#' Update Html Plugin
#'
#'
#' @param pluginDir plugin directory to copy from
#' @param ayxDir alteryx directories to copy to
#' @export
copyHtmlPlugin <- function(pluginDir = ".", ayxDir = getAyxDirs()){
  dirs <- dirNames()
  pluginName <- basename(normalizePath(pluginDir))
  ayxPluginDir <- file.path(ayxDir$htmlplugin, pluginName)
  if (!dir.exists(ayxPluginDir)) dir.create(ayxPluginDir)
  ayxMacroDir <- ayxDir$macro
  
  files <- getPluginFiles(pluginDir)
  
  pluginFiles <- file.path(pluginDir, files$htmlplugin)
  ayxPluginFiles <- list.files(ayxPluginDir, full.names = TRUE)
  ayxPluginFiles <- ayxPluginFiles[!grepl(dirs$macros, ayxPluginFiles)]
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
  ayxMacroDir <- file.path(ayxPluginDir, dirs$macros)
  if (!dir.exists(ayxMacroDir)){
    dir.create(ayxMacroDir)
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
  message("Copying Helper Macros")
  if (length(files$helpers) > 0){
    if (!dir.exists(hdir <- file.path(ayxPluginDir, dirs$macros, 'Supporting_Macros'))){
      dir.create(hdir)
    }
    sapply(files$helpers, function(helper){
      message("Copying helper macros...", basename(helper))
      file.copy(helper, hdir)
    })
  }
  
}

#' Create a YXI file
#' 
#' 
#' YXI files are zip files with a `.yxi` extension that allow a plugin to be
#' packaged along with all its dependencies. This function allows a user to
#' create a YXI file programatically. The resulting `.yxi` file can be
#' distributed standalone.
#' 
#' @param pluginDir plugin directory
#' @param toDir to directory
#' @export
createYXI <- function(pluginDir = ".", toDir = "."){
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


