#' Scaffold a new workflow based on a template
#' 
#' 
#' @export
#' @param name name
#' @param outDir output directory
#' @param template template
#' @param edit open file for editing
#' @param ... additional arguments
scaffoldWorkflow <- function(name = 'mymacro', outDir  = ".", 
                             template =  aTemplate('predictive_template.yxmc'), edit = TRUE, ...){
  if (!file.exists(outDir)){
    dir.create(outDir)
  }
  tFile = template
  oFile = file.path(outDir, paste0(name, ".", tools::file_ext(template)))
  file.copy(tFile, oFile)
  extractRcode(oFile, ...)
  rFiles = list.files(outDir, pattern = '\\.R$', full.names = TRUE)
  rFiles2 = grep(name, rFiles, value = TRUE)
  if (edit){
    invisible(lapply(rFiles2, file.edit))
    message("Switching to directory ", outDir)
    setwd(outDir)
  }
}

#' Scaffold a new plugin with a predictive macro
#'
#'
#' @export
#' @param pluginName name of the plugin
#' @param ... extra arguments to pass on to createPluginFromMacro
scaffoldPlugin <- function(pluginName, ...){
  if (!dir.exists(pluginName)){
    #dir.create(pluginName)
    from <- system.file('pluginTemplate', package = 'AlteryxRhelper')
    copy_dir(from, pluginName)
    file.rename(
      file.path(pluginName, 'template.txt'), 
      file.path(pluginName, paste0(pluginName, '.Rproj'))
    )
    file.rename(
      file.path(pluginName, 'gitignore'), 
      file.path(pluginName, '.gitignore')
    )
    readmes <- list.files(
      pluginName, pattern = 'README', recursive = TRUE, full.names = TRUE
    )
    for (readme in readmes){
      updateReadme(readme, pluginName)
    }
  }
  cwd = getwd(); on.exit(setwd(cwd)); setwd(pluginName)
  dirs <- dirNames()
  scaffoldWorkflow(pluginName, outDir = dirs$macros, edit = F)
  if (!dir.exists(dirs$extras)){
    dir.create(dirs$extras)
  }
  if (!file.exists('app.min.js')){
    app = system.file('templates', 'app.min.js', package = 'AlteryxRhelper')
    file.copy(app, ".")
  }
  if (!file.exists('app.css')){
    file.create('app.css')
  }
  createPluginFromMacro(".", ...)
  # message("Initializing a git repository...")
  # r <- git2r::init(".")
  # paths <- unlist(git2r::status(r))
  # git2r::add(r, paths)
  # git2r::commit(r, 'Initial Commit')
}


#' Scaffold a plugin based on an existing predictive macro.
#'
#'
#' @export
#' @param macro name of the predictive macro
scaffoldPluginFromMacro <- function(macro){
  dirs <- dirNames()
  scaffoldPlugin(macro)
  setwd(macro)
  copyPredictiveAndHelperMacros(paste0(macro, '.yxmc'))
  extractRcode(
    file.path(dirs$macros, paste0(macro, '.yxmc')), 
    extractInput = 'config'
  )
  createPluginFromMacro()
}
