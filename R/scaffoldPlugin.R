#' Scaffold a new plugin with a predictive macro
#'
#'
#' @export
#' @param pluginName name of the plugin
scaffoldPlugin <- function(pluginName){
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
      pluginName, pattern = 'README', recursive = TRUE, full = TRUE
    )
    for (readme in readmes){
      updateReadme(readme, pluginName)
    }
  }
  cwd = getwd(); on.exit(setwd(cwd)); setwd(pluginName)
  scaffoldWorkflow(pluginName, outDir = 'Supporting_Macros', edit = F)
  if (!dir.exists('Supporting_Files')){
    dir.create('Supporting_Files')
  }
  if (!file.exists('app.min.js')){
    app = system.file('templates', 'app.min.js', package = 'AlteryxRhelper')
    file.copy(app, ".")
  }
  if (!file.exists('app.css')){
    file.create('app.css')
  }
  createPluginFromMacro(".")
  message("Initializing a git repository...")
  r <- git2r::init(".")
  # paths <- unlist(git2r::status(r))
  # git2r::add(r, paths)
  # git2r::commit(r, 'Initial Commit')
}


updateReadme <- function(readme, pluginName){
  input <- paste(readLines(readme, warn = F), collapse = '\n')
  output <- whisker::whisker.render(input, list(pluginName = pluginName))
  cat(output, file = readme)
}