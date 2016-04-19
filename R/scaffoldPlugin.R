#' Scaffold a new plugin with a predictive macro
#'
#'
#' @export
#' @param pluginName name of the plugin
scaffoldPlugin <- function(pluginName){
  if (!dir.exists(pluginName)){
    dir.create(pluginName)
  }
  cwd = getwd(); on.exit(setwd(cwd)); setwd(pluginName)
  scaffoldWorkflow(pluginName, outDir = 'Supporting_Macros', edit = F)
  if (!file.exists('app.min.js')){
    app = system.file('templates', 'app.min.js', package = 'AlteryxRhelper')
    file.copy(app, ".")
  }
  if (!file.exists('app.css')){
    file.create('app.css')
  }
  createPluginFromMacro(".")
}
