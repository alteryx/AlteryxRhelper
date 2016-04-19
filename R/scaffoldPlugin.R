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
  createPluginFromMacro(".")
}