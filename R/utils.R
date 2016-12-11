#' Check if the execution environment is Alteryx.
#' 
#' Determine if code is being executed inside an Alteryx R Tool. It utilizes the
#' presence of certain global variables like \code{AlteryxDataOutput} to detect
#' if the execution environment is Alteryx or not.
#' @export
inAlteryx <- function(){
  exists("AlteryxDataOutput", .GlobalEnv)
}

#' List predictive macros.
#' 
#' This returns a list of predictive macros in Alteryx. The list can be filtered
#' by passing additional arguments to the \code{\link{dir}} function.
#' 
#' @export
#' @param ... additional arguments to pass on to dir
listPredictiveMacros <- function(...){
  dir(getPathToPredictiveMacros(), ...)
}

getPathToPredictiveMacros <- function(){
  alteryx = getOption('alteryx.path')
  file.path(alteryx, paste0('R-', '3.2.3'), 'plugin', 'Macros', 
    'Predictive Tools'
  )
}

#' Copy predictive macro
#'
#'
#' @export
#' @param macro macro
#' @param to to
#' @param ... extra arguments to pass on to file.copy
copyPredictiveMacro <- function(macro, to = ".", ...){
  p <- getPathToPredictiveMacros()
  pred_tools_path = normalizePath(p)
  from <- file.path(pred_tools_path, macro)
  if (file.exists(from)){
    file.copy(from, to = to, ...)
  } else {
    message(macro, ' not found.') 
  }
}

#' Check if a file is older than the other
#'
#' @param x path to the first file
#' @param y path to the second file
#' @export
isOlder = function(x, y){
  if (!file.exists(x)){return(TRUE)}
  file.info(y)$mtime > file.info(x)$mtime
}

fieldSize = function(fieldType){
  x = c(V_WString = 1073741823, Double = 8, Int32 = 4)
  x[fieldType]
}

# writeMeta = function(nOutput, fields, fieldType = 'V_WString', source = "R-Data:"){
#   for (i in length(fields)){
#     fType = names(fields)[i]
#     if (is.null(fType)) fType = fieldType
#     size = fieldSize(fType)
#     AlteryxRDataX::write.AlteryxMetaInfo(nOutput = nOutput, name = f, fieldType = fType,
#       size = size, source = source
#     )
#   }
# }

# template template
aTemplate = function(template){
  system.file('templates', template, package = 'AlteryxRhelper')
}




# Dump and quit on error
dumpAndQuit <- function() {
  wd = getOption('alteryx.wd')
  # Save debugging info to file last.dump.rda
  f <- file.path(wd, 'last.dump.rda')
  AlteryxRDataX::AlteryxMessage(paste0('Saving dump to ', f))
  AlteryxRDataX::AlteryxMessage("You can debug the R code by loading the rda file and
    running load('last.dump.rda');debugger();
  ")
  dump.frames(to.file = TRUE)
  file.copy('last.dump.rda', f, overwrite = TRUE)
  # Quit R with error status
  q(status = 1)
}

listMacros <- function(repo = 'install'){
  paths = c(
    install = 'C:\\Program Files\\Alteryx\\R-3.1.3\\plugin\\Macros\\Predictive Tools',
    svn = 'C:\\Users\\ramnath\\Desktop\\R_Packages\\PredictiveMacros'
  )
  list.files(paths[repo], pattern = '.yxmc')
}

findMacro <- function(macro, repo = 'install'){
  paths = c(
    install = 'C:\\Program Files\\Alteryx\\R-3.1.3\\plugin\\Macros\\Predictive Tools',
    svn = 'C:\\Users\\ramnath\\Desktop\\R_Packages\\PredictiveMacros'
  )
  macro = file.path(paths[repo], paste0(macro, '.yxmc'))
  if (file.exists(macro)){
    return(macro)
  } else {
    message('Sorry. I could not find ', macro)
  }
}

copyMacro <- function(macro, todir = '.', repo = 'svn', ...){
  from = findMacro(macro)
  file.copy(from, todir, ...)
}


copyPredictiveAndHelperMacros <- function(macro, pluginDir = '.'){
  dirs <- dirNames()
  to <- file.path(pluginDir, dirs$macros)
  copyPredictiveMacro(macro, to = to, overwrite = TRUE)
  iconFile <- file.path(
    pluginDir,
    sprintf("%sIcon.png", tools:::file_path_sans_ext(basename(macro)))
  )
  p <- getPathToPredictiveMacros()
  pred_tools_path = normalizePath(p)
  macroPath <- file.path(pred_tools_path, macro)
  extractIcon(macroPath, iconFile)
  macro <- file.path(to, macro)
  doc <- xmlInternalTreeParse(macro)
  root <- xmlRoot(doc)
  helpers <- getNodeSet(root, '//EngineSettings[@Macro]')
  sapply(helpers, function(d){
    x <- gsub('\\\\', '/', xmlGetAttr(d, 'Macro'))
    copyPredictiveMacro(x, file.path(dirs$macros, dirname(x)))
  })
}

dirNames <- function(){
  list(
    macros = 'Supporting_Macros',
    extras = 'Extras'
  )
}

#' Generate table of configuration items
#' 
#' 
#' @export
#' @import jsonlite
#' @param yxmcFile path to macro
generateConfigurationTable <- function(yxmcFile){
  d <- yxmc2yaml(yxmcFile)
  d2 <- plyr::ldply(names(d), function(x){
    d_ = d[[x]]
    foo = function(x){x[is.null(x)] <- ""; x}
    c(
      dataName = x, 
      label = ifelse(is.null(d_$label), "", d_$label),
      default = ifelse(
        is.null(d_$default), 
        ifelse(is.null(d_$placeholder), "",  d_$placeholder),
        d_$default
      ),
      values = ifelse(is.null(d_$values), "", 
        gsub(",", ", ", jsonlite::toJSON(names(d_$values), auto_unbox = TRUE))
      )
    )
  })
}

copy_dir <- function (from, to) {
  if (!(file.exists(to))) {
    dir.create(to, recursive = TRUE)
    message("Copying files to ", to, "...")
    file.copy(list.files(from, full.names = T), to, recursive = TRUE)
  }
}

updateReadme <- function(readme, pluginName){
  input <- paste(readLines(readme, warn = F), collapse = '\n')
  output <- whisker::whisker.render(input, list(pluginName = pluginName))
  cat(output, file = readme)
}

# Render README.Rmd files in the plugin folder
renderReadmes <- function(pluginDir = ".", ...){
  AlteryxRhelper:::with_dir_(pluginDir, {
    readmes <- list.files(
      ".", pattern = 'README', recursive = TRUE, full.names = TRUE
    )
    for (readme in readmes){
      rmarkdown::render(readme, ...)
    }
  })
}

with_dir_ <- function (new, code) {
  old <- setwd(dir = new)
  on.exit(setwd(old))
  force(code)
}

bool_map <- function(x){
  c(True = TRUE, False = FALSE)[[x]]
}

toKeyValuePairs <- function(x){
  y <- strsplit(x, "\n")[[1]]
  if (grepl(":", y[1])){
    y <- strsplit(y, ":\\s*")
    as.list(setNames(sapply(y, '[[', 1), sapply(y, '[[', 2)))
  } else {
    as.list(setNames(y, y))
  }
}

getPluginFiles <- function(pluginDir = "."){
  dirs <- dirNames()
  pluginName = basename(normalizePath(pluginDir, mustWork = TRUE))
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


