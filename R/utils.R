#' Returns TRUE if called inside an Alteryx R Tool.
#'
#' @export
inAlteryx <- function(){
  'package:AlteryxRDataX' %in% search()
}

listPredictiveMacros <- function(...){
  dir(getPathToPredictiveMacros(), ...)
}

getPathToPredictiveMacros <- function(){
  alteryx = getOption('alteryx.path')
  file.path(alteryx, paste0('R-', getRversion()), 'plugin', 'Macros', 
    'Predictive Tools'
  )
}

#' Copy predictive macro
#'
#'
#' @export
#' @param macro macro
#' @param to to
copyPredictiveMacro <- function(macro, to = "."){
  p <- getPathToPredictiveMacros()
  pred_tools_path = normalizePath(p)
  file.copy(file.path(pred_tools_path, macro), to = to)
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

#' Template
#' 
#' 
#' @export
#' @param template template
aTemplate = function(template){
  system.file('templates', template, package = 'AlteryxRhelper')
}

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
    template =  aTemplate('empty.yxmc'), edit = TRUE, ...){
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


#' Dump and quit on error
#' 
#' 
#' @export
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


copyPredictiveAndHelperMacros <- function(macro, to = '.'){
  copyPredictiveMacro(macro, to = to)
  macro <- file.path(to, macro)
  doc <- xmlInternalTreeParse(macro)
  root <- xmlRoot(doc)
  helpers <- getNodeSet(root, '//EngineSettings[@Macro]')
  sapply(helpers, function(d){
    x <- gsub('\\\\', '/', xmlGetAttr(d, 'Macro'))
    copyPredictiveMacro(x, 'Supporting_Macros/Helper_Macros')
  })
  d <- paste(readLines(macro, warn = F), collapse = '\n')
  d <- gsub("Supporting_Macros", "Helper_Macros", d)
  cat(d, file = macro)
}




