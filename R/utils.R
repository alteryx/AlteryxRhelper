#' Returns TRUE if called inside an Alteryx R Tool.
#'
#' @export
inAlteryx <- function(){
  'package:AlteryxRDataX' %in% search()
}


#' Copy predictive macro
#'
#' @export
copyPredictiveMacro <- function(macro, to = "."){
  alteryx = getOption('alteryx.path')
  p <- file.path(alteryx, 'R-3.1.2/plugin/Macros/Predictive Tools')
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

writeMeta = function(nOutput, fields, fieldType = 'V_WString', source = "R-Data:"){
  for (i in length(field)){
    fType = names(fields)[i]
    if (is.null(fType)) fType = fieldType
    size = fieldSize(fType)
    AlteryxRDataX::write.AlteryxMetaInfo(nOutput = nOutput, name = f, fieldType = fType,
      size = size, source = source
    )
  }
}

#' @export
aTemplate = function(template){
  system.file('templates', template, package = 'AlteryxRhelper')
}

#' Scaffold a new workflow based on a template
#' 
#' @export
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




