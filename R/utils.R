#' Returns TRUE if called inside an Alteryx R Tool.
#'
#' @export
inAlteryx <- function(){
  #'package:AlteryxRDataX' %in% search()
  exists("AlteryxDataOutput", .GlobalEnv)
}

#' List predictive macros
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
#' @param ... extra arguments to pass on to file.copy
copyPredictiveMacro <- function(macro, to = ".", ...){
  p <- getPathToPredictiveMacros()
  pred_tools_path = normalizePath(p)
  file.copy(file.path(pred_tools_path, macro), to = to, ...)
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
    copyPredictiveMacro(x, file.path(dirs$macros, 'Supporting_Macros'))
  })
  #d <- paste(readLines(macro, warn = F), collapse = '\n')
  #d <- gsub("Supporting_Macros", "Helper_Macros", d)
  #cat(d, file = macro)
}

dirNames <- function(){
  list(
    macros = 'Supporting_Macros',
    extras = 'Extras'
  )
}

#' Extract Icon
#' 
#' @param yxmc path to yxmc file
#' @param out icon file to write out to
extractIcon <- function(yxmc, out){
  doc <- xmlInternalTreeParse(yxmc)
  root <- xmlRoot(doc)
  macroImg <- xmlValue(getNodeSet(root, '//MacroImage')[[1]])
  
  #x <- RCurl::base64Decode(macroImg, "raw")
  x <- base64enc::base64decode(what = macroImg)
  writeBin(x, out)
}

#' Stop
#' 
#' @param msg message
#' @param ... extra arguments to pass on to stop.Alteryx
#' @export
stop.Alteryx <- function(msg, ...){
  if (inAlteryx()){
    AlteryxRDataX::stop.Alteryx(msg, ...)
  } else {
    stop(msg)
  }
}

updateReadme <- function(readme, pluginName){
  input <- paste(readLines(readme, warn = F), collapse = '\n')
  output <- whisker::whisker.render(input, list(pluginName = pluginName))
  cat(output, file = readme)
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

copy_dir <- function (from, to) {
  if (!(file.exists(to))) {
    dir.create(to, recursive = TRUE)
    message("Copying files to ", to, "...")
    file.copy(list.files(from, full.names = T), to, recursive = TRUE)
  }
}

#' Make a circular or square icon and save it as a png file
#'
#' @export
#' 
#' @import grid
#' @param iconPath path to save icon to
#' @param shape shape of the icon (circle or rect)
#' @param fill fill color
#' @param label a label to use for the icon
makeIcon <- function(iconPath, shape = 'circle', fill = sample(colors(), 1), 
                     label = NULL){
  png(iconPath, width = 48, height = 48, units = 'px')
  vp <- viewport(x=0.5,y=0.5,width=1, height=1)
  pushViewport(vp)
  
  if (shape == 'circle'){
    grid.circle(x=0.5, y=0.5, r=0.45, gp = gpar(fill = fill))
  } else {
    grid.rect(x = 0.5, y = 0.5, width = 0.9, height = 0.9, gp = gpar(fill = fill))
  }
  if (!is.null(label)){
    grid.text(label, gp = gpar(col = 'white', cex = 1.5))
  }
  dev.off()
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
