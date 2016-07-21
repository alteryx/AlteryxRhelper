#' Insert R code into an Alteryx macro
#'
#' @param template yxmc file to insert code into
#' @param rfile R file containing code to insert
#' @param outFile output file to save the udpated macro to
#' @param n position of the RScript node at which to insert the R code
#'   RScript nodes are numbered sequentially in the order in which they appear in
#'   the macro.
#' @import XML
#' @export
insertRcode <- function(template, rfile, outFile = template,
    n = as.numeric(gsub("^.*([1-9])\\.R$", "\\1", rfile))){
  xml <- xmlInternalTreeParse(template)
  r <- xmlRoot(xml)
  x <- getNodeSet(r, '//Nodes//Node//Properties//RScript')
  if (length(xmlChildren(x[[n]])) > 0){
    removeChildren(x[[n]], .all=TRUE)
  }
  rcode <- paste(paste(readLines(rfile), collapse = '\n'), '\n', collapse = '')
  xmlValue(x[[n]]) <- gsub("#_#", "", rcode)
  saveXML(xml, outFile)
  message('Inserted R code from ', rfile, ' into ', outFile)
}

#' Save R code extracted from an Alteryx Macro
#'
#' @param template yxmc file to extract code from
#' @param outFile output file to save the updated macro to
#' @param extractInput question inputs automatically get extracted to list
#'   object with this name
#' @export
extractRcode <- function(template, outFile = NULL, extractInput = NULL){
  if (is.null(outFile)){
    outFile = tools::file_path_sans_ext(template)
  }
  xml <- xmlInternalTreeParse(template)
  r <- xmlRoot(xml)
  x <- getNodeSet(r, '//Nodes//Node//Properties//RScript')
  rcode <- xmlSApply(x, xmlValue)
  saveTo <- paste0(outFile, seq_along(rcode), ".R")
  invisible(lapply(seq_along(rcode), function(i){
    if (!is.null(extractInput)){
      inputs <- extractQuestionConstants(template, extractInput)
      # rcode_ <- sub("(## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----\n.*\n##----\n)", "", rcode[[i]])
      rcode_ <- sub("(## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----\n.*\n##----\n)", inputs, rcode[[i]])
      #rcode_ <- paste(c(inputs, rcode_), collapse = '\n')
    } else {
      rcode_ = rcode[[i]]
    }
    cat(rcode_, file = saveTo[i])
  }))
}

# runWorkflow = function(yxmd){
#   alteryx = getOption("alteryx.path")
#   cwd = getwd()
#   engine_dir = normalizePath(file.path(alteryx, "bin"))
#   yxmd = normalizePath(yxmd, mustWork = TRUE)
#   setwd(engine_dir)
#   cmd = paste("AlteryxEngineCmd.exe", yxmd)
#   out = system(cmd, intern = TRUE)
#   tfiles = paste0(tools::file_path_sans_ext(basename(yxmd)), "_files")
#   on.exit({
#     message("Exiting function...")
#     setwd(cwd)
#     if (file.exists(tfiles)){
#       message("Deleting temporary assets...")
#       unlink(tfiles, recursive = TRUE)
#     }
#   })
#   return(out)
# }

#' Function to update html from sample
#'
#'
#' @export
#' @param macro path to macro
updateHtml = function(macro){
  rfile = paste0(macro, '.R')
  mcfile = paste0(macro, '.yxmc')
  mdfile = paste0(macro, '_sample.yxmd')
  htmlfile = paste0(macro, '_sample.html')
  if (isOlder(htmlfile, rfile)){
    if (isOlder(mcfile, rfile)){
      message("Updating macro...")
      insertRcode(mcfile, rfile, mcfile)
    }
    message('Running yxmd...')
    runWorkflow(mdfile)
  }
}

#' Run an Alteryx workflow
#'
#' @param file parameter to pass on to AlteryxEngineCmd
#' @export
runWorkflow = function(file){
  alteryx = getOption("alteryx.path")
  cwd = getwd()
  engine_dir = normalizePath(file.path(alteryx, "bin"))
  param = normalizePath(file, mustWork = TRUE)
  setwd(engine_dir)
  cmd = paste("AlteryxEngineCmd.exe", shQuote(param))
  out = system(cmd, intern = TRUE)
  on.exit({
    message("Exiting function...")
    setwd(cwd)
  })
  return(out)
}

