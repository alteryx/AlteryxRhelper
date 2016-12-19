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

#' Syntactic sugar for insertRcode to cover the most common usage pattern.
#' @export
insertRcode2 <- function(pluginDir = "."){
  name = tools::file_path_sans_ext(dir(pattern = '.Rproj'))
  rFiles = list.files('Supporting_Macros', pattern = '\\.R$')
  for (rFile in rFiles){
    insertRcode(
      sprintf("Supporting_Macros/%s.yxmc", name),
      sprintf("Supporting_Macros/%s", rFile)
    )
  }
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
