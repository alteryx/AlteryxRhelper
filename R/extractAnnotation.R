#' Extract annotation from Inputs
#'
#'
#' @param pluginName path to yxmc file
#' @param type whether to extract from input or output
extractInputOutput <- function(pluginName, type = "input"){
  if (type == 'input'){
    y <- 'AlteryxBasePluginsGui.MacroInput.MacroInput'
  } else {
    y <- 'AlteryxBasePluginsGui.MacroOutput.MacroOutput'
  }
  xml <- xmlInternalTreeParse(pluginName)
  r <- xmlRoot(xml)
  query <- sprintf("//Node[GuiSettings[contains(@Plugin, '%s')]]//Properties//Annotation//AnnotationText", y)
  g <- getNodeSet(r, query)
  annotation <- xmlSApply(g, xmlValue)
  d <- paste0(seq_along(annotation), ". ", annotation)
  cat(paste(d, collapse = '\n'))
}

#' Extract configuration annotation from macro
#' 
#' 
#' @param pluginName path to yxmc file
extractConfig <- function(pluginName){
  xml <- xmlInternalTreeParse(pluginName)
  r <- xmlRoot(xml)
  query <- "//Node[GuiSettings[contains(@Plugin, 'AlteryxGuiToolkit.Questions')]]//Properties//Annotation"
  g <- getNodeSet(r, query)
  
  annotation_ <- xmlSApply(g, function(x){
    d <- xmlChildren(x)
    annotation = xmlValue(d$AnnotationText)
    name = xmlValue(d$Name)
    if (is.na(annotation)) NULL else (setNames(annotation, name))
  })
  
  annotation = as.list(unlist(Filter(Negate(is.null), annotation_)))
  d2 <- yxmc2yaml(pluginName)
  d3 <- lapply(names(d2), function(x){
    if (x %in% names(annotation)){
      d2[[x]]$note <- annotation[[x]]
    }
    return(d2[[x]])
  })
  
  d4 <- Filter(function(d){!is.null(d$note)}, d3)
  d5 <- sapply(seq_along(d4), function(i){
    x = d4[[i]]
    sprintf("%s. __%s:__ %s", i, if (is.null(x$label)) x$text else x$label, x$note)
  })
  cat(paste(d5, collapse = "\n"))
}
