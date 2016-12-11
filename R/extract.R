#' Extract annotation from Inputs
#'
#'
#' @param pluginName path to yxmc file
#' @param type whether to extract from input or output
#' @export
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
#' @export
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

#' Extract question constants and return code that can be inserted
#' 
#' @param path to macro from which to extract question constants
#' @param input variable to assign the question constants to
#' @export
extractQuestionConstants <- function(macro, input = 'config'){
  xml <- xmlInternalTreeParse(macro)
  r <- xmlRoot(xml)
  #g <- getNodeSet(r, '//Question[not(Questions)]')
  g <- getNodeSet(r, '//Question')
  l <- lapply(g, xmlToList)
  l <- l[sort(sapply(l, function(x){x$Name}), index.return = TRUE)$ix]
  x = paste(Filter(Negate(is.null), lapply(l, makeCall)), collapse = ',\n')
  paste0('## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----\nlibrary(AlteryxPredictive)\n', input, ' <- list(\n', x, "\n)", "\noptions(alteryx.wd = '%Engine.WorkflowDirectory%')\noptions(alteryx.debug = ", input, "$debug)\n##----")
}

# Make a question constant
makeQ <- function(nm){
  paste0("'%Question.", nm, "%'")
}

# Make a string
makeCall <- function(x){
  f = c(
    NumericUpDown = "numericInput",
    RadioGroup = "radioInput",
    BooleanGroup = "checkboxInput",
    TextBox = "textInput",
    ListBox = 'listInput'
  )
  f1 = unname(f[x$Type])
  if (is.na(f1)){return(NULL)}
  if (f1 == 'listInput'){
    if (x$Multiple['value'] == "False"){
      f1 = 'dropdownInput'
    }
  }
  x1 = makeQ(x$Name)
  default = x$Default
  if (f1 != 'listInput' && !is.null(default <- x$Default)){
    f2 <- getFromNamespace(f1, 'AlteryxRhelper')
    default <- f2(unname(x$Default))
    if (f1 %in% c('textInput', 'dropdownInput')) default = paste0("'", default, "'")
    call_ = paste0(f1, '(', x1, " , ", default, ')')
  } else {
    call_ = paste0(f1, '(', x1, ')')
  }
  paste0(" `", x$Name, '` = ', call_)
}

#' Extract Icon
#' 
#' @param yxmc path to yxmc file
#' @param out icon file to write out to
extractIcon <- function(yxmc, out){
  doc <- xmlInternalTreeParse(yxmc)
  root <- xmlRoot(doc)
  macroImg <- xmlValue(getNodeSet(root, '//MacroImage')[[1]])
  x <- base64enc::base64decode(what = macroImg)
  writeBin(x, out)
}

#' Make a circular or square icon and save it to a png file.
#'
#' 
#' @param iconPath path to save icon to
#' @param shape shape of the icon (circle or rect)
#' @param fill fill color
#' @param label a label to use for the icon
#' @import grid
#' @export
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

