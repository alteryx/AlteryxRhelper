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
    default <- match.fun(f1)(unname(x$Default))
    if (f1 %in% c('textInput', 'dropdownInput')) default = paste0("'", default, "'")
    call_ = paste0(f1, '(', x1, " , ", default, ')')
  } else {
    call_ = paste0(f1, '(', x1, ')')
  }
  paste(" ", x$Name, '=', call_)
}

# Make an input string to be inserted at the top of R code
makeInput <- function(template, input){
  xml <- xmlInternalTreeParse(template)
  r <- xmlRoot(xml)
  #g <- getNodeSet(r, '//Question[not(Questions)]')
  g <- getNodeSet(r, '//Question')
  l <- lapply(g, xmlToList)
  l <- l[sort(sapply(l, function(x){x$Name}), index.return = TRUE)$ix]
  x = paste(Filter(Negate(is.null), lapply(l, makeCall)), collapse = ',\n')
  x2 = paste0('## DO NOT MODIFY: Auto Inserted by AlteryxRhelper ----\nlibrary(AlteryxRhelper)\n', input, ' <- list(\n', x, "\n)", "\noptions(alteryx.wd = '%Engine.WorkflowDirectory%')\noptions(alteryx.debug = ", input, "$debug)\n##----")
}
