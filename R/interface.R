#' Question Inputs
#'
#' @export
#' @rdname questions
numericInput = function(x, default){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    as.numeric(x)
  }
}

#' @export
#' @rdname questions
checkboxInput = function(x, default = FALSE){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    x == 'True'
  }
}

#' @export
#' @rdname questions
radioInput = function(x, default = FALSE){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    x == 'True'
  }
}

#' @export
#' @rdname questions
textInput = function(x, default = '[""]'){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x) || grepl('^%Engine\\.(.*)%$', x)){
    default
  } else {
    x
  }
}

#' @export
#' @rdname questions
listInput = function(x, default = '[""]'){
  if (x == ""  ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    as.list(jsonlite::fromJSON(as.character(x)))
  }
}

#' @export
#' @rdname questions
dropdownInput = function(x, default = ""){
  if (x == "" || grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    x
  }
}


#' @export
#' @rdname questions
selectInput <- function(...){
  x = c(...)
  names(Filter(isTRUE, x))
}

#' Find inputs that start with a root and have the value TRUE
#'
#' @export
findTrueInput = function(input, root){
  root = paste0('^', root)
  #x = do.call('selectInput', input[grep(root, names(input))])
  x = names(Filter(isTRUE, input[grep(root, names(input))]))
  sub(paste0(root, '\\.'), '', x)
}

#' Find inputs that start with a root
#'
#' @export
findInputs = function(input, root){
  x = input[grepl(paste0("^", root), names(input))]
  names(x) = sub("^(.*)\\.", '', names(x))
  return(x)
}
