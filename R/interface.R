#' Question Inputs
#'
#' @export
#' @param x x
#' @param default default
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
    #as.list(jsonlite::fromJSON(as.character(x)))
    if (grepl(",", x)) strsplit(x, ",")[[1]] else x
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
#' @param ... arguments to pass to selectInput
selectInput <- function(...){
  x = c(...)
  names(Filter(isTRUE, x))
}

#' Find inputs that start with a root and have the value TRUE
#'
#'
#' @export
#' @param input input
#' @param root root
findTrueInput = function(input, root){
  root = paste0('^', root)
  #x = do.call('selectInput', input[grep(root, names(input))])
  x = names(Filter(isTRUE, input[grep(root, names(input))]))
  sub(paste0(root, '\\.'), '', x)
}

#' Find inputs that start with a root
#'
#' @export
#' @inheritParams findTrueInput
findInputs = function(input, root){
  x = input[grepl(paste0("^", root), names(input))]
  names(x) = sub("^(.*)\\.", '', names(x))
  return(x)
}
