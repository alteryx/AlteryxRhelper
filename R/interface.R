# Question Inputs
numericInput = function(x, default){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    as.numeric(x)
  }
}

checkboxInput = function(x, default = FALSE){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    x == 'True'
  }
}

radioInput = function(x, default = FALSE){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    x == 'True'
  }
}

textInput = function(x, default = ""){
  if (x == "" ||  grepl('^%Question\\.(.*)%$', x) || grepl('^%Engine\\.(.*)%$', x)){
    default
  } else {
    x
  }
}

listInput = function(x, default = '[""]'){
  if (x == ""  ||  grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    #as.list(jsonlite::fromJSON(as.character(x)))
    if (grepl(",", x)) strsplit(x, ",")[[1]] else x
  }
}

dropdownInput = function(x, default = ""){
  if (x == "" || grepl('^%Question\\.(.*)%$', x)){
    default
  } else {
    x
  }
}

selectInput <- function(...){
  x = c(...)
  names(Filter(isTRUE, x))
}


findTrueInput = function(input, root){
  root = paste0('^', root)
  #x = do.call('selectInput', input[grep(root, names(input))])
  x = names(Filter(isTRUE, input[grep(root, names(input))]))
  sub(paste0(root, '\\.'), '', x)
}

findInputs = function(input, root){
  x = input[grepl(paste0("^", root), names(input))]
  names(x) = sub("^(.*)\\.", '', names(x))
  return(x)
}
