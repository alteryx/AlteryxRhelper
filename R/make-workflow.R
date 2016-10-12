df2TextInput <- function(x){
  config <- read_xml("<Configuration />")
  xml_add_child(config, "NumRows", value = as.character(NROW(x)))
  fields <- xml_add_child(config, "Fields")
  for (name in names(x)){
    xml_add_child(fields, "Field", name  = name)
  }
  data = xml_add_child(config, "Data")
  for (i in 1:NROW(x)){
    row = xml_add_child(data, "r")
    for(j in 1:NCOL(x)){
      xml_add_child(row, "c", as.character(x[i,j]))
    }
  }
  config
}

#' @import xml2
config2xml <- function(props, collapse = ","){
  config <- read_xml("<Configuration />")
  for (k in names(props)){
    xml_add_child(config, "Value", name = k, paste(props[[k]], collapse = collapse))
  }
  config
}

#' @export
makeWorkflow <- function(template, data, config, inputs_id, config_id, 
    comment_id = NULL, comment = NULL, outFile, collapse = ","){
  getTool <- function(id){
    sprintf("//Node[@ToolID = '%s']/Properties/Configuration", id)
  }

  doc <- read_xml(template)
  n2 <- xml_find_first(doc, getTool(inputs_id))
  textinput <- df2TextInput(data)
  invisible(xml_replace(n2, textinput))

  n3 <- xml_find_first(doc, getTool(config_id))
  xmlConfig <- config2xml(config, collapse = collapse)
  invisible(xml_replace(n3, xmlConfig))
  
  if (!is.null(comment_id)){
    n4 <- xml_find_first(doc, getTool(comment_id))
    n5 <- xml_child(n4)
    xml_text(n5) <- comment
  }
  write_xml(doc, outFile)
}

#' @export
makeWorkflow2 <- function(template, repl, outFile, collapse = ","){
  getTool <- function(id){
    sprintf("//Node[@ToolID = '%s']/Properties/Configuration", id)
  }
  doc <- read_xml(template)
  lapply(repl, function(r){
    if (r$type == 'input'){
      n2 <- xml_find_first(doc, getTool(r$node))
      textinput <- df2TextInput(r$data)
      invisible(xml_replace(n2, textinput))
    } else if (r$type == 'config'){
      n3 <- xml_find_first(doc, getTool(r$node))
      xmlConfig <- config2xml(r$data, collapse = collapse)
      invisible(xml_replace(n3, xmlConfig))
    } else if (r$type == 'text'){
      n4 <- xml_find_first(doc, getTool(r$node))
      n5 <- xml_child(n4)
      xml_text(n5) <- r$data
    }
  })
  write_xml(doc, outFile)
}
