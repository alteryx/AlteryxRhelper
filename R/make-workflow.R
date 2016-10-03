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
makeWorkflow <- function(template, data, config, inputs_id, config_id, outFile, 
    collapse = ","){
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
  write_xml(doc, outFile)
}
