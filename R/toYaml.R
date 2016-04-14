toYaml = function(type, g){
  UseMethod('toYaml')
}

toYaml.FileBrowse = function(type, g){
  x = xmlToList(g)
  list(
    label = x$Description,
    dataName = x$Name,
    type = 'FileBrowse'
  )
}

bool_map <- function(x){
  c(True = TRUE, False = FALSE)[[x]]
}

toYaml.TextBox = function(type, g){
  x = xmlToList(g)
  list(
    label = x$Description,
    dataName = x$Name,
    type = 'TextBox',
    password = tolower(x$Password[['value']])
  )
}

toYaml.BooleanGroup = function(type, g){
  x = xmlToList(g)
  list(
    dataName = x$Name,
    type = "CheckBox",
    text = x$Description,
    default = bool_map(x$Default[['value']])
  )
}

toYaml.NumericUpDown = function(type, g){
  x = xmlToList(g)
  list(
    label = x$Description,
    dataName = x$Name,
    type = "NumericSpinner",
    default = x$Default[[1]],
    minimum = x$Minimum[[1]],
    maximum = x$Maximum[[1]],
    step = x$Increment[[1]]
  )
}

toYaml.ListBox = function(type, g){
  x = xmlToList(g)
  out <- list(
    label = x$Description,
    dataName = x$Name
  )
  cg <- xmlChildren(g)
  if ("Manual_Values" %in% names(cg)){
    if (x$Multiple[['value']] == "True"){
      out <- append(out, list(
        type = 'MultiSelectListBox',
        dataType = 'MultiStringSelector'
      ))
    } else {
      out <- append(out, list(type = 'DropDown'))
    }
    values = toKeyValuePairs(xmlValue(cg$Manual_Values))
    value = x$Default
    append(out, list(    
      default = value,
      values = values
    ))
  } else {
    append(out, list(
      type = "DropDown",
      dataType = if (x$Multiple[['value']] == "True") {
        "FieldSelectorMulti" 
      } else {
        "FieldSelector"
      },
      inputNumber = "0",
      connectionNumber = "0",
      fieldType = "All"
    ))
  }
}

toYaml.Date <- function(type, g){
  x = xmlToList(g)
  list(
    label = x$Description,
    dataName = x$Name,
    type = "DateTimeField",
    dateFormat = "YYYY-MM-DD"
  )
}

toKeyValuePairs <- function(x){
  y <- strsplit(x, "\n")[[1]]
  if (grepl(":", y[1])){
    y <- strsplit(y, ":\\s*")
    as.list(setNames(sapply(y, '[[', 1), sapply(y, '[[', 2)))
  } else {
    as.list(setNames(y, y))
  }
}

renderToYaml <- function(g){
  x = xmlToList(g)
  types = c("FileBrowse", "TextBox", "BooleanGroup", "ListBox", "NumericUpDown", "Date")
  if (x$Type %in% types){
    toYaml(structure(x$Type, class = x$Type), g)
  } else {
    NULL
  }
}

# template <- system.file("templates", "sample1.yxmc", package = 'AlteryxRhelper')
# d <- yxmc2yaml(template)
yxmc2yaml <- function(template, asYaml = F){
  xml <- xmlInternalTreeParse(template)
  r <- xmlRoot(xml)
  g <- getNodeSet(r, '//Question')
  l <- lapply(g, xmlToList)
  m <- Filter(Negate(is.null), lapply(g, renderToYaml))
  names(m) <- sapply(m, '[[', 'dataName')
  m <- lapply(m, function(x){x$dataName = NULL; x})
  if (asYaml) yaml::as.yaml(m) else m
}
