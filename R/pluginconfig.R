#' Make a circular or square icon and save it as a png file
#'
#' @export
#' 
#' @import grid
#' @param iconPath path to save icon to
#' @param shape shape of the icon (circle or rect)
#' @param fill fill color
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

# Get input/output configuration information from a macro
# template <- "~/Desktop/SNIPPETS/dev/Optimization/Supporting_Macros/Optimization.yxmc"
# x = getIO(template)
getIO <- function(template){
  xml <- XML::xmlInternalTreeParse(template)
  r <- XML::xmlRoot(xml)
  g <- getNodeSet(r, '//Question')
  l <- lapply(g, XML::xmlToList)
  getMacroIO <- function(type = 'MacroInput'){
    toolIds = unlist(Filter(Negate(is.null), sapply(l, function(d){
      if (d$Type == type) d$ToolId[['value']]
    })))
    inputs <- lapply(toolIds, function(i){
      query = sprintf('//Node[@ToolID="%s"]//Properties//Configuration', i)
      node = getNodeSet(r, query)
      XML::xmlToList(node[[1]])[c('Name', 'Abbrev', 'Optional')]
    })
    lapply(inputs, function(x){
      if (is.null(x$Abbrev)){
        x$Abbrev = ""
      } else {
        x$Abbrev = gsub("\n|\\s+", "", x$Abbrev)
      }
      return(x)
    })
  }
  help <- getNodeSet(r, '//MacroCustomHelpLink')
  helpLink = if (length(help) > 0){
    xmlValue(help[[1]])
  } else {
    ""
  }
  list(
    inputs = getMacroIO('MacroInput'),
    outputs = getMacroIO('MacroOutput'),
    pluginName = tools::file_path_sans_ext(basename(template)),
    #properties = getNodeSet(r, '//Properties//MetaInfo[not(@connection)]')
    # properties = Filter(
    #     Negate(is.null),
    #     XML::xmlToList(getNodeSet(r, "//Properties//MetaInfo[not(@connection)]")[[1]])
    # ),
    properties = getProperties(r),
    helpLink = helpLink
  )
}


#' Make plugin configuration xml file
#' 
#' 
#' @import XML
#' @export
#' @param inputs inputs
#' @param outputs outputs
#' @param pluginName pluginName
#' @param properties properties
#' @examples 
#' x = list(
#'   inputs = list(
#'     list(Name = 'Input A', Abbrev = "A"),
#'     list(Name = 'Input B', Abbrev = "B")
#'   ),
#'   outputs = list(
#'     list(Name = 'Output C', Abbrev = "C")
#'   ),
#'   pluginName = 'Foo'
#' )
#' do.call(makePluginConfig, x)
makePluginConfig <- function(inputs, outputs, pluginName, properties = NULL, 
    helpLink = ""){
  dirs <- dirNames()
  # Create Config XML
  #d = suppressWarnings(XML::xmlTree())
  #d$addNode("AlteryxJavaScriptPlugin", close = FALSE)
  d = suppressWarnings(XML::xmlTree("AlteryxJavaScriptPlugin"))
  d$addNode("EngineSettings", attrs= list(
    EngineDLL = "Macro",
    EngineDLLEntryPoint = sprintf("%s/%s/%s.yxmc", pluginName, dirs$macros, pluginName),
    SDKVersion = "10.1"
  ))
  d$addNode("GuiSettings", attrs = list(
    Html = sprintf("%sGui.html", pluginName),
    Icon = sprintf("%sIcon.png", pluginName),
    SDKVersion = "10.1",
    Help = helpLink
  ), close = F)
  d$addNode("InputConnections", .children = sapply(inputs, function(x){
    d$addNode("Connection", attrs = list(
      Name = x$Name,
      AllowMultiple = "False",
      Optional = if (is.null(x$Optional)) "False" else x$Optional,
      Type = "Connection",
      Label = if (x$Abbrev == "NULL") "" else x$Abbrev
    ))
  }))
  d$addNode("OutputConnections", .children = sapply(outputs, function(x){
    d$addNode("Connection", attrs = list(
      Name = x$Name,
      AllowMultiple = "False",
      Optional = "False",
      Type = "Connection",
      Label = if (x$Abbrev == "NULL") "" else x$Abbrev
    ))
  }))
  d$closeNode()
  if (!is.null(properties)){
    if (properties[['Name']] == '{{ pluginName }}'){
      properties[['Name']] = pluginName
    }
    d$addNode("Properties", close = F)
    d$addNode(
      "MetaInfo", 
      .children = sapply(names(properties), function(k){
        d$addNode(k, properties[[k]])
      })
    )
  }
  d$closeNode()
  d$value() 
}

#' Macro to Plugin Config
#' 
#' 
#' @export
#' @param yxmcFile path to macro
#' @param saveToFile file to save config.xml to
yxmc2PluginConfig <- function(yxmcFile, saveToFile = NULL){
  x <- getIO(yxmcFile)
  y <- do.call(makePluginConfig, x)
  if (!is.null(saveToFile)){
    saveXML(y, file = saveToFile)
    return(saveToFile)
  } else {
    return(y)
  }
}

getProperties <- function(r){
  props <- getNodeSet(r, "//Properties//MetaInfo[not(@connection)]//*")
  nms <- xmlSApply(props, xmlName)
  vals <- xmlSApply(props, xmlValue)
  setNames(as.list(vals), nms)
}
