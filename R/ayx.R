ayxOption <- function(x){
  tag('alteryx-option', x)
}

#' Alteryx Plugin Widget
#' 
#'
#' @import htmltools
#' @param x x
ayxPluginWidget = function(x){
  if (!is.null(x$values)){
    values = lapply(seq_along(x$values), function(i){
      list(
        uiobject = x$values[[i]], 
        dataname = names(x$values)[i],
        default = if(!is.null(x$default)) {
          if(names(x$values)[[i]] == x$default) "true" else NULL
        } else {
          NULL
        }
      )
    })
    x[[length(x) + 1]] <- do.call(tagList, lapply(values, function(d){
      ayxOption(list(d$dataname, uiobject = d$uiobject, default = d$default))
    }))
    x$values <- NULL
    x$default <- NULL
  }
  label <- x$label
  x$label <- NULL
  tagList(
    HTML(paste("<!-- ", x$dataName, " -->")),
    tag('label', label),
    tag('alteryx-pluginwidget', x),
    HTML("  ")
  )
}


# yxmcFile <- system.file('templates', 'sample1.yxmc', package = 'AlteryxRhelper')
# config <- yxmc2yaml(yxmcFile)
# renderAyxWidgets(config)
renderAyxWidgets <- function(config){
  d2 <- lapply(seq_along(config), function(i){
    config[[i]]$dataName = names(config)[i]; 
    config[[i]]
  })
  do.call(tagList, lapply(d2, ayxPluginWidget))
}

makePage <- function(config, layout = NULL){
  #config <- yaml::yaml.load_file(config)
  config <- lapply(seq_along(config), function(i){
    config[[i]]$id = names(config)[i]
    config[[i]]$dataName = names(config)[i]; 
    config[[i]]
  })
  makeFieldSet <- function(x, id){
    tags$fieldset(id = id,
      tags$legend(id),
      do.call(tagList, lapply(x, function(x){
        d_ <- config[[x]]; d_$dataName = x;
        ayxPluginWidget(d_)
      }))             
    )
  }
  if (!is.null(layout)){
    layout <- yaml::yaml.load(layout)
    do.call(tagList, lapply(names(layout), function(k){
      makeFieldSet(layout[[k]], k)
    }))
  } else {
    tags$fieldset(
      do.call(tagList, lapply(config, ayxPluginWidget))
    ) 
  }
}

makeGuiHtml <- function(widgets, pluginName = "", template = NULL){
  if (is.null(template)) {
    template <- system.file('templates', 'GuiTemplate.html', package = 'AlteryxRhelper')
  }
  gui <- htmltools::htmlTemplate(template, widgets = widgets, title = pluginName)
  #writeLines(as.character(gui), sprintf("%sGui.html", pluginName))
  return(gui)
}


writeGuiHtml <- function(pluginDir, htmlFile = NULL, overrides = NULL){
  pluginDir <- normalizePath(pluginDir)
  pluginName <- basename(pluginDir)
  if (is.null(htmlFile)){
    htmlFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  }
  yxmcFile <- file.path(pluginDir, 'Supporting_Macros', sprintf("%s.yxmc", pluginName))
  x1 <- yxmc2yaml(yxmcFile)
  if (file.exists(ov <- file.path(pluginDir, 'Gui', 'overrides.yaml'))){
    overrides <- yaml::yaml.load_file(ov)
  }
  if (!is.null(overrides)){
    x1 <- modifyList(x1, overrides)
  }
  x2 <- makePage(x1)
  x3 <- makeGuiHtml(x2, pluginName = pluginName)
  cat(as.character(x3), file = htmlFile)
}

#' Create Plugin from Macro
#' 
#' 
#' @export
#' @param pluginDir plugin directory
#' @param overrides should an override file be used
#' @param layout should a layout file be used
#' @import yaml
createPluginFromMacro <- function(pluginDir = ".", overrides = NULL, layout = NULL){
  pluginDir = normalizePath(pluginDir)
  pluginName <- basename(pluginDir)
  yxmcFile <- file.path(pluginDir, 'Supporting_Macros', sprintf("%s.yxmc", pluginName))
  if (is.null(layout)){
    if (file.exists(l <- file.path(pluginDir, 'Gui', 'layout.html'))){
      layout = TRUE
    }
  }
  if (is.null(layout)){
    writeGuiHtml(pluginDir, overrides = overrides)
  } else {
    writeGuiHtmlFromLayout(pluginDir, overrides = overrides) 
  }
  yxmc2PluginConfig(yxmcFile, saveToFile = paste0(pluginName, "Config.xml"))
  if (!file.exists(icon <- paste0(pluginName, "Icon.png"))){
    makeIcon(icon)
  }
}

writeGuiHtmlFromLayout <- function(pluginDir, htmlFile = NULL, overrides = NULL){
  pluginDir <- normalizePath(pluginDir)
  pluginName <- basename(pluginDir)
  if (is.null(htmlFile)){
    htmlFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  }
  mylayout <- paste(readLines(file.path(pluginDir, 'Gui', 'layout.html')), collapse = '\n')
  yxmcFile <- file.path(pluginDir, 'Supporting_Macros', sprintf("%s.yxmc", pluginName))
  x1 <- yxmc2yaml(yxmcFile)
  if (file.exists(ov <- file.path(pluginDir, "Gui", "overrides.yaml"))){
    overrides <- yaml::yaml.load_file(ov)
  }
  if (!is.null(overrides)){
    x1 <- modifyList(x1, overrides)
  }
  x1b <- lapply(seq_along(x1), function(i){
    x1[[i]]$id = names(x1)[i]
    x1[[i]]
  })
  names(x1b) <- names(x1)
  w = renderAyxWidgets(x1b)
  names(w) = names(x1b)
  
  htmlTextTemplate = function(...){
    htmlTemplate(text_ = mylayout, ...)
  }
  x2 <- do.call(htmlTextTemplate, w)
  x3 <- makeGuiHtml(x2, pluginName = pluginName)
  cat(as.character(x3), file = htmlFile)
}
