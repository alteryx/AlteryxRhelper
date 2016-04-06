ayxOption <- function(x){
  tag('alteryx-option', x)
}

#' @export
#' @import htmltools
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

#' @export
#' 
#' 
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

#' @export
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

#' @export
makeGuiHtml <- function(widgets, pluginName = "", template = NULL){
  if (is.null(template)) {
    template <- system.file('templates', 'GuiTemplate.html', package = 'AlteryxRhelper')
  }
  gui <- htmltools::htmlTemplate(template, widgets = widgets, title = pluginName)
  #writeLines(as.character(gui), sprintf("%sGui.html", pluginName))
  return(gui)
}

#' @export
writeGuiHtml <- function(yxmcFile, htmlFile, overrides = NULL){
  x1 <- yxmc2yaml(yxmcFile)
  if (file.exists(ov <- file.path(dirname(yxmcFile), 'overrides.yaml'))){
    overrides <- yaml::yaml.load_file(ov)
  }
  if (!is.null(overrides)){
    x1 <- modifyList(x1, overrides)
  }
  x2 <- makePage(x1)
  x3 <- makeGuiHtml(x2, pluginName = tools::file_path_sans_ext(basename(yxmcFile)))
  cat(as.character(x3), file = htmlFile)
}

#' @export
createPluginFromMacro <- function(yxmcFile, overrides = NULL, layout = NULL){
  pluginName <- tools::file_path_sans_ext(basename(yxmcFile))
  if (is.null(layout)){
    writeGuiHtml(yxmcFile, paste0(pluginName, "Gui.html"), overrides = overrides)
  } else {
    writeGuiHtmlFromLayout(yxmcFile, paste0(pluginName, "Gui.html"), overrides = overrides) 
  }
  yxmc2PluginConfig(yxmcFile, saveTo = paste0(pluginName, "Config.xml"))
  if (!file.exists(icon <- paste0(pluginName, "Icon.png"))){
    makeIcon(icon)
  }
}


#' @export
writeGuiHtmlFromLayout <- function(yxmcFile, htmlFile, overrides = NULL){
  mylayout <- paste(readLines(file.path(dirname(yxmcFile), 'layout.html')), collapse = '\n')
  x1 <- yxmc2yaml(yxmcFile)
  if (file.exists(ov <- file.path(dirname(yxmcFile), 'overrides.yaml'))){
    overrides <- yaml::yaml.load_file(ov)
  }
  if (!is.null(overrides)){
    x1 <- modifyList(x1, overrides)
  }
  w = renderAyxWidgets(x1)
  names(w) = names(x1)
  
  htmlTextTemplate = function(...){
    htmlTemplate(text_ = mylayout, ...)
  }
  x2 <- do.call(htmlTextTemplate, w)
  x3 <- makeGuiHtml(x2, pluginName = tools::file_path_sans_ext(basename(yxmcFile)))
  cat(as.character(x3), file = htmlFile)
}
