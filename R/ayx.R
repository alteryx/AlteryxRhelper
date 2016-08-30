ayxOption <- function(x){
  htmltools::tag('alteryx-option', x)
}

# Alteryx Plugin Widget
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
    if (x$type != 'CheckBox') tags$label(label, `for` = x$id) else NULL,
    tag('alteryx-pluginwidget', x)
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
      tags$legend('Configuration'),
      do.call(tagList, lapply(config, ayxPluginWidget))
    ) 
  }
}

makeGuiHtml <- function(widgets, pluginName = "", template = NULL){
  if (is.null(template)) {
    template <- system.file('templates', 'GuiTemplate.html', package = 'AlteryxRhelper')
  }
  gui <- htmltools::htmlTemplate(template, widgets = widgets, title = pluginName)
  return(gui)
}


writeGuiHtml <- function(pluginDir, htmlFile = NULL, overrides = NULL){
  dirs <- dirNames()
  pluginDir <- normalizePath(pluginDir)
  pluginName <- basename(pluginDir)
  if (is.null(htmlFile)){
    htmlFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  }
  yxmcFile <- file.path(
    pluginDir, dirs$macros, sprintf("%s.yxmc", pluginName)
  )
  layoutDir <- file.path(pluginDir, dirs$extras, 'Gui')
  if (!dir.exists(layoutDir)){
    dir.create(layoutDir)
  }
  layoutFile = file.path(layoutDir, 'layout.html.sample')
  x1 <- yxmc2yaml(yxmcFile)
  x1b <- paste(
    c(
      "<fieldset>", 
      "<legend>Configuration</legend>",
      paste0("{{ ", "`", names(x1), "`", " }}"), 
      "</fieldset>\n"
    ), 
    collapse = '\n'
  )
  cat(x1b, file = layoutFile)
  if (file.exists(ov <- file.path(pluginDir, dirs$extras, 'Gui', 'overrides.yaml'))){
    overrides <- yaml::yaml.load_file(ov)
  }
  if (!is.null(overrides)){
    x1 <- modifyList(x1, overrides)
  }
  x2 <- makePage(x1)
  x3 <- makeGuiHtml(x2, pluginName = pluginName)
  cat(as.character(x3), file = htmlFile)
}

writeGuiHtmlFromLayout <- function(pluginDir, htmlFile = NULL, overrides = NULL){
  dirs <- dirNames()
  pluginDir <- normalizePath(pluginDir)
  pluginName <- basename(pluginDir)
  if (is.null(htmlFile)){
    htmlFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  }
  mylayout <- paste(
    readLines(file.path(pluginDir, dirs$extras, 'Gui', 'layout.html')), 
    collapse = '\n'
  )
  yxmcFile <- file.path(
    pluginDir, dirs$macros, sprintf("%s.yxmc", pluginName)
  )
  x1 <- yxmc2yaml(yxmcFile)
  if (file.exists(ov <- file.path(pluginDir, dirs$extras, "Gui", "overrides.yaml"))){
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
