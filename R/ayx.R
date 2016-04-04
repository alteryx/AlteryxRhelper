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
