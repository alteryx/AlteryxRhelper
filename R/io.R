#' Alteryx Read Function
#'
#' This function reads data from an Alteryx input stream. Every time a
#' macro/workflow runs this function, the input data gets saved as an rds file
#' with the prefix ".input", to the macro/workflow directory. This allows the R
#' code to be run outside of Alteryx.
#'
#' @export
#' @param name name
#' @param mode mode
#' @param bIncludeRowNames include row names
#' @param default default
read.Alteryx2 = function(name, mode = "data.frame",
    bIncludeRowNames = FALSE, default){
  inAlteryx = function(){'package:AlteryxRDataX' %in% search()}
  if (inAlteryx()){
    wdir = getOption('alteryx.wd', '%Engine.WorkflowDirectory%')
    requireNamespace('AlteryxRDataX')
    d <- AlteryxRDataX::read.Alteryx(name = name, mode = mode, bIncludeRowNames = FALSE)
    # i need to figure out a way to make things work during debug as well
    # as production. it would be ideal to expose this flag in the macro ui
    # so that an end user can flip the debug mode to inspect further
    if (getOption('alteryx.debug', F)){
      f <- paste0(wdir, '.input', name, '.rds')
      msg <- paste0('Saving input ', name, ' to ', f)
      AlteryxRDataX::AlteryxMessage(msg)
      if (file.exists(f)){file.remove(f)}
      saveRDS(d, file = f)
    }
  } else {
    if (file.exists(f <- paste0('.input', name, '.rds'))){
      d <- readRDS(f)
    } else if (!(missing(default))){
      d <- default
    } else {
      stop(paste("Missing Input", name))
    }
  }
  return(d)
}

#' Alteryx Write Function
#'
#'
#' @export
#' @param data data
#' @param nOutput output connection number
#' @param bIncludeRowNames include row names
#' @param source source
write.Alteryx2 = function(data, nOutput = 1, bIncludeRowNames = FALSE, source = ""){
  if (inAlteryx()){
    requireNamespace('AlteryxRDataX')
    AlteryxRDataX::write.Alteryx(data = data, nOutput = nOutput,
      bIncludeRowNames = bIncludeRowNames, source = source
    )
  } else {
    head(data)
  }
}

#' Alteryx Graph Function
#'
#'
#' @export
#' @param expr expression to generate graph
#' @param nOutput output connection number
#' @param width width
#' @param height height
#' @param ... additional arguments
AlteryxGraph2 = function(expr, nOutput = 1, width = 576, height = 576, ...){
  print_ = function(expr){if (inherits(expr, 'ggplot')){print(expr)} else{expr}}
  if ('package:AlteryxRDataX' %in% search()){
    requireNamespace('AlteryxRDataX')
    AlteryxRDataX::AlteryxGraph(nOutput, width = width, height = height, ...)
    print_(expr)
    invisible(dev.off())
  } else {
    print_(expr)
  }
}
