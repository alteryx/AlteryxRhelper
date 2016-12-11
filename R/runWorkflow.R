#' Run an Alteryx workflow
#'
#' @param file parameter to pass on to AlteryxEngineCmd
#' @export
runWorkflow = function(file){
  alteryx = getOption("alteryx.path")
  cwd = getwd()
  engine_dir = normalizePath(file.path(alteryx, "bin"))
  param = normalizePath(file, mustWork = TRUE)
  setwd(engine_dir)
  cmd = paste("AlteryxEngineCmd.exe", shQuote(param))
  out = system(cmd, intern = TRUE)
  on.exit({
    message("Exiting function with status ", attr(out, 'status'))
    setwd(cwd)
  })
  return(out)
}

#' @export
runWorkflow2 <- function(file){
  message("Running ", file)
  r <- runWorkflow(file)
  results <- parseResult(r)
  name <- tools::file_path_sans_ext(basename(file))
  modifyList(list(name = name), results)
}


#' Run Workflows in a directory
#' 
#' @param wdir directory containing the workflows to run.
#' @export
runWorkflows <- function(wdir){
  f <- list.files(wdir, pattern = '.yxmd', full.names = TRUE)
  lapply(f, function(x){
    message("Running workflow ", x)
    runWorkflow2(x)
  })
}
