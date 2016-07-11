#' Run all tests in the Supporting_Macros/tests folder
#' 
#' 
#' 
#' @param pluginDir plugin directory
#' @export
runTests <- function(pluginDir = ".", build_doc = TRUE){
  dirs <- dirNames()
  with_dir_(file.path(pluginDir, dirs$extras, 'Tests'), {
    tests <- list.files(".",  pattern = '.yxmd')
    results <- lapply(seq_along(tests), function(i){
      message("Testing ", tools::file_path_sans_ext(basename(tests[i])))
      runWorkflow(tests[i])
    })
    names(results) <- basename(tests)
    y <- lapply(results, function(x){
      ifelse(is.null(attr(x, 'status')), 0, attr(x, 'status'))
    })
    x <- jsonlite::toJSON(y, auto_unbox = TRUE)
    cat(x, file = '_tests.json')
    if (build_doc) {
      rmarkdown::render('README.Rmd')
      browseURL('README.html')
    }
  })
}

with_dir_ <- function (new, code) {
  old <- setwd(dir = new)
  on.exit(setwd(old))
  force(code)
}
