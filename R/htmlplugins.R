copy_dir <- function (from, to) {
  if (!(file.exists(to))) {
    dir.create(to, recursive = TRUE)
    message("Copying files to ", to, "...")
    file.copy(list.files(from, full.names = T), to, recursive = TRUE)
  }
}

createNewPlugin <- function(plugin_name, template = 'TemplatePlugin'){
  copy_dir(template, plugin_name)
  cwd = getwd(); on.exit(setwd(cwd)); setwd(plugin_name)
  d <- list.files(pattern = paste0("^", template))
  lapply(d, function(f){
    file.rename(f, gsub(template, plugin_name, f))
  })

  d2 <- list.files(pattern = ".xml")
  d2_contents <- readLines(d2, warn = F)
  d2_contents_new <- gsub(template, plugin_name, d2_contents)
  writeLines(d2_contents_new, d2)
}


createYXI <- function(pluginName, fromRoot = '~/Desktop/SNIPPETS/dev'){
  files = list.files(file.path(fromRoot, pluginName), 
    full.names = TRUE,
    pattern = "^[^App]"
  )
  zip(
    paste0(fromRoot, "/", pluginName, '.yxi'),
    files
  )
}


#' Install HTML Plugin in Alteryx
#' 
#' 
#' @export
#' @param pluginDir plugin directory
#' @param alteryxDir alteryx directory
#' @param build whether or not to build js files
updateHtmlPlugin <- function(pluginDir = ".", alteryxDir = getOption('alteryx.path'), 
    build = FALSE){
  pluginName = tools::file_path_sans_ext(basename(normalizePath(pluginDir)))
  with_dir <- function (new, code){
    old <- setwd(dir = new)
    on.exit(setwd(old))
    force(code)
  }
  
  cwd = getwd(); setwd(pluginDir); on.exit(setwd(cwd));
  if (build){
    with_dir('App', system("nwb build-umd"))
    file.copy(file.path('App', 'umd', 'app.min.js'), ".", overwrite = TRUE)
  }
  if (!dir.exists(alteryxDir)){
    stop("The directory to copy the plugin to ", alteryxDir, " does not exist")
  }
  to = file.path(alteryxDir, 'bin', 'HtmlPlugins', pluginName)
  if (!(file.exists(to))) {
    dir.create(to, recursive = TRUE)
  }
  
  files = list.files(full.names = F, recursive = TRUE)
  files = files[!grepl('^(Supporting_Macros|App|Gui)', files)]
  file.copy(files, to, recursive = TRUE)

  # Copy Supporting Macro
  supporting_macro <- list.files(file.path(".", 'Supporting_Macros'), full.names = TRUE,
    pattern = '^.*\\.yxmc$'                               
  )
  if (length(supporting_macro) > 0){
    file.copy(
      supporting_macro,
      file.path(alteryxDir, 'bin', 'RuntimeData', 'Macros', 'Supporting_Macros'),
      overwrite = TRUE
    )
  }
}