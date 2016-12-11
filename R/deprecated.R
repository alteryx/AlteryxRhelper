# Add README.Rmd with Help
addHelpDoc <- function(out = 'Supporting_Macros/README.Rmd'){
  helpFile = system.file("templates", "help_template.Rmd", package = 'AlteryxRhelper')
  if (file.copy(helpFile, out)){
    message("Copied help template to ", out)
  }
}


isOlder2 <- function(target, ...){
  if (!file.exists(target)){ return(TRUE) }
  any(Filter(Negate(is.na), file.mtime(target) < file.mtime(...)))
}

# Build Plugin
# @param pluginDir directory containing the plugin
# @param build whether or not to run npm build
buildPlugin <- function(pluginDir = ".", build = FALSE){
  yxmc <- list.files(file.path(pluginDir, "Supporting_Macros"), pattern = ".yxmc$", full.names = T)
  pluginName = tools::file_path_sans_ext(basename(yxmc))
  guiFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  configFile <- file.path(pluginDir, sprintf("%sConfig.xml", pluginName))
  rFile <- list.files(file.path(pluginDir, "Supporting_Macros"), pattern = ".R$", full.names = T)
  updated <- FALSE
  if (dir.exists('Gui')){
    to_update <- isOlder2(guiFile, 'Gui/layout.html', 'Gui/overrides.yaml', yxmc)
  } else {
    to_update <-  isOlder2(guiFile, yxmc)
  }
  if (to_update){
    updated <- TRUE
    message("Updating Gui.html and Config.xml")
    createPluginFromMacro(pluginDir)
  }
  if (isOlder2(yxmc, rFile)){
    updated <- TRUE
    insertRcode(yxmc, rFile)
  }
  if (build){
    l <- as.list(append('app.min.js', list.files("App/src", recursive = TRUE)))
    if (do.call('isOlder2', l)){
      withr::with_dir("App", system('npm run build-umd'))
      file.copy('App/dist/src.js', 'app.min.js', overwrite = TRUE)
      updated = TRUE
    }
  }
  if (updated == FALSE){
    message("Nothing to update...")
  } else {
    updateHtmlPlugin(pluginDir)
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

# Create a YXI installable
# 
# 
# @export
# @param pluginDir directory containing the plugin files
# @param toDir directory to write the yxi file to
createYXI0 <- function(pluginDir = ".", toDir = "."){
  toDir <- normalizePath(toDir)
  pluginName = basename(normalizePath(pluginDir))
  dirName = dirname(normalizePath(pluginDir))
  cwd = getwd(); setwd(dirName); on.exit(setwd(cwd));
  files = list.files(pluginName, full.names = F, recursive = TRUE)
  files = files[!grepl('^(Supporting_Macros|App|Gui|library)', files)]
  files = files[!grepl('(README|*.Rproj)', files)]
  files2 = list.files(
    file.path(pluginName, "Supporting_Macros"), 
    pattern = ".yxmc", 
    full.names = TRUE
  )
  files2 = files2[!grepl("(\\.R|\\.bak|\\.md)", files2)]
  filesToCopy = c(file.path(pluginName, files), files2)
  zip(
    file.path(toDir, paste0(pluginName, '.yxi')),
    filesToCopy,
    flags = ""
  )
}


# Install HTML Plugin in Alteryx
# 
# 
# @export
# @param pluginDir plugin directory
# @param alteryxDir alteryx directory
# @param build whether or not to build js files
updateHtmlPlugin <- function(pluginDir = ".", alteryxDir = getOption('alteryx.path'),     build = FALSE){
  pluginName = tools::file_path_sans_ext(basename(normalizePath(pluginDir)))
  with_dir <- function (new, code){
    old <- setwd(dir = new)
    on.exit(setwd(old))
    force(code)
  }
  
  cwd = getwd(); setwd(pluginDir); on.exit(setwd(cwd));
  if (build){
    message("Building app.min.js ...")
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
  files = files[file.mtime(files) > file.mtime(dir(to, full.names = TRUE))]
  message('Copying ', length(files), ' to HtmlPlugins')
  file.copy(files, to, recursive = TRUE)
  
  # Copy Supporting Macro
  supporting_macro <- list.files(
    file.path(".", 'Supporting_Macros'), full.names = TRUE, pattern = '^.*\\.yxmc$'    )
  if (length(supporting_macro) > 0){
    message('Copying macro to Supporting_Macros ...')
    file.copy(
      supporting_macro,
      file.path(alteryxDir, 'bin', 'RuntimeData', 'Macros', 'Supporting_Macros'),
      overwrite = TRUE
    )
  }
}

# Update SVN Folder

# @param pluginDir plugin directory
# @param alteryxDir alteryx directory
# @param build whether or not to build
# @export
updateSvnFolder <- function(pluginDir = ".", alteryxDir = "C://Desktop", 
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
  to = file.path(alteryxDir, 'HtmlPlugins', pluginName)
  if (!(file.exists(to))) {
    dir.create(to, recursive = TRUE)
  }
  
  files = list.files(full.names = F, recursive = TRUE)
  files = files[!grepl('^(Supporting_Macros|App|Gui)', files)]
  file.copy(files, to, recursive = TRUE)
  
  # Copy Supporting Macro
  supporting_macro <- list.files(
    file.path(".", 'Supporting_Macros'), full.names = TRUE, pattern = '^.*\\.yxmc$'    )
  if (length(supporting_macro) > 0){
    file.copy(
      supporting_macro,
      file.path(alteryxDir, 'RuntimeData', 'Macros', 'Supporting_Macros'),
      overwrite = TRUE
    )
  }
}

# Function to update html from sample
#
#
# @export
# @param macro path to macro
updateHtml = function(macro){
  rfile = paste0(macro, '.R')
  mcfile = paste0(macro, '.yxmc')
  mdfile = paste0(macro, '_sample.yxmd')
  htmlfile = paste0(macro, '_sample.html')
  if (isOlder(htmlfile, rfile)){
    if (isOlder(mcfile, rfile)){
      message("Updating macro...")
      insertRcode(mcfile, rfile, mcfile)
    }
    message('Running yxmd...')
    runWorkflow(mdfile)
  }
}

# Build Plugin
# 
# 
# @export
# @param pluginDir directory containing the plugin
# @param build whether or not to run npm build
buildPlugin2 <- function(pluginDir = ".", build = FALSE){
  yxmc <- list.files(
    file.path(pluginDir, "Supporting_Macros"), pattern = ".yxmc$", 
    full.names = T
  )
  pluginName = tools::file_path_sans_ext(basename(yxmc))
  guiFile <- file.path(pluginDir, sprintf("%sGui.html", pluginName))
  configFile <- file.path(pluginDir, sprintf("%sConfig.xml", pluginName))
  rFile <- list.files(
    file.path(pluginDir, "Supporting_Macros"), pattern = ".R$", full.names = T
  )
  updated <- FALSE
  if (dir.exists('Gui')){
    to_update <- isOlder2(guiFile, 'Gui/layout.html', 'Gui/overrides.yaml', yxmc)
  } else {
    to_update <-  isOlder2(guiFile, yxmc)
  }
  if (to_update){
    updated <- TRUE
    message("Updating Gui.html and Config.xml")
    createPluginFromMacro(pluginDir)
  }
  if (isOlder2(yxmc, rFile)){
    updated <- TRUE
    insertRcode(yxmc, rFile)
  }
  if (build){
    l <- as.list(append('app.min.js', list.files("App/src", recursive = TRUE)))
    if (do.call('isOlder2', l)){
      withr::with_dir("App", system('npm run build-umd'))
      file.copy('App/dist/src.js', 'app.min.js', overwrite = TRUE)
      updated = TRUE
    }
  }
  if (updated == FALSE){
    message("Nothing to update...")
  } else {
    copyHtmlPlugin(pluginDir)
  }
}
