#' Make Tutorial
#' 
#' 
#' @export
makeTutorial <- function(docx, title = "Tutorial", 
    preview = FALSE, standalone = FALSE){
  require(rvest)
  tf <- tempfile(fileext = '.html')
  rmarkdown::pandoc_convert(
    input = docx, to = 'html', output = tf, options = c("--section-divs", "--mathml", "--html-q-tags")
  )
  doc <- read_html(tf)
  
  annotation <- doc %>% html_nodes("div.section")
  name <- doc %>% html_nodes("h2") %>% html_text()
  
  g4 <- data.frame(
    step_title = as.character(name[-1]),
    step_content = as.character(annotation[-1])
  )
  
  tutorial <- list(
    title = as.character(name[1]),
    steps = as.character(annotation[-1]),
    steptitles = as.character(name[-1])
  )
  
  library(whisker)
  tplFile <- system.file('starterkit', 'tutorial', 'tpl.html', package = 'AlteryxRhelper')
  tpl <- paste(readLines(tplFile), collapse = '\n')
  d <- whisker.render(
    tpl,
    list(
      tutorial = jsonlite::toJSON(tutorial, auto_unbox = TRUE),
      assetDir = system.file('starterkit', 'tutorial', package = 'AlteryxRhelper'),
      title = title
    )
  )
  cat(as.character(d), file = 'index.html')
  if (preview){
    app <- servr::httw(daemon = TRUE)
    return(app)
  }
  if (standalone){
    htmlwidgets:::pandoc_self_contained_html('index.html', 'index_self.html')
  }
}

